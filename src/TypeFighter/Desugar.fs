// Source-to-source desugaring between parse and solve. Three rewrites:
//
//   1. Stream-argument hoisting (ANF): a stream-typed expression (Var of a
//      StreamValue like `count`, or a saturated StreamFn call like
//      `ema(i, 0.5)`) used in an ARGUMENT position is hoisted into a fresh
//      let in the enclosing statement chain. The existing step-let lift in
//      the solver and the emit then handle it unchanged:
//          ema(count, 0.5) * 2
//      becomes
//          let __tfv0 = count
//          let __tfv1 = ema(__tfv0, 0.5)
//          __tfv1 * 2
//      Exception ("don't desugar"): when the callee is a prelude binding
//      whose declared parameter at that position is itself step-record
//      shaped, the stream is passed through un-hoisted.
//
//   2. UFCS method-call syntax, resolved purely by SCOPE: `recv.F(args)`
//      rewrites to `F(recv, args)` whenever F is a visible callable (user
//      fn-binding, prelude fn, or via the namespace member index). The
//      receiver's shape plays no role, so any expression works as a
//      receiver -- literals, call results, lambda parameters, aliases --
//      and chains like `"Ada".greet().shout()` compose freely. Conceptual
//      model: a record field IS a function (the accessor { F: a & r } -> a);
//      `x.F` always means "apply F to x", and a visible callable simply
//      shadows the implicit accessor. Edge cases (documented): a field
//      named like a visible callable is unreachable via CALL syntax
//      (ordinary shadowing -- read it without call parens instead); plain
//      `x.F` without parens always stays a field access.
//
//   3. Dotted namespaces: `math.ema(...)` rewrites to the flat prelude
//      name `__math_ema(...)` (both as a call head and first-class).
//      Namespaces are plain names — a user binding shadows them.
//
// The pass is identity-preserving: unchanged subtrees keep their object
// identity, and every rebuilt or synthetic node is recorded in a
// (new -> old) remap so the parser's reference-identity range map keeps
// working for error squiggles and hovers.
module PureState.TypeFighter.Desugar

open PureState.TypeFighter
open PureState.TypeFighter.StepRecord

type Oracle =
    { kinds: StreamKinds
      // names that are callable (prelude fns); user fn-bindings are
      // discovered while walking
      callables: Set<string>
      // prelude fns with declared step-record params: 0-based positions
      // that must NOT be hoisted
      streamParams: Map<string, Set<int>>
      // dotted namespaces: ns -> member -> flat prelude name
      namespaceMembers: Map<string, Map<string, string>> }

type Result =
    { expr: Expr<unit>
      // newNode -> oldNode, reference-identity keyed. Follow transitively.
      remap: System.Collections.Generic.Dictionary<obj, obj> }

// Per-scope classification carried down the walk.
type private Scope =
    { kinds: StreamKinds
      callables: Set<string>
      // declared step-record parameter positions of prelude fns; entries
      // drop out when a user binding shadows the name
      streamParams: Map<string, Set<int>>
      // names bound by stream-lets (= lifted per-tick values)
      lifted: Set<string>
      // dotted namespaces in scope; entries drop out when a user
      // binding shadows the namespace name
      namespaces: Map<string, Map<string, string>>
      // inside a step-shaped record literal hoisting is disabled
      noHoist: bool }

let run (oracle: Oracle) (expr: Expr<unit>) : Result =
    let remap = System.Collections.Generic.Dictionary<obj, obj>(HashIdentity.Reference)
    let mapNew (newNode: Expr<unit>) (oldNode: Expr<unit>) =
        if not (obj.ReferenceEquals(newNode, oldNode)) then
            remap.[box newNode] <- box oldNode
        newNode

    let used = usedIdents expr
    let mutable freshIdx = 0
    let freshName () =
        let mutable n = sprintf "__tfv%d" freshIdx
        freshIdx <- freshIdx + 1
        while used.Contains n do
            n <- sprintf "__tfv%d" freshIdx
            freshIdx <- freshIdx + 1
        n

    let scope0 =
        { kinds = oracle.kinds
          callables = oracle.callables
          streamParams = oracle.streamParams
          lifted = Set.empty
          namespaces = oracle.namespaceMembers
          noHoist = false }

    // Is F a callable binding name in scope?
    let isCallable (scope: Scope) (name: string) =
        scope.callables.Contains name
        || (match scope.kinds.TryFind name with
            | Some (BindingKind.StreamFn _) -> Some true
            | _ -> None) = Some true

    // `ns.member` where ns is an in-scope namespace: the flat prelude name.
    let nsFlatName (scope: Scope) (source: Expr<unit>) (fname: string) =
        match source with
        | Expr.Var v ->
            scope.namespaces.TryFind v.ident
            |> Option.bind (fun members -> members.TryFind fname)
        | _ -> None

    // UFCS target: a direct callable keeps its name; otherwise the method
    // name resolves through the namespace member index (first namespace
    // in map order wins on ambiguity).
    let ufcsTarget (scope: Scope) (fname: string) =
        if isCallable scope fname then Some fname
        else
            scope.namespaces
            |> Map.toSeq
            |> Seq.tryPick (fun (_, members) -> members.TryFind fname)

    // The dual of hoisting: a plain VALUE expression used where the callee
    // declares a step-record parameter becomes a constant-state stream that
    // re-evaluates the expression per tick (monadic return/pure):
    //   { __init: UnitValue, __step: <s> => { val: <expr>, nextState: UnitValue } }
    let wrapAsStream (e: Expr<unit>) : Expr<unit> =
        let unitVal () = Expr.Var {| ident = BuiltinValues.unitValueIdent; tvar = () |}
        let resultRecord =
            Expr.MkRecord
                {| items =
                    [ RecordItem.Property {| fname = BuiltinTypes.StepFields.value; value = e |}
                      RecordItem.Property {| fname = BuiltinTypes.StepFields.nextState; value = unitVal () |} ]
                   tvar = () |}
        let stepFn =
            Expr.Fun {| ident = { identName = freshName (); tvar = () }; body = resultRecord; tvar = () |}
        let record =
            Expr.MkRecord
                {| items =
                    [ RecordItem.Property {| fname = BuiltinTypes.StepFields.init; value = unitVal () |}
                      RecordItem.Property {| fname = BuiltinTypes.StepFields.step; value = stepFn |} ]
                   tvar = () |}
        remap.[box record] <- box e
        record

    // Wrap hoisted (name, rhs) lets around `body`, outermost first. The
    // synthetic Let and Var nodes are remapped to the original argument
    // node so squiggles for lift errors land on the argument text.
    let wrapHoists (hoists: (string * Expr<unit> * Expr<unit>) list) (body: Expr<unit>) =
        (hoists, body)
        ||> List.foldBack (fun (name, rhs, origArg) acc ->
            let letNode =
                Expr.Let
                    {| ident = { identName = name; tvar = () }
                       value = rhs
                       body = acc
                       tvar = () |}
            remap.[box letNode] <- box origArg
            letNode)

    // Statement-chain context: Let/Do spines, lambda and match-arm bodies,
    // and the program root. Hoists from contained expressions are inserted
    // here, directly above the statement that consumes them.
    let rec rewriteChain (scope: Scope) (e: Expr<unit>) : Expr<unit> =
        match e with
        | Expr.Let l ->
            let value', hoists = rewriteExpr scope l.value
            let hoistScope =
                (scope, hoists)
                ||> List.fold (fun s (n, _, _) ->
                    { s with
                        kinds = s.kinds |> Map.add n BindingKind.Plain
                        lifted = s.lifted |> Set.add n })
            let identKind =
                if isStreamExpr hoistScope.kinds value' then BindingKind.Plain
                else bindingKindOfRhs hoistScope.kinds value'
            let bodyScope =
                { hoistScope with
                    kinds = hoistScope.kinds |> Map.add l.ident.identName identKind
                    streamParams = hoistScope.streamParams |> Map.remove l.ident.identName
                    callables =
                        match value' with
                        | Expr.Fun _ -> hoistScope.callables |> Set.add l.ident.identName
                        | _ -> hoistScope.callables |> Set.remove l.ident.identName
                    lifted =
                        if isStreamExpr hoistScope.kinds value'
                        then hoistScope.lifted |> Set.add l.ident.identName
                        else hoistScope.lifted |> Set.remove l.ident.identName
                    namespaces = hoistScope.namespaces |> Map.remove l.ident.identName }
            let body' = rewriteChain bodyScope l.body
            let rebuilt =
                if obj.ReferenceEquals(value', l.value) && obj.ReferenceEquals(body', l.body) && hoists.IsEmpty
                then e
                else
                    Expr.Let {| ident = l.ident; value = value'; body = body'; tvar = l.tvar |}
                    |> fun n -> mapNew n e
            wrapHoists hoists rebuilt
        | Expr.Do d ->
            let action', hoists = rewriteExpr scope d.action
            let hoistScope =
                (scope, hoists)
                ||> List.fold (fun s (n, _, _) ->
                    { s with
                        kinds = s.kinds |> Map.add n BindingKind.Plain
                        lifted = s.lifted |> Set.add n })
            let body' = rewriteChain hoistScope d.body
            let rebuilt =
                if obj.ReferenceEquals(action', d.action) && obj.ReferenceEquals(body', d.body) && hoists.IsEmpty
                then e
                else
                    Expr.Do {| action = action'; body = body'; tvar = d.tvar |}
                    |> fun n -> mapNew n e
            wrapHoists hoists rebuilt
        | terminal ->
            let e', hoists = rewriteExpr scope terminal
            wrapHoists hoists e'

    // Expression context: returns the rewritten node plus the hoists that
    // must be inserted in the nearest enclosing statement chain. Each hoist
    // is (freshName, rhs, originalArgNode).
    and rewriteExpr (scope: Scope) (e: Expr<unit>) : Expr<unit> * (string * Expr<unit> * Expr<unit>) list =
        match e with
        | Expr.Lit _ | Expr.Var _ -> e, []

        | Expr.Fun f ->
            let bodyScope =
                { scope with
                    kinds = scope.kinds |> Map.add f.ident.identName BindingKind.Plain
                    callables = scope.callables |> Set.remove f.ident.identName
                    streamParams = scope.streamParams |> Map.remove f.ident.identName
                    lifted = scope.lifted |> Set.remove f.ident.identName
                    namespaces = scope.namespaces |> Map.remove f.ident.identName }
            let body' = rewriteChain bodyScope f.body
            if obj.ReferenceEquals(body', f.body) then e, []
            else
                (Expr.Fun {| ident = f.ident; body = body'; tvar = f.tvar |} |> fun n -> mapNew n e), []

        | Expr.App _ ->
            // Work on the whole curried chain at once. Collect the old App
            // spine (outermost..innermost) for identity remapping.
            let rec collectSpine e spine =
                match e with
                | Expr.App a -> collectSpine a.func (e :: spine)
                | _ -> e, spine
            let head, _ = collectSpine e []
            let _, args = collectAppChain e

            // Namespace member as call head: math.ema(...) -> __math_ema(...).
            // UFCS: recv.F(args) -> F(recv, args) when F is callable (directly
            // or via the namespace member index) and the receiver can't be a
            // record carrying F. `recv.F()` parses as App(PropAcc, UnitValue);
            // the synthetic unit arg is dropped.
            let head, args, headRewritten =
                match head with
                | Expr.PropAcc p ->
                    match nsFlatName scope p.source p.ident.identName with
                    | Some flat ->
                        let fnVar = Expr.Var {| ident = flat; tvar = p.tvar |}
                        remap.[box fnVar] <- box head
                        fnVar, args, true
                    | None ->
                        match ufcsTarget scope p.ident.identName with
                        | Some target ->
                            let fnVar = Expr.Var {| ident = target; tvar = p.ident.tvar |}
                            remap.[box fnVar] <- box head
                            let args =
                                match args with
                                | [ Expr.Var u ] when u.ident = BuiltinValues.unitValueIdent -> [ p.source ]
                                | _ -> p.source :: args
                            fnVar, args, true
                        | _ -> head, args, false
                | _ -> head, args, false

            let head', headHoists =
                match head with
                | Expr.Var _ | Expr.Lit _ -> head, []
                | _ -> rewriteExpr scope head

            let headName = match head' with Expr.Var v -> Some v.ident | _ -> None
            let streamParamPositions =
                headName
                |> Option.bind scope.streamParams.TryFind
                |> Option.defaultValue Set.empty

            let rewrittenArgs =
                args
                |> List.mapi (fun i arg ->
                    let arg', argHoists = rewriteExpr scope arg
                    if streamParamPositions.Contains i then
                        // Declared step-record parameter. An explicit step
                        // literal passes through; an AST-level stream (count,
                        // a combinator call) is hoisted to its own slot and
                        // its per-tick value re-wrapped, so the callee always
                        // receives a protocol record, never a slot binding;
                        // any other value is wrapped directly (return/pure).
                        match arg' with
                        | Expr.MkRecord r when hasStepShape r.items -> arg', argHoists
                        | _ when isStreamExpr scope.kinds arg' ->
                            let n = freshName ()
                            let argVar = Expr.Var {| ident = n; tvar = arg'.TVar |}
                            remap.[box argVar] <- box arg
                            wrapAsStream argVar, argHoists @ [ n, arg', arg ]
                        | _ -> wrapAsStream arg', argHoists
                    elif not scope.noHoist && isStreamExpr scope.kinds arg' then
                        let n = freshName ()
                        let argVar = Expr.Var {| ident = n; tvar = arg'.TVar |}
                        remap.[box argVar] <- box arg
                        argVar, argHoists @ [ n, arg', arg ]
                    else
                        arg', argHoists)
            let args' = rewrittenArgs |> List.map fst
            let hoists = headHoists @ (rewrittenArgs |> List.collect snd)

            let unchanged =
                not headRewritten
                && obj.ReferenceEquals(head', head)
                && List.forall2 (fun (a: Expr<unit>) (b: Expr<unit>) -> obj.ReferenceEquals(a, b)) args' args
            if unchanged && hoists.IsEmpty then e, []
            else
                let rebuilt =
                    (head', args')
                    ||> List.fold (fun func arg ->
                        Expr.App {| func = func; arg = arg; tvar = () |})
                mapNew rebuilt e, hoists

        | Expr.Let _ | Expr.Do _ ->
            // Statement chains in expression position don't occur in parsed
            // source; handle defensively by keeping hoists local.
            rewriteChain scope e, []

        | Expr.PropAcc p ->
            // First-class namespace member: math.ema as a value.
            match nsFlatName scope p.source p.ident.identName with
            | Some flat ->
                let fnVar = Expr.Var {| ident = flat; tvar = p.tvar |}
                remap.[box fnVar] <- box e
                fnVar, []
            | None ->
                let source', hoists = rewriteExpr scope p.source
                if obj.ReferenceEquals(source', p.source) then e, hoists
                else
                    (Expr.PropAcc {| source = source'; ident = p.ident; tvar = p.tvar |} |> fun n -> mapNew n e), hoists

        | Expr.MkArray a ->
            let rewritten = a.values |> List.map (rewriteExpr scope)
            let values' = rewritten |> List.map fst
            let hoists = rewritten |> List.collect snd
            if List.forall2 (fun (x: Expr<unit>) (y: Expr<unit>) -> obj.ReferenceEquals(x, y)) values' a.values then e, hoists
            else
                (Expr.MkArray {| values = values'; tvar = a.tvar |} |> fun n -> mapNew n e), hoists

        | Expr.MkRecord r ->
            // Inside a step-shaped literal, hoisting would break the
            // `{ __init, __step }` shape the solver and emit rely on.
            let itemScope =
                if hasStepShape r.items then { scope with noHoist = true } else scope
            let rewritten =
                r.items
                |> List.map (fun item ->
                    match item with
                    | RecordItem.Property f ->
                        let v', hs = rewriteExpr itemScope f.value
                        (if obj.ReferenceEquals(v', f.value) then item
                         else RecordItem.Property {| fname = f.fname; value = v' |}), hs
                    | RecordItem.Positional v ->
                        let v', hs = rewriteExpr itemScope v
                        (if obj.ReferenceEquals(v', v) then item
                         else RecordItem.Positional v'), hs)
            let items' = rewritten |> List.map fst
            let hoists = rewritten |> List.collect snd
            if List.forall2 (fun (x: RecordItem<unit>) (y: RecordItem<unit>) -> obj.ReferenceEquals(x, y)) items' r.items then e, hoists
            else
                (Expr.MkRecord {| items = items'; tvar = r.tvar |} |> fun n -> mapNew n e), hoists

        | Expr.Match m ->
            let scrutinee', hoists = rewriteExpr scope m.scrutinee
            let arms' =
                m.arms
                |> List.map (fun arm ->
                    let armScope =
                        match arm.pattern with
                        | MatchPattern.Var v ->
                            { scope with
                                kinds = scope.kinds |> Map.add v.identName BindingKind.Plain
                                streamParams = scope.streamParams |> Map.remove v.identName
                                lifted = scope.lifted |> Set.remove v.identName
                                namespaces = scope.namespaces |> Map.remove v.identName }
                        | _ -> scope
                    // Arm bodies are chain contexts: hoists stay inside the
                    // arm (only the taken arm evaluates per tick).
                    let body' = rewriteChain armScope arm.body
                    if obj.ReferenceEquals(body', arm.body) then arm
                    else { pattern = arm.pattern; body = body' })
            let unchanged =
                obj.ReferenceEquals(scrutinee', m.scrutinee)
                && List.forall2 (fun (a: MatchArm<unit>) b -> obj.ReferenceEquals(a, b)) arms' m.arms
            if unchanged then e, hoists
            else
                (Expr.Match {| scrutinee = scrutinee'; arms = arms'; tvar = m.tvar |} |> fun n -> mapNew n e), hoists

    { expr = rewriteChain scope0 expr; remap = remap }

// Number of leading Let bindings in a statement chain. Hosts use this to
// measure how many bindings their stdlib prefix contributes (Expr cases are
// assembly-internal, so the walk lives here).
let countTopLets (expr: Expr<unit>) : int =
    let rec count e n =
        match e with
        | Expr.Let l -> count l.body (n + 1)
        | _ -> n
    count expr 0

// Names of the leading Let bindings, in source order. Single source of
// truth for "what does the stdlib prefix define" (editors need the names
// for highlighting/completion and must not duplicate the list).
let topLetNames (expr: Expr<unit>) : string list =
    let rec walk e acc =
        match e with
        | Expr.Let l -> walk l.body (l.ident.identName :: acc)
        | _ -> List.rev acc
    walk expr []

// Drop unused stdlib bindings (the first `stdlibCount` lets of the chain)
// before solving and emitting: the solver is superlinear in constraint
// count, so an unpruned ~30-block stdlib costs seconds per keystroke.
// Sound because stdlib lets are pure top-level definitions; positions are
// untouched (text stays full-length, only AST nodes are removed).
// Returns the pruned chain plus the number of KEPT stdlib bindings.
let pruneStdlibBindings
    (stdlibCount: int)
    (remap: System.Collections.Generic.Dictionary<obj, obj>)
    (expr: Expr<unit>)
    : Expr<unit> * int
    =
    let rec collectStdlib n (e: Expr<unit>) acc =
        if n = 0 then List.rev acc, e
        else
            match e with
            | Expr.Let l -> collectStdlib (n - 1) l.body ((l.ident.identName, l.value, l.tvar) :: acc)
            | _ -> List.rev acc, e
    let stdlibLets, userPart = collectStdlib stdlibCount expr []
    if stdlibLets.IsEmpty then expr, 0 else

    let needed = System.Collections.Generic.HashSet<string>(usedIdents userPart)
    let kept =
        (stdlibLets, [])
        ||> List.foldBack (fun (name, value, tvar) kept ->
            if needed.Contains name then
                for ident in usedIdents value do needed.Add ident |> ignore
                (name, value, tvar) :: kept
            else kept)
    if kept.Length = stdlibLets.Length then expr, stdlibLets.Length
    else
        let pruned =
            (kept, userPart)
            ||> List.foldBack (fun (name, value, tvar) body ->
                let letNode =
                    Expr.Let
                        {| ident = { identName = name; tvar = () }
                           value = value
                           body = body
                           tvar = tvar |}
                remap.[box letNode] <- box value
                letNode)
        pruned, kept.Length
