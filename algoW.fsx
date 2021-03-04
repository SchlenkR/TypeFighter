// from: https://gist.github.com/praeclarum/5fbef41ea9c296590f23

module AlgorithmW

// HINDLEY-MILNER TYPE INFERENCE
// Based on http://catamorph.de/documents/AlgorithmW.pdf
// (Now at http://web.archive.org/web/20170704013532/http://catamorph.de/documents/AlgorithmW.pdf)

type Lit =
    | LInt of int
    | LBool of bool

type Exp =
    | EVariable of string
    | ELiteral of Lit
    | EApplication of Function : Exp * Argument : Exp
    | EAbstraction of ParameterName : string * Body : Exp
    | ELet of Name : string * Value : Exp * Body : Exp

type Type =
    | TInt
    | TBool
    | TVariable of string
    | TFunction of ParameterType : Type * ReturnType : Type

type TypeSubst = Map<string, Type>
    
type TypeScheme =
    {
        Variables : string list
        Type : Type
    }

type TypeEnv =
    {
        Schemes : Map<string, TypeScheme>
    }


type Type with
    member this.FreeTypeVariables =
        match this with
        | TInt
        | TBool -> Set.empty
        | TVariable n -> Set.singleton n
        | TFunction (t1, t2) ->
            let v1 = t1.FreeTypeVariables
            let v2 = t2.FreeTypeVariables
            Set.union v1 v2
    member this.Apply (ts : TypeSubst) =
        match this with
        | TVariable n ->
            match ts.TryFind n with
            | Some t -> t
            | None -> TVariable n
        | TFunction (t1, t2) ->
            TFunction (t1.Apply ts, t2.Apply ts)
        | _ -> this

type TypeScheme with
    member this.FreeTypeVariables =
        this.Type.FreeTypeVariables - (Set.ofList this.Variables)
    member this.Apply (s : TypeSubst) =
        {
            Variables = this.Variables
            Type =
                let newSubst = List.fold (fun ns i -> Map.remove i ns) s this.Variables
                this.Type.Apply newSubst
        }

type TypeEnv with
    member this.Remove (var : string) =
        {
            Schemes = this.Schemes.Remove var
        }
    member this.FreeTypeVariables =
        Seq.fold (fun v (nts : System.Collections.Generic.KeyValuePair<string, TypeScheme>) ->
            Set.union v nts.Value.FreeTypeVariables) Set.empty this.Schemes
    member this.Apply (ts : TypeSubst) =
        { Schemes = this.Schemes |> Map.map (fun k v -> v.Apply ts) }


/// Missing in F# 3.1
let unionMap<'K,'V when 'K : comparison> (s1 : Map<'K,'V>) (s2 : Map<'K,'V>) : Map<'K, 'V> =
    if s1.Count = 0 then s2
    else
        if s2.Count = 0 then s1
        else
            let keep2 = s2 |> Seq.filter (fun kv -> not (s1.ContainsKey kv.Key)) |> Array.ofSeq
            Seq.append s1 keep2
            |> Array.ofSeq // weird bug in F# Interactive needs Array
            |> Array.map (fun kv -> kv.Key, kv.Value)
            |> Map.ofArray

/// A Map with just one entry
let singletonMap<'K, 'V when 'K : comparison> (k : 'K) (v : 'V) : Map<'K, 'V> =
     Map.ofSeq [k, v]

/// Apply `s1` to `s2` then merge the results
let composeSubst (s1 : TypeSubst) (s2 : TypeSubst) : TypeSubst =
    let ns2 = Map.map (fun k (v : Type) -> v.Apply s1) s2
    unionMap ns2 s1

/// Abstracts a type over all type variables that are not free
let generalize (env : TypeEnv) (t : Type) =
    {
        Variables = List.ofSeq (t.FreeTypeVariables - env.FreeTypeVariables)
        Type = t
    }

/// Generates a new type variable. STATEFUL.
let newTyVar =
    let nextIndex = ref 1
    fun n ->
        let nn = sprintf "%s%d" n !nextIndex
        nextIndex := !nextIndex + 1
        TVariable nn

/// Replace all bound type variables with fresh variables
let instantiate (ts : TypeScheme) =
    let nvars = List.map (fun _ -> newTyVar "a") ts.Variables
    let s = Map.ofSeq (Seq.zip ts.Variables nvars)
    ts.Type.Apply s

let rec unify (t1 : Type) (t2 : Type) : TypeSubst =
    match t1, t2 with
    | TFunction (l1, r1), TFunction (l2, r2) ->
        let s1 = unify l1 l2
        let s2 = unify (r1.Apply s1) (r2.Apply s1)
        composeSubst s1 s2
    | TVariable u, t -> varBind u t
    | t, TVariable u -> varBind u t
    | TInt, TInt -> Map.empty
    | TBool, TBool -> Map.empty
    | _ -> failwith "Types do not unify: %A vs %A" t1 t2

/// Bind a name to a Type and return that binding
and varBind (u : string) (t : Type) : TypeSubst =
    match t with
    | TVariable u' when u = u' -> Map.empty
    | _ when t.FreeTypeVariables.Contains u ->
        failwith "Occur check fails: %s vs %A" u t
    | _ -> singletonMap u t

/// Type inference with pending substitutions
let rec ti (env : TypeEnv) (e : Exp) : TypeSubst * Type =
    match e with
    | EVariable n ->
        match env.Schemes.TryFind n with
        | None -> failwithf "Unbound variable: %s" n
        | Some sigma ->
            let t = instantiate sigma
            Map.empty, t
    | ELiteral l -> tiLit env l
    | EAbstraction (n, e) ->
        let tv = newTyVar "a"
        let env1 = env.Remove n
        let env2 : TypeEnv =
            let ts = { Type = tv; Variables = [] }
            { Schemes = unionMap env1.Schemes (singletonMap n ts) }
        let s1, t1 = ti env2 e
        s1, TFunction (tv.Apply s1, t1)
    | EApplication (e1, e2) ->
        let tv = newTyVar "a"
        let s1, t1 = ti env e1
        let s2, t2 = ti (env.Apply s1) e2
        let s3 = unify (t1.Apply s2) (TFunction (t2, tv))
        composeSubst (composeSubst s3 s2) s1, tv.Apply s3
    | ELet (x, e1, e2) ->
        let s1, t1 = ti env e1
        let env1 = env.Remove x
        let tp = generalize (env.Apply s1) t1
        let env2 = { Schemes = Map.add x tp env1.Schemes }
        let s2, t2 = ti (env2.Apply s1) e2
        composeSubst s1 s2, t2

and tiLit (env : TypeEnv) (l : Lit) : TypeSubst * Type =
    match l with
    | LInt _ -> Map.empty, TInt
    | LBool _ -> Map.empty, TBool

/// Type inference with all substitutions applied
let typeInference (env : Map<string, TypeScheme>) (e : Exp) =
    let s, t = ti { Schemes = env } e
    t.Apply s

/// Test this puppy
let test (e : Exp) =
    try
        let t = typeInference Map.empty e
        printfn "%A :: %A" e t
    with ex -> printfn "ERROR %O" ex


[<EntryPoint>]
let main argv = 
    [
        ELet ("id", EAbstraction ("x", EVariable "x"), EVariable "id")
        ELet ("id", ELiteral (LInt 42), EVariable "id")
    ]
    |> Seq.iter test
    0
