import { areEqual, MapEx } from './utils';

type Result<S, E> = Readonly<
  | { kind: "Ok", value: S }
  | { kind: "Error", error: E }
>;

type VarNum = number;

type Ident = Readonly<{
  identName: string;
  // this should only occur in enumerated exprs, but we leave it aleays here for now,
  // and give it a meaningful value when enumerating
  tvar: VarNum;
}>;

type Lit = Readonly<
  | { kind: 'Void' }
  | { kind: 'Number'; value: number }
  | { kind: 'Bool'; value: boolean }
  | { kind: 'String'; value: string }
>;

type Expr<Meta> = Readonly<
  { meta: Meta } & (
    | { kind: 'Lit'; value: Lit }
    | { kind: 'Var'; ident: string }
    | { kind: 'App'; func: Expr<Meta>; arg: Expr<Meta> }
    | { kind: 'Fun'; ident: Ident; body: Expr<Meta> }
    | { kind: 'Let'; ident: Ident; value: Expr<Meta>; body: Expr<Meta> }
    | { kind: 'Do'; action: Expr<Meta>; body: Expr<Meta> }
  )
>;

type InitialExpr = Expr<{}>;

type Enumerated = { tvar: VarNum };
type EnumeratedExpr = Readonly<Expr<Enumerated>>;

// Explain: Put some examples here, and explain what "types" can be
type MonoTyp = Readonly<
  | { kind: 'TVar'; varNum: VarNum }
  | { kind: 'TApp'; name: string; args: readonly MonoTyp[] }
  | { kind: 'TFun'; from: MonoTyp; to: MonoTyp }
>;

type EnvItem = Readonly<
  | { kind: 'Internal'; varNum: VarNum }
  | { kind: 'External'; typ: MonoTyp }
>;

type Env = Map<string, EnvItem>;

type ExternalEnv = Array<[string, MonoTyp]>;

type Bound = { env: Env };
type BoundExpr = Readonly<Expr<Enumerated & Bound>>;

type Constraint = Readonly<{
  t1: MonoTyp;
  t2: MonoTyp;
}>;

type ConstraintResult = Readonly<{
  constraintSet: readonly Constraint[];
  envExpr: BoundExpr;
}>;

type SolutionItem = Readonly<{
  tvar: VarNum;
  typ: MonoTyp;
}>;

type SolverRun = Readonly<{
  cycle: number;
  constraintSet: readonly Constraint[];
  solutionSet: readonly SolutionItem[];
}>;

type UnificationError = string;

type Solution = Readonly<
  { finalTyp: MonoTyp, solution: readonly SolutionItem[] }
>;

type SolutionResult = Result<Solution, UnificationError>;

type SolverResult = Readonly<{
  solverRuns: readonly SolverRun[];
  solutionResult: SolutionResult;
}>;

type FinalSolverResult = Readonly<SolverResult & { expr: BoundExpr }>;

module Result {
  export const ok = <S, E>(value: S): Result<S, E> => ({ kind: "Ok", value });
  export const error = <S, E>(error: E): Result<S, E> => ({ kind: "Error", error });
}

function exhaustivenessGuard<T>(x: never): T {
  throw new Error("That should never happen: " + x);
}

function subst(tvarToReplace: VarNum, withTyp: MonoTyp, inTyp: MonoTyp): MonoTyp {
  switch (inTyp.kind) {
    case 'TVar':
      return inTyp.varNum === tvarToReplace ? withTyp : inTyp;
    case 'TApp':
      return {
        kind: 'TApp',
        name: inTyp.name,
        args: inTyp.args.map(arg => subst(tvarToReplace, withTyp, arg)),
      };
    case 'TFun':
      return {
        kind: 'TFun',
        from: subst(tvarToReplace, withTyp, inTyp.from),
        to: subst(tvarToReplace, withTyp, inTyp.to),
      };
    default:
      return exhaustivenessGuard(inTyp);
  }
}

function enumerateExpr(rootExpr: InitialExpr): EnumeratedExpr {
  let currVar = -1;

  function newVar() {
    currVar += 1;
    return { meta: { tvar: currVar } };
  }

  function enunmerateRec(expr: InitialExpr): EnumeratedExpr {

    switch (expr.kind) {
      case 'Lit':
      case 'Var':
        return { ...expr, ...newVar() };
      case 'App':
        return {
          ...{ ...expr, ...newVar() },
          func: enunmerateRec(expr.func),
          arg: enunmerateRec(expr.arg),
        };
      case 'Fun':
        return {
          ...{ ...expr, ...newVar() },
          ident: { ...expr.ident, ...newVar() },
          body: enunmerateRec(expr.body),
        };
      case 'Let':
        return {
          ...{ ...expr, ...newVar() },
          ident: { ...expr.ident, ...newVar() },
          value: enunmerateRec(expr.value),
          body: enunmerateRec(expr.body),
        };
      case 'Do':
        return {
          ...{ ...expr, ...newVar() },
          action: enunmerateRec(expr.action),
          body: enunmerateRec(expr.body),
        };
      default:
        return exhaustivenessGuard(expr);
    }
  }

  return enunmerateRec(rootExpr);
}

module MkExpr {
  type DirectLiteral = number | boolean | string;

  type LitOrDirectLiteral = Lit | DirectLiteral;

  function toLit(lit: LitOrDirectLiteral): Lit {
    if (typeof lit === 'number')
      return { kind: 'Number', value: lit };
    if (typeof lit === 'boolean')
      return { kind: 'Bool', value: lit };
    if (typeof lit === 'string')
      return { kind: 'String', value: lit };
    return lit;
  }

  export const lit = (value: LitOrDirectLiteral): InitialExpr =>
    ({ kind: 'Lit', value: toLit(value), meta: {} });
  export const var_ = (ident: string): InitialExpr =>
    ({ kind: 'Var', ident, meta: {} });
  const mkAppRec = (args: InitialExpr[]): InitialExpr => {
    if (args.length === 0)
      throw new Error('Cannot create an application without arguments');
    if (args.length === 1)
      return args[0];
    return { kind: 'App', func: mkAppRec(args.slice(0, args.length - 1)), arg: args[args.length - 1], meta: {} };
  }
  export const app = (func: InitialExpr, arg: InitialExpr, ...args: InitialExpr[]): InitialExpr =>
    mkAppRec([func, arg, ...args]);
  export const fun = (ident: string, body: InitialExpr): InitialExpr =>
    ({ kind: 'Fun', ident: { identName: ident, tvar: 0 }, body: body, meta: {} });
  export const let_ = (ident: string, value: InitialExpr, body: InitialExpr): InitialExpr =>
    ({ kind: 'Let', ident: { identName: ident, tvar: 0 }, value: value, body: body, meta: {} });
  export const do_ = (action: InitialExpr, body: InitialExpr): InitialExpr =>
    ({ kind: 'Do', action: action, body: body, meta: {} });

  export const void_ = lit({ kind: 'Void' });
  export const number = (value: number) => lit({ kind: 'Number', value });
  export const bool = (value: boolean) => lit({ kind: 'Bool', value });
  export const string = (value: string) => lit({ kind: 'String', value });
}

module MkTyp {
  export const var_ = (varNum: VarNum): MonoTyp => ({ kind: 'TVar', varNum });
  export const app = (name: string, args: readonly MonoTyp[]): MonoTyp => ({ kind: 'TApp', name, args: args });
  const mkFunRec = (args: MonoTyp[]): MonoTyp => {
    if (args.length === 0)
      throw new Error('Cannot create a function type without arguments');
    if (args.length === 1)
      return args[0];
    return { kind: 'TFun', from: args[0], to: mkFunRec(args.slice(1)) };
  }
  export const fun = (from: MonoTyp, arg: MonoTyp, ...args: MonoTyp[]): MonoTyp =>
    mkFunRec([from, arg, ...args]);

  export const void_ = app('Void', []);
  export const number = app('Number', []);
  export const bool = app('Bool', []);
  export const string = app('String', []);
}

module BuiltinTypes {
  export const voidTyp: MonoTyp = { kind: 'TApp', name: 'Void', args: [] };
  export const numberTyp: MonoTyp = { kind: 'TApp', name: 'Number', args: [] };
  export const boolTyp: MonoTyp = { kind: 'TApp', name: 'Bool', args: [] };
  export const stringTyp: MonoTyp = { kind: 'TApp', name: 'String', args: [] };
};

function generateConstraints(rootEnv: Env, rootExpr: EnumeratedExpr): ConstraintResult {
  const constraintSet: Constraint[] = [];

  function appendConstraint(varNum: number, t2: MonoTyp) {
    const t1: MonoTyp = { kind: 'TVar', varNum };
    constraintSet.push({ t1, t2 });
  }

  function generateConstraintsRec(env: Env, expr: EnumeratedExpr): BoundExpr {

    function resolveFromEnv(ident: string): MonoTyp {
      const resolvedIdent = env.get(ident);
      if (!resolvedIdent)
        throw new Error(`Unbound variable '${ident}'`);
      switch (resolvedIdent.kind) {
        case "Internal":
          return MkTyp.var_(resolvedIdent.varNum);
        case "External":
          // TODO: Don't forget to instanciate when we have poly types
          return resolvedIdent.typ;
        default:
          return exhaustivenessGuard(resolvedIdent);
      }
    }

    switch (expr.kind) {
      case "Lit": {
        /*
            Syntax:     lit
            Premise:    -
            Conclusion: expr: τ
        */

        switch (expr.value.kind) {
          case "Void":
            appendConstraint(expr.meta.tvar, BuiltinTypes.voidTyp);
            break;
          case "Number":
            appendConstraint(expr.meta.tvar, BuiltinTypes.numberTyp);
            break;
          case "Bool":
            appendConstraint(expr.meta.tvar, BuiltinTypes.boolTyp);
            break;
          case "String":
            appendConstraint(expr.meta.tvar, BuiltinTypes.stringTyp);
            break;
          default:
            exhaustivenessGuard(expr.value);
        }

        return {
          ...expr,
          meta: { ...expr.meta, env }
        }
      }

      case "Var": {
        /*
            Syntax:     ident
            Premise:    env(ident) = τ
            Conclusion: expr: τ
        */

        const resolvedTyp = resolveFromEnv(expr.ident);
        appendConstraint(expr.meta.tvar, resolvedTyp);

        return {
          ...expr,
          meta: { ...expr.meta, env }
        };
      }

      case "App": {
        /*
            Syntax (example):  func arg
            Premise:           func:τ -> τ', arg:τ
            Conclusion:        expr: τ'
        */

        const argExpr = generateConstraintsRec(env, expr.arg);
        const funcExpr = generateConstraintsRec(env, expr.func);
        appendConstraint(expr.func.meta.tvar, MkTyp.fun(MkTyp.var_(expr.arg.meta.tvar), MkTyp.var_(expr.meta.tvar)));

        return {
          ...expr,
          arg: argExpr,
          func: funcExpr,
          meta: { ...expr.meta, env }
        };
      }

      case "Fun": {
        /*
            Syntax (example):  ident => body
            Premise:           env + ident:τ ⊢ body:τ'
            Conclusion:        expr: τ -> τ'
        */

        // explain: Shadowing
        const bodyExpr =
          generateConstraintsRec(
            MapEx.setOrUpdate(env, expr.ident.identName, { kind: "Internal", varNum: expr.ident.tvar }),
            expr.body);
        appendConstraint(expr.meta.tvar, MkTyp.fun(MkTyp.var_(expr.ident.tvar), MkTyp.var_(expr.body.meta.tvar)));

        return {
          ...expr,
          body: bodyExpr,
          meta: { ...expr.meta, env }
        }
      }

      case "Let": {
        /*
            Syntax (example):   let ident = value
                                body
            Premise:            value:τ, env + ident:τ ⊢ body:τ'
            Conclusion:         expr: τ'
        */

        const valueExpr = generateConstraintsRec(env, expr.value);
        appendConstraint(expr.ident.tvar, MkTyp.var_(expr.value.meta.tvar));
        appendConstraint(expr.meta.tvar, MkTyp.var_(expr.body.meta.tvar));

        const bodyExpr =
          generateConstraintsRec(
            MapEx.setOrUpdate(env, expr.ident.identName, { kind: "Internal", varNum: expr.ident.tvar }),
            expr.body);

        return {
          ...expr,
          value: valueExpr,
          body: bodyExpr,
          meta: { ...expr.meta, env }
        }
      }

      case "Do": {
        /*
            Syntax (example):   action
                                body
            Premise:            action:τ, body:τ'
            Conclusion:         expr: τ'
        */

        const bodyExpr = generateConstraintsRec(env, expr.body);
        const actionExpr = generateConstraintsRec(env, expr.action);
        appendConstraint(expr.meta.tvar, MkTyp.var_(expr.body.meta.tvar));
        appendConstraint(expr.action.meta.tvar, BuiltinTypes.voidTyp);

        return {
          ...expr,
          action: actionExpr,
          body: bodyExpr,
          meta: { ...expr.meta, env }
        };
      }

      default:
        return exhaustivenessGuard(expr);
    }
  }

  const envExpr = generateConstraintsRec(rootEnv, rootExpr);

  return { constraintSet, envExpr };
}

function throwUniError(message: string) {
  throw new Error(`Unification error: ${message}`);
}

function solveConstraints(constraintSet: readonly Constraint[], envExpr: BoundExpr): SolverResult {
  const solverRuns: SolverRun[] = [];

  function solveRec(constraintSet: readonly Constraint[], solutionSet: readonly SolutionItem[]): readonly SolutionItem[] {
    solverRuns.push({ cycle: solverRuns.length, constraintSet, solutionSet });

    if (constraintSet.length === 0)
      return solutionSet;

    const currC = constraintSet[0];
    const remainingConstraints: Readonly<Constraint[]> = constraintSet.slice(1);
    const nextConstraintSet: Constraint[] = [];
    const nextSolutionSet: SolutionItem[] = [];

    function unifyTypes(source: EnumeratedExpr, t1: MonoTyp, t2: MonoTyp) {

      if (areEqual(t1, t2))
        return;

      if (t1.kind === 'TVar')
        nextConstraintSet.push({ t1, t2 });
      else if (t2.kind === 'TVar')
        nextConstraintSet.push({ t1: t2, t2: t1 });
      else if (t1.kind === 'TApp' && t2.kind === 'TApp') {
        if (t1.name !== t2.name)
          throwUniError(`Type mismatch: ${t1.name} != ${t2.name}`);
        if (t1.args.length !== t2.args.length)
          throwUniError(`Type mismatch: ${t1.name} has different number of arguments`);
        for (let i = 0; i < t1.args.length; i++) {
          unifyTypes(source, t1.args[i], t2.args[i]);
        }
      }
      else if (t1.kind === 'TFun' && t2.kind === 'TFun') {
        unifyTypes(source, t1.from, t2.from);
        unifyTypes(source, t1.to, t2.to);
      }
      else
        throwUniError(`Type mismatch: ${t1.kind} != ${t2.kind}`);
    }

    if (currC.t1.kind === 'TVar') {
      const left = currC.t1.varNum;
      const right = currC.t2;

      for (const otherC of remainingConstraints) {
        nextConstraintSet.push({
          t1: subst(left, right, otherC.t1),
          t2: subst(left, right, otherC.t2),
        });
      }

      for (const s of solutionSet) {
        nextSolutionSet.push({ tvar: s.tvar, typ: subst(left, right, s.typ) });
      }
      nextSolutionSet.push({ tvar: left, typ: right });
    }
    else {
      unifyTypes(envExpr, currC.t1, currC.t2);

      for (const otherC of remainingConstraints)
        nextConstraintSet.push(otherC);

      for (const s of solutionSet) {
        nextSolutionSet.push(s);
      }
    }

    return solveRec(nextConstraintSet, nextSolutionSet);
  }

  try {
    const solution = solveRec(constraintSet, []);
    const finalTyp = solution.find(s => s.tvar === envExpr.meta.tvar);
    if (!finalTyp)
      throw new Error('Final type not found - that should never happen when there did not occur any unification errors.');
    return {
      solverRuns,
      solutionResult: Result.ok({ finalTyp: finalTyp.typ, solution })
    };
  } catch (ex) {
    if (ex instanceof Error) {
      return {
        solverRuns,
        solutionResult: Result.error(ex.message)
      };
    }
    else {
      throw ex;
    }
  }
}

function solve(env: ExternalEnv, expr: InitialExpr): FinalSolverResult {
  const envX: Env = new Map(env.map(([k, v]) => [k, { kind: 'External', typ: v }]));
  const enumeratedExpr = enumerateExpr(expr);
  const { constraintSet, envExpr } = generateConstraints(envX, enumeratedExpr);
  const res = solveConstraints([...constraintSet], envExpr);
  return { ...res, expr: envExpr };
}

export { InitialExpr, MonoTyp, ExternalEnv, MkTyp, MkExpr, FinalSolverResult, SolverRun, solve }
