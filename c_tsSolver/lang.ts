type VarNum = number;

type Ident = {
  identName: string;
  tvar: VarNum;
};

type Expr<Meta> = Meta &
  (
    | {
      kind: 'Lit';
      value:
      | { kind: 'Void' }
      | { kind: 'Number'; value: number }
      | { kind: 'Bool'; value: boolean }
      | { kind: 'String'; value: string };
    }
    | { kind: 'Var'; ident: string }
    | { kind: 'App'; func: Expr<Meta>; arg: Expr<Meta> }
    | { kind: 'Fun'; ident: Ident; body: Expr<Meta> }
    | { kind: 'Let'; ident: Ident; value: Expr<Meta>; body: Expr<Meta> }
    | { kind: 'Do'; action: Expr<Meta>; body: Expr<Meta> }
  );

// Explain: Put some examples here, and explain what "types" can be
type MonoTyp =
  | { kind: 'TVar'; varNum: VarNum }
  | { kind: 'TApp'; name: string; args: readonly MonoTyp[] }
  | { kind: 'TFun'; from: MonoTyp; to: MonoTyp };

module BuiltinTypes {
  export const voidTyp: MonoTyp = { kind: 'TApp', name: 'Void', args: [] };
  export const numberTyp: MonoTyp = { kind: 'TApp', name: 'Number', args: [] };
  export const boolTyp: MonoTyp = { kind: 'TApp', name: 'Bool', args: [] };
  export const stringTyp: MonoTyp = { kind: 'TApp', name: 'String', args: [] };
};

type Env = Map<string, EnvItem>;

type EnvItem =
  | { kind: 'Internal'; varNum: VarNum }
  | { kind: 'External'; typ: MonoTyp };

type Constraint = {
  t1: MonoTyp;
  t2: MonoTyp;
};

type Solution = {
  tvar: VarNum;
  typ: MonoTyp;
};

type SolverRun = {
  cycle: number;
  constraintSet: Constraint[];
  solutionSet: Solution[];
};

type Enumerated = { tvar: VarNum };

type Contextual = { env: Env };

type InitialExpr = Expr<{}>;

type EnumeratedExpr = Expr<Enumerated>;

type EnvExpr = Expr<Enumerated & Contextual>;

function subst(tvarToReplace: VarNum, withTyp: MonoTyp, inTyp: MonoTyp) {
  switch (inTyp.kind) {
    case 'TVar':
      return inTyp.varNum === tvarToReplace ? withTyp : inTyp;
    case 'TApp':
      return {
        kind: 'TApp',
        app: {
          name: inTyp.name,
          args: inTyp.args.map(arg => subst(tvarToReplace, withTyp, arg)),
        },
      };
    case 'TFun':
      return {
        kind: 'TFun',
        from: subst(tvarToReplace, withTyp, inTyp.from),
        to: subst(tvarToReplace, withTyp, inTyp.to),
      };
    default:
      const _: never = inTyp;
      throw new Error();
  }
}

function enumerateExpr(rootExpr: InitialExpr): EnumeratedExpr {
  let currVar = -1;

  function newVar(): VarNum {
    currVar += 1;
    return currVar;
  }

  function enunmerateRec(expr: InitialExpr): EnumeratedExpr {

    switch (expr.kind) {
      case 'Lit':
      case 'Var':
        return { ...expr, tvar: newVar() };
      case 'App':
        return {
          ...{ ...expr, tvar: newVar() },
          func: enunmerateRec(expr.func),
          arg: enunmerateRec(expr.arg),
        };
      case 'Fun':
        return {
          ...{ ...expr, tvar: newVar() },
          ident: { ...expr.ident, tvar: newVar() },
          body: enunmerateRec(expr.body),
        };
      case 'Let':
        return {
          ...{ ...expr, tvar: newVar() },
          ident: { ...expr.ident, tvar: newVar() },
          value: enunmerateRec(expr.value),
          body: enunmerateRec(expr.body),
        };
      case 'Do':
        return {
          ...{ ...expr, tvar: newVar() },
          action: enunmerateRec(expr.action),
          body: enunmerateRec(expr.body),
        };
      default:
        const _: never = expr;
        throw new Error();
    }
  }

  return enunmerateRec(rootExpr);
}

module Typ {
  export const var_ = (varNum: VarNum): MonoTyp => ({ kind: 'TVar', varNum });
  export const app = (name: string, args: readonly MonoTyp[]): MonoTyp => ({ kind: 'TApp', name, args });
  export const fun = (from: MonoTyp, to: MonoTyp): MonoTyp => ({ kind: 'TFun', from, to });
}

module Expr {
  export const lit = (value: { kind: 'Void' }): Expr<{}> => ({ kind: 'Lit', value });
  export const var_ = (ident: string): Expr<{}> => ({ kind: 'Var', ident });
  export const app = (func: InitialExpr, arg: InitialExpr): Expr<{}> => ({ kind: 'App', func, arg });
  export const fun = (ident: Ident, body: InitialExpr): Expr<{}> => ({ kind: 'Fun', ident, body });
  export const let_ = (ident: Ident, value: InitialExpr, body: InitialExpr): Expr<{}> => ({ kind: 'Let', ident, value, body });
  export const do_ = (action: InitialExpr, body: InitialExpr): InitialExpr => ({ kind: 'Do', action, body });
}

module MapEx {
  export const setOrUpdate = <K, V>(map: Map<K, V>, key: K, value: V): Map<K, V> => {
    const newMap = new Map(map);
    newMap.set(key, value);
    return newMap;
  }
}

function generateConstraints(rootEnv: Env, rootExpr: EnumeratedExpr) {
  const constraintSet = new Set<Constraint>();

  function appendConstraint(varNum: number, t2: MonoTyp) {
    const t1: MonoTyp = { kind: 'TVar', varNum };
    constraintSet.add({ t1, t2 });
  }

  function generateConstraintsRec(env: Env, expr: EnumeratedExpr): EnvExpr {

    function resolveFromEnv(ident: string): MonoTyp {
      const resolvedIdent = env.get(ident);
      if (!resolvedIdent)
        throw new Error(`Unbound variable '${ident}'`);
      switch (resolvedIdent.kind) {
        case "Internal":
          return Typ.var_(resolvedIdent.varNum);
        case "External":
          // TODO: Don't forget to instanciate when we have poly types
          return resolvedIdent.typ;
        default:
          const _: never = resolvedIdent;
          throw new Error();
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
            appendConstraint(expr.tvar, BuiltinTypes.voidTyp);
            break;
          case "Number":
            appendConstraint(expr.tvar, BuiltinTypes.numberTyp);
            break;
          case "Bool":
            appendConstraint(expr.tvar, BuiltinTypes.boolTyp);
            break;
          case "String":
            appendConstraint(expr.tvar, BuiltinTypes.stringTyp);
            break;
          default:
            const _: never = expr.value;
            throw new Error();
        }

        return { ...expr, env };
      }

      case "Var": {
        /*
            Syntax:     ident
            Premise:    env(ident) = τ
            Conclusion: expr: τ
        */

        const resolvedTyp = resolveFromEnv(expr.ident);
        appendConstraint(expr.tvar, resolvedTyp);

        return { ...expr, env };
      }

      case "App": {
        /*
            Syntax (example):  func arg
            Premise:           func:τ -> τ', arg:τ
            Conclusion:        expr: τ'
        */

        const argExpr = generateConstraintsRec(env, expr.arg);
        const funcExpr = generateConstraintsRec(env, expr.func);
        appendConstraint(expr.func.tvar, Typ.fun(Typ.var_(expr.arg.tvar), Typ.var_(expr.tvar)));

        return {
          ...{ ...expr, env },
          arg: argExpr,
          func: funcExpr
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
        appendConstraint(expr.tvar, Typ.fun(Typ.var_(expr.ident.tvar), Typ.var_(expr.body.tvar)));

        return {
          ...{ ...expr, env },
          body: bodyExpr
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
        appendConstraint(expr.ident.tvar, Typ.var_(expr.value.tvar));
        appendConstraint(expr.tvar, Typ.var_(expr.body.tvar));

        const bodyExpr =
          generateConstraintsRec(
            MapEx.setOrUpdate(env, expr.ident.identName, { kind: "Internal", varNum: expr.ident.tvar }),
            expr.body);

        return {
          ...{ ...expr, env },
          value: valueExpr,
          body: bodyExpr
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
        appendConstraint(expr.tvar, Typ.var_(expr.body.tvar));
        appendConstraint(expr.action.tvar, BuiltinTypes.voidTyp);

        return {
          ...{ ...expr, env },
          action: actionExpr,
          body: bodyExpr
        };
      }

      default:
        const _: never = expr;
        throw new Error();
    }
  }
}
