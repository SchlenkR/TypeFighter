import * as Lang from './lang';

export function shouldSolve(expr: Lang.InitialExpr, expected: Lang.MonoTyp, env: Lang.ExternalEnv) {
  const actual = Lang.solve(env, expr);
  if (actual.solutionResult.kind !== 'Ok') {
    throw new Error(actual.solutionResult.error);
  } else if (actual.solutionResult.value.finalTyp !== expected) {
    throw new Error(`Expected ${expected}, but got ${actual.solutionResult.value}`);
  }
}

export module Print {
  const indent = '    ';

  export function typ(typ: Lang.MonoTyp): string {
    switch (typ.kind) {
      case 'TVar':
        return `tv_${typ.varNum}`;
      case 'TApp':
        const args: string = typ.args.length === 0
          ? ''
          : ('<' + typ.args.map(Print.typ).join(', ') + '>');
        return `${typ.name}${args}`;
      case 'TFun':
        return `(${Print.typ(typ.from)} -> ${Print.typ(typ.to)})`;
    }
  }

  const newBuilder = () => {
    const lines: string[] = [];
    return {
      append: (line: string) => {
        lines.push(line);
      },
      br: () => lines.push(''),
      build: () => lines.join('\n'),
    };
  }

  export function solverRun(run: Lang.SolverRun): string {
    const lines = newBuilder();

    lines.append(`---- Solver Run ${run.cycle} ----`);

    lines.append(`${indent}Constraints:`);
    for (const c of run.constraintSet) {
      lines.append(`${indent}${indent}${Print.typ(c.t1)}  ==  ${Print.typ(c.t2)}`);
    }

    lines.append(`${indent}Solutions:`);
    for (const s of run.solutionSet) {
      lines.append(`${indent}${indent}tv_${s.tvar}  :=  ${Print.typ(s.typ)}`);
    }

    return lines.build();
  }

  export function solution(solution: Lang.FinalSolverResult): string {
    const lines = newBuilder();

    for (const run of solution.solverRuns) {
      lines.append(Print.solverRun(run));
    }

    if (solution.solutionResult.kind === 'Error') {
      lines.append(`${indent}Error: ${solution.solutionResult.error}`);
      return lines.build();
    }

    lines.append(`${indent}finalTyp  :=  ${Print.typ(solution.solutionResult.value.finalTyp)}`);

    return lines.build();
  }
}


// module ExprEx {
//   export function flatten<Meta>(rootExpr: Expr<Meta>): readonly Expr<Meta>[] {
//     const exprs: Expr<Meta>[] = [];

//     function flattenRec(expr: Expr<Meta>) {
//       exprs.push(expr);

//       switch (expr.kind) {
//         case 'Lit':
//         case 'Var':
//           break;
//         case 'App':
//           flattenRec(expr.func);
//           flattenRec(expr.arg);
//           break;
//         case 'Fun':
//           flattenRec(expr.body);
//           break;
//         case 'Let':
//           flattenRec(expr.value);
//           flattenRec(expr.body);
//           break;
//         case 'Do':
//           flattenRec(expr.action);
//           flattenRec(expr.body);
//           break;
//         default:
//           exhaustivenessGuard(expr);
//       }
//     }

//     flattenRec(rootExpr);

//     return exprs;
//   }
// }
