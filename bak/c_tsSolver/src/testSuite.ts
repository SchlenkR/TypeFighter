import * as Lang from './lang';
import { areEqual } from './utils';

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

    lines.br();
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

export function shouldSolve(env: Lang.ExternalEnv, expr: Lang.InitialExpr, expected: Lang.MonoTyp) {
  const finalSolveResult = Lang.solve(env, expr);
  console.log(Print.solution(finalSolveResult));

  if (finalSolveResult.solutionResult.kind !== 'Ok') {
    throw new Error(finalSolveResult.solutionResult.error);
  } else if (!areEqual(finalSolveResult.solutionResult.value.finalTyp, expected)) {
    throw new Error(`Expected ${Print.typ(expected)}, but got ${Print.typ(finalSolveResult.solutionResult.value.finalTyp)}`);
  }
}

export function shouldFail(env: Lang.ExternalEnv, expr: Lang.InitialExpr) {
  const finalSolveResult = Lang.solve(env, expr);
  console.log(Print.solution(finalSolveResult));
  if (finalSolveResult.solutionResult.kind === 'Ok') {
    throw new Error('Expected failure, but got success');
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
