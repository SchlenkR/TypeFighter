import { MkExpr, MkTyp, solve } from './lang';
import { Print } from './testSuite';

const myFirstProgram =
    MkExpr.app(
        MkExpr.app(
            MkExpr.var_('add'),
            1
        ),
        2
    );

const solution =
    solve(
        [
            ['add', MkTyp.fun(MkTyp.number, MkTyp.number, MkTyp.number)]
        ],
        myFirstProgram);

console.log(Print.solution(solution));
