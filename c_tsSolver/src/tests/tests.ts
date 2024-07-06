import { MkExpr, MkTyp, MonoTyp } from '../lang';
import { shouldFail, shouldSolve } from '../testSuite';

const addNumbers: [string, MonoTyp] = ['addNumbers', MkTyp.fun(MkTyp.number, MkTyp.number, MkTyp.number)];

describe('basic inference', () => {
    test('addNumbers 1 2', () => {
        /*
            addNumbers 1 2
        */
        const program =
            MkExpr.app(
                MkExpr.app(
                    MkExpr.var_('addNumbers'),
                    1
                ),
                2
            );

        shouldSolve(
            [
                addNumbers
            ],
            program,
            MkTyp.number);
    });


    test('addNumbers 1 2 3', () => {
        /*
            addNumbers 1 "Hello"
        */
        const program =
            MkExpr.app(
                MkExpr.app(
                    MkExpr.var_('addNumbers'),
                    1
                ),
                "Hello"
            );

        shouldFail(
            [
                addNumbers
            ],
            program);
    });
});