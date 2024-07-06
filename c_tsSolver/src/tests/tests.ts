import { MkExpr, MkTyp, MonoTyp } from '../lang';
import { shouldFail, shouldSolve } from '../testSuite';

const addNumbers: [string, MonoTyp] = ['addNumbers', MkTyp.fun(MkTyp.number, MkTyp.number, MkTyp.number)];

describe('basic inference', () => {

    test('calling curried function with 2 correct args', () => {
        /*
            addNumbers 1 2
        */
        const program = MkExpr.app(MkExpr.var_('addNumbers'), 1, 2);

        shouldSolve(
            [
                addNumbers
            ],
            program,
            MkTyp.number);
    });


    test('passing string to as second args to addNumbers curried function', () => {
        /*
            addNumbers 1 "Hello"
        */
        const program = MkExpr.app(MkExpr.var_('addNumbers'), 1, "Hello");

        shouldFail(
            [
                addNumbers
            ],
            program);
    });
});
