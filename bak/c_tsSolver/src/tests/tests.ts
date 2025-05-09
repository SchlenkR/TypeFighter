import { MkExpr, MkTyp, MonoTyp } from '../lang';
import { shouldFail, shouldSolve } from '../testSuite';

const addNumbers: [string, MonoTyp] = ['addNumbers', MkTyp.fun(MkTyp.number, MkTyp.number, MkTyp.number)];

describe('basic inference', () => {

    test('An arrow function', () => {
        /*
            fun x -> x
        */
        const program = MkExpr.fun('x', MkExpr.var_('x'));

        // TODO: No polymorphic types yet
        shouldSolve(
            [],
            program,
            MkTyp.fun(MkTyp.var_(0), MkTyp.var_(0)));
    });

    test('calling curried function with 2 correct args', () => {
        /*
            addNumbers 1 2
        */
        const program = MkExpr.app(MkExpr.var_('addNumbers'), MkExpr.lit(1), MkExpr.lit(2));

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
        const program = MkExpr.app(MkExpr.var_('addNumbers'), MkExpr.lit(1), MkExpr.lit("Hello"));

        shouldFail(
            [
                addNumbers
            ],
            program);
    });
});
