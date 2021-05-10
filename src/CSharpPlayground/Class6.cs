using System;

class Abs_1
{
    public A Invoke<A>(A x) { throw new Exception(); }
}

class Abs_4<A>
{
    public Func<A, A> id;
    public Func<C, Tuple<B, C>> Invoke<B, C>(B a) { throw new Exception(); }
}

class Abs_5<B, A>
{
    public B a;
    public Func<A, A> id;
    public Tuple<B, C> Invoke<C, B>(C b) { throw new Exception(); }
}
