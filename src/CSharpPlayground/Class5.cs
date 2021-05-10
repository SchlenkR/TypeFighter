using System;

// <A> muss hier weg
class C1
{
    // <A> muss hier hin
    public A Invoke<A>(A x)
    {
        throw new Exception();
    }
}

class C2<B, C>
{
    // Nicht Func<>, sondern die DisplayClass der Abs
    // der Wert muss _von außen_ instanziiert und hier reingegeben werden
    // (closure)
    public C1 id;

    public Func<C, Tuple<B, C>> Invoke(B a)
    {
        throw new Exception();
    }
}

class C3<C, B>
{
    public B a;

    // Nicht Func<>, sondern die DisplayClass der Abs
    // der Wert muss _von außen_ instanziiert und hier reingegeben werden
    // (closure)
    public C1 id;

    public Tuple<B, C> Invoke(C b)
    {
        throw new Exception();
    }
}
