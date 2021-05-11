using System;

class DisplayClass_1
{
    public T1 Invoke<T1>(T1 x)
    {
        // var: x
    }
}

class DisplayClass_2<T1>
{
    public Func<T1, T1> id;
    public Func<T3, Tuple<T2, T3>> Invoke<T2, T3>(T2 a)
    {
    }
}

class DisplayClass_3<T1, T2>
{
    public T2 a;
    public Func<T1, T1> id;
    public Tuple<T2, T3> Invoke<T3>(T3 b)
    {
        // var: a
        // var: b
    }
}

