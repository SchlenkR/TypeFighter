using System;
using System.Collections.Generic;
using System.Linq;

namespace CSharpTest
{
    public static class Class2
    {
        class S1<T1>
        {
            private readonly T1 _a;

            public S1(T1 a)
            {
                _a = a;
            }

            public TOut Invoke<TIn, TOut>(TIn value)
            {
                throw new NotImplementedException();
            }
        }

        class S2<T1, T2>
        {
            private readonly T1 _a;
            private readonly T2 _b;

            public S2(T1 a, T2 b)
            {
                _a = a;
                _b = b;
            }

            public TOut Invoke<TIn, TOut>(TIn value)
            {
                throw new NotImplementedException();
            }
        }
    }

    public static class Class1
    {
        private static Func<T, T> Fun<T>(Func<T, T> del) => del;

        public static object Query()
        {
            var arr = new[] { 1, 2, 3 };

            return arr.Select(x =>
            {
                return arr.Select(y =>
                {
                    T Id<T>(T input)
                    {
                        Console.WriteLine(y.ToString());
                        return input;
                    }

                    return arr.Select(z =>
                    {
                        return x + y + z;
                    });
                });
            });
        }
    }

    public static class Test
    {
        class BlockEdge
        {
            public string Name { get; set; }
        }

        class Context
        {
            public IEnumerable<BlockEdge> BlockEdges { get; set; }
        }

        private static IEnumerable<T> GetOrUpdateView<T>(string name, Func<IEnumerable<T>> createView)
        {
            throw new NotImplementedException();
        }

        public static void DoIt()
        {
            var c = new Context();
            var view = GetOrUpdateView("x", () => c.BlockEdges);
        }
    }
}
