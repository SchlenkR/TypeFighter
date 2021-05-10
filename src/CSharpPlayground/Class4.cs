using System;

namespace CSharpPlayground
{
    public class Class4
    {
        private static T Execute<T>(Func<T, T> id, T value) => id(value);

        private T Id<T>(T x) => x;

        public void DoIt()
        {
            Execute(Id, 10);
            Execute(Id, "Hello");
        }
    }
}
