using System;
using System.Collections.Generic;
using System.Linq;

namespace CSharpPlayground
{
    static class Module
    {
        struct DisplayClass2
        {
            public string now;
            public DateTime a;

            public string Invoke(string b)
            {
                string[] formats = a.GetDateTimeFormats();
                return now + b;
            }
        }

        struct DisplayClass1
        {
            public string now;

            public IEnumerable<string> Invoke(DateTime a)
            {
                string[] formats = a.GetDateTimeFormats();

                DisplayClass2 dc2 = new DisplayClass2 { now = now, a = a };
                return formats.Select<string, string>(new Func<string, string>(dc2.Invoke));
            }
        }

        static IEnumerable<IEnumerable<string>> TestC()
        {
            string now = DateTime.Now.ToString();
            DateTime[] items = new DateTime[] { DateTime.Now };

            DisplayClass1 dc1 = new DisplayClass1 { now = now };
            return items.Select<DateTime, IEnumerable<string>>(new Func<DateTime, IEnumerable<string>>(dc1.Invoke));
        }
    }
}


namespace X
{
    static class Y
    {
        private static Func<double, Func<double, double>> add;
        private static Func<IEnumerable<double>, Func<Func<double, double>, IEnumerable<double>>> map;
        private static IEnumerable<double> Numbers;

        static void DoIt()
        {
            //double x = 10d;
            //Func<double, double> _loc_2 = new Func<double, double>(number =>
            //{
            //    return add.Invoke(number).Invoke(x);
            //});
            //var _loc_3 = map.Invoke(Numbers).Invoke(_loc_2);
            //IEnumerable<double> _loc_1 = _loc_3;
        }
    }
}

namespace Y
{
    struct Record<T1, T2>
    {
        public T1 Value1;
        public T2 Value2;
    }

    class Test
    {
        public void DoIt<T>(T value)
        {
            var record = new Record<T, string> { Value1 = value, Value2 = "Huez" };
        }
    }
}


namespace Z
{
    public struct RECORD_0<T0, T1>
    {
        public T0 myNUmber;
        public T1 myString;
    }

    public struct RECORD_1<T0>
    {
        public T0 whatever;
    }

    public class Main
    {
        RECORD_1<A> _loc_2<A>(A x)
        {
            RECORD_1<A> _loc_3 = new RECORD_1<A>();
            _loc_3.whatever = x;
            return _loc_3;
        }

        public static void Do<A>()
        {
            Func<A, RECORD_1<A>> id = _loc_2;
            RECORD_0<RECORD_1<double>, RECORD_1<string>> _loc_4 = new RECORD_0<RECORD_1<double>, RECORD_1<string>>();
            _loc_4.myString = id.Invoke("Hello World");
            _loc_4.myNUmber = id.Invoke(42d);
            RECORD_0<RECORD_1<double>, RECORD_1<string>> _loc_1 = _loc_4;
        }
    }
}