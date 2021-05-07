using System;
using System.Collections.Generic;
using System.Linq;

namespace CSharpPlayground
{
    static class Class1
    {
        static IEnumerable<IEnumerable<string>> TestB()
        {
            string now = DateTime.Now.ToString();
            DateTime[] items = new DateTime[] { DateTime.Now };
            return items.Select<DateTime, IEnumerable<string>>((DateTime a) =>
            {
                string[] formats = a.GetDateTimeFormats();
                return formats.Select<string, string>((string b) => now + b);
            });
        }

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
                return formats.Select<string, string>(dc2.Invoke);
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
