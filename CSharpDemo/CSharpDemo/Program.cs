using System;
using System.Collections.Generic;
using System.Linq;

namespace CSharpDemo
{
    internal class Program
    {
        private static void Main()
        {
            var data = new List<List<string>>
            {
                new List<string> {"red", "green", "blue"},
                new List<string> {"apple", "banana", "peach", "mellon"},
                new List<string> {"one", "two"}
            };

            foreach (var item in IterateDynamicLoop(data).Select(x => string.Join("-", x)))
                Console.WriteLine(item);

            Console.ReadLine();
        }

        public static IEnumerable<IEnumerable<T>> IterateDynamicLoop<T>(IList<List<T>> data)
        {
            var count = data.Count;

            var loopIndex = count - 1;
            var counters = new int[count];
            var bounds = data.Select(x => x.Count).ToArray();

            do
            {
                yield return Enumerable.Range(0, count).Select(x => data[x][counters[x]]);
            } while (IncrementLoopState(counters, bounds, ref loopIndex));
        }

        private static bool IncrementLoopState(IList<int> counters, IList<int> bounds, ref int loopIndex)
        {
            if (loopIndex < 0)
                return false;

            counters[loopIndex] = counters[loopIndex] + 1;

            var result = true;

            if (counters[loopIndex] >= bounds[loopIndex])
            {
                counters[loopIndex] = 0;
                loopIndex--;
                result = IncrementLoopState(counters, bounds, ref loopIndex);
                loopIndex++;
            }

            return result;
        }
    }
}