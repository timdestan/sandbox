using System;
using System.Collections.Generic;
using System.Linq;

namespace ReadableTimeSpans
{
    public static class Extensions
    {
        private static bool None<T>(this IEnumerable<T> self)
        {
            return ! self.Any();
        }

        private static IEnumerable<T> Only<T>(this T self)
        {
            yield return self;
        }

        public static IEnumerable<AmountOfTime> InTermsOf(this TimeSpan self, IEnumerable<UnitOfTime> units)
        {
            var amountsOfUnits = self.GetAmounts(units);
            return amountsOfUnits.None() 
                ? ZeroAmountOfSmallest(units).Only() 
                : amountsOfUnits;
        }

        private static IEnumerable<AmountOfTime> GetAmounts(this TimeSpan self, IEnumerable<UnitOfTime> units)
        {
            var leftoverTicks = self.Ticks;

            foreach (var unit in units.LargestToSmallest())
            {
                var numberOfUnits = leftoverTicks / unit.Ticks;
                if (numberOfUnits > 0)
                {
                    yield return new AmountOfTime(numberOfUnits, unit);
                }
                leftoverTicks %= unit.Ticks;
            }
        } 

        private static AmountOfTime ZeroAmountOfSmallest(IEnumerable<UnitOfTime> units)
        {
            var smallestUnit = units.SmallestToLargest().First();
            return new AmountOfTime(0, smallestUnit);
        }

        private static IEnumerable<UnitOfTime> LargestToSmallest(this IEnumerable<UnitOfTime> units)
        {
            return units.SmallestToLargest().Reverse();
        }

        private static IEnumerable<UnitOfTime> SmallestToLargest(this IEnumerable<UnitOfTime> units)
        {
            return from unit in units
                orderby unit.Ticks
                select unit;
        }

        public static string Pluralize(this string self)
        {
            return self + "s";
        }
    }
}