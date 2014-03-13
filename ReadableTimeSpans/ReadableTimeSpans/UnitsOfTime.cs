using System.Collections.Generic;
using AshMind.Extensions;

namespace ReadableTimeSpans
{
    public static class UnitsOfTime
    {
        public static UnitOfTime Second = new UnitOfTime(1.Seconds(), "second");
        public static UnitOfTime Minute = new UnitOfTime(1.Minutes(), "minute");
        public static UnitOfTime Hour = new UnitOfTime(1.Hours(), "hour");
        public static UnitOfTime Day = new UnitOfTime(1.Days(), "day");

        public static IEnumerable<UnitOfTime> All
        {
            get
            {
                return new[]
                {
                    Second, Minute, Hour, Day
                };
            }
        }
    }
}