using System;

namespace ReadableTimeSpans
{
    public class AmountOfTime
    {
        private readonly long _number;
        private readonly UnitOfTime _timeUnit;

        public AmountOfTime(long number, UnitOfTime timeUnit)
        {
            _number = number;
            _timeUnit = timeUnit;
        }

        public long Number
        {
            get { return _number; }
        }

        public UnitOfTime TimeUnit
        {
            get { return _timeUnit; }
        }

        public TimeSpan ToTimeSpan()
        {
            return TimeSpan.FromTicks(Number * TimeUnit.Ticks);
        }
    }
}