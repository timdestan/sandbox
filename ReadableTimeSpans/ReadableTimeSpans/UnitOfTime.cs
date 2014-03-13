using System;

namespace ReadableTimeSpans
{
    public struct UnitOfTime
    {
        private readonly TimeSpan _timeSpan;
        private readonly string _unit;

        public UnitOfTime(TimeSpan timeSpan, string unit) : this()
        {
            _timeSpan = timeSpan;
            _unit = unit;
        }

        public long Ticks
        {
            get { return _timeSpan.Ticks; }
        }

        public string Unit
        {
            get { return _unit; }
        }
    }
}