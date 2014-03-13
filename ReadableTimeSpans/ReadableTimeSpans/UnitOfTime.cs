using System;

namespace ReadableTimeSpans
{
    public struct UnitOfTime
    {
        private readonly TimeSpan _timeSpan;
        private readonly string _unitName;

        public UnitOfTime(TimeSpan timeSpan, string unitName) : this()
        {
            _timeSpan = timeSpan;
            _unitName = unitName;
        }

        public long Ticks
        {
            get { return _timeSpan.Ticks; }
        }

        public string UnitName
        {
            get { return _unitName; }
        }
    }
}