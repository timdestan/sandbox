using System;
using System.Collections.Generic;
using System.Linq;

namespace ReadableTimeSpans
{
    public class TimeSpanFormatter
    {
        private readonly IEnumerable<UnitOfTime> _units;

        public TimeSpanFormatter(IEnumerable<UnitOfTime> units)
        {
            _units = units;
        }

        public string Format(TimeSpan timeSpan)
        {
            var representation = timeSpan.InTermsOf(_units);
            return string.Join(", ", representation.Select(Format));
        }

        private string Format(AmountOfTime amount)
        {
            var unitDescription = amount.Number == 1
                ? amount.TimeUnit.UnitName
                : amount.TimeUnit.UnitName.Pluralize();

            return string.Format("{0} {1}", amount.Number, unitDescription);
        }
    }
}
