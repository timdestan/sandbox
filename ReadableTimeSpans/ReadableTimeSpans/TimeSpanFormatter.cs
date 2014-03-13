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
            var unitsSmallestToLargest =
                from unit in _units
                orderby unit.Ticks
                select unit;

            if (timeSpan == TimeSpan.Zero)
            {
                // Seems like there should be a cleaner way to handle this.
                var smallestUnit = unitsSmallestToLargest.First();
                return string.Format("0 {0}s", smallestUnit.Unit);
            }
            else
            {
                var formatter = CreatePipeline(unitsSmallestToLargest.Select(unit => new FormattingStep(unit)));

                return string.Join(", ", formatter(timeSpan.Ticks));
            }
        }

        private Func<long, IEnumerable<string>> CreatePipeline(IEnumerable<FormattingStep> stepsSmallestToLargest)
        {
            return stepsSmallestToLargest.Aggregate<FormattingStep, Func<long, IEnumerable<string>>>(
                _ => Enumerable.Empty<string>(),
                (cont, step) => remainingTicks => step.Format(remainingTicks, cont));
        }

    }
}
