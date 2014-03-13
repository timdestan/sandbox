using System;
using System.Collections.Generic;

namespace ReadableTimeSpans
{
    public class FormattingStep
    {
        private readonly UnitOfTime _unitOfTime;
        
        public FormattingStep(UnitOfTime unitOfTime)
        {
            _unitOfTime = unitOfTime;
        }

        public IEnumerable<string> Format(long ticks, Func<long, IEnumerable<string>> remainderContinuation)
        {
            var wholeNumberOfThisUnit = ticks / _unitOfTime.Ticks;

            if (wholeNumberOfThisUnit > 0)
            {
                yield return Format(wholeNumberOfThisUnit);
            }

            var leftover = ticks % _unitOfTime.Ticks;

            if (leftover > 0)
            {
                foreach (var description in remainderContinuation(leftover))
                {
                    yield return description;
                }
            }
        }

        private string Format(long wholeNumber)
        {
            var unit = wholeNumber == 1
                ? _unitOfTime.Unit
                : Pluralize(_unitOfTime.Unit);

            return string.Format("{0} {1}", wholeNumber, unit);
        }

        private string Pluralize(string word)
        {
            return word + "s";
        }
    }
}