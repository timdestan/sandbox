using System;
using NUnit.Framework;
using FluentAssertions;

namespace ReadableTimeSpans.Tests
{
    [TestFixture]
    public class TimeSpanFormatterTests
    {
        private TimeSpanFormatter _formatter;

        [SetUp]
        public void Setup()
        {
            _formatter = new TimeSpanFormatter(UnitsOfTime.All);
        }

        private string Format(TimeSpan time)
        {
            return _formatter.Format(time);
        }

        [Test]
        public void ZeroTimeSpanDisplaysZeroSeconds()
        {
            Format(TimeSpan.Zero).Should().Be("0 seconds");
        }

        [Test]
        public void TimeSpansThatAreAWholeNumberOfSomeUnitWork()
        {
            Format(1.Seconds()).Should().Be("1 second");
            Format(2.Seconds()).Should().Be("2 seconds");

            Format(1.Minutes()).Should().Be("1 minute");
            Format(32.Minutes()).Should().Be("32 minutes");

            Format(1.Hours()).Should().Be("1 hour");
            Format(8.Hours()).Should().Be("8 hours");

            Format(1.Days()).Should().Be("1 day");
            Format(12.Days()).Should().Be("12 days");
        }

        [Test]
        public void TimeSpansThatNeedToBeDisplayedWithMoreThanOneUnitWork()
        {
            Format(28.Days() + 6.Hours() + 42.Minutes() + 12.Seconds()).Should().Be("28 days, 6 hours, 42 minutes, 12 seconds");
            Format(6.Hours() + 42.Minutes()).Should().Be("6 hours, 42 minutes");
            Format(28.Days() + 12.Seconds()).Should().Be("28 days, 12 seconds");
        }
    }
}
