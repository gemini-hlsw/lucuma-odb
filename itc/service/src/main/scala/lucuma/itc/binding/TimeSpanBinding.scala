// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import lucuma.core.optics.Format
import lucuma.core.util.TimeSpan

object TimeSpanBinding {

  val Microseconds: Matcher[TimeSpan] =
    LongBinding.emap { µs =>
      TimeSpan.FromMicroseconds
        .getOption(µs)
        .toRight(s"$µs is outside of the range for TimeSpan in µs")
    }

  private def bigDecimalMatcher(
    name:   String,
    format: Format[BigDecimal, TimeSpan]
  ): Matcher[TimeSpan] =
    BigDecimalBinding.emap { bd =>
      format
        .getOption(bd)
        .toRight(s"$bd is outside of the range for TimeSpan in $name")
    }

  val Milliseconds: Matcher[TimeSpan] =
    bigDecimalMatcher("milliseconds", TimeSpan.FromMilliseconds)

  val Seconds: Matcher[TimeSpan] =
    bigDecimalMatcher("seconds", TimeSpan.FromSeconds)

  val Minutes: Matcher[TimeSpan] =
    bigDecimalMatcher("minutes", TimeSpan.FromMinutes)

  val Hours: Matcher[TimeSpan] =
    bigDecimalMatcher("hours", TimeSpan.FromHours)

  val Iso: Matcher[TimeSpan] =
    StringBinding.emap { s =>
      TimeSpan.FromString
        .getOption(s)
        .toRight(s"`$s` cannot be parsed as a valid ISO 8601 time string in TimeSpan range.")
    }
}
