// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.odb.graphql.binding.*

case class TimingWindowEndInput(
  atUtc: Option[Timestamp],
  after: Option[TimeSpan],
  repeat: Option[TimingWindowRepeatInput]
)

object TimingWindowEndInput:
  object messages:
    val OnlyOneDefinition: String = "Only one of atUtc or after may be specified."
    val RepeatOnlyWhenAfter: String = "repeat can only be specified when after is specified."
    val RepeatPeriodGreaterThanAfter: String = "repeat.period must be greater than after."

  val Binding: Matcher[TimingWindowEndInput] =
    ObjectFieldsBinding.rmap {
      case List(
        TimestampBinding.Option("atUtc", rAt),
        TimeSpanInput.Binding.Option("after", rAfter),
        TimingWindowRepeatInput.Binding.Option("repeat", rRepeat)
      ) => (rAt, rAfter, rRepeat).parMapN(TimingWindowEndInput(_, _, _)).flatMap {
        case TimingWindowEndInput(Some(_), Some(_), _) => Matcher.validationFailure(messages.OnlyOneDefinition)
        case TimingWindowEndInput(_, None, Some(_))    => Matcher.validationFailure(messages.RepeatOnlyWhenAfter)
        case TimingWindowEndInput(_, Some(duration), Some(TimingWindowRepeatInput(period, _)))
          if period <= duration                        => Matcher.validationFailure(messages.RepeatPeriodGreaterThanAfter)
        case other                                     => Result(other)
      }
    }
