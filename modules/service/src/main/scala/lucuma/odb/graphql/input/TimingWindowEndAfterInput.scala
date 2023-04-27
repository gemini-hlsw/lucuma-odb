// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.binding.*

case class TimingWindowEndAfterInput(
  duration: TimeSpan,
  repeat: Option[TimingWindowRepeatInput]
)

object TimingWindowEndAfterInput:
  val Binding: Matcher[TimingWindowEndAfterInput] =
    ObjectFieldsBinding.rmap {
      case List(
        TimeSpanInput.Binding("duration", rDuration),
        TimingWindowRepeatInput.Binding.Option("repeat", rRepeat)
      ) => (rDuration, rRepeat).parMapN(TimingWindowEndAfterInput(_, _))
    }
