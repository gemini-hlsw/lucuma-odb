// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import lucuma.core.util.Timestamp
import lucuma.odb.graphql.binding.*

// TODO Validate somewhere that only one is present
// TODO Also that atUtc happens after startUtc, and that repeat period is greater than duration
case class TimingWindowEndInput(
  atUtc: Option[Timestamp],
  after: Option[TimingWindowEndAfterInput]
)

object TimingWindowEndInput:
  val Binding: Matcher[TimingWindowEndInput] =
    ObjectFieldsBinding.rmap {
      case List(
        TimestampBinding.Option("atUtc", rAt),
        TimingWindowEndAfterInput.Binding.Option("after", rAfter)
      ) => (rAt, rAfter).parMapN(TimingWindowEndInput(_, _))
    }
