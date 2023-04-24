// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import lucuma.core.util.Timestamp
import lucuma.odb.graphql.binding.*
import cats.syntax.all.*

case class TimingWindowEndInput(
  endAt: Option[Timestamp],
  endAfter: Option[TimingWindowEndAfterInput]
)

object TimingWindowEndInput:
  val Binding: Matcher[TimingWindowEndInput] =
    ObjectFieldsBinding.rmap {
      case List(
        TimestampBinding.Option("at", rAt),
        TimingWindowEndAfterInput.Binding.Option("after", rAfter)
      ) => (rAt, rAfter).parMapN(TimingWindowEndInput(_, _))
    }
