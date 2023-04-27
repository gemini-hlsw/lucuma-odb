// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import lucuma.core.enums.TimingWindowInclusion
import lucuma.core.util.Timestamp
import lucuma.odb.graphql.binding.*

case class TimingWindowInput(
  inclusion: TimingWindowInclusion,
  startUtc: Timestamp,
  end: Option[TimingWindowEndInput]
)

object TimingWindowInput:
  val TimingWindowInclusionBinding: Matcher[TimingWindowInclusion] =
    enumeratedBinding[TimingWindowInclusion]

  val Binding: Matcher[TimingWindowInput] =
    ObjectFieldsBinding.rmap {
      case List(
        TimingWindowInclusionBinding("inclusion", rInclusion),
        TimestampBinding("startUtc", rStart),
        TimingWindowEndInput.Binding.Option("end", rEnd)
      ) => (rInclusion, rStart, rEnd).parMapN(TimingWindowInput(_, _, _))
    }
