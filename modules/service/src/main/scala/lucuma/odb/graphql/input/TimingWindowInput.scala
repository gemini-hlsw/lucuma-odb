// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import lucuma.core.model.TimingWindow
import lucuma.core.model.TimingWindowInclusion
import lucuma.core.util.Timestamp
import lucuma.odb.graphql.binding.*
import cats.syntax.all.*

case class TimingWindowInput(
  // id: TimingWindow.Id,
  inclusion: TimingWindowInclusion,
  start: Timestamp,
  end: Option[TimingWindowEndInput]
)

object TimingWindowInput:
  val TimingWindowInclusionBinding: Matcher[TimingWindowInclusion] =
    enumeratedBinding[TimingWindowInclusion]

  val Binding: Matcher[TimingWindowInput] =
    ObjectFieldsBinding.rmap {
      case List(
        // TimingWindowIdBinding("timingWindowId", rTimingWindowId),
        TimingWindowInclusionBinding("inclusion", rInclusion),
        TimestampBinding("start", rStart),
        TimingWindowEndInput.Binding.Option("end", rEnd)
      // ) => (rTimingWindowId, rInclusion, rStart, rEnd).parMapN(TimingWindowInput(_, _, _, _))
      ) => (rInclusion, rStart, rEnd).parMapN(TimingWindowInput(_, _, _))
    }
