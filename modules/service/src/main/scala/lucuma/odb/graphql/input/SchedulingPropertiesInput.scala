// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import grackle.Result
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

case class SchedulingPropertiesInput(
  isSplittable:  Option[Boolean],
  timingWindows: Nullable[List[TimingWindowInput]]
)

object SchedulingPropertiesInput:

  val Binding: Matcher[SchedulingPropertiesInput] =
    ObjectFieldsBinding.rmap:
      case List(
        BooleanBinding.Option("isSplittable", rSplit),
        TimingWindowInput.Binding.List.Nullable("timingWindows", rTiming)
      ) => (rSplit, rTiming).parMapN(SchedulingPropertiesInput(_, _))