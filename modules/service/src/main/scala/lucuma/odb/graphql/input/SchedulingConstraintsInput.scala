// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.SchedulingMode
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

case class SchedulingConstraintsInput(
  schedulingMode: Option[SchedulingMode],
  timingWindows:  Nullable[List[TimingWindowInput]]
):

  /**
   * Would applying this edit leave the observation unsplittable?  The mode is
   * the only thing that decides this now: it is a single ordered value rather
   * than an explicit choice floored by the Target of Opportunity activation,
   * so there is no second source to consult.
   */
  def makesUnsplittable: Boolean =
    schedulingMode.exists(!_.isSplittable)

object SchedulingConstraintsInput:

  // Resolves the (mutually exclusive) `schedulingMode` and deprecated
  // `isSplittable` inputs into a single mode.  The deprecated flag maps
  // `true -> Unconstrained` and `false -> NoSplitting`; the two upper rungs are
  // not reachable through it.
  //
  // The field order below must match the schema's declaration order: Grackle
  // matches `case List(...)` positionally, not by the names in the pattern.
  val Binding: Matcher[SchedulingConstraintsInput] =
    ObjectFieldsBinding.rmap:
      case List(
        SchedulingModeBinding.Option("schedulingMode", rMode),
        BooleanBinding.Option("isSplittable", rSplit),
        TimingWindowInput.Binding.List.Nullable("timingWindows", rTiming)
      ) =>
        (rMode, rSplit, rTiming).parMapN: (mode, split, timing) =>
          (mode, split) match
            case (Some(_), Some(_)) =>
              Result.failure("Only one of `schedulingMode` and the deprecated `isSplittable` may be specified.")
            case (_, Some(s))       =>
              val m = if s then SchedulingMode.Unconstrained else SchedulingMode.NoSplitting
              Result(SchedulingConstraintsInput(m.some, timing))
            case (_, None)          =>
              Result(SchedulingConstraintsInput(mode, timing))
        .flatten
