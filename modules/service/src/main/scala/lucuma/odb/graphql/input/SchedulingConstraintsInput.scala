// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import grackle.Result
import lucuma.odb.data.ExecutionRequirement
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

case class SchedulingConstraintsInput(
  executionRequirement: Option[ExecutionRequirement],
  timingWindows:        Nullable[List[TimingWindowInput]]
)

object SchedulingConstraintsInput:

  // Resolves the (mutually exclusive) `executionRequirement` and deprecated
  // `isSplittable` inputs into a single optional `ExecutionRequirement`.  `None`
  // means "leave the requirement untouched"; the deprecated `isSplittable` maps
  // `true -> Unconstrained` and `false -> NoSplitting` (`Uninterruptible` is not
  // reachable through it).
  val Binding: Matcher[SchedulingConstraintsInput] =
    ObjectFieldsBinding.rmap:
      case List(
        ExecutionRequirementBinding.Option("executionRequirement", rReq),
        BooleanBinding.Option("isSplittable", rSplit),
        TimingWindowInput.Binding.List.Nullable("timingWindows", rTiming)
      ) =>
        (rReq, rSplit, rTiming).parMapN: (req, split, timing) =>
          (req, split) match
            case (Some(_), Some(_)) =>
              Result.failure("Only one of `executionRequirement` and the deprecated `isSplittable` may be specified.")
            case (Some(r), None)    =>
              Result(SchedulingConstraintsInput(r.some, timing))
            case (None, Some(s))    =>
              val r = if s then ExecutionRequirement.Unconstrained else ExecutionRequirement.NoSplitting
              Result(SchedulingConstraintsInput(r.some, timing))
            case (None, None)       =>
              Result(SchedulingConstraintsInput(none, timing))
        .flatten
