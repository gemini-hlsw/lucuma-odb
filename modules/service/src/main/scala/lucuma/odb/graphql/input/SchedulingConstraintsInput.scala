// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.ExecutionRequirement
import lucuma.core.enums.TooActivation
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

case class SchedulingConstraintsInput(
  tooActivation:                Option[TooActivation],
  explicitExecutionRequirement: Nullable[ExecutionRequirement],
  timingWindows:                Nullable[List[TimingWindowInput]]
):

  /**
   * Would applying this edit leave the observation unsplittable?  True when the
   * explicit requirement is set to something that forbids splitting, and also
   * when the activation is raised high enough that its default forbids it.
   */
  def makesUnsplittable: Boolean =
    explicitExecutionRequirement.toOption.exists(!_.isSplittable) ||
    tooActivation.exists(!_.executionRequirementDefault.isSplittable)

object SchedulingConstraintsInput:

  // Resolves the (mutually exclusive) `explicitExecutionRequirement` and
  // deprecated `isSplittable` inputs into a single nullable requirement.  The
  // deprecated flag maps `true -> Unconstrained` and `false -> NoSplitting`
  // (`Uninterruptible` is not reachable through it).
  //
  // The field order below must match the schema's declaration order: Grackle
  // matches `case List(...)` positionally, not by the names in the pattern.
  val Binding: Matcher[SchedulingConstraintsInput] =
    ObjectFieldsBinding.rmap:
      case List(
        TooActivationBinding.Option("tooActivation", rToo),
        ExecutionRequirementBinding.Nullable("explicitExecutionRequirement", rReq),
        BooleanBinding.Option("isSplittable", rSplit),
        TimingWindowInput.Binding.List.Nullable("timingWindows", rTiming)
      ) =>
        (rToo, rReq, rSplit, rTiming).parMapN: (too, req, split, timing) =>
          (req.toOption, split) match
            case (Some(_), Some(_)) =>
              Result.failure("Only one of `explicitExecutionRequirement` and the deprecated `isSplittable` may be specified.")
            case (_, Some(s))       =>
              val r = if s then ExecutionRequirement.Unconstrained else ExecutionRequirement.NoSplitting
              Result(SchedulingConstraintsInput(too, Nullable.NonNull(r), timing))
            case (_, None)          =>
              Result(SchedulingConstraintsInput(too, req, timing))
        .flatten
