// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import lucuma.core.enums.SchedulingMode
import lucuma.core.enums.TooActivation
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

case class SchedulingConstraintsInput(
  tooActivation:  Option[TooActivation],
  schedulingMode: Option[SchedulingMode],
  timingWindows:  Nullable[List[TimingWindowInput]]
):

  /**
   * The scheduling mode this edit writes.  An explicit mode is written as given.
   * Failing that, a Rapid or Interrupting activation writes Uninterruptible:
   * every Target of Opportunity is Uninterruptible, so the mode comes with the
   * activation rather than being a separate choice.  Lowering the activation
   * writes nothing -- the mode stays where it was, for the PI to relax, since an
   * automatic downgrade would quietly change what the observation is scheduled
   * under.
   */
  def schedulingModeToWrite: Option[SchedulingMode] =
    schedulingMode.orElse:
      tooActivation.filter(_.requiresUninterruptible).as(SchedulingMode.Uninterruptible)

  /**
   * Would applying this edit leave the observation unsplittable?  The mode is
   * the only thing that decides this, but a Target of Opportunity activation
   * brings its own mode with it.
   */
  def makesUnsplittable: Boolean =
    schedulingModeToWrite.exists(!_.isSplittable)

object SchedulingConstraintsInput:

  // The field order below must match the schema's declaration order: Grackle
  // matches `case List(...)` positionally, not by the names in the pattern.
  //
  // The only relation between the axes expressed here is the implication in
  // `schedulingModeToWrite`.  Whether an explicit mode contradicts a Target of
  // Opportunity activation depends on the values the observation ends up with,
  // and an edit may supply only one of them, so that check belongs where both
  // are known -- ObservationService -- with the CHECK constraint behind it.
  val Binding: Matcher[SchedulingConstraintsInput] =
    ObjectFieldsBinding.rmap:
      case List(
        TooActivationBinding.Option("tooActivation", rActivation),
        SchedulingModeBinding.Option("schedulingMode", rMode),
        TimingWindowInput.Binding.List.Nullable("timingWindows", rTiming)
      ) =>
        (rActivation, rMode, rTiming).parMapN(SchedulingConstraintsInput.apply)
