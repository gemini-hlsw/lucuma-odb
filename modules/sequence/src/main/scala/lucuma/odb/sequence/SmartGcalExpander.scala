// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.Id
import cats.data.NonEmptyList
import cats.syntax.applicative.*
import cats.syntax.either.*
import fs2.Pipe
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SmartGcalType
import lucuma.core.model.sequence.StepConfig
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep

/**
 * Expands `SmartGcal` steps into a (non-empty) list of fully specified `Gcal`
 * steps.
 *
 * @tparam D dynamic instrument configuration
 */
trait SmartGcalExpander[F[_], D] {

  /**
   * Expands a single step. If `step` is a smart gcal step it is replaced with
   * one or more normal gcal steps, if possible.  If `step` isn't a smart gcal
   * step it is returned unmodified as the single element of a `NonEmptyList`.
   */
  def expandStep(
    step: ProtoStep[D]
  ): F[Either[String, NonEmptyList[ProtoStep[D]]]]

  /**
   * Expands a single atom, replacing any smart gcal steps with one or more
   * normal gcal steps, if possible.
   */
  def expandAtom(
    atom: ProtoAtom[ProtoStep[D]]
  ): F[Either[String, ProtoAtom[ProtoStep[D]]]]

  /**
   * Expands the given sequence, attempting to replace smart gcal steps with
   * normal gcal steps.  For each atom in the Stream, if successful, the
   * expanded atom without smart gcal steps is emitted in a `Right`.  When
   * unsuccessful, the missing mapping is formatted and wrapped with a `Left`.
   */
  def expandSequence: Pipe[F, ProtoAtom[ProtoStep[D]], Either[String, ProtoAtom[ProtoStep[D]]]]

}

object SmartGcalExpander {

  /**
   * An expander implementation that always produces the same gcal
   * configurations regardless of input.
   */
  case class Constant[D](
    inst: (SmartGcalType, D) => D,
    gcal: SmartGcalType      => StepConfig.Gcal,
    clas: ObserveClass
  ) extends SmartGcalExpander[Id, D]:
    private def expand(step: ProtoStep[D]): ProtoStep[D] =
      step.stepConfig match
        case StepConfig.SmartGcal(t) => ProtoStep(inst(t, step.value), gcal(t), clas)
        case _                       => step

    override def expandStep(
      step: ProtoStep[D]
    ): Id[Either[String, NonEmptyList[ProtoStep[D]]]] =
      NonEmptyList.one(expand(step)).asRight.pure

    override def expandAtom(
      atom: ProtoAtom[ProtoStep[D]]
    ): Id[Either[String, ProtoAtom[ProtoStep[D]]]] =
      ProtoAtom(atom.description, atom.steps.map(expand)).asRight.pure

    // TODO: SEQUENCE UPDATE
    // can this be removed altogether?
    override def expandSequence: Pipe[Id, ProtoAtom[ProtoStep[D]], Either[String, ProtoAtom[ProtoStep[D]]]] =
      ???

}