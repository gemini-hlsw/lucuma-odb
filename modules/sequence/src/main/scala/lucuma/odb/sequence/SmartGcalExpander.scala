// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.Applicative
import cats.data.NonEmptyList
import cats.syntax.applicative.*
import cats.syntax.either.*
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
trait SmartGcalExpander[F[_], D]:

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


object SmartGcalExpander:

  /** A simple implementation for testing. */
  def pure[F[_], D](
    lookup: (SmartGcalType, D) => (D, StepConfig.Gcal, ObserveClass)
  )(using Applicative[F]): SmartGcalExpander[F, D] =
    new SmartGcalExpander[F, D]:
      private def expand(step: ProtoStep[D]): ProtoStep[D] =
        step.stepConfig match
          case StepConfig.SmartGcal(s) =>
            val (d, g, c) = lookup(s, step.value)
            ProtoStep(d, g, step.telescopeConfig, c)
          case _                       =>
            step

      override def expandStep(
        step: ProtoStep[D]
      ): F[Either[String, NonEmptyList[ProtoStep[D]]]] =
        NonEmptyList.one(expand(step)).asRight.pure[F]

      override def expandAtom(
        atom: ProtoAtom[ProtoStep[D]]
      ): F[Either[String, ProtoAtom[ProtoStep[D]]]] =
        ProtoAtom(atom.description, atom.steps.map(expand)).asRight.pure[F]