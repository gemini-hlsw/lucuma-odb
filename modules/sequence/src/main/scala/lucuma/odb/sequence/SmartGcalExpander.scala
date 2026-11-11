// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.Applicative
import cats.Functor
import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.semigroup.*
import lucuma.core.enums.CalibrationSetType
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SmartGcalType
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepViolation
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep

/**
 * Expands `SmartGcal` steps into fully specified `Gcal` steps.  A smart gcal
 * step carries the instrument configuration of the step it calibrates.
 * Implementations apply the instrument's smart gcal rules
 * (`lucuma.odb.smartgcal.SmartGcalRules`), so callers neither adapt that
 * configuration for the lookup nor adjust the configuration of the steps they
 * get back.
 *
 * @tparam S static instrument configuration
 * @tparam D dynamic instrument configuration
 */
trait SmartGcalExpander[F[_], S, D]:

  import SmartGcalExpander.Calibration
  import SmartGcalExpander.Missing

  /**
   * Expands a single step. A smart gcal step is replaced with the gcal steps
   * its lookup defines: none at all when only rows with a zero step count
   * match, `Missing` when nothing matches.  Any other step is returned
   * unmodified as the single element of the list.
   */
  def expandStep(
    static: S,
    step:   ProtoStep[D]
  ): F[Either[Missing, List[ProtoStep[D]]]]

  /** Expands a single step, leaving out and reporting a missing calibration. */
  def expandOrReport(
    static:      S,
    step:        ProtoStep[D],
    calibration: Calibration
  )(using Functor[F]): F[(List[StepViolation], List[ProtoStep[D]])] =
    expandStep(static, step).map:
      _.fold(m => (List(m.violation(calibration)), List.empty), steps => (List.empty, steps))

  /** Expands a (flat, arc) pair, flat first, each looked up on its own. */
  def expandFlatAndArc(
    static: S,
    flat:   ProtoStep[D],
    arc:    ProtoStep[D]
  )(using Monad[F]): F[(List[StepViolation], List[ProtoStep[D]])] =
    (
      expandOrReport(static, flat, Calibration.SpectroscopicFlat),
      expandOrReport(static, arc, Calibration.Arc)
    ).mapN(_ |+| _)

  /** Expands the steps of an atom; no atom is left when none of them remains. */
  def expandAtom(
    static:      S,
    atom:        ProtoAtom[ProtoStep[D]],
    calibration: Calibration
  )(using Monad[F]): F[(List[StepViolation], Option[ProtoAtom[ProtoStep[D]]])] =
    atom.steps.toList.foldMapM(expandOrReport(static, _, calibration)).map: (missing, steps) =>
      (missing, NonEmptyList.fromList(steps).map(ProtoAtom(atom.description, _)))

object SmartGcalExpander:

  /** The calibrations a sequence looks up, as their reports name them. */
  enum Calibration:
    case ImagingFlat
    case SpectroscopicFlat
    case Arc
    case IfuAlignmentFlat
    case DaytimeSet(kind: CalibrationSetType)

    def name: String =
      this match
        case ImagingFlat                                => "imaging flat"
        case SpectroscopicFlat                          => "spectroscopic flat"
        case Arc                                        => "arc"
        case IfuAlignmentFlat                           => "IFU alignment flat"
        case DaytimeSet(CalibrationSetType.SmartFlat)   => "flat"
        case DaytimeSet(CalibrationSetType.SmartArc)    => "arc"
        case DaytimeSet(CalibrationSetType.DayBaseline) => "day baseline"
        case DaytimeSet(kind)                           => kind.name.toLowerCase

  /** Why a smart gcal step has no expansion. */
  enum Missing:

    /** No row of the table matches the lookup key, given formatted. */
    case NoMapping(key: String)

    /** The configuration has no lookup key, for the reason given. */
    case NoKey(reason: String)

    def message: String =
      this match
        case NoMapping(key) => s"missing Smart GCAL mapping: $key"
        case NoKey(reason)  => reason

    def violation(calibration: Calibration): StepViolation =
      val detail = this match
        case NoMapping(key) => key
        case NoKey(reason)  => reason
      StepViolation(
        ObservationValidationCode.MissingSmartGcal,
        s"No Smart GCAL ${calibration.name} is defined for this configuration: $detail"
      )

  /** The complaint when a lookup that must yield steps defines none. */
  val NoSteps: String =
    "Smart GCAL defines no calibration steps for this configuration"

  /** The atoms a sequence is left with, or the reason it has none. */
  def remaining[A](missing: List[StepViolation], atoms: List[A]): Either[String, (List[StepViolation], List[A])] =
    Either.cond(atoms.nonEmpty, (missing, atoms), missing.headOption.fold(NoSteps)(_.message))

  /** A simple implementation for testing. */
  def pure[F[_], S, D](
    lookup: (SmartGcalType, S, D) => (D, StepConfig.Gcal, ObserveClass)
  )(using Applicative[F]): SmartGcalExpander[F, S, D] =
    new SmartGcalExpander[F, S, D]:
      private def expand(static: S, step: ProtoStep[D]): ProtoStep[D] =
        step.stepConfig match
          case StepConfig.SmartGcal(s) =>
            val (d, g, c) = lookup(s, static, step.value)
            ProtoStep(d, g, step.telescopeConfig, c)
          case _                       =>
            step

      override def expandStep(
        static: S,
        step:   ProtoStep[D]
      ): F[Either[Missing, List[ProtoStep[D]]]] =
        List(expand(static, step)).asRight.pure[F]
