// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.Applicative
import cats.Functor
import cats.Monad
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SmartGcalType
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepViolation
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

  /**
   * Expands a single step, leaving a missing calibration out of the sequence
   * and reporting it instead.
   *
   * @param calibration what is being looked up, as the report names it
   */
  def expandOrReport(
    static:      S,
    step:        ProtoStep[D],
    calibration: String
  )(using Functor[F]): F[(List[StepViolation], List[ProtoStep[D]])] =
    expandStep(static, step).map:
      _.fold(m => (List(m.violation(calibration)), List.empty), steps => (List.empty, steps))

  /**
   * Expands a spectroscopic (flat, arc) pair, flat first.  Each is looked up
   * on its own: whichever is missing is left out and reported.
   */
  def expandFlatAndArc(
    static: S,
    flat:   ProtoStep[D],
    arc:    ProtoStep[D]
  )(using Monad[F]): F[(List[StepViolation], List[ProtoStep[D]])] =
    for
      (fv, fs) <- expandOrReport(static, flat, SmartGcalExpander.Calibration.SpectroscopicFlat)
      (av, as) <- expandOrReport(static, arc, SmartGcalExpander.Calibration.Arc)
    yield (fv ++ av, fs ++ as)

object SmartGcalExpander:

  /** Why a smart gcal step has no expansion. */
  enum Missing:

    /** No row of the table matches the lookup key, given formatted. */
    case NoMapping(key: String)

    /** The configuration has no lookup key, for the reason given. */
    case NoKey(reason: String)

    def detail: String =
      this match
        case NoMapping(key) => key
        case NoKey(reason)  => reason

    def message: String =
      this match
        case NoMapping(key) => s"missing Smart GCAL mapping: $key"
        case NoKey(reason)  => reason

    def violation(calibration: String): StepViolation =
      StepViolation(ObservationValidationCode.MissingSmartGcal, Missing.message(calibration, detail))

  object Missing:

    def message(calibration: String, detail: String): String =
      s"No Smart GCAL $calibration is defined for this configuration: $detail"

  /** The complaint when a lookup that must yield steps defines none. */
  val NoSteps: String =
    "Smart GCAL defines no calibration steps for this configuration"

  /** The calibrations a sequence looks up, as their reports name them. */
  object Calibration:
    val ImagingFlat: String       = "imaging flat"
    val SpectroscopicFlat: String = "spectroscopic flat"
    val Arc: String               = "arc"
    val IfuAlignmentFlat: String  = "IFU alignment flat"

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
