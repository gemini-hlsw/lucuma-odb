// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos
package spectroscopy

import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.order.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.util.AtomBuilder

/**
 * The step grouping for an acquisition sequence: an initial atom and a single
 * repeating atom, emitted `count` times.
 *
 * The initial step carries a breakpoint on its last step.
 *
 * The steps themselves are mode-specific and the breakpoint is placed by
 * [[AcquisitionAtoms]], not by the steps.
 */
final case class AcquisitionSteps[D](
  initial:   NonEmptyList[ProtoStep[D]],
  repeating: NonEmptyList[ProtoStep[D]]
)

/**
 * Shared GMOS acquisition sequence generation methods.
 *
 * Both GMOS long slit and MOS acquisition call this. They differ only in
 * the steps they compute and the repeat count.
 */
object AcquisitionAtoms:

  val FastReadModeLimit: TimeSpan = 60.secTimeSpan

  /** The amp read mode an acquisition step of the given exposure time reads out at. */
  def readMode(exposureTime: TimeSpan): GmosAmpReadMode =
    if exposureTime <= FastReadModeLimit then GmosAmpReadMode.Fast else GmosAmpReadMode.Slow

  private val InitialAtomName: Option[NonEmptyString] =
    NonEmptyString.unapply("Initial Acquisition")

  private val RepeatingAtomName: Option[NonEmptyString] =
    NonEmptyString.unapply("Fine Adjustments")

  private def breakpointOnLastSep[D](
    steps: NonEmptyList[ProtoStep[D]]
  ): NonEmptyList[ProtoStep[D]] =
    NonEmptyList.fromListUnsafe(steps.init :+ steps.last.withBreakpoint)

  /**
   * Builds the acquisition stream: one initial atom, then `repeatCount` copies
   * of the repeating atom.
   */
  def stream[D](
    builder:     AtomBuilder[D],
    steps:       AcquisitionSteps[D],
    repeatCount: Int
  ): Stream[Pure, Atom[D]] =
    val initial = breakpointOnLastSep(steps.initial)
    (for
      a0 <- builder.build(InitialAtomName, 0, 0, initial)
      as <- (1 to repeatCount).toList.traverse: aix =>
              builder.build(RepeatingAtomName, aix, 0, steps.repeating)
    yield Stream.emits(a0 :: as)).runA(StepTimeEstimateCalculator.Last.empty[D]).value

  /**
   * The acquisition sequence generator, or an empty sequence for a Twilight
   * calibration, or an error if no positive exposure time is available.  The
   * mode-specific steps are computed from the exposure time by `computeSteps`.
   *
   * @param oid          observation id, for error messages
   * @param time         ITC integration time (or an error); the exposure time
   *                     used by `computeSteps` is read from it
   * @param calRole      calibration role; Twilight yields an empty sequence
   * @param atomBuilder  atom builder for the acquisition sequence type
   * @param modeName     observing mode name, for the error message
   * @param computeSteps mode-specific step computation given the exposure time
   * @param repeatCount  number of repeating atoms to emit after the initial one
   */
  def instantiate[D](
    oid:          Observation.Id,
    time:         Either[OdbError, IntegrationTime],
    calRole:      Option[CalibrationRole],
    atomBuilder:  AtomBuilder[D],
    modeName:     String,
    computeSteps: TimeSpan => AcquisitionSteps[D],
    repeatCount:  Int
  ): Either[OdbError, SequenceGenerator[D]] =
    calRole match
      case Some(CalibrationRole.Twilight) =>
        SequenceGenerator.empty[D].asRight
      case _                              =>
        time
          .filterOrElse(
            _.exposureTime.toNonNegMicroseconds.value > 0,
            OdbError.SequenceUnavailable(oid, s"Could not generate a sequence for $oid: $modeName acquisition requires a positive exposure time.".some)
          )
          .map: t =>
            new SequenceGenerator[D]:
              override val generate: Stream[Pure, Atom[D]] =
                stream(atomBuilder, computeSteps(t.exposureTime), repeatCount)

end AcquisitionAtoms
