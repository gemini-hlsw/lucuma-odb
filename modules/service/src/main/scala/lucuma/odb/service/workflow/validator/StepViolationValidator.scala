// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import cats.data.NonEmptyChain
import cats.syntax.all.*
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.enums.SequenceType
import lucuma.core.model.ObservationValidation
import lucuma.core.model.sequence.SequenceDigest
import lucuma.core.model.sequence.StepViolation
import lucuma.odb.data.ObservationValidationMap
import lucuma.odb.sequence.isDayCalibration

/**
 * Reports the problems found in the steps of the sequence (see `StepCheck`),
 * such as exposure times that break an instrument's rules or GMOS steps exposed
 * to second order light, each under its own validation code.  The steps are
 * checked as the sequence digest is computed, so generated and edited
 * sequences alike are covered.
 */
object StepViolationValidator extends ObservationValidator:

  /**
   * The problem and the sequence it was found in, e.g. "Science sequence:
   * Exposure times for GMOS North must be at least 1 s."
   */
  def message(sequenceType: SequenceType, v: StepViolation): String =
    s"${sequenceType.tag.capitalize} sequence: ${v.message}"

  def validation(sequenceType: SequenceType, v: StepViolation): ObservationValidation =
    ObservationValidation(v.code, NonEmptyChain.one(message(sequenceType, v)))

  // Acquisition first, then science, each with errors before warnings.
  private def validations(
    info: ObservationValidationInfo
  )(of: (SequenceType, StepViolation) => Option[ObservationValidation]): ObservationValidationMap =
    def in(sequenceType: SequenceType, digest: SequenceDigest): ObservationValidationMap =
      digest.stepViolations.toList.flatMap(of(sequenceType, _)).foldMap(ObservationValidationMap.singleton)
    info.executionDigest.foldMap: d =>
      in(SequenceType.Acquisition, d.acquisition) |+| in(SequenceType.Science, d.science)

  override def apply(info: ObservationValidationInfo): ObservationValidationMap =
    validations(info)((t, v) => validation(t, v).some)

  /** The one step violation observations outside a proposal are subject to. */
  val missingSmartGcal: ObservationValidator = info =>
    validations(info): (t, v) =>
      Option.when(v.code === ObservationValidationCode.MissingSmartGcal):
        // A daytime calibration has no acquisition, so naming the sequence adds nothing.
        if info.calibrationRole.isDayCalibration then ObservationValidation(v.code, NonEmptyChain.one(v.message))
        else validation(t, v)
