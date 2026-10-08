// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import cats.syntax.all.*
import lucuma.core.enums.SequenceType
import lucuma.core.model.ObservationValidation
import lucuma.core.model.sequence.SequenceDigest
import lucuma.core.model.sequence.exposure.ExposureTimeViolation
import lucuma.odb.data.ObservationValidationMap

/**
 * Reports exposure times that break an instrument's rules.  Those that cannot
 * be taken at all are configuration errors, those merely outside the
 * recommended limits are (dismissable) exposure time warnings.  The violations
 * are found as the sequence digest is computed, so they cover generated and
 * edited sequences alike.
 */
object ExposureTimeValidator extends ObservationValidator:

  /**
   * The rule broken and the sequence that breaks it, e.g. "Science sequence:
   * Exposure times for GMOS North must be at least 1 s."
   */
  def message(sequenceType: SequenceType, v: ExposureTimeViolation): String =
    s"${sequenceType.tag.capitalize} sequence: ${v.description}"

  def validation(sequenceType: SequenceType, v: ExposureTimeViolation): ObservationValidation =
    if v.isError then ObservationValidation.configuration(message(sequenceType, v))
    else ObservationValidation.Warning.exposureTime(message(sequenceType, v))

  // Acquisition first, then science, each with errors before warnings.
  override def apply(info: ObservationValidationInfo): ObservationValidationMap =
    def validations(sequenceType: SequenceType, digest: SequenceDigest): ObservationValidationMap =
      digest.exposureTimeViolations.toList.foldMap(v => ObservationValidationMap.singleton(validation(sequenceType, v)))
    info.executionDigest.foldMap: d =>
      validations(SequenceType.Acquisition, d.acquisition) |+| validations(SequenceType.Science, d.science)
