// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import cats.syntax.all.*
import lucuma.core.model.ObservationValidation
import lucuma.core.util.TimeSpan
import lucuma.odb.data.ObservationValidationMap
import lucuma.odb.sequence.ExposureTimeIssue
import lucuma.odb.sequence.ExposureTimeIssue.Kind

/**
 * Reports exposure times outside an instrument's limits.  Those that cannot be
 * taken at all are configuration errors, those merely outside the recommended
 * range are (dismissable) exposure time warnings.  The issues are found by
 * obscalc as it walks the sequence, so they cover generated and edited
 * sequences alike.
 */
object ExposureTimeValidator extends ObservationValidator:

  private def formatSeconds(time: TimeSpan): String =
    time.toSeconds.bigDecimal.stripTrailingZeros.toPlainString

  def message(i: ExposureTimeIssue): String =
    val steps  =
      if i.stepCount.value === 1 then s"1 ${i.sequenceType.tag} step has an exposure time"
      else s"${i.stepCount.value} ${i.sequenceType.tag} steps have exposure times"
    val limit  = i.kind match
      case Kind.BelowMinimum            => s"below the ${formatSeconds(i.limit)} s minimum"
      case Kind.BelowRecommendedMinimum => s"below the recommended ${formatSeconds(i.limit)} s minimum"
      case Kind.AboveRecommendedMaximum => s"above the recommended ${formatSeconds(i.limit)} s maximum"
      case Kind.AboveMaximum            => s"above the ${formatSeconds(i.limit)} s maximum"
    val extreme = if i.kind.isBelow then "shortest" else "longest"
    s"$steps $limit for ${i.subject} ($extreme ${formatSeconds(i.extreme)} s)."

  def validation(i: ExposureTimeIssue): ObservationValidation =
    if i.kind.isError then ObservationValidation.configuration(message(i))
    else ObservationValidation.Warning.exposureTime(message(i))

  override def apply(info: ObservationValidationInfo): ObservationValidationMap =
    info.exposureTimeIssues.foldMap(i => ObservationValidationMap.singleton(validation(i)))
