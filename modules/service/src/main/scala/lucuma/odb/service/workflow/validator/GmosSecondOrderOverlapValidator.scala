// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import cats.data.NonEmptyList
import cats.syntax.all.*
import lucuma.core.math.Wavelength
import lucuma.core.model.ObservationValidation
import lucuma.core.model.sequence.gmos.SecondOrderOverlap
import lucuma.odb.data.ObservationValidationMap

import scala.collection.immutable.SortedSet

/**
 * Warns when second order light may overlap the spectrum of a GMOS
 * spectroscopy science step.  The steps are found as the sequence digest is
 * computed (see `SecondOrderOverlapCheck`), so generated and edited sequences
 * alike are covered, and only steps that remain to be executed are reported.
 */
object GmosSecondOrderOverlapValidator extends ObservationValidator:

  def message(onset: Wavelength, affected: NonEmptyList[Wavelength]): String =
    val plural = if affected.size > 1 then "s" else ""
    s"Second order light overlaps the spectrum beyond ${formatNm(onset)} at central wavelength$plural ${affected.map(formatNm).mkString_(", ")}. " +
      "It is recommended to use a blocking filter to avoid second order overlap."

  private def formatNm(w: Wavelength): String =
    s"${Wavelength.decimalNanometers.reverseGet(w).bigDecimal.stripTrailingZeros.toPlainString} nm"

  /**
   * One message per onset (steps behind different filters have different
   * onsets), each listing the affected central wavelengths in order.
   */
  def validation(overlaps: SortedSet[SecondOrderOverlap]): Option[ObservationValidation] =
    NonEmptyList
      .fromList(
        overlaps.toList.groupByNel(_.onset).toList.map: (onset, os) =>
          message(onset, os.map(_.centralWavelength))
      )
      .map(ms => ObservationValidation.Warning.secondOrderOverlap(ms.head, ms.tail*))

  override def apply(info: ObservationValidationInfo): ObservationValidationMap =
    info.executionDigest
      .flatMap(d => validation(d.acquisition.secondOrderOverlaps ++ d.science.secondOrderOverlaps))
      .foldMap(ObservationValidationMap.singleton)
