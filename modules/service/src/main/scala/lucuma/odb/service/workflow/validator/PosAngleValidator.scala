// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import cats.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.model.ObservationValidation
import lucuma.core.model.PosAngleConstraint
import lucuma.odb.data.ObservationValidationMap

/**
 * Position angle checks for MOS observations validations.
 */
object PosAngleValidator extends ObservationValidator:

  val ModeMessage: String =
    "MOS observations must use a fixed position angle."

  def angleMessage(maskAngle: Angle, observationAngle: Angle): String =
    s"The mask was designed for a position angle of ${format(maskAngle)}, but the observation is fixed at ${format(observationAngle)}."

  private def format(a: Angle): String =
    f"${a.toDoubleDegrees}%.2f°"

  def apply(info: ObservationValidationInfo): ObservationValidationMap =
    if !info.observingMode.exists(_.isMos) then
      ObservationValidationMap.empty
    else
      info.posAngleConstraint match
        case PosAngleConstraint.Fixed(a) =>
          info.maskPositionAngle.filter(_ =!= a).foldMap: m =>
            ObservationValidationMap.singleton(ObservationValidation.Warning.configuration(angleMessage(m, a)))
        case _                           =>
          ObservationValidationMap.singleton(ObservationValidation.configuration(ModeMessage))
