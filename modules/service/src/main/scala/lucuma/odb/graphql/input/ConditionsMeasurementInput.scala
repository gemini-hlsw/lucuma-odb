// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.odb.data.Extinction
import lucuma.odb.data.Tag
import lucuma.odb.graphql.binding.*

final case class ConditionsMeasurementInput(
  source: Tag,
  seeing: Option[Angle],
  extinction: Option[Extinction],
  wavelength: Option[Wavelength],
  azimuth: Option[Angle],
  elevation: Option[Angle]
)

object ConditionsMeasurementInput {

  val Binding: Matcher[ConditionsMeasurementInput] =
    ObjectFieldsBinding.rmap {
      case List(
        TagBinding("source", rSource),
        AngleInput.Binding.Option("seeing", rSeeing),
        ExtinctionBinding.Option("extinction", rExtinction),
        WavelengthInput.Binding.Option("wavelength", rWavelength),
        AngleInput.Binding.Option("azimuth", rAzimuth),
        AngleInput.Binding.Option("elevation", rElevation),
      ) =>
        (rSource, rSeeing, rExtinction, rWavelength, rAzimuth, rElevation).parTupled.flatMap { (source, seeing, extinction, wavelength, azimuth, elevation) =>
          if List(seeing, extinction, wavelength, azimuth, elevation).exists(_.isDefined) then
            (azimuth, elevation) match {
              case (None, None) | (Some(_), Some(_)) =>
                Result(ConditionsMeasurementInput(source, seeing, extinction, wavelength, azimuth, elevation))
              case _ =>
                Matcher.validationFailure("Azimuth and elevation must both be defined, or must both be empty.")                  
            }
          else
            Matcher.validationFailure("At least one of seeing, wavelength, extinction, azimuth, and elevation must be defined.")                  
        }
    }

}


