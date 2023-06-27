// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.data.NonEmptyList
import cats.syntax.all.*
import edu.gemini.grackle.Path
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Result
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.odb.data.Extinction
import lucuma.odb.data.Nullable
import lucuma.odb.data.Tag
import lucuma.odb.graphql.binding._

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
            Result(ConditionsMeasurementInput(source, seeing, extinction, wavelength, azimuth, elevation))
          else
            Result.failure("At least one of seeing, wavelength, extinction, azimuth, and elevation must be defined.")                  
        }
    }

}

