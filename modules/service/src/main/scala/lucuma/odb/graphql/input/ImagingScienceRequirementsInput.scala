// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.FocalPlane
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.PosIntBinding
import lucuma.odb.graphql.binding.enumeratedBinding

final case class ImagingScienceRequirementsInput(
  wavelength:         Nullable[Wavelength],
  resolution:         Nullable[PosInt],
  exposureTimeMode:   Nullable[ExposureTimeMode],
  wavelengthCoverage: Nullable[Wavelength],
  focalPlane:         Nullable[FocalPlane],
  focalPlaneAngle:    Nullable[Angle],
)

object ImagingScienceRequirementsInput:

  val Default: ImagingScienceRequirementsInput =
    ImagingScienceRequirementsInput(
      wavelength         = Nullable.Null,
      resolution         = Nullable.Null,
      exposureTimeMode   = Nullable.Null,
      wavelengthCoverage = Nullable.Null,
      focalPlane         = Nullable.Null,
      focalPlaneAngle    = Nullable.Null,
    )

  val FocalPlaneBinding: Matcher[FocalPlane] =
    enumeratedBinding[FocalPlane]

  val Binding: Matcher[ImagingScienceRequirementsInput] =
    ObjectFieldsBinding.rmap {
      case List(
        WavelengthInput.Binding.Nullable("wavelength", rWavelength),
        PosIntBinding.Nullable("resolution", rResolution),
        ExposureTimeModeInput.Binding.Nullable("exposureTimeMode", rExposureTimeMode),
        WavelengthInput.Binding.Nullable("wavelengthCoverage", rWavelengthCoverage),
        FocalPlaneBinding.Nullable("focalPlane", rFocalPlane),
        AngleInput.Binding.Nullable("focalPlaneAngle", rFocalPlaneAngle),
      ) =>
        (rWavelength, rResolution, rExposureTimeMode, rWavelengthCoverage, rFocalPlane, rFocalPlaneAngle)
          .parMapN(ImagingScienceRequirementsInput.apply)
    }
