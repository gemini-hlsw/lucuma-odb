// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel._
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.FocalPlane
import lucuma.core.enums.SpectroscopyCapabilities
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.enumeratedBinding
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.PosBigDecimalBinding
import lucuma.odb.graphql.binding.PosIntBinding

/*
input SpectroscopyScienceRequirementsInput {
  # The wavelength field may be unset by assigning a null value, or ignored by skipping it altogether
  wavelength: WavelengthInput

  # The resolution field may be unset by assigning a null value, or ignored by skipping it altogether
  resolution: PosInt

  # The signalToNoise field may be unset by assigning a null value, or ignored by skipping it altogether
  signalToNoise: PosBigDecimal

  # The signalToNoiseAt field may be unset by assigning a null value, or ignored by skipping it altogether
  signalToNoiseAt: WavelengthInput

  # The wavelengthCoverage field may be unset by assigning a null value, or ignored by skipping it altogether
  wavelengthCoverage: WavelengthInput

  # The focalPlane field may be unset by assigning a null value, or ignored by skipping it altogether
  focalPlane: FocalPlane

  # The focalPlaneAngle field may be unset by assigning a null value, or ignored by skipping it altogether
  focalPlaneAngle: AngleInput

  # The capabilities field may be unset by assigning a null value, or ignored by skipping it altogether
  capabilities: SpectroscopyCapabilities
}
*/

final case class SpectroscopyScienceRequirementsInput(
  wavelength:         Nullable[Wavelength],
  resolution:         Nullable[PosInt],
  signalToNoise:      Nullable[PosBigDecimal],
  signalToNoiseAt:    Nullable[Wavelength],
  wavelengthCoverage: Nullable[Wavelength],
  focalPlane:         Nullable[FocalPlane],
  focalPlaneAngle:    Nullable[Angle],
  capability:         Nullable[SpectroscopyCapabilities]
)

object SpectroscopyScienceRequirementsInput {

  val Default: SpectroscopyScienceRequirementsInput =
    SpectroscopyScienceRequirementsInput(
      wavelength         = Nullable.Null,
      resolution         = Nullable.Null,
      signalToNoise      = Nullable.Null,
      signalToNoiseAt    = Nullable.Null,
      wavelengthCoverage = Nullable.Null,
      focalPlane         = Nullable.Null,
      focalPlaneAngle    = Nullable.Null,
      capability         = Nullable.Null
    )

  val FocalPlaneBinding: Matcher[FocalPlane] =
    enumeratedBinding[FocalPlane]

  val SpectroscopyCapabilitiesBinding: Matcher[SpectroscopyCapabilities] =
    enumeratedBinding[SpectroscopyCapabilities]

  val Binding: Matcher[SpectroscopyScienceRequirementsInput] =
    ObjectFieldsBinding.rmap {
      case List(
        WavelengthInput.Binding.Nullable("wavelength", rWavelength),
        PosIntBinding.Nullable("resolution", rResolution),
        PosBigDecimalBinding.Nullable("signalToNoise", rSignalToNoise),
        WavelengthInput.Binding.Nullable("signalToNoiseAt", rSignalToNoiseAt),
        WavelengthInput.Binding.Nullable("wavelengthCoverage", rWavelengthCoverage),
        FocalPlaneBinding.Nullable("focalPlane", rFocalPlane),
        AngleInput.Binding.Nullable("focalPlaneAngle", rFocalPlaneAngle),
        SpectroscopyCapabilitiesBinding.Nullable("capability", rCapability)
      ) =>
        (rWavelength, rResolution, rSignalToNoise, rSignalToNoiseAt, rWavelengthCoverage, rFocalPlane, rFocalPlaneAngle, rCapability).parMapN(
          SpectroscopyScienceRequirementsInput.apply
        )
    }
}