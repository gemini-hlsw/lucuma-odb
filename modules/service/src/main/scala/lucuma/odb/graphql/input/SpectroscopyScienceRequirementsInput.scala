// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.FocalPlane
import lucuma.core.enums.SpectroscopyCapabilities
import lucuma.core.math.Angle
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.PosIntBinding
import lucuma.odb.graphql.binding.SignalToNoiseBinding
import lucuma.odb.graphql.binding.enumeratedBinding

final case class SpectroscopyScienceRequirementsInput(
  wavelength:         Nullable[Wavelength],
  resolution:         Nullable[PosInt],
  signalToNoise:      Nullable[SignalToNoise],
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
        SignalToNoiseBinding.Nullable("signalToNoise", rSignalToNoise),
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
