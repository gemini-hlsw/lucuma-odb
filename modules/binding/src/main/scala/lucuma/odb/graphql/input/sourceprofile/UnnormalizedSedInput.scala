// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input
package sourceprofile

import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import coulomb.Quantity
import coulomb.syntax.*
import coulomb.units.si.*
import eu.timepit.refined.types.numeric
import grackle.Result
import grackle.syntax.*
import lucuma.core.enums.*
import lucuma.core.model.UnnormalizedSED
import lucuma.odb.graphql.binding.*

object UnnormalizedSedInput {

  val StellarLibrarySpectrumBinding: Matcher[StellarLibrarySpectrum] = enumeratedBinding
  val CoolStarTemperatureBinding: Matcher[CoolStarTemperature] = enumeratedBinding
  val GalaxySpectrumBinding: Matcher[GalaxySpectrum] = enumeratedBinding
  val PlanetSpectrumBinding: Matcher[PlanetSpectrum] = enumeratedBinding
  val QuasarSpectrumBinding: Matcher[QuasarSpectrum] = enumeratedBinding
  val HiiRegionSpectrum: Matcher[HIIRegionSpectrum] = enumeratedBinding
  val PlanetaryNebulaSpectrum: Matcher[PlanetaryNebulaSpectrum] = enumeratedBinding

  val Binding: Matcher[UnnormalizedSED] =
    OneOfBinding(
      "stellarLibrary"          -> StellarLibrarySpectrumBinding.map(UnnormalizedSED.StellarLibrary(_)),
      "coolStar"                -> CoolStarTemperatureBinding.map(UnnormalizedSED.CoolStarModel(_)),
      "galaxy"                  -> GalaxySpectrumBinding.map(UnnormalizedSED.Galaxy(_)),
      "planet"                  -> PlanetSpectrumBinding.map(UnnormalizedSED.Planet(_)),
      "quasar"                  -> QuasarSpectrumBinding.map(UnnormalizedSED.Quasar(_)),
      "hiiRegion"               -> HiiRegionSpectrum.map(UnnormalizedSED.HIIRegion(_)),
      "planetaryNebula"         -> PlanetaryNebulaSpectrum.map(UnnormalizedSED.PlanetaryNebula(_)),
      "powerLaw"                -> BigDecimalBinding.map(UnnormalizedSED.PowerLaw(_)),
      "blackBodyTempK"          -> IntBinding.rmap: v =>
        numeric.PosInt.from(v).fold(Result.failure(_), pbd => UnnormalizedSED.BlackBody(pbd.withUnit[Kelvin]).success),
      "fluxDensities"           -> FluxDensityInput.Binding.List.rmap: fds =>
        NonEmptyList.fromList(fds)
          .toResult("fluxDensities cannot be empty")
          .map(nel => UnnormalizedSED.UserDefined(NonEmptyMap.of(nel.head, nel.tail*))),
      "fluxDensitiesAttachment" -> AttachmentIdBinding.map(UnnormalizedSED.UserDefinedAttachment(_))
    )

}
