// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input
package sourceprofile

import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.syntax.all.*
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
    ObjectFieldsBinding.rmap {
      case List(
        StellarLibrarySpectrumBinding.Option("stellarLibrary", rStellarLibrary),
        CoolStarTemperatureBinding.Option("coolStar", rCoolStar),
        GalaxySpectrumBinding.Option("galaxy", rGalaxy),
        PlanetSpectrumBinding.Option("planet", rPlanet),
        QuasarSpectrumBinding.Option("quasar", rQuasar),
        HiiRegionSpectrum.Option("hiiRegion", rHiiRegion),
        PlanetaryNebulaSpectrum.Option("planetaryNebula", rPlanetaryNebula),
        BigDecimalBinding.Option("powerLaw", rPowerLaw),
        IntBinding.Option("blackBodyTempK", rBlackBodyTempK),
        FluxDensityInput.Binding.List.Option("fluxDensities", rFluxDensities),
        AttachmentIdBinding.Option("fluxDensitiesAttachment", rFluxDensitiesAttachment)
      ) =>
        val rBlackBody =
          rBlackBodyTempK.flatMap(_.traverse: v =>
            numeric.PosInt.from(v).fold(Result.failure(_), pbd => UnnormalizedSED.BlackBody(pbd.withUnit[Kelvin]).success)
          )
        val rUserDefined =
          rFluxDensities.flatMap(_.traverse: fds =>
            NonEmptyList.fromList(fds)
              .toResult("fluxDensities cannot be empty")
              .map(nel => UnnormalizedSED.UserDefined(NonEmptyMap.of(nel.head, nel.tail*)))
          )
        (rStellarLibrary, rCoolStar, rGalaxy, rPlanet, rQuasar, rHiiRegion, rPlanetaryNebula, rPowerLaw, rBlackBody, rUserDefined, rFluxDensitiesAttachment).parFlatMapN {
          (stellarLibrary, coolStar, galaxy, planet, quasar, hiiRegion, planetaryNebula, powerLaw, blackBody, userDefined, attachment) =>
            oneOrFail[UnnormalizedSED](
              stellarLibrary.map(UnnormalizedSED.StellarLibrary(_))         -> "stellarLibrary",
              coolStar.map(UnnormalizedSED.CoolStarModel(_))                -> "coolStar",
              galaxy.map(UnnormalizedSED.Galaxy(_))                         -> "galaxy",
              planet.map(UnnormalizedSED.Planet(_))                         -> "planet",
              quasar.map(UnnormalizedSED.Quasar(_))                         -> "quasar",
              hiiRegion.map(UnnormalizedSED.HIIRegion(_))                   -> "hiiRegion",
              planetaryNebula.map(UnnormalizedSED.PlanetaryNebula(_))       -> "planetaryNebula",
              powerLaw.map(UnnormalizedSED.PowerLaw(_))                     -> "powerLaw",
              blackBody                                                     -> "blackBodyTempK",
              userDefined                                                   -> "fluxDensities",
              attachment.map(UnnormalizedSED.UserDefinedAttachment(_))      -> "fluxDensitiesAttachment"
            )
        }
    }

}
