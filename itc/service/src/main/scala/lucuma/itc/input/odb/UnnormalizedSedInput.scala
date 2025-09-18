// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input
package sourceprofile

import cats.data.NonEmptyMap
import cats.syntax.all.*
import coulomb.Quantity
import coulomb.syntax.*
import coulomb.units.si.*
import eu.timepit.refined.types.numeric
import grackle.Result
import lucuma.core.enums.*
import lucuma.core.model.UnnormalizedSED
import lucuma.odb.graphql.binding.*

object UnnormalizedSedInput {

  val StellarLibrarySpectrumBinding: Matcher[StellarLibrarySpectrum] = enumeratedBinding
  val CoolStarTemperatureBinding: Matcher[CoolStarTemperature]       = enumeratedBinding
  val GalaxySpectrumBinding: Matcher[GalaxySpectrum]                 = enumeratedBinding
  val PlanetSpectrumBinding: Matcher[PlanetSpectrum]                 = enumeratedBinding
  val QuasarSpectrumBinding: Matcher[QuasarSpectrum]                 = enumeratedBinding
  val HiiRegionSpectrum: Matcher[HIIRegionSpectrum]                  = enumeratedBinding
  val PlanetaryNebulaSpectrum: Matcher[PlanetaryNebulaSpectrum]      = enumeratedBinding

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
        (rStellarLibrary,
         rCoolStar,
         rGalaxy,
         rPlanet,
         rQuasar,
         rHiiRegion,
         rPlanetaryNebula,
         rPowerLaw,
         rBlackBodyTempK,
         rFluxDensities,
         rFluxDensitiesAttachment
        ).parTupled.flatMap {
          case (Some(v), None, None, None, None, None, None, None, None, None, None) =>
            Result(UnnormalizedSED.StellarLibrary(v))
          case (None, Some(v), None, None, None, None, None, None, None, None, None) =>
            Result(UnnormalizedSED.CoolStarModel(v))
          case (None, None, Some(v), None, None, None, None, None, None, None, None) =>
            Result(UnnormalizedSED.Galaxy(v))
          case (None, None, None, Some(v), None, None, None, None, None, None, None) =>
            Result(UnnormalizedSED.Planet(v))
          case (None, None, None, None, Some(v), None, None, None, None, None, None) =>
            Result(UnnormalizedSED.Quasar(v))
          case (None, None, None, None, None, Some(v), None, None, None, None, None) =>
            Result(UnnormalizedSED.HIIRegion(v))
          case (None, None, None, None, None, None, Some(v), None, None, None, None) =>
            Result(UnnormalizedSED.PlanetaryNebula(v))
          case (None, None, None, None, None, None, None, Some(v), None, None, None) =>
            Result(UnnormalizedSED.PowerLaw(v))
          case (None, None, None, None, None, None, None, None, Some(v), None, None) =>
            numeric.PosInt.from(v) match
              case Left(err)  => Result.failure(err)
              case Right(pbd) => Result(UnnormalizedSED.BlackBody(pbd.withUnit[Kelvin]))
          case (None, None, None, None, None, None, None, None, None, Some(v), None) =>
            v match
              case Nil    => Result.failure("fluxDensities cannot be empty")
              case h :: t => Result(UnnormalizedSED.UserDefined(NonEmptyMap.of(h, t*)))
          case (None, None, None, None, None, None, None, None, None, None, Some(v)) =>
            Result(UnnormalizedSED.UserDefinedAttachment(v))
          case _                                                                     =>
            Result.failure:
              "Exactly one of stellarLibrary, coolStar, galaxy, planet, quasar, hiiRegion, planetaryNebula, powerLaw, blackBodyTempK, fluxDensities, fluxDensitiesAttachment must be specified."
        }
    }

}
