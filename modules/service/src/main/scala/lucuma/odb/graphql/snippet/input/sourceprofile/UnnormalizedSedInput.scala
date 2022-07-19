// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.snippet
package input
package sourceprofile

import lucuma.core.model.UnnormalizedSED
import lucuma.odb.graphql.util.Bindings._
import lucuma.core.enum.StellarLibrarySpectrum
import lucuma.core.enum._
import cats.syntax.all._
import edu.gemini.grackle.Result
import eu.timepit.refined.types.numeric.PosBigDecimal
import coulomb.Quantity
import cats.data.NonEmptyMap

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
        BigDecimalBinding.Option("blackBodyTempK", rBlackBodyTempK),
        FluxDensityInput.Binding.List.Option("fluxDensities", rFluxDensities),
      ) =>
        (rStellarLibrary, rCoolStar, rGalaxy, rPlanet, rQuasar, rHiiRegion, rPlanetaryNebula, rPowerLaw, rBlackBodyTempK, rFluxDensities).parTupled.flatMap {

          case (Some(v), None, None, None, None, None, None, None, None, None) => Result(UnnormalizedSED.StellarLibrary(v))
          case (None, Some(v), None, None, None, None, None, None, None, None) => Result(UnnormalizedSED.CoolStarModel(v))
          case (None, None, Some(v), None, None, None, None, None, None, None) => Result(UnnormalizedSED.Galaxy(v))
          case (None, None, None, Some(v), None, None, None, None, None, None) => Result(UnnormalizedSED.Planet(v))
          case (None, None, None, None, Some(v), None, None, None, None, None) => Result(UnnormalizedSED.Quasar(v))
          case (None, None, None, None, None, Some(v), None, None, None, None) => Result(UnnormalizedSED.HIIRegion(v))
          case (None, None, None, None, None, None, Some(v), None, None, None) => Result(UnnormalizedSED.PlanetaryNebula(v))
          case (None, None, None, None, None, None, None, Some(v), None, None) => Result(UnnormalizedSED.PowerLaw(v))

          case (None, None, None, None, None, None, None, None, Some(v), None) =>
            PosBigDecimal.from(v) match {
              case Left(err)  => Result.failure(err)
              case Right(pbd) => Result(UnnormalizedSED.BlackBody(Quantity(pbd)))
            }

          case (None, None, None, None, None, None, None, None, None, Some(v)) =>
            v match {
              case Nil => Result.failure("fluxDensities cannot be empty")
              case h :: t => Result(UnnormalizedSED.UserDefined(NonEmptyMap.of(h, t: _*)))
            }

          case _ =>
            Result.failure("Exactly one of stellarLibrary, coolStar, galaxy, planet, quasar, hiiRegion, planetaryNebula, powerLaw, blackBodyTempK, fluxDensities must be specified.")

        }
    }

}