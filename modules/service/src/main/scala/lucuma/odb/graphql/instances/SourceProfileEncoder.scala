// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.instances

import eu.timepit.refined.types.numeric
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax._
import lucuma.core.enums.Band
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.Wavelength
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.SpectralDefinition.BandNormalized
import lucuma.core.model.SpectralDefinition.EmissionLines
import lucuma.core.model.UnnormalizedSED
import lucuma.core.model.UnnormalizedSED.BlackBody
import lucuma.core.model.UnnormalizedSED.CoolStarModel
import lucuma.core.model.UnnormalizedSED.Galaxy
import lucuma.core.model.UnnormalizedSED.HIIRegion
import lucuma.core.model.UnnormalizedSED.Planet
import lucuma.core.model.UnnormalizedSED.PlanetaryNebula
import lucuma.core.model.UnnormalizedSED.PowerLaw
import lucuma.core.model.UnnormalizedSED.Quasar
import lucuma.core.model.UnnormalizedSED.StellarLibrary
import lucuma.core.model.UnnormalizedSED.UserDefined

import scala.collection.immutable.SortedMap

object SourceProfileEncoder extends SourceProfileEncoder

trait SourceProfileEncoder {

  // type SourceProfile
  implicit val EncoderSourceProfile: Encoder[SourceProfile] = {
    case SourceProfile.Point(sd)   => Json.obj("point"    -> encode(sd))
    case SourceProfile.Uniform(sd) => Json.obj("uniform"  -> encode(sd))
    case g: SourceProfile.Gaussian => Json.obj("gaussian" -> encode(g))
  }

  // type SpectralDefinitionIntegrated, type SpectralDefinitionSurface
  private def encode[A](sd: SpectralDefinition[A]): Json =
    Json.obj(
      sd match {
        case bn: BandNormalized[A] => "bandNormalized" -> encode(bn)
        case el: EmissionLines[A]  => "emissionLines"  -> encode(el)
      }
    )

  // type GaussianSource
  private def encode(g: SourceProfile.Gaussian): Json =
    Json.obj(
      "fwhm" -> encode(g.fwhm),
      g.spectralDefinition match {
        case bn: BandNormalized[Integrated] => "bandNormalized" -> encode(bn)
        case el: EmissionLines[Integrated]  => "emissionLines"  -> encode(el)
      }
    )

  // type Angle
  private def encode(a: Angle): Json = {
    val ha = Angle.hourAngle.get(a)
    Json.obj(
      "microarcseconds" -> Json.fromString(a.toMicroarcseconds.toString),
      "microseconds"    -> Json.fromString(ha.toMicroseconds.toString),
      // "milliarcseconds" -> ???,
      // "milliseconds"    -> ???,
      // "arcseconds"      -> ???,
      // "seconds"         -> ???,
      // "arcminutes"      -> ???,
      // "minutes"         -> ???,
      // "degrees"         -> ???,
      // "hours"           -> ???,
    )
  }

  // type BandNormalizedIntegrated, type BandNormalizedIntegratedSurface
  private def encode[A](bn: BandNormalized[A]): Json =
    Json.obj(
      "brightnesses" -> encode(bn.brightnesses),
      "sed"          -> encode(bn.sed),
    )

  // type [BandBrightnessIntegrated!]!, type [BandBrightnessSurface!]!
  private def encode[A](bs: SortedMap[Band, BrightnessMeasure[A]]): Json =
    Json.fromValues(bs.toList.map { case (k, v) => encode(k, v) })

  // type BandBrightnessIntegrated, BandBrightnessSurface
  private def encode[A](b: Band, m: BrightnessMeasure[A]): Json =
    Json.obj(
      "band"  -> Json.fromString(b.tag),
      "value" -> Json.fromString(m.value.toString),
      "units" -> Json.fromString(m.units.serialized), // ??? this doesn't seem good enough
      "error" -> m.error.fold(Json.Null)(v => Json.fromString(v.toString))
    )

  // type UnnormalizedSed
  private def encode(sed: UnnormalizedSED): Json =
    sed match {
      case StellarLibrary(librarySpectrum)          => Json.obj("stellarLibrary"  -> librarySpectrum.asJson)
      case CoolStarModel(temperature)               => Json.obj("coolStar"        -> temperature.asJson) // todo: tag
      case Galaxy(galaxySpectrum)                   => Json.obj("galaxy"          -> galaxySpectrum.asJson)
      case Planet(planetSpectrum)                   => Json.obj("planet"          -> planetSpectrum.asJson)
      case Quasar(quasarSpectrum)                   => Json.obj("quasar"          -> quasarSpectrum.asJson)
      case HIIRegion(hiiRegionSpectrum)             => Json.obj("hiiRegion"       -> hiiRegionSpectrum.asJson)
      case PlanetaryNebula(planetaryNebulaSpectrum) => Json.obj("planetaryNebula" -> planetaryNebulaSpectrum.asJson)
      case PowerLaw(index)                          => Json.obj("powerLaw"        -> index.asJson)
      case BlackBody(temperature)                   => Json.obj("blackBodyTempK"  -> temperature.value.value.asJson)
      case UserDefined(fluxDensities)               => Json.obj("fluxDensities"   -> fluxDensities.toSortedMap.toList.map(encode).asJson)
    }

  // type FluxDensityEntry
  private def encode(e: (Wavelength, numeric.PosBigDecimal)): Json =
    Json.obj(
      "wavelength" -> encode(e._1),
      "density"    -> e._2.value.asJson,
    )

  // type Wavelength
  private def encode(w: Wavelength): Json =
    Json.obj(
      "picometers"  -> w.toPicometers.value.value.asJson,
      "angstroms"   -> Wavelength.decimalAngstroms.reverseGet(w).asJson,
      "nanometers"  -> Wavelength.decimalNanometers.reverseGet(w).asJson,
      "micrometers" -> Wavelength.decimalMicrometers.reverseGet(w).asJson,
    )

}

