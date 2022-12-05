// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.instances

import cats.data.NonEmptyMap
import cats.instances.order._
import cats.syntax.bifunctor.*
import cats.syntax.either.*
import cats.syntax.traverse.*
import coulomb.Quantity
import coulomb.units.si.Kelvin
import eu.timepit.refined.api.Refined
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import io.circe.ACursor
import io.circe.Codec
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.refined._
import io.circe.syntax._
import lucuma.core.enums.Band
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.HourAngle
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.*
import lucuma.core.math.units.KilometersPerSecond
import lucuma.core.model.EmissionLine
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
import lucuma.core.util.Enumerated

import scala.collection.immutable.SortedMap

object SourceProfileCodec extends SourceProfileCodec

trait SourceProfileCodec {

  import SourceProfileCodecHelper._
  import SourceProfileCodecHelper.given

  // type SourceProfile
  given Codec[SourceProfile] with {
    def apply(sp: SourceProfile): Json =
      sp match {
        case SourceProfile.Point(sd)   => Json.obj("point"   -> sd.asJson)
        case SourceProfile.Uniform(sd) => Json.obj("uniform" -> sd.asJson)
        case g: SourceProfile.Gaussian => Json.obj("gaussian" -> g.asJson)
      }

    def apply(c: HCursor): Decoder.Result[SourceProfile] =
      c.downField("point").as[SpectralDefinition[Integrated]].map(SourceProfile.Point(_))    orElse
        c.downField("uniform").as[SpectralDefinition[Surface]].map(SourceProfile.Uniform(_)) orElse
        c.downField("gaussian").as[SourceProfile.Gaussian]
  }

}

trait SourceProfileCodecHelper {

  // SpectralDefinition[T]
  given [T](using Enumerated[Units Of Brightness[T]], Enumerated[Units Of LineFlux[T]], Enumerated[Units Of FluxDensityContinuum[T]]): Codec[SpectralDefinition[T]] with {
    def apply(sd: SpectralDefinition[T]): Json =
      Json.obj(
        sd match {
          case bn: BandNormalized[T] => "bandNormalized" -> bn.asJson
          case el: EmissionLines[T]  => "emissionLines"  -> el.asJson
        }
      )

    def apply(c: HCursor): Decoder.Result[SpectralDefinition[T]] =
      c.downField("bandNormalized").as[BandNormalized[T]] orElse
        c.downField("emissionLines").as[EmissionLines[T]]

  }

  given Codec[SourceProfile.Gaussian] with {
    def apply(g: SourceProfile.Gaussian): Json =
      g.spectralDefinition
       .asJson
       .mapObject(_.add("fwhm", g.fwhm.asJson))

    def apply(c: HCursor): Decoder.Result[SourceProfile.Gaussian] =
      for {
        f <- c.downField("fwhm").as[Angle]
        s <- Codec[SpectralDefinition[Integrated]].apply(c)
      } yield SourceProfile.Gaussian(f, s)

  }


  // Measure[N] Of T
  given [N, T](using Codec[N], Enumerated[Units Of T]): Codec[Measure[N] Of T] with {
    def apply(m: Measure[N] Of T): Json =
      Json.obj(
        "value" -> m.value.asJson,
        "units" -> m.units.serialized.asJson,
        "error" -> m.error.fold(Json.Null)(_.asJson)
      )

    def apply(c: HCursor): Decoder.Result[Measure[N] Of T] =
      for {
        v <- c.downField("value").as[N]
        u <- c.downField("units").as[Units Of T]
        e <- c.downField("error").as[Option[N]]
      } yield lucuma.core.math.dimensional.tag[T](Measure[N](v, u, e))
  }

  // Map entry (A, B) with named keys
  private def entryCodec[A: Codec, B: Encoder: Decoder](
    aKey: String,
    bKey: String
  ): Codec[(A, B)] =
    new Codec[(A, B)] {
      def apply(entry: (A, B)): Json =
        Json.obj(
          aKey -> entry._1.asJson,
          bKey -> entry._2.asJson
        )

      def apply(c: HCursor): Decoder.Result[(A, B)] =
        for {
          a <- c.downField(aKey).as[A]
          b <- c.downField(bKey).as[B]
        } yield (a, b)
    }

  val CodecFluxDensityEntry: Codec[(Wavelength, PosBigDecimal)] =
    entryCodec("wavelength", "density")

  given Codec[UnnormalizedSED] with {
    def apply(sed: UnnormalizedSED): Json =
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
        case UserDefined(fluxDensities)               => Json.obj("fluxDensities"   -> fluxDensities.toSortedMap.toList.map(CodecFluxDensityEntry.apply).asJson)
      }

    def apply(c: HCursor): Decoder.Result[UnnormalizedSED] = {
      def decode[A: Enumerated](n: String)(f: A => UnnormalizedSED): Decoder.Result[UnnormalizedSED] =
        c.downField(n).as[A].map(f)

      decode("stellarLibrary")(StellarLibrary(_))                                            orElse
        decode("coolStar")(CoolStarModel(_))                                                 orElse
        decode("galaxy")(Galaxy(_))                                                          orElse
        decode("planet")(Planet(_))                                                          orElse
        decode("quasar")(Quasar(_))                                                          orElse
        decode("hiiRegion")(HIIRegion(_))                                                    orElse
        decode("planetaryNebula")(PlanetaryNebula(_))                                        orElse
        c.downField("powerLaw").as[BigDecimal].map(PowerLaw(_))                              orElse
        c.downField("blackBodyTempK").as[PosInt].map { k => BlackBody(Quantity[Kelvin](k)) } orElse
        c.downField("fluxDensities")
         .values
         .toRight(DecodingFailure("fluxDensities should hold an array of values", c.history))
         .flatMap(_.toList.traverse(json => CodecFluxDensityEntry(json.hcursor)).map(SortedMap.from))
         .flatMap(m => NonEmptyMap.fromMap(m).toRight(DecodingFailure("At least one flux density entry is required for a user defined SED", c.history)))
         .map(UserDefined(_))                                                                orElse
        DecodingFailure(s"Could not decode SED: ${c.focus.map(_.spaces2)}", c.history).asLeft[UnnormalizedSED]
    }

  }

  def CodecBandBrightness[T](using Enumerated[Units Of Brightness[T]]): Codec[(Band, BrightnessMeasure[T])] =
    new Codec[(Band, BrightnessMeasure[T])] {
      def apply(bandBrightness: (Band, BrightnessMeasure[T])): Json =
        bandBrightness
          ._2
          .asJson
          .mapObject(_.add("band", bandBrightness._1.asJson))

      def apply(c: HCursor): Decoder.Result[(Band, BrightnessMeasure[T])] =
        for {
          b <- c.downField("band").as[Band]
          m <- Codec[BrightnessMeasure[T]].apply(c)
        } yield (b, m)
    }

  given [T](using Enumerated[Units Of Brightness[T]]): Codec[BandNormalized[T]] with {

    def decodeBrightnessMap(c: ACursor): Decoder.Result[SortedMap[Band, BrightnessMeasure[T]]] =
      c.values.fold(SortedMap.empty[Band, BrightnessMeasure[T]].asRight) {
        _.toList
         .traverse(json => CodecBandBrightness[T](json.hcursor))
         .map(SortedMap.from)
      }

    def apply(bn: BandNormalized[T]): Json =
      Json.obj(
        "brightnesses" -> bn.brightnesses.toList.map(CodecBandBrightness[T](_)).asJson,
        "sed"          -> bn.sed.asJson,
      )

    def apply(c: HCursor): Decoder.Result[BandNormalized[T]] =
      for {
        s <- c.downField("sed").as[UnnormalizedSED]
        b <- decodeBrightnessMap(c.downField("brightnesses"))
      } yield BandNormalized(s, b)

  }

  given [T](using Enumerated[Units Of LineFlux[T]], Enumerated[Units Of FluxDensityContinuum[T]]): Codec[EmissionLines[T]] with {

    def decodeLinesMap(c: ACursor): Decoder.Result[SortedMap[Wavelength, EmissionLine[T]]] =
      c.values.fold(SortedMap.empty[Wavelength, EmissionLine[T]].asRight) {
        _.toList
         .traverse(json => CodecWavelengthLine[T](json.hcursor))
         .map(SortedMap.from)
      }

    def apply(el: EmissionLines[T]): Json =
      Json.obj(
        "lines"                -> el.lines.toList.map(CodecWavelengthLine[T].apply(_)).asJson,
        "fluxDensityContinuum" -> el.fluxDensityContinuum.asJson
      )

    def apply(c: HCursor): Decoder.Result[EmissionLines[T]] =
      for {
        l <- decodeLinesMap(c.downField("lines"))
        f <- c.downField("fluxDensityContinuum").as[Measure[PosBigDecimal] Of FluxDensityContinuum[T]]
      } yield EmissionLines(l, f)

  }

  def CodecWavelengthLine[T](using Enumerated[Units Of LineFlux[T]]): Codec[(Wavelength, EmissionLine[T])] =
    new Codec[(Wavelength, EmissionLine[T])] {
      def apply(waveLine: (Wavelength, EmissionLine[T])): Json =
        waveLine
          ._2
          .asJson
          .mapObject(_.add("wavelength", waveLine._1.asJson))

      def apply(c: HCursor): Decoder.Result[(Wavelength, EmissionLine[T])] =
        for {
          w <- c.downField("wavelength").as[Wavelength]
          e <- Codec[EmissionLine[T]].apply(c)
        } yield (w, e)
    }

  given [T](using Enumerated[Units Of LineFlux[T]]): Codec[EmissionLine[T]] with {

    def apply(el: EmissionLine[T]): Json =
      Json.obj(
        "lineWidth" -> el.lineWidth.value.asJson,
        "lineFlux"  -> el.lineFlux.asJson
      )

    def apply(c: HCursor): Decoder.Result[EmissionLine[T]] =
      for {
        w <- c.downField("lineWidth").as[PosBigDecimal].map(pbd => Quantity[KilometersPerSecond](pbd))
        f <- c.downField("lineFlux").as[Measure[PosBigDecimal] Of LineFlux[T]]
      } yield EmissionLine(w, f)

  }

  given Codec[PosBigDecimal] =
    Codec[BigDecimal].iemap(PosBigDecimal.from)(_.value)

  given Codec[BigDecimal] with {
    def apply(d: BigDecimal): Json =
      d.bigDecimal.toPlainString.asJson

    def apply(c: HCursor): Decoder.Result[BigDecimal] =
      Decoder[String].apply(c).flatMap { s =>
        Either.catchNonFatal(BigDecimal(s)).leftMap { _ =>
          DecodingFailure(s"Could not decode $s as a decimal value", c.history)
        }
      }
  }

  given Codec[Angle] with {

    def apply(a: Angle): Json = {

      val ha = Angle.hourAngle.get(a)

      def convertAngle(div: Int): Json =
        (BigDecimal(a.toMicroarcseconds) / div).asJson

      def convertHourAngle(div: Int): Json =
        (BigDecimal(ha.toMicroseconds) / div).asJson

      Json.obj(
        "microarcseconds" -> a.toMicroarcseconds.asJson,
        "microseconds"    -> ha.toMicroseconds.asJson,
        "milliarcseconds" -> convertAngle(1_000),
        "milliseconds"    -> convertHourAngle(1_000),
        "arcseconds"      -> convertAngle(1_000_000),
        "seconds"         -> convertHourAngle(1_000_000),
        "arcminutes"      -> convertAngle(60 * 1_000_000),
        "minutes"         -> convertHourAngle(60 * 1_000_000),
        "degrees"         -> convertAngle(3_600 * 1_000_000),
        "hours"           -> convertHourAngle(3_600 * 1_000_000),
        "hms"             -> HourAngle.HMS(ha).format.asJson,
        "dms"             -> Angle.dms.get(a).format.asJson
      )
    }

    def apply(c: HCursor): Decoder.Result[Angle] =
      c.downField("microarcseconds").as[Long].map(Angle.fromMicroarcseconds)

  }


  given Codec[Wavelength] with {
    def apply(w: Wavelength): Json =
      Json.obj(
        "picometers"  -> w.toPicometers.value.value.asJson,
        "angstroms"   -> Wavelength.decimalAngstroms.reverseGet(w).asJson,
        "nanometers"  -> Wavelength.decimalNanometers.reverseGet(w).asJson,
        "micrometers" -> Wavelength.decimalMicrometers.reverseGet(w).asJson,
      )

    def apply(c: HCursor): Decoder.Result[Wavelength] =
      c.downField("picometers").as[Int].flatMap { pm =>
        Wavelength
          .fromPicometers
          .getOption(pm)
          .toRight(DecodingFailure(s"Invalid wavelength picometers value: $pm", c.history))
      }
  }

}

object SourceProfileCodecHelper extends SourceProfileCodecHelper

