// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.data.NonEmptyMap
import cats.instances.order.*
import cats.syntax.either.*
import cats.syntax.traverse.*
import coulomb.Quantity
import coulomb.units.si.Kelvin
import eu.timepit.refined.api.Refined
import eu.timepit.refined.types.numeric.PosInt
import io.circe.ACursor
import io.circe.Codec
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.enums.Band
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.BrightnessValue
import lucuma.core.math.FluxDensityContinuumValue
import lucuma.core.math.LineFluxValue
import lucuma.core.math.LineWidthValue
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.*
import lucuma.core.math.units.KilometersPerSecond
import lucuma.core.model.Attachment
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
import lucuma.core.model.UnnormalizedSED.UserDefinedAttachment
import lucuma.core.util.*

import scala.collection.immutable.SortedMap

trait SourceProfileCodec {

  import angle.decoder.given
  import numeric.given
  import wavelength.decoder.given

  private def refinedBigDecimalCodec[V](
    name:           String,
    range:          String,
    toBigDecimal:   V => BigDecimal,
    fromBigDecimal: BigDecimal => Either[String, V]
  ): Codec[V] =
    new Codec[V] {
      def apply(v: V): Json =
        toBigDecimal(v).asJson

      def apply(c: HCursor): Decoder.Result[V] =
        c.as[BigDecimal].flatMap { bd =>
          fromBigDecimal(bd).leftMap(_ => DecodingFailure(s"Illegal $name value, $bd. Must be in $range", c.history))
        }
    }

  given Codec[BrightnessValue] =
    refinedBigDecimalCodec[BrightnessValue](
      "brightness value",
      "[-30, 100,000,000]",
      _.value.value,
      BrightnessValue.from
    )

  given Codec[FluxDensityContinuumValue] =
    refinedBigDecimalCodec[FluxDensityContinuumValue](
      "flux density continuum value",
      "[0, 1]",
      _.value.value,
      FluxDensityContinuumValue.from
    )

  given Codec[LineFluxValue] =
    refinedBigDecimalCodec[LineFluxValue](
      "line flux value",
      "[0, 1]",
      _.value.value,
      LineFluxValue.from
    )

  given Codec[LineWidthValue] =
    refinedBigDecimalCodec[LineWidthValue](
      "line width value",
      "[0, 1,000,000]",
      _.value.value,
      LineWidthValue.from
    )

  given Codec[LineWidthQuantity] with {
    def apply(v: LineWidthQuantity): Json =
      v.value.asJson

    def apply(c: HCursor): Decoder.Result[LineWidthQuantity] =
      c.as[LineWidthValue].map { lwv => Quantity[KilometersPerSecond](lwv) }
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
      } yield tag[T](Measure[N](v, u, e))
  }

  // Map entry (A, B) with named keys
  private def entryDecoder[A: Decoder, B: Decoder](
    aKey: String,
    bKey: String
  ): Decoder[(A, B)] =
    Decoder.instance[(A, B)] { c =>
      for {
        a <- c.downField(aKey).as[A]
        b <- c.downField(bKey).as[B]
      } yield (a, b)
    }

  private def entryEncoder[A: Encoder, B: Encoder](
    aKey: String,
    bKey: String
  ): Encoder[(A, B)] =
    Encoder.instance[(A, B)] { (entry: (A, B)) =>
      Json.obj(
        aKey -> entry._1.asJson,
        bKey -> entry._2.asJson
      )
    }

  val DecoderFluxDensityEntry: Decoder[(Wavelength, BigDecimal)] =
    entryDecoder("wavelength", "density")

  def EncoderFluxDensityEntry(using Encoder[Wavelength]): Encoder[(Wavelength, BigDecimal)] =
    entryEncoder("wavelength", "density")

  given Decoder[UnnormalizedSED] =
    Decoder.instance { c =>
      def decode[A: Enumerated](n: String)(f: A => UnnormalizedSED): Decoder.Result[UnnormalizedSED] =
        c.downField(n).as[A].map(f)

      decode("stellarLibrary")(StellarLibrary(_))                                              orElse
        decode("coolStar")(CoolStarModel(_))                                                   orElse
        decode("galaxy")(Galaxy(_))                                                            orElse
        decode("planet")(Planet(_))                                                            orElse
        decode("quasar")(Quasar(_))                                                            orElse
        decode("hiiRegion")(HIIRegion(_))                                                      orElse
        decode("planetaryNebula")(PlanetaryNebula(_))                                          orElse
        c.downField("powerLaw").as[BigDecimal].map(PowerLaw(_))                                orElse
        c.downField("blackBodyTempK").as[PosInt].map { k => BlackBody(Quantity[Kelvin](k)) }   orElse
        c.downField("fluxDensities")
         .values
         .toRight(DecodingFailure("fluxDensities should hold an array of values", c.history))
         .flatMap(_.toList.traverse(json => DecoderFluxDensityEntry(json.hcursor)).map(SortedMap.from))
         .flatMap(m => NonEmptyMap.fromMap(m).toRight(DecodingFailure("At least one flux density entry is required for a user defined SED", c.history)))
         .map(UserDefined(_))                                                                  orElse
        c.downField("fluxDensitiesAttachment").as[Attachment.Id].map(UserDefinedAttachment(_)) orElse
        DecodingFailure(s"Could not decode SED: ${c.focus.map(_.spaces2)}", c.history).asLeft[UnnormalizedSED]
    }


  given (using Encoder[Wavelength]): Encoder[UnnormalizedSED] =
    Encoder.instance { a =>
      Json.obj(
        "stellarLibrary" -> Json.Null, // one of these will be replaced
        "coolStar"       -> Json.Null, // one of these will be replaced
        "galaxy"         -> Json.Null, // one of these will be replaced
        "planet"         -> Json.Null, // one of these will be replaced
        "quasar"         -> Json.Null, // one of these will be replaced
        "hiiRegion"      -> Json.Null, // one of these will be replaced
        "planetaryNebula"-> Json.Null, // one of these will be replaced
        "powerLaw"       -> Json.Null, // one of these will be replaced
        "blackBodyTempK" -> Json.Null, // one of these will be replaced
        "fluxDensities"  -> Json.Null, // one of these will be replaced
        "fluxDensitiesAttachment"  -> Json.Null, // one of these will be replaced
        a match {
          case StellarLibrary(librarySpectrum)          => "stellarLibrary"          -> librarySpectrum.asJson
          case CoolStarModel(temperature)               => "coolStar"                -> temperature.asJson // todo: tag
          case Galaxy(galaxySpectrum)                   => "galaxy"                  -> galaxySpectrum.asJson
          case Planet(planetSpectrum)                   => "planet"                  -> planetSpectrum.asJson
          case Quasar(quasarSpectrum)                   => "quasar"                  -> quasarSpectrum.asJson
          case HIIRegion(hiiRegionSpectrum)             => "hiiRegion"               -> hiiRegionSpectrum.asJson
          case PlanetaryNebula(planetaryNebulaSpectrum) => "planetaryNebula"         -> planetaryNebulaSpectrum.asJson
          case PowerLaw(index)                          => "powerLaw"                -> index.asJson
          case BlackBody(temperature)                   => "blackBodyTempK"          -> temperature.value.value.asJson
          case UserDefined(fluxDensities)               => "fluxDensities"           -> fluxDensities.toSortedMap.toList.map(EncoderFluxDensityEntry.apply).asJson
          case UserDefinedAttachment(attachmentId)      => "fluxDensitiesAttachment" -> attachmentId.asJson
        }
      )
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

  given [T](using Enumerated[Units Of Brightness[T]]): Decoder[BandNormalized[T]] = {

    def decodeBrightnessMap(c: ACursor): Decoder.Result[SortedMap[Band, BrightnessMeasure[T]]] =
      c.values.fold(SortedMap.empty[Band, BrightnessMeasure[T]].asRight) {
        _.toList
         .traverse(json => CodecBandBrightness[T](json.hcursor))
         .map(SortedMap.from)
      }

    Decoder.instance { c =>
      for {
        s <- c.downField("sed").as[Option[UnnormalizedSED]]
        b <- decodeBrightnessMap(c.downField("brightnesses"))
      } yield BandNormalized(s, b)
    }
  }

  given [T](using Encoder[Wavelength], Enumerated[Units Of Brightness[T]]): Encoder[BandNormalized[T]] =
    Encoder.instance { (bn: BandNormalized[T]) =>
      Json.obj(
        "brightnesses" -> bn.brightnesses.toList.map(CodecBandBrightness[T](_)).asJson,
        "sed"          -> bn.sed.asJson
      )
    }

  given [T](using Enumerated[Units Of LineFlux[T]], Enumerated[Units Of FluxDensityContinuum[T]]): Decoder[EmissionLines[T]] = {

    def decodeLinesMap(c: ACursor): Decoder.Result[SortedMap[Wavelength, EmissionLine[T]]] =
      c.values.fold(SortedMap.empty[Wavelength, EmissionLine[T]].asRight) {
        _.toList
         .traverse(json => DecoderWavelengthLine[T](json.hcursor))
         .map(SortedMap.from)
      }

    Decoder.instance { c =>
      for {
        l <- decodeLinesMap(c.downField("lines"))
        f <- c.downField("fluxDensityContinuum").as[FluxDensityContinuumMeasure[T]]
      } yield EmissionLines(l, f)
    }
  }

  given [T](using Encoder[Wavelength], Enumerated[Units Of LineFlux[T]], Enumerated[Units Of FluxDensityContinuum[T]]): Encoder[EmissionLines[T]] =
    Encoder.instance { (el: EmissionLines[T]) =>
      Json.obj(
        "lines"                -> el.lines.toList.map(EncoderWavelengthLine[T].apply(_)).asJson,
        "fluxDensityContinuum" -> el.fluxDensityContinuum.asJson
      )
    }

  def DecoderWavelengthLine[T](using Enumerated[Units Of LineFlux[T]]): Decoder[(Wavelength, EmissionLine[T])] =
    Decoder.instance { c =>
      for {
        w <- c.downField("wavelength").as[Wavelength]
        e <- Codec[EmissionLine[T]].apply(c)
      } yield (w, e)
    }

  def EncoderWavelengthLine[T](using Encoder[Wavelength], Enumerated[Units Of LineFlux[T]]): Encoder[(Wavelength, EmissionLine[T])] =
    Encoder.instance { (waveLine: (Wavelength, EmissionLine[T])) =>
      waveLine
        ._2
        .asJson
        .mapObject(_.add("wavelength", waveLine._1.asJson))
    }

  given [T](using Enumerated[Units Of LineFlux[T]]): Codec[EmissionLine[T]] with {

    def apply(el: EmissionLine[T]): Json =
      Json.obj(
        "lineWidth" -> el.lineWidth.asJson,
        "lineFlux"  -> el.lineFlux.asJson
      )

    def apply(c: HCursor): Decoder.Result[EmissionLine[T]] =
      for {
        w <- c.downField("lineWidth").as[LineWidthQuantity]
        f <- c.downField("lineFlux").as[LineFluxMeasure[T]]
      } yield EmissionLine(w, f)

  }

  // SpectralDefinition[T]
  given [T](using Enumerated[Units Of Brightness[T]], Enumerated[Units Of LineFlux[T]], Enumerated[Units Of FluxDensityContinuum[T]]): Decoder[SpectralDefinition[T]] =
    Decoder.instance { c =>
      c.downField("bandNormalized").as[BandNormalized[T]] orElse
        c.downField("emissionLines").as[EmissionLines[T]]
    }

  given [T](using Encoder[Wavelength], Enumerated[Units Of Brightness[T]], Enumerated[Units Of LineFlux[T]], Enumerated[Units Of FluxDensityContinuum[T]]): Encoder[SpectralDefinition[T]] =
    Encoder.instance { (sd: SpectralDefinition[T]) =>
      Json.obj(
        "bandNormalized" -> Json.Null, // one of these will be replaced
        "emissionLines"  -> Json.Null, // one of these will be replaced
        sd match {
          case bn: BandNormalized[T] => "bandNormalized" -> bn.asJson
          case el: EmissionLines[T]  => "emissionLines"  -> el.asJson
        }
      )
    }

  given Decoder[SourceProfile.Gaussian] =
    Decoder.instance { c =>
      for {
        f <- c.downField("fwhm").as[Angle]
        s <- Decoder[SpectralDefinition[Integrated]].apply(c)
      } yield SourceProfile.Gaussian(f, s)
    }

  given (using Encoder[Angle], Encoder[Wavelength]): Encoder[SourceProfile.Gaussian] =
    Encoder.instance { (g: SourceProfile.Gaussian) =>
      g.spectralDefinition
       .asJson
       .mapObject(_.add("fwhm", g.fwhm.asJson))
    }

  // type SourceProfile
  given Decoder[SourceProfile] =
    Decoder.instance { c =>
      c.downField("point").as[SpectralDefinition[Integrated]].map(SourceProfile.Point(_))    orElse
        c.downField("uniform").as[SpectralDefinition[Surface]].map(SourceProfile.Uniform(_)) orElse
        c.downField("gaussian").as[SourceProfile.Gaussian]
    }

  given (using Encoder[Angle], Encoder[Wavelength]): Encoder[SourceProfile] =
    Encoder.instance { a =>
      Json.obj(
        "point"    -> Json.Null, // one of these will be replaced
        "uniform"  -> Json.Null, // one of these will be replaced
        "gaussian" -> Json.Null, // one of these will be replaced
        a match {
          case SourceProfile.Point(sd)   => "point"   -> sd.asJson
          case SourceProfile.Uniform(sd) => "uniform" -> sd.asJson
          case g: SourceProfile.Gaussian => "gaussian"->  g.asJson
        }
      )
    }

}

object sourceprofile extends SourceProfileCodec
