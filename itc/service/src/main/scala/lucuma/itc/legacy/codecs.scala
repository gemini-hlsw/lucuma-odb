// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.data.NonEmptyChain
import cats.syntax.all.*
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.refineV
import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.Redshift
import lucuma.core.math.SignalToNoise
import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.syntax.*
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.core.syntax.display.*
import lucuma.core.syntax.string.*
import lucuma.itc.GraphType
import lucuma.itc.ItcGraph
import lucuma.itc.ItcGraphGroup
import lucuma.itc.ItcSeries
import lucuma.itc.SeriesDataType
import lucuma.itc.legacy.syntax.all.*
import lucuma.itc.service.ItcObservingConditions
import lucuma.itc.service.ObservingMode
import lucuma.itc.service.syntax.all.given

import java.math.MathContext
import scala.util.Try

private[legacy] object codecs:
  ////////////////////////////////////////////////////////////
  //
  // These are encoders/decoders used to communicate with the
  // old ocs2-based itc
  //
  ////////////////////////////////////////////////////////////
  private def toItcAirmass(m: Double): Double =
    if (m <= 1.35) 1.2 else if (m <= 1.75) 1.5 else 2.0

  given Encoder[ItcObservingConditions] =
    import lucuma.itc.legacy.syntax.conditions.*
    Encoder.forProduct5("exactiq", "exactcc", "wv", "sb", "airmass") { a =>
      (Json.obj(
         "arcsec"            -> Json.fromBigDecimal(
           a.iq.round(MathContext.DECIMAL32)
         )
       ),
       Json.obj("extinction" -> Json.fromBigDecimal(a.cc)),
       a.wv.ocs2Tag,
       a.sb.ocs2Tag,
       toItcAirmass(a.airmass)
      )
    }

  given Encoder[Wavelength] = w =>
    Json.fromString:
      s"${Wavelength.decimalNanometers.reverseGet(w)} nm"

  given Decoder[Wavelength] = w =>
    val key: String = w.keys.flatMap(_.headOption).orEmpty
    Decoder
      .decodeBigDecimal(w)
      .flatMap(w =>
        Wavelength.decimalNanometers
          .getOption(w)
          .toRight(
            DecodingFailure(s"Invalid wavelength value no enum value matched for $w",
                            List(CursorOp.Field(key))
            )
          )
      )

  private val encodeGmosNorthSpectroscopy: Encoder[ObservingMode.SpectroscopyMode.GmosNorth] =
    new Encoder[ObservingMode.SpectroscopyMode.GmosNorth] {
      def apply(a: ObservingMode.SpectroscopyMode.GmosNorth): Json =
        Json.obj(
          // Translate observing mode to OCS2 style
          "centralWavelength" -> a.centralWavelength.asJson,
          "filter"            -> Json.obj(
            "FilterNorth" -> a.filter.fold[Json](Json.fromString("NONE"))(r =>
              Json.fromString(r.ocs2Tag)
            )
          ),
          "grating"           -> Json.obj("DisperserNorth" -> Json.fromString(a.disperser.ocs2Tag)),
          "fpMask"            -> Json.obj("FPUnitNorth" -> Json.fromString(a.fpu.builtin.ocs2Tag)),
          "spectralBinning"   -> Json.fromInt(
            a.ccdMode.map(_.xBin).getOrElse(GmosXBinning.One).count.value
          ),
          "site"              -> Json.fromString("GN"),
          "ccdType"           -> Json.fromString("HAMAMATSU"),
          "ampReadMode"       -> Json.fromString(
            a.ccdMode.map(_.ampReadMode).getOrElse(GmosAmpReadMode.Fast).tag.toUpperCase
          ),
          "builtinROI"        -> Json.fromString(
            a.roi.getOrElse(GmosRoi.FullFrame).tag.toScreamingSnakeCase
          ),
          "spatialBinning"    -> Json
            .fromInt(a.ccdMode.map(_.yBin).getOrElse(GmosYBinning.One).count.value),
          "customSlitWidth"   -> Json.Null,
          "ampGain"           -> Json.fromString(
            a.ccdMode.map(_.ampGain).getOrElse(GmosAmpGain.Low).tag.toUpperCase
          )
        )
    }

  private val encodeGmosNorthImaging: Encoder[ObservingMode.ImagingMode.GmosNorth] = a =>
    Json.obj(
      // Translate observing mode to OCS2 style
      "centralWavelength" -> a.centralWavelength.asJson,
      "filter"            -> Json.obj(
        "FilterNorth" ->
          Json.fromString(a.filter.ocs2Tag)
      ),
      "grating"           -> Json.obj("DisperserNorth" -> "MIRROR".asJson),
      "fpMask"            -> Json.obj("FPUnitNorth" -> "FPU_NONE".asJson),
      "spectralBinning"   -> Json.fromInt(
        a.ccdMode.map(_.xBin).getOrElse(GmosXBinning.Two).count.value
      ),
      "site"              -> Json.fromString("GN"),
      "ccdType"           -> Json.fromString("HAMAMATSU"),
      "ampReadMode"       -> Json.fromString(
        a.ccdMode.map(_.ampReadMode).getOrElse(GmosAmpReadMode.Fast).tag.toUpperCase
      ),
      "builtinROI"        -> Json.fromString("FULL_FRAME"),
      "spatialBinning"    -> Json
        .fromInt(a.ccdMode.map(_.yBin).getOrElse(GmosYBinning.Two).count.value),
      "customSlitWidth"   -> Json.Null,
      "ampGain"           -> Json.fromString(
        a.ccdMode.map(_.ampGain).getOrElse(GmosAmpGain.Low).tag.toUpperCase
      )
    )

  private val encodeGmosSouthSpectroscopy: Encoder[ObservingMode.SpectroscopyMode.GmosSouth] = a =>
    Json.obj(
      // Translate observing mode to OCS2 style
      "centralWavelength" -> a.centralWavelength.asJson,
      "filter"            -> Json.obj(
        "FilterSouth" -> a.filter.fold[Json](Json.fromString("NONE"))(r =>
          Json.fromString(r.ocs2Tag)
        )
      ),
      "grating"           -> Json.obj("DisperserSouth" -> Json.fromString(a.disperser.ocs2Tag)),
      "fpMask"            -> Json.obj("FPUnitSouth" -> Json.fromString(a.fpu.builtin.ocs2Tag)),
      "spectralBinning"   -> Json.fromInt(
        a.ccdMode.map(_.xBin).getOrElse(GmosXBinning.One).count.value
      ),
      "site"              -> Json.fromString("GS"),
      "ccdType"           -> Json.fromString("HAMAMATSU"),
      "ampReadMode"       -> Json.fromString(
        a.ccdMode.map(_.ampReadMode).getOrElse(GmosAmpReadMode.Fast).tag.toUpperCase
      ),
      "builtinROI"        -> Json.fromString(
        a.roi.getOrElse(GmosRoi.FullFrame).tag.toScreamingSnakeCase
      ),
      "spatialBinning"    -> Json.fromInt(
        a.ccdMode.map(_.yBin).getOrElse(GmosYBinning.One).count.value
      ),
      "customSlitWidth"   -> Json.Null,
      "ampGain"           -> Json.fromString(
        a.ccdMode.map(_.ampGain).getOrElse(GmosAmpGain.Low).tag.toUpperCase
      )
    )

  private val encodeF2Spectroscopy: Encoder[ObservingMode.SpectroscopyMode.Flamingos2] = a =>
    Json.obj(
      // Translate observing mode to OCS2 style
      "filter"          -> Json.fromString(a.filter.ocs2Tag),
      "grism"           -> Json.fromString(a.disperser.ocs2Tag),
      "mask"            -> Json.fromString(a.fpu.ocs2Tag),
      "readMode"        -> Json.fromString("FAINT_OBJECT_SPEC"),
      "customSlitWidth" -> Json.Null
    )

  private val encodeF2Imaging: Encoder[ObservingMode.ImagingMode.Flamingos2] = a =>
    Json.obj(
      // Translate observing mode to OCS2 style
      "filter"          -> Json.fromString(a.filter.ocs2Tag),
      "grism"           -> Json.fromString("NONE"),
      "mask"            -> Json.fromString("FPU_NONE"),
      "readMode"        -> Json.fromString("FAINT_OBJECT_SPEC"),
      "customSlitWidth" -> Json.Null
    )

  private val encodeGmosSouthImaging: Encoder[ObservingMode.ImagingMode.GmosSouth] =
    new Encoder[ObservingMode.ImagingMode.GmosSouth] {
      def apply(a: ObservingMode.ImagingMode.GmosSouth): Json =
        Json.obj(
          // Translate observing mode to OCS2 style
          "centralWavelength" -> a.centralWavelength.asJson,
          "filter"            -> Json.obj(
            "FilterSouth" ->
              Json.fromString(a.filter.ocs2Tag)
          ),
          "grating"           -> Json.obj("DisperserSouth" -> "MIRROR".asJson),
          "fpMask"            -> Json.obj("FPUnitSouth" -> "FPU_NONE".asJson),
          "spectralBinning"   -> Json.fromInt(
            a.ccdMode.map(_.xBin).getOrElse(GmosXBinning.Two).count.value
          ),
          "site"              -> Json.fromString("GS"),
          "ccdType"           -> Json.fromString("HAMAMATSU"),
          "ampReadMode"       -> Json.fromString(
            a.ccdMode.map(_.ampReadMode).getOrElse(GmosAmpReadMode.Fast).tag.toUpperCase
          ),
          "builtinROI"        -> Json.fromString("FULL_FRAME"),
          "spatialBinning"    -> Json.fromInt(
            a.ccdMode.map(_.yBin).getOrElse(GmosYBinning.Two).count.value
          ),
          "customSlitWidth"   -> Json.Null,
          "ampGain"           -> Json.fromString(
            a.ccdMode.map(_.ampGain).getOrElse(GmosAmpGain.Low).tag.toUpperCase
          )
        )
    }

  private given Encoder[ItcInstrumentDetails] = (a: ItcInstrumentDetails) =>
    a.mode match
      case a: ObservingMode.SpectroscopyMode.GmosNorth  =>
        Json.obj("GmosParameters" -> encodeGmosNorthSpectroscopy(a))
      case a: ObservingMode.SpectroscopyMode.GmosSouth  =>
        Json.obj("GmosParameters" -> encodeGmosSouthSpectroscopy(a))
      case a: ObservingMode.SpectroscopyMode.Flamingos2 =>
        Json.obj("Flamingos2Parameters" -> encodeF2Spectroscopy(a))
      case a: ObservingMode.ImagingMode.Flamingos2      =>
        Json.obj("Flamingos2Parameters" -> encodeF2Imaging(a))
      case a: ObservingMode.ImagingMode.GmosNorth       =>
        Json.obj("GmosParameters" -> encodeGmosNorthImaging(a))
      case a: ObservingMode.ImagingMode.GmosSouth       =>
        Json.obj("GmosParameters" -> encodeGmosSouthImaging(a))

  private given Encoder[ItcWavefrontSensor] = Encoder[String].contramap(_.ocs2Tag)

  private given Encoder[ItcTelescopeDetails] = (a: ItcTelescopeDetails) =>
    Json.obj(
      "mirrorCoating"  -> Json.fromString("SILVER"),
      "instrumentPort" -> Json.fromString("SIDE_LOOKING"),
      "wfs"            -> a.wfs.asJson
    )

  given Encoder[SourceProfile] = (a: SourceProfile) =>
    import SourceProfile._
    a match {
      case Point(_)          =>
        Json.obj("PointSource" -> Json.obj())
      case Uniform(_)        => Json.obj("UniformSource" -> Json.obj())
      case Gaussian(fwhm, _) =>
        Json.obj(
          "GaussianSource" -> Json.obj(
            "fwhm" -> Angle.signedDecimalArcseconds.get(fwhm).asJson
          )
        )
    }

  private given Encoder[UnnormalizedSED] = (a: UnnormalizedSED) =>
    import UnnormalizedSED.*
    a match
      case BlackBody(t)               =>
        Json.obj(
          "BlackBody" -> Json.obj(
            "temperature" -> Json.fromDoubleOrNull(t.value.value.toDouble)
          )
        )
      case PowerLaw(i)                =>
        Json.obj("PowerLaw" -> Json.obj("index" -> Json.fromDoubleOrNull(i.toDouble)))
      case StellarLibrary(s)          =>
        Json.obj("Library" -> Json.obj("LibraryStar" -> Json.fromString(s.ocs2Tag)))
      case s: CoolStarModel           =>
        Json.obj("Library" -> Json.obj("LibraryStar" -> Json.fromString(s.ocs2Tag)))
      case PlanetaryNebula(s)         =>
        Json.obj("Library" -> Json.obj("LibraryNonStar" -> Json.fromString(s.ocs2Tag)))
      case Galaxy(s)                  =>
        Json.obj("Library" -> Json.obj("LibraryNonStar" -> Json.fromString(s.ocs2Tag)))
      case Planet(s)                  =>
        Json.obj("Library" -> Json.obj("LibraryNonStar" -> Json.fromString(s.ocs2Tag)))
      case HIIRegion(s)               =>
        Json.obj("Library" -> Json.obj("LibraryNonStar" -> Json.fromString(s.ocs2Tag)))
      case Quasar(s)                  =>
        Json.obj("Library" -> Json.obj("LibraryNonStar" -> Json.fromString(s.ocs2Tag)))
      case UserDefinedAttachment(_)   => sys.error("Unsupported")
      case UserDefined(fluxDensities) =>
        Json.obj(
          "UserDefined" -> Json.obj(
            "UserDefinedSpectrum" -> Json.obj(
              "name"     -> Json.fromString("UserDefined"),
              "spectrum" -> Json.fromString:
                fluxDensities.toNel.toList
                  .map: (w, f) =>
                    s"${w.toNanometers} $f"
                  .mkString("\n")
            )
          )
        )

  private given Encoder[Band] =
    Encoder[String].contramap(_.shortName)

  private given Encoder[Redshift] =
    Encoder.forProduct1("z")(_.z)

  private def encodeEmissionLine[T](
    wavelength:           Wavelength,
    lineWidth:            LineWidthQuantity,
    lineflux:             LineFluxMeasure[T],
    fluxDensityContinuum: FluxDensityContinuumMeasure[T]
  ): Json =
    // Old itc doesn't understand the last angular surface unit
    Json.obj(
      "EmissionLine" -> Json.obj(
        "wavelength" -> wavelength.asJson,
        "width"      -> lineWidth.toMeasure.shortName.asJson,
        "flux"       -> lineflux.exact.shortName.replace("/arcsec²", "").asJson,
        "continuum"  -> fluxDensityContinuum.exact.shortName.replace("/arcsec²", "").asJson
      )
    )

  given Encoder[ItcSourceDefinition] = (s: ItcSourceDefinition) =>
    val source: Json = s.sourceProfile match
      case SourceProfile.Point(_)          =>
        Json.obj("PointSource" -> Json.obj())
      case SourceProfile.Uniform(_)        => Json.obj("UniformSource" -> Json.obj())
      case SourceProfile.Gaussian(fwhm, _) =>
        Json.obj(
          "GaussianSource" -> Json.obj(
            "fwhm" -> Angle.signedDecimalArcseconds.get(fwhm).asJson
          )
        )

    val units: Json = (s.bandOrLine, s.sourceProfile) match
      case (Left(band), SourceProfile.Point(SpectralDefinition.BandNormalized(_, brightnesses))) =>
        brightnesses
          .get(band)
          .map: b =>
            Json.obj("MagnitudeSystem" -> b.units.abbv.stripSuffix(" mag").asJson)
          .getOrElse(Json.Null)
      case (Left(band),
            SourceProfile.Uniform(SpectralDefinition.BandNormalized(_, brightnesses))
          ) =>
        brightnesses
          .get(band)
          .map: b =>
            Json.obj("SurfaceBrightness" -> b.units.abbv.asJson)
          .getOrElse(Json.Null)
      case (Left(band),
            SourceProfile.Gaussian(_, SpectralDefinition.BandNormalized(_, brightnesses))
          ) =>
        brightnesses
          .get(band)
          .map: b =>
            Json.obj("MagnitudeSystem" -> b.units.abbv.stripSuffix(" mag").asJson)
          .getOrElse(Json.Null)
      case (Right(_),
            SourceProfile.Point(SpectralDefinition.EmissionLines(_, fluxDensityContinuum))
          ) =>
        Json.obj("MagnitudeSystem" -> fluxDensityContinuum.units.abbv.asJson)
      case (Right(_),
            SourceProfile.Uniform(SpectralDefinition.EmissionLines(_, fluxDensityContinuum))
          ) =>
        Json.obj("SurfaceBrightness" -> fluxDensityContinuum.units.abbv.asJson)
      case (Right(_),
            SourceProfile.Gaussian(_, SpectralDefinition.EmissionLines(_, fluxDensityContinuum))
          ) =>
        Json.obj("MagnitudeSystem" -> fluxDensityContinuum.units.abbv.asJson)
      case _                                                                                     =>
        Json.Null

    val value: Json = (s.bandOrLine, s.sourceProfile) match
      case (Left(band), SourceProfile.Point(SpectralDefinition.BandNormalized(_, brightnesses))) =>
        brightnesses
          .get(band)
          .map(_.value)
          .asJson
      case (Left(band),
            SourceProfile.Uniform(SpectralDefinition.BandNormalized(_, brightnesses))
          ) =>
        brightnesses
          .get(band)
          .map(_.value)
          .asJson
      case (Left(band),
            SourceProfile.Gaussian(_, SpectralDefinition.BandNormalized(_, brightnesses))
          ) =>
        brightnesses
          .get(band)
          .map(_.value)
          .asJson
      case _                                                                                     =>
        Json.Null

    val distribution: Json = (s.bandOrLine, s.sourceProfile) match
      case (Left(band), SourceProfile.Point(SpectralDefinition.BandNormalized(sed, _)))       =>
        sed.asJson
      case (Right(wavelength),
            SourceProfile.Point(SpectralDefinition.EmissionLines(lines, fluxDensityContinuum))
          ) =>
        lines
          .get(wavelength)
          .map: line =>
            encodeEmissionLine(wavelength, line.lineWidth, line.lineFlux, fluxDensityContinuum)
          .getOrElse(Json.Null)
      case (Left(band), SourceProfile.Uniform(SpectralDefinition.BandNormalized(sed, _)))     =>
        sed.asJson
      case (Right(wavelength),
            SourceProfile.Uniform(SpectralDefinition.EmissionLines(lines, fluxDensityContinuum))
          ) =>
        lines
          .get(wavelength)
          .map: line =>
            encodeEmissionLine(wavelength, line.lineWidth, line.lineFlux, fluxDensityContinuum)
          .getOrElse(Json.Null)
      case (Left(band), SourceProfile.Gaussian(_, SpectralDefinition.BandNormalized(sed, _))) =>
        sed.asJson
      case (Right(wavelength),
            SourceProfile.Gaussian(_, SpectralDefinition.EmissionLines(lines, fluxDensityContinuum))
          ) =>
        lines
          .get(wavelength)
          .map: line =>
            encodeEmissionLine(wavelength, line.lineWidth, line.lineFlux, fluxDensityContinuum)
          .getOrElse(Json.Null)
      case _                                                                                  =>
        Json.Null

    val normBand: Json = // Use a dummy value in case of emission lines
      s.bandOrLine.fold(_.asJson, _ => (Band.R: Band).asJson)

    Json.obj(
      "profile"      -> source,
      "normBand"     -> normBand,
      "norm"         -> value,
      "redshift"     -> s.redshift.asJson,
      "units"        -> units,
      "distribution" -> distribution
    )

  given Encoder[ItcParameters] =
    deriveEncoder[ItcParameters]

  private given Decoder[SeriesDataType] = (c: HCursor) =>
    Decoder.decodeJsonObject(c).flatMap { str =>
      val key = str.keys.headOption.orEmpty
      Try(SeriesDataType.valueOf(key)).toEither.leftMap { _ =>
        DecodingFailure(s"no enum value matched for $key", List(CursorOp.Field(key)))
      }
    }

  private given Decoder[GraphType] = (c: HCursor) =>
    val byLegacyKey: Map[String, GraphType] = Map(
      "SignalChart"      -> GraphType.SignalGraph,
      "SignalPixelChart" -> GraphType.SignalPixelGraph,
      "S2NChart"         -> GraphType.S2NGraph
    )

    Decoder.decodeJsonObject(c).flatMap { str =>
      val key: String = str.keys.headOption.orEmpty
      byLegacyKey
        .get(key)
        .toRight:
          DecodingFailure(s"no enum value matched for $key", List(CursorOp.Field(key)))
    }

  private given Decoder[ItcSeries] = (c: HCursor) =>
    for
      title <- c.downField("title").as[String]
      dt    <- c.downField("dataType").as[SeriesDataType]
      data  <- c.downField("data")
                 .as[List[List[Double]]]
                 .map { i =>
                   (i.lift(0), i.lift(1)) match
                     case (Some(a), Some(b)) if a.length === b.length => a.zip(b)
                     case _                                           => List.empty
                 }
    yield ItcSeries(title, dt, data)

  given Decoder[ItcGraph] = (c: HCursor) =>
    for
      series <- c.downField("series").as[List[ItcSeries]]
      d      <- c.downField("chartType").as[GraphType]
    yield ItcGraph(d, series)

  given Decoder[ItcGraphGroup] = (c: HCursor) =>
    c.downField("charts").as[NonEmptyChain[ItcGraph]].map(ItcGraphGroup.apply)

  given Decoder[GraphsRemoteResult] = (c: HCursor) =>
    for
      graphs <- (c.downField("ItcSpectroscopyResult") |+| c.downField("ItcImagingResult"))
                  .downField("chartGroups")
                  .as[NonEmptyChain[ItcGraphGroup]]
      ccd    <- (c.downField("ItcSpectroscopyResult") |+| c.downField("ItcImagingResult"))
                  .downField("ccds")
                  .as[NonEmptyChain[ItcRemoteCcd]]
    yield GraphsRemoteResult(ccd, graphs)

  // SignalToNoise has already a decoder in lucuma-core, but it is too strict to
  // be used with the legacy ocs code
  given Decoder[SignalToNoise] = (c: HCursor) =>
    val maxSN  = SignalToNoise.Max.toBigDecimal
    val maxSNR = SignalToNoise.Max.asRight
    c.as[BigDecimal]
      .flatMap: s =>
        val r = s.setScale(2, BigDecimal.RoundingMode.HALF_UP)
        // Sometimes ITC returns absurdely large values, lets cap it at max
        if (r >= maxSN) maxSNR
        else
          SignalToNoise.FromBigDecimalRounding
            .getOption(s.setScale(2, BigDecimal.RoundingMode.HALF_UP))
            .toRight(DecodingFailure("Invalid SignalToNoise value", c.history))

  given Decoder[Exposures] = (c: HCursor) =>
    for
      time  <- c.downField("exposureTime").as[Double]
      count <-
        c
          .downField("exposures")
          .as[Int]
          .flatMap:
            refineV[NonNegative](_)
              .leftMap(e => DecodingFailure(e, c.downField("exposures").history))
    yield Exposures(time, count)

  given Decoder[AllExposureCalculations] = (c: HCursor) =>
    for
      results  <- c.downField("detectors").as[NonEmptyChain[Exposures]]
      selected <- c.downField("selected").as[Int]
    yield AllExposureCalculations(results, selected)

  given Decoder[SignalToNoiseAt] = (c: HCursor) =>
    for
      wv     <- c.downField("wavelength").as[Wavelength]
      single <- c.downField("single").as[SignalToNoise]
      total  <- c.downField("final").as[SignalToNoise]
    yield SignalToNoiseAt(wv, SingleSN(single), TotalSN(total))

  given Decoder[IntegrationTimeRemoteResult] = (c: HCursor) =>
    val spec: Option[Decoder.Result[IntegrationTimeRemoteResult]] =
      for {
        t    <- c.downField("ItcSpectroscopyResult")
                  .downField("times")
                  .success
                  .map:
                    _.as[AllExposureCalculations]
        s    <- c.downField("ItcSpectroscopyResult")
                  .downField("signalToNoiseAt")
                  .success
                  .map:
                    _.as[Option[SignalToNoiseAt]]
        ccds <- c.downField("ItcSpectroscopyResult")
                  .downField("ccds")
                  .success
                  .map:
                    _.as[NonEmptyChain[ItcRemoteCcd]]
      } yield (t, s, ccds).mapN(IntegrationTimeRemoteResult(_, _, _))

    val img: Option[Decoder.Result[IntegrationTimeRemoteResult]] =
      for {
        t    <- c.downField("ItcImagingResult")
                  .downField("times")
                  .success
                  .map:
                    _.as[AllExposureCalculations]
        s    <- c.downField("ItcImagingResult")
                  .downField("signalToNoiseAt")
                  .success
                  .map:
                    _.as[Option[SignalToNoiseAt]]
        ccds <- c.downField("ItcImagingResult")
                  .downField("ccds")
                  .success
                  .map:
                    _.as[NonEmptyChain[ItcRemoteCcd]]
      } yield (t, s, ccds).mapN(IntegrationTimeRemoteResult(_, _, _))

    spec
      .orElse(img)
      .getOrElse(Left(DecodingFailure("No valid IntegrationTimeRemoteResult", c.history)))
