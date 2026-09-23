// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.enums.ExchangeObservingModeType
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosNorthIfuFpu
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosSouthIfuFpu
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFpuIfu
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.enums.WaterVapor
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Region
import lucuma.core.model.CloudExtinction
import lucuma.core.model.Configuration
import lucuma.core.model.Configuration.Conditions
import lucuma.core.model.Configuration.ObservingMode
import lucuma.core.model.Configuration.ObservingMode.*
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.ImageQuality
import lucuma.odb.json.angle.query.given
import lucuma.odb.json.coordinates.query.given
import lucuma.odb.json.region.query.given
import lucuma.odb.syntax.observingModeType.*

object configurationrequest:

  trait QueryCodec:
    object DecodingFailures:
      val NoReferenceCoordinates = DecodingFailure("Reference coordinates are undefined.", Nil)
      val NoObservingMode = DecodingFailure("Observing mode is undefined.", Nil)

    given Decoder[Conditions] = hc =>
      for
        c <- hc.downField("cloudExtinction").as[CloudExtinction.Preset]
        i <- hc.downField("imageQuality").as[ImageQuality.Preset]
        s <- hc.downField("skyBackground").as[SkyBackground]
        w <- hc.downField("waterVapor").as[WaterVapor]
      yield Conditions(c, i, s, w)

    given Encoder[Conditions] = c =>
      Json.obj(
        "cloudExtinction" -> c.cloudExtinction.asJson,
        "imageQuality" -> c.imageQuality.asJson,
        "skyBackground" -> c.skyBackground.asJson,
        "waterVapor" -> c.waterVapor.asJson,
      )

    val DecodeFlamingos2LongSlit: Decoder[Flamingos2LongSlit] = hc =>
      hc.downField("disperser").as[Flamingos2Disperser].map(Flamingos2LongSlit(_))

    val DecodeFlamingos2Mos: Decoder[Flamingos2Mos] = hc =>
      hc.downField("disperser").as[Flamingos2Disperser].map(Flamingos2Mos(_))

    val DecodeGmosNorthImaging: Decoder[GmosNorthImaging] = hc =>
      hc.downField("filters").as[List[GmosNorthFilter]].map(GmosNorthImaging(_))

    val DecodeGmosNorthLongSlit: Decoder[GmosNorthLongSlit] = hc =>
      hc.downField("grating").as[GmosNorthGrating].map(GmosNorthLongSlit(_))

    val DecodeGmosNorthMos: Decoder[GmosNorthMos] = hc =>
      hc.downField("grating").as[GmosNorthGrating].map(GmosNorthMos(_))

    val DecodeGmosSouthImaging: Decoder[GmosSouthImaging] = hc =>
      hc.downField("filters").as[List[GmosSouthFilter]].map(GmosSouthImaging(_))

    val DecodeGmosSouthLongSlit: Decoder[GmosSouthLongSlit] = hc =>
      hc.downField("grating").as[GmosSouthGrating].map(GmosSouthLongSlit(_))

    val DecodeGmosSouthMos: Decoder[GmosSouthMos] = hc =>
      hc.downField("grating").as[GmosSouthGrating].map(GmosSouthMos(_))

    val DecodeGmosNorthIfu: Decoder[GmosNorthIfu] = hc =>
      for
        grating <- hc.downField("grating").as[GmosNorthGrating]
        fpu     <- hc.downField("fpu").as[GmosNorthIfuFpu]
      yield GmosNorthIfu(grating, fpu)

    val DecodeGmosSouthIfu: Decoder[GmosSouthIfu] = hc =>
      for
        grating <- hc.downField("grating").as[GmosSouthGrating]
        fpu     <- hc.downField("fpu").as[GmosSouthIfuFpu]
      yield GmosSouthIfu(grating, fpu)

    val DecodeGnirsLongSlit: Decoder[GnirsLongSlit] = hc =>
      for
        grating <- hc.downField("grating").as[GnirsGrating]
        camera  <- hc.downField("camera").as[GnirsCamera]
        prism   <- hc.downField("prism").as[GnirsPrism]
      yield GnirsLongSlit(grating, camera, prism)

    val DecodeGnirsIfu: Decoder[GnirsIfu] = hc =>
      for
        grating <- hc.downField("grating").as[GnirsGrating]
        fpu     <- hc.downField("fpu").as[GnirsFpuIfu]
      yield GnirsIfu(grating, fpu)

    val DecodeVisitor: Decoder[Visitor] = hc =>
      for
        m <- hc.downField("mode").as[VisitorObservingModeType]
        r <- hc.downField("radius").as[Angle]
      yield Visitor(m, r)

    /**
     * Decodes what the `observingMode` selection returns, dispatching on `mode` -- which the
     * schema declares non-null -- rather than trying each sub-object in turn. Matching the
     * enum makes this exhaustive, so a mode added to `ObservingModeType` without a case here
     * fails the build instead of surfacing at runtime as an unreadable configuration. It also
     * keeps the reported error honest: a sub-object that fails to decode says so, rather than
     * being swallowed and reported as an unrecognized mode.
     */
    given Decoder[ObservingMode] = hc =>
      val modeField = hc.downField("mode")

      def sub[A <: ObservingMode](field: String, d: Decoder[A]): Decoder.Result[ObservingMode] =
        hc.downField(field).as(using d)

      modeField.as[ObservingModeType].flatMap:
        case ObservingModeType.Flamingos2LongSlit => sub("flamingos2LongSlit", DecodeFlamingos2LongSlit)
        case ObservingModeType.Flamingos2Mos      => sub("flamingos2Mos",      DecodeFlamingos2Mos)
        case ObservingModeType.GmosNorthImaging   => sub("gmosNorthImaging",   DecodeGmosNorthImaging)
        case ObservingModeType.GmosNorthLongSlit  => sub("gmosNorthLongSlit",  DecodeGmosNorthLongSlit)
        case ObservingModeType.GmosNorthMos       => sub("gmosNorthMos",       DecodeGmosNorthMos)
        case ObservingModeType.GmosNorthIfu       => sub("gmosNorthIfu",       DecodeGmosNorthIfu)
        case ObservingModeType.GmosSouthImaging   => sub("gmosSouthImaging",   DecodeGmosSouthImaging)
        case ObservingModeType.GmosSouthLongSlit  => sub("gmosSouthLongSlit",  DecodeGmosSouthLongSlit)
        case ObservingModeType.GmosSouthMos       => sub("gmosSouthMos",       DecodeGmosSouthMos)
        case ObservingModeType.GmosSouthIfu       => sub("gmosSouthIfu",       DecodeGmosSouthIfu)
        case ObservingModeType.GnirsLongSlit      => sub("gnirsLongSlit",      DecodeGnirsLongSlit)
        case ObservingModeType.GnirsIfu           => sub("gnirsIfu",           DecodeGnirsIfu)
        case _: VisitorObservingModeType          => sub("visitor",            DecodeVisitor)

        // These modes have no parameters, so the mode alone identifies them.
        case ObservingModeType.Flamingos2Imaging  => Flamingos2Imaging.asRight
        case ObservingModeType.GhostIfu           => GhostIfu.asRight
        case ObservingModeType.GnirsImaging       => GnirsImaging.asRight
        case ObservingModeType.Igrins2LongSlit    => Igrins2LongSlit.asRight

        // Exchange observations are not approved through configuration requests and have no
        // `Configuration.ObservingMode`. `ConfigurationService` relies on this failing.
        case _: ExchangeObservingModeType         =>
          // Report the mode as it arrived, rather than the enum's name.
          Left(DecodingFailure(s"couldn't decode mode: ${modeField.as[String].getOrElse("")}", Nil))

    given Encoder[ObservingMode] = m =>
      Json.obj(
        // `instrument` and `mode` are the two scalar fields of `ConfigurationObservingMode`.
        // They are derived from the mode rather than stored, exactly as the Grackle mappings
        // derive them, so that this JSON and the SQL-mapped path agree field for field.
        "instrument"         -> m.tpe.instrumentOption.asJson,
        "mode"               -> m.tpe.asJson,
        "flamingos2Imaging"  -> Json.Null, // one of these will be replaced below
        "flamingos2LongSlit" -> Json.Null, // one of these will be replaced below
        "flamingos2Mos"      -> Json.Null, // one of these will be replaced below
        "ghostIfu"           -> Json.Null, // one of these will be replaced below
        "gmosNorthImaging"   -> Json.Null, // one of these will be replaced below
        "gmosNorthLongSlit"  -> Json.Null, // one of these will be replaced below
        "gmosNorthMos"       -> Json.Null, // one of these will be replaced below
        "gmosSouthImaging"   -> Json.Null, // one of these will be replaced below
        "gmosSouthLongSlit"  -> Json.Null, // one of these will be replaced below
        "gmosSouthMos"       -> Json.Null, // one of these will be replaced below
        "gmosNorthIfu"       -> Json.Null, // one of these will be replaced below
        "gmosSouthIfu"       -> Json.Null, // one of these will be replaced below
        "gnirsLongSlit"      -> Json.Null,
        "gnirsIfu"           -> Json.Null,
        "gnirsImaging"       -> Json.Null,
        "igrins2LongSlit"    -> Json.Null, // one of these will be replaced below
        "visitor"            -> Json.Null,  // one of these will be replaced below
        m match
          case Flamingos2Imaging                     => "flamingos2Imaging"  -> Json.obj("ignore" -> Json.Null)
          case Flamingos2LongSlit(disperser)         => "flamingos2LongSlit" -> Json.obj("disperser" -> disperser.asJson)
          case Flamingos2Mos(disperser)              => "flamingos2Mos"      -> Json.obj("disperser" -> disperser.asJson)
          case GhostIfu                              => "ghostIfu"           -> Json.obj("ignore" -> Json.Null)
          case GmosNorthImaging(filters)             => "gmosNorthImaging"   -> Json.obj("filters" -> filters.asJson)
          case GmosNorthLongSlit(grating)            => "gmosNorthLongSlit"  -> Json.obj("grating" -> grating.asJson)
          case GmosNorthMos(grating)                 => "gmosNorthMos"       -> Json.obj("grating" -> grating.asJson)
          case GmosSouthImaging(filters)             => "gmosSouthImaging"   -> Json.obj("filters" -> filters.asJson)
          case GmosSouthLongSlit(grating)            => "gmosSouthLongSlit"  -> Json.obj("grating" -> grating.asJson)
          case GmosSouthMos(grating)                 => "gmosSouthMos"       -> Json.obj("grating" -> grating.asJson)
          case GmosNorthIfu(grating, fpu)            => "gmosNorthIfu"       -> Json.obj("grating" -> grating.asJson, "fpu" -> fpu.asJson)
          case GmosSouthIfu(grating, fpu)            => "gmosSouthIfu"       -> Json.obj("grating" -> grating.asJson, "fpu" -> fpu.asJson)
          case GnirsLongSlit(grating, camera, prism) => "gnirsLongSlit"      -> Json.obj("grating" -> grating.asJson, "camera" -> camera.asJson, "prism" -> prism.asJson)
          case GnirsIfu(grating, fpu)                => "gnirsIfu"           -> Json.obj("grating" -> grating.asJson, "fpu" -> fpu.asJson)
          case GnirsImaging                          => "gnirsImaging"       -> Json.obj("ignore" -> Json.Null)
          case Igrins2LongSlit                       => "igrins2LongSlit"    -> Json.obj("ignore" -> Json.Null)
          case Visitor(mode, radius)                 => "visitor"            -> Json.obj("mode" -> mode.asJson, "radius" -> radius.asJson)
      )

    given Encoder[Either[Coordinates, Region]] = e =>
      Json.obj(
        "coordinates" -> e.left.toOption.asJson,
        "region" -> e.toOption.asJson,
      )

    given Decoder[Option[Either[Coordinates, Region]]] = hc =>
      (
        hc.downField("coordinates").as[Option[Coordinates]],
        hc.downField("region").as[Option[Region]]
      ).tupled.flatMap:
        case (Some(c), None)    => Left(c).some.asRight
        case (None, Some(r))    => Right(r).some.asRight
        case (None, None)       => None.asRight
        case (Some(_), Some(_)) => Left(DecodingFailure("Cannot decode target; both coords and region are defined.", Nil))

    /** A decoder based on the GraphQL schema, used for recursive service queries. */
    given Decoder[Configuration] = hc =>
      (
        hc.downField("conditions").as[Conditions],
        hc.downField("target").as[Option[Either[Coordinates, Region]]], // may be missing
        hc.downField("observingMode").as[Option[ObservingMode]]
      ).tupled.flatMap:
        case (conds, Some(coords), Some(mode)) => Right(Configuration(conds, coords, mode, None))
        case (conds, None, _)                  => Left(DecodingFailures.NoReferenceCoordinates)
        case (conds, _, None)                  => Left(DecodingFailures.NoObservingMode)

    given Encoder[Configuration] = c =>
      Json.obj(
        "conditions" -> c.conditions.asJson,
        "target" -> c.target.asJson,
        "observingMode" -> c.observingMode.asJson
      )

    given Decoder[ConfigurationRequest] = hc =>
      for
        id <- hc.downField("id").as[ConfigurationRequest.Id]
        st <- hc.downField("status").as[ConfigurationRequestStatus]
        ju <- hc.downField("justification").as[Option[NonEmptyString]]
        cf <- hc.downField("configuration").as[Configuration]
      yield ConfigurationRequest(id, st, ju, cf)

    given Encoder[ConfigurationRequest] = cr =>
      Json.obj(
        "id" -> cr.id.asJson,
        "status" -> cr.status.asJson,
        "justification" -> cr.justification.asJson,
        "configuration" -> cr.configuration.asJson
      )

  object query extends QueryCodec
