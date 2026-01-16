// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.enums.EphemerisKeyType
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Region
import lucuma.core.math.RightAscension
import lucuma.core.model.CatalogInfo
import lucuma.core.model.Ephemeris
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import lucuma.core.model.Target.Nonsidereal
import lucuma.core.model.Target.Opportunity
import lucuma.core.model.Target.Sidereal

object target {

  trait TargetDecoder {
    import cataloginfo.given
    import declination.decoder.given
    import epoch.given
    import parallax.decoder.given
    import propermotion.decoder.given
    import radialvelocity.decoder.given
    import rightascension.decoder.given
    import sourceprofile.given
    import region.decoder.given

    given Decoder[Coordinates] =
      Decoder.instance(c =>
        for {
          ra  <- c.downField("ra").as[RightAscension]
          dec <- c.downField("dec").as[Declination]
        } yield Coordinates(ra, dec)
      )

    given Decoder[SiderealTracking] =
      Decoder.instance(c =>
        for {
          bc  <- c.as[Coordinates]
          ep  <- c.downField("epoch").as[Epoch]
          pm  <- c.downField("properMotion").as[Option[ProperMotion]]
          rv  <- c.downField("radialVelocity").as[Option[RadialVelocity]]
          par <- c.downField("parallax").as[Option[Parallax]]
        } yield SiderealTracking(bc, ep, pm, rv, par)
      )

    given Decoder[Target.Sidereal] =
      Decoder.instance(c =>
        for {
          name          <- c.downField("name").as[NonEmptyString]
          sourceProfile <- c.downField("sourceProfile").as[SourceProfile]
          s             <- c.downField("sidereal").as[HCursor]
          tracking      <- s.as[SiderealTracking]
          catalogInfo   <- s.downField("catalogInfo").as[Option[CatalogInfo]]
        } yield Target.Sidereal(name, tracking, sourceProfile, catalogInfo)
      )

    given Decoder[Target.Nonsidereal] =
      Decoder.instance(c =>
        for {
          name          <- c.downField("name").as[NonEmptyString]
          ephemerisKey  <- c.downField("nonsidereal").downField("key").as[Ephemeris.Key]
          sourceProfile <- c.downField("sourceProfile").as[SourceProfile]
        } yield Target.Nonsidereal(name, ephemerisKey, sourceProfile)
      )

    given Decoder[Target.Opportunity] =
      Decoder.instance(c =>
        for {
          name          <- c.downField("name").as[NonEmptyString]
          region        <- c.downField("opportunity").downField("region").as[Region]
          sourceProfile <- c.downField("sourceProfile").as[SourceProfile]
        } yield Target.Opportunity(name, region, sourceProfile)
      )

    given Decoder[Target] =
      List[Decoder[Target]](
        Decoder[Target.Sidereal].widen,
        Decoder[Target.Nonsidereal].widen,
        Decoder[Target.Opportunity].widen,
      ).reduceLeft(_ or _)
  }

  object decoder extends TargetDecoder

  trait InternalCodec {

    // This only encodes the part that goes under "sidereal" in the API
    protected def siderealDefinitionEncoderInternal(using
      Encoder[RightAscension],
      Encoder[Declination],
      Encoder[Epoch],
      Encoder[ProperMotion],
      Encoder[RadialVelocity],
      Encoder[Parallax],
      Encoder[CatalogInfo]
    ): Encoder[Target.Sidereal] =
      Encoder.instance { s =>
        Json.obj(
          "ra"             -> s.tracking.baseCoordinates.ra.asJson,
          "dec"            -> s.tracking.baseCoordinates.dec.asJson,
          "epoch"          -> s.tracking.epoch.asJson,
          "properMotion"   -> s.tracking.properMotion.asJson,
          "radialVelocity" -> s.tracking.radialVelocity.asJson,
          "parallax"       -> s.tracking.parallax.asJson,
          "catalogInfo"    -> s.catalogInfo.asJson
        )
      }

    // This only encodes the part that goes under "nonsidereal" in the API
    val nonsiderealDefinitionEncoder: Encoder[Target.Nonsidereal] =
      Encoder.instance { n =>
        Json.obj(
          "des"     -> n.ephemerisKey.des.asJson,
          "keyType" -> n.ephemerisKey.keyType.asJson,
          "key"     -> n.ephemerisKey.asJson
        )
      }

    // This only encodes the part that goes under "opportunity" in the API
    val opportunityDefinitionEncoder: Encoder[Target.Opportunity] =
      import region.query.given
      Encoder.instance: o =>
        Json.obj("region" -> o.region.asJson)

    protected def subtypeSlice(target: Target)(using
      Encoder[RightAscension],
      Encoder[Declination],
      Encoder[Epoch],
      Encoder[ProperMotion],
      Encoder[RadialVelocity],
      Encoder[Parallax],
      Encoder[CatalogInfo]
    ): (String, Json) =
      target match
        case s @ Sidereal(_, _, _, _) => "sidereal"    -> s.asJson(using siderealDefinitionEncoderInternal)
        case n @ Nonsidereal(_, _, _) => "nonsidereal" -> n.asJson(using nonsiderealDefinitionEncoder)
        case o @ Opportunity(_, _, _) => "opportunity" -> o.asJson(using opportunityDefinitionEncoder)

    // NOTE: This does not include the id, existence and program that are part of the Target in the API
    protected def encoderTargetInternal(using
      Encoder[RightAscension],
      Encoder[Declination],
      Encoder[Epoch],
      Encoder[ProperMotion],
      Encoder[RadialVelocity],
      Encoder[Parallax],
      Encoder[CatalogInfo],
      Encoder[SourceProfile]
    ): Encoder[Target] =
      Encoder.instance { t =>
        Json.obj(
          "name"          -> t.name.asJson,
          "sourceProfile" -> t.sourceProfile.asJson,
          "sidereal"      -> Json.Null, // one of these will be replaced
          "nonsidereal"   -> Json.Null, // one of these will be replaced
          "opportunity"   -> Json.Null, // one of these will be replaced
          subtypeSlice(t)
        )
      }
  }

  trait QueryCodec extends TargetDecoder with InternalCodec {
    import all.query.given

    // This only encodes the part that goes under "sidereal" in the API
    val siderealDefinitionEncoder: Encoder[Target.Sidereal] = siderealDefinitionEncoderInternal

    // Returns the JSON for either "sidereal" or "nonsidereal". 
    // ie. either ("sidereal", JSON) or ("nonsidereal", JSON)
    def siderealOrNonJson(target: Target): (String, Json) = subtypeSlice(target)

    // NOTE: This does not include the id, existence and program that are part of the Target in the API
    given Encoder_Target: Encoder[Target] = encoderTargetInternal

  }

  object query extends QueryCodec

  trait TransportCodec extends TargetDecoder with InternalCodec {
    import all.transport.given

    // This only encodes the part that goes under "sidereal" in the API
    val siderealDefinitionEncoder: Encoder[Target.Sidereal] = siderealDefinitionEncoderInternal

    // Returns the JSON for either "sidereal" or "nonsidereal". 
    // ie. either ("sidereal", JSON) or ("nonsidereal", JSON)
    def siderealOrNonJson(target: Target): (String, Json) = subtypeSlice(target)

    // NOTE: This does not include the id, existence and program that are part of the Target in the API
    given Encoder_Target: Encoder[Target] = encoderTargetInternal

  }

  object transport extends TransportCodec
}
