// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostIfu1FiberAgitator
import lucuma.core.enums.GhostIfu2FiberAgitator
import lucuma.core.enums.GhostIfuMappingType
import lucuma.core.enums.GhostReadMode
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.math.Coordinates
import lucuma.core.model.SiderealTracking
import lucuma.core.model.sequence.ghost.GhostDetector
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.ghost.GhostIfuMapping
import lucuma.core.model.sequence.ghost.GhostIfuMapping.*
import lucuma.core.model.sequence.ghost.GhostStaticConfig
import lucuma.core.util.TimeSpan

trait GhostCodec:

  trait DecoderGhost:
    import target.decoder.given
    import time.decoder.given

    given Decoder[SingleTarget] =
      Decoder.instance: c =>
        c.downField("ifu1").as[SiderealTracking].map(SingleTarget.apply)

    given Decoder[TargetPlusSky] =
      Decoder.instance: c =>
        for
          t <- c.downField("ifu1").as[SiderealTracking]
          r <- c.downField("ifu2").as[Coordinates]
        yield TargetPlusSky(t, r)

    given Decoder[SkyPlusTarget] =
      Decoder.instance: c =>
        for
          r <- c.downField("ifu1").as[Coordinates]
          t <- c.downField("ifu2").as[SiderealTracking]
        yield SkyPlusTarget(r, t)

    given Decoder[DualTarget] =
      Decoder.instance: c =>
        for
          t1 <- c.downField("ifu1").as[SiderealTracking]
          t2 <- c.downField("ifu2").as[SiderealTracking]
        yield DualTarget(t1, t2)

    given Decoder[GhostIfuMapping] =
      Decoder.instance: c =>
        for
          t <- c.downField("mappingType").as[GhostIfuMappingType]
          m <- t match
                 case GhostIfuMappingType.Nonsidereal   => Nonsidereal.asRight
                 case GhostIfuMappingType.SingleTarget  => c.downField("singleTarget").as[SingleTarget]
                 case GhostIfuMappingType.TargetPlusSky => c.downField("targetPlusSky").as[TargetPlusSky]
                 case GhostIfuMappingType.SkyPlusTarget => c.downField("skyPlusTarget").as[SkyPlusTarget]
                 case GhostIfuMappingType.DualTarget    => c.downField("dualTarget").as[DualTarget]
        yield m

    given Decoder[GhostStaticConfig] =
      Decoder.instance: c =>
        for
          r <- c.downField("resolutionMode").as[GhostResolutionMode]
          f <- c.downField("ifuMapping").as[GhostIfuMapping]
          s <- c.downField("slitViewingCameraExposureTime").as[Option[TimeSpan]]
        yield GhostStaticConfig(r, f, s)

    given Decoder[GhostDetector] =
      Decoder.instance: c =>
        for
          t <- c.downField("exposureTime").as[TimeSpan]
          n <- c.downField("exposureCount").as[PosInt]
          b <- c.downField("binning").as[GhostBinning]
          r <- c.downField("readMode").as[GhostReadMode]
        yield GhostDetector(t, n, b, r)

    given Decoder[GhostDynamicConfig] =
      Decoder.instance: c =>
        for
          r  <- c.downField("red").as[GhostDetector]
          b  <- c.downField("blue").as[GhostDetector]
          u1 <- c.downField("ifu1FiberAgitator").as[GhostIfu1FiberAgitator]
          u2 <- c.downField("ifu2FiberAgitator").as[GhostIfu2FiberAgitator]
        yield GhostDynamicConfig(
          GhostDetector.Red(r),
          GhostDetector.Blue(b),
          u1,
          u2
        )

  object decoder extends DecoderGhost

  trait InternalCodec extends DecoderGhost:
    protected def coordinatesEncoder: Encoder[Coordinates]
    protected def siderealTrackingEncoder: Encoder[SiderealTracking]

    given Encoder[SingleTarget] =
      Encoder.instance: a =>
        Json.obj(
          "ifu1" -> a.ifu1.asJson(using siderealTrackingEncoder)
        )

    given Encoder[TargetPlusSky] =
      Encoder.instance: a =>
        Json.obj(
          "ifu1" -> a.ifu1.asJson(using siderealTrackingEncoder),
          "ifu2" -> a.ifu2.asJson(using coordinatesEncoder)
        )

    given Encoder[SkyPlusTarget] =
      Encoder.instance: a =>
        Json.obj(
          "ifu1" -> a.ifu1.asJson(using coordinatesEncoder),
          "ifu2" -> a.ifu2.asJson(using siderealTrackingEncoder)
        )

    given Encoder[DualTarget] =
      Encoder.instance: a =>
        Json.obj(
          "ifu1" -> a.ifu1.asJson(using siderealTrackingEncoder),
          "ifu2" -> a.ifu2.asJson(using siderealTrackingEncoder)
        )

    given Encoder[GhostIfuMapping] =
      def subtypeSlice(m: GhostIfuMapping): Option[(String, Json)] =
        m match
          case Nonsidereal             => none
          case m @ SingleTarget(_)     => ("singleTarget",  m.asJson).some
          case m @ TargetPlusSky(_, _) => ("targetPlusSky", m.asJson).some
          case m @ SkyPlusTarget(_, _) => ("skyPlusTarget", m.asJson).some
          case m @ DualTarget(_, _)    => ("dualTarget",    m.asJson).some

      Encoder.instance: a =>
        Json.fromFields(
          List(
            ("mappingType",   a.mappingType.asJson).some,
            ("singleTarget",  Json.Null).some,
            ("targetPlusSky", Json.Null).some,
            ("skyPlusTarget", Json.Null).some,
            ("dualTarget",    Json.Null).some,
            subtypeSlice(a)
          ).flatten
        )

    given (using Encoder[TimeSpan]): Encoder[GhostStaticConfig] =
      Encoder.instance: a =>
        Json.obj(
          "resolutionMode"                -> a.resolutionMode.asJson,
          "ifuMapping"                    -> a.ifuMapping.asJson,
          "slitViewingCameraExposureTime" -> a.slitViewingCameraExposureTime.asJson
        )

    given (using Encoder[TimeSpan]): Encoder[GhostDetector] =
      Encoder.instance: a =>
        Json.obj(
          "exposureTime"  -> a.exposureTime.asJson,
          "exposureCount" -> a.exposureCount.asJson,
          "binning"       -> a.binning.asJson,
          "readMode"      -> a.readMode.asJson
        )

    given (using Encoder[TimeSpan]): Encoder[GhostDynamicConfig] =
      Encoder.instance: a =>
        Json.obj(
          "red"               -> a.red.asJson,
          "blue"              -> a.blue.asJson,
          "ifu1FiberAgitator" -> a.ifu1FiberAgitator.asJson,
          "ifu2FiberAgitator" -> a.ifu2FiberAgitator.asJson
        )


  trait QueryCodec extends InternalCodec:
    override protected val coordinatesEncoder: Encoder[Coordinates] =
      coordinates.query.Encoder_Coordinates

    override protected val siderealTrackingEncoder: Encoder[SiderealTracking] =
      target.query.siderealTrackingEncoder

  object query extends QueryCodec

  trait TransportCodec extends InternalCodec:
    override protected val coordinatesEncoder: Encoder[Coordinates] =
      coordinates.transport.Encoder_Coordinates

    override protected val siderealTrackingEncoder: Encoder[SiderealTracking] =
      target.transport.siderealTrackingEncoder

  object transport extends TransportCodec

object ghost extends GhostCodec