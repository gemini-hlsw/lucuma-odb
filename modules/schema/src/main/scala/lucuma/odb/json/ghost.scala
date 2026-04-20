// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import eu.timepit.refined.types.numeric.PosInt
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostIfu1FiberAgitator
import lucuma.core.enums.GhostIfu2FiberAgitator
import lucuma.core.enums.GhostReadMode
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.model.sequence.ghost.GhostDetector
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.ghost.GhostStaticConfig
import lucuma.core.util.TimeSpan

trait GhostCodec:

  import time.decoder.given

  given Decoder[GhostStaticConfig] =
    Decoder.instance: c =>
      c.downField("resolutionMode")
       .as[GhostResolutionMode]
       .map(GhostStaticConfig.apply)

  given Encoder[GhostStaticConfig] =
    Encoder.instance: a =>
      Json.obj("resolutionMode" -> a.resolutionMode.asJson)

  given Decoder[GhostDetector] =
    Decoder.instance: c =>
      for
        t <- c.downField("exposureTime").as[TimeSpan]
        n <- c.downField("exposureCount").as[PosInt]
        b <- c.downField("binning").as[GhostBinning]
        r <- c.downField("readMode").as[GhostReadMode]
      yield GhostDetector(t, n, b, r)

  given (using Encoder[TimeSpan]): Encoder[GhostDetector] =
    Encoder.instance: a =>
      Json.obj(
        "exposureTime"  -> a.exposureTime.asJson,
        "exposureCount" -> a.exposureCount.asJson,
        "binning"       -> a.binning.asJson,
        "readMode"      -> a.readMode.asJson
      )

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

  given (using Encoder[TimeSpan]): Encoder[GhostDynamicConfig] =
    Encoder.instance: a =>
      Json.obj(
        "red"               -> a.red.asJson,
        "blue"              -> a.blue.asJson,
        "ifu1FiberAgitator" -> a.ifu1FiberAgitator.asJson,
        "ifu2FiberAgitator" -> a.ifu2FiberAgitator.asJson
      )

object ghost extends GhostCodec