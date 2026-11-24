// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import eu.timepit.refined.types.numeric.PosInt
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.enums.ScorpioChannelSelection
import lucuma.core.enums.ScorpioDisperser
import lucuma.core.enums.ScorpioFpu
import lucuma.core.enums.ScorpioVisReadoutMode
import lucuma.core.model.sequence.scorpio.ScorpioChannelConfig
import lucuma.core.model.sequence.scorpio.ScorpioDynamicConfig
import lucuma.core.model.sequence.scorpio.ScorpioFpuMask
import lucuma.core.model.sequence.scorpio.ScorpioStaticConfig
import lucuma.core.util.TimeSpan

import time.decoder.given

/**
 * Only the visible channels and the long slit are represented, so a config
 * with anything else does not round trip.  The ODB rejects those before they
 * are stored (see ScorpioSequenceService).
 */
trait ScorpioCodec:

  given Decoder[ScorpioStaticConfig] =
    Decoder.instance:
      _.downField("channelSelection").as[ScorpioChannelSelection].map(ScorpioStaticConfig(_))

  given Encoder[ScorpioStaticConfig] =
    Encoder.instance: a =>
      Json.obj(
        "channelSelection" -> a.channelSelection.asJson
      )

  given Decoder[ScorpioChannelConfig.Vis] =
    Decoder.instance: c =>
      for
        t <- c.downField("exposureTime").as[TimeSpan]
        n <- c.downField("exposureCount").as[PosInt]
        r <- c.downField("readoutMode").as[ScorpioVisReadoutMode]
      yield ScorpioChannelConfig.Vis(t, n, r)

  given (using Encoder[TimeSpan]): Encoder[ScorpioChannelConfig.Vis] =
    Encoder.instance: a =>
      Json.obj(
        "exposureTime"  -> a.exposureTime.asJson,
        "exposureCount" -> a.exposureCount.asJson,
        "readoutMode"   -> a.readoutMode.asJson
      )

  given Decoder[ScorpioDynamicConfig] =
    Decoder.instance: c =>
      for
        g <- c.downField("g").as[Option[ScorpioChannelConfig.Vis]]
        r <- c.downField("r").as[Option[ScorpioChannelConfig.Vis]]
        i <- c.downField("i").as[Option[ScorpioChannelConfig.Vis]]
        z <- c.downField("z").as[Option[ScorpioChannelConfig.Vis]]
        f <- c.downField("fpu").as[ScorpioFpu]
        d <- c.downField("disperser").as[ScorpioDisperser]
      yield ScorpioDynamicConfig(g, r, i, z, None, None, None, None, ScorpioFpuMask.LongSlit(f), d)

  given (using Encoder[TimeSpan]): Encoder[ScorpioDynamicConfig] =
    Encoder.instance: a =>
      Json.obj(
        "g"         -> a.g.asJson,
        "r"         -> a.r.asJson,
        "i"         -> a.i.asJson,
        "z"         -> a.z.asJson,
        "fpu"       -> a.fpu.longSlit.asJson,
        "disperser" -> a.disperser.asJson
      )

object scorpio extends ScorpioCodec
