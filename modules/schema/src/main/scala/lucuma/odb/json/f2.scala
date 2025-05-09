// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.syntax.either.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.F2CustomSlitWidth
import lucuma.core.enums.F2Disperser
import lucuma.core.enums.F2Filter
import lucuma.core.enums.F2Fpu
import lucuma.core.enums.F2LyotWheel
import lucuma.core.enums.F2ReadMode
import lucuma.core.enums.F2ReadoutMode
import lucuma.core.enums.F2Reads
import lucuma.core.enums.MosPreImaging
import lucuma.core.model.sequence.f2.F2DynamicConfig
import lucuma.core.model.sequence.f2.F2FpuMask
import lucuma.core.model.sequence.f2.F2StaticConfig
import lucuma.core.util.TimeSpan

trait Flamingos2Codec:

  import time.decoder.given

  given Decoder[F2StaticConfig] =
    Decoder.instance[F2StaticConfig]: c =>
      for
        m <- c.downField("mosPreImaging").as[MosPreImaging]
        e <- c.downField("useElectronicOffsetting").as[Boolean]
      yield F2StaticConfig(m, e)

  given Encoder[F2StaticConfig] =
    Encoder.instance[F2StaticConfig]: a =>
      Json.obj(
        "mosPreImaging"           -> a.mosPreImaging.asJson,
        "useElectronicOffsetting" -> a.useElectronicOffseting.asJson
      )

  given Decoder[F2FpuMask.Custom] =
    Decoder.instance: c =>
      for
        f <- c.downField("filename").as[String].flatMap: s =>
               NonEmptyString.from(s).leftMap: m =>
                 DecodingFailure(s"Flamingos 2 custom mask file name cannot be empty", c.history)
        s <- c.downField("slitWidth").as[F2CustomSlitWidth]
      yield F2FpuMask.Custom(f, s)

  given Encoder[F2FpuMask.Custom] =
    Encoder.instance: a =>
      Json.obj(
        "filename"  -> a.filename.value.asJson,
        "slitWidth" -> a.slitWidth.asJson
      )

  given Decoder[F2FpuMask] =
    Decoder.instance: c =>
      if c.value.isNull then F2FpuMask.Imaging.asRight
      else
        c.downField("builtin")
         .as[F2Fpu]
         .map(a => F2FpuMask.Builtin(a))
         .orElse(c.downField("customMask").as[F2FpuMask.Custom])

  given Encoder[F2FpuMask] =
    Encoder.instance[F2FpuMask]:
      case F2FpuMask.Imaging        =>
        Json.Null
      case F2FpuMask.Builtin(v)     =>
        Json.obj(
          "builtin" -> v.asJson,
          "customMask"  -> Json.Null
        )
      case c@F2FpuMask.Custom(_, _) =>
        Json.obj(
          "builtin" -> Json.Null,
          "customMask"  -> c.asJson
        )

  given Decoder[F2DynamicConfig] =
    Decoder.instance[F2DynamicConfig]: c =>
      for
        e <- c.downField("exposure").as[TimeSpan]
        d <- c.downField("disperser").as[Option[F2Disperser]]
        f <- c.downField("filter").as[F2Filter]
        r <- c.downField("readMode").as[F2ReadMode]
        l <- c.downField("lyot").as[F2LyotWheel]
        u <- c.downField("mask").as[F2FpuMask]
        m <- c.downField("readoutMode").as[Option[F2ReadoutMode]]
        s <- c.downField("reads").as[Option[F2Reads]]
      yield F2DynamicConfig(e, d, f, r, l, u, m, s)

  given (using Encoder[TimeSpan]): Encoder[F2DynamicConfig] =
    Encoder.instance[F2DynamicConfig]: a =>
      Json.obj(
        "exposure"    -> a.exposure.asJson,
        "disperser"   -> a.disperser.asJson,
        "filter"      -> a.filter.asJson,
        "readMode"    -> a.readMode.asJson,
        "lyot"        -> a.lyot.asJson,
        "mask"        -> a.fpu.asJson,
        "readoutMode" -> a.readoutMode.asJson,
        "reads"       -> a.reads.asJson
      )

object f2 extends Flamingos2Codec