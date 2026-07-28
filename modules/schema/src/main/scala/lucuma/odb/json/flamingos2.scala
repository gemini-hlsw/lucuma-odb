// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.syntax.either.*
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.Flamingos2CustomSlitWidth
import lucuma.core.enums.Flamingos2Decker
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.Flamingos2ReadoutMode
import lucuma.core.enums.Flamingos2Reads
import lucuma.core.enums.MosPreImaging
import lucuma.core.math.Wavelength
import lucuma.core.model.Attachment
import lucuma.core.model.Defined
import lucuma.core.model.MaskDefinition
import lucuma.core.model.ToBeDefined
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.util.TimeSpan

trait Flamingos2Codec:

  import time.decoder.given

  given Decoder[Flamingos2StaticConfig] =
    Decoder.instance[Flamingos2StaticConfig]: c =>
      for
        m <- c.downField("mosPreImaging").as[MosPreImaging]
        e <- c.downField("useElectronicOffsetting").as[Boolean]
      yield Flamingos2StaticConfig(m, e)

  given Encoder[Flamingos2StaticConfig] =
    Encoder.instance[Flamingos2StaticConfig]: a =>
      Json.obj(
        "mosPreImaging"           -> a.mosPreImaging.asJson,
        "useElectronicOffsetting" -> a.useElectronicOffsetting.asJson
      )

  // This needs an explicit name because the default, given_Decoder_Custom
  // would clash with the default for the GMOS custom mask.
  given given_Decoder_Flamingos2FpuMask_Custom: Decoder[Flamingos2FpuMask.Custom] =
    Decoder.instance: c =>
      for
        m <- c.downField("attachmentId").as[Option[Attachment.Id]]
        s <- c.downField("slitWidth").as[Flamingos2CustomSlitWidth]
      yield Flamingos2FpuMask.Custom(m.fold[MaskDefinition](ToBeDefined)(Defined(_)), s)

  given given_Encoder_Flamingos2FpuMask_Custom: Encoder[Flamingos2FpuMask.Custom] =
    Encoder.instance: a =>
      Json.obj(
        "attachmentId" -> (a.mask match
          case ToBeDefined => Json.Null
          case Defined(id) => id.asJson
        ),
        "slitWidth"    -> a.slitWidth.asJson
      )

  given Decoder[Flamingos2FpuMask] =
    Decoder.instance: c =>
      if c.value.isNull then Flamingos2FpuMask.Imaging.asRight
      else
        c.downField("builtin")
         .as[Flamingos2Fpu]
         .map(a => Flamingos2FpuMask.Builtin(a))
         .orElse(c.downField("customMask").as[Flamingos2FpuMask.Custom])

  given Encoder[Flamingos2FpuMask] =
    Encoder.instance[Flamingos2FpuMask]:
      case Flamingos2FpuMask.Imaging        =>
        Json.Null
      case Flamingos2FpuMask.Builtin(v)     =>
        Json.obj(
          "builtin" -> v.asJson,
          "customMask"  -> Json.Null
        )
      case c@Flamingos2FpuMask.Custom(_, _) =>
        Json.obj(
          "builtin" -> Json.Null,
          "customMask"  -> c.asJson
        )

  given Decoder[Flamingos2DynamicConfig] =
    Decoder.instance[Flamingos2DynamicConfig]: c =>
      for
        e <- c.downField("exposure").as[TimeSpan]
        d <- c.downField("disperser").as[Option[Flamingos2Disperser]]
        f <- c.downField("filter").as[Flamingos2Filter]
        r <- c.downField("readMode").as[Flamingos2ReadMode]
        l <- c.downField("lyotWheel").as[Flamingos2LyotWheel]
        u <- c.downField("fpu").as[Flamingos2FpuMask]
        k <- c.downField("decker").as[Flamingos2Decker]
        m <- c.downField("readoutMode").as[Flamingos2ReadoutMode]
        s <- c.downField("reads").as[Flamingos2Reads]
      yield Flamingos2DynamicConfig(e, d, f, r, l, u, k, m, s)

  given (using Encoder[TimeSpan], Encoder[Wavelength]): Encoder[Flamingos2DynamicConfig] =
    Encoder.instance[Flamingos2DynamicConfig]: a =>
      Json.obj(
        "exposure"          -> a.exposure.asJson,
        "disperser"         -> a.disperser.asJson,
        "filter"            -> a.filter.asJson,
        "readMode"          -> a.readMode.asJson,
        "lyotWheel"         -> a.lyotWheel.asJson,
        "fpu"               -> a.fpu.asJson,
        "decker"            -> a.decker.asJson,
        "readoutMode"       -> a.readoutMode.asJson,
        "reads"             -> a.reads.asJson,
        "centralWavelength" -> a.centralWavelength.asJson
      )

object flamingos2 extends Flamingos2Codec
