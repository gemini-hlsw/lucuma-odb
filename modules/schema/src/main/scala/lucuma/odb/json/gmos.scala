// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.syntax.either.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.enums.GmosAmpCount
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosCustomSlitWidth
import lucuma.core.enums.GmosDtax
import lucuma.core.enums.GmosEOffsetting
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosNorthDetector
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosNorthStageMode
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthDetector
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosSouthStageMode
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.MosPreImaging
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.model.sequence.gmos.GmosGratingConfig
import lucuma.core.model.sequence.gmos.GmosNodAndShuffle
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.util.TimeSpan


trait GmosCodec {

  import offset.decoder.given
  import time.decoder.given
  import wavelength.decoder.given

  given Decoder[GmosNodAndShuffle] =
    Decoder.instance { c =>
      for {
        a <- c.downField("posA").as[Offset]
        b <- c.downField("posB").as[Offset]
        e <- c.downField("eOffset").as[GmosEOffsetting]
        o <- c.downField("shuffleOffset").as[PosInt]
        y <- c.downField("shuffleCycles").as[PosInt]
      } yield GmosNodAndShuffle(a, b, e, o, y)
    }

  given (using Encoder[Offset]): Encoder[GmosNodAndShuffle] =
    Encoder.instance { c =>
      Json.obj(
        "posA"          -> c.posA.asJson,
        "posB"          -> c.posB.asJson,
        "eOffset"       -> c.eOffset.asJson,
        "shuffleOffset" -> c.shuffleOffset.asJson,
        "shuffleCycles" -> c.shuffleCycles.asJson
      )
    }

  given given_Decoder_StaticConfig_GmosNorth: Decoder[StaticConfig.GmosNorth] =
    Decoder.instance { c =>
      for {
        s <- c.downField("stageMode").as[GmosNorthStageMode]
        d <- c.downField("detector").as[GmosNorthDetector]
        p <- c.downField("mosPreImaging").as[MosPreImaging]
        n <- c.downField("nodAndShuffle").as[Option[GmosNodAndShuffle]]
      } yield StaticConfig.GmosNorth(s, d, p, n)
    }

  given given_Encoder_StaticConfig_GmosNorth(using Encoder[Offset]): Encoder[StaticConfig.GmosNorth] =
    Encoder.instance { c =>
      Json.obj(
        "stageMode"     -> c.stageMode.asJson,
        "detector"      -> c.detector.asJson,
        "mosPreImaging" -> c.mosPreImaging.asJson,
        "nodAndShuffle" -> c.nodAndShuffle.asJson
      )
    }

  given given_Decoder_StaticConfig_GmosSouth: Decoder[StaticConfig.GmosSouth] =
    Decoder.instance { c =>
      for {
        s <- c.downField("stageMode").as[GmosSouthStageMode]
        d <- c.downField("detector").as[GmosSouthDetector]
        p <- c.downField("mosPreImaging").as[MosPreImaging]
        n <- c.downField("nodAndShuffle").as[Option[GmosNodAndShuffle]]
      } yield StaticConfig.GmosSouth(s, d, p, n)
    }

  given given_Encoder_StaticConfig_GmosSouth(using Encoder[Offset]): Encoder[StaticConfig.GmosSouth] =
    Encoder.instance { c =>
      Json.obj(
        "stageMode"     -> c.stageMode.asJson,
        "detector"      -> c.detector.asJson,
        "mosPreImaging" -> c.mosPreImaging.asJson,
        "nodAndShuffle" -> c.nodAndShuffle.asJson
      )
    }

  given Decoder[GmosCcdMode] =
    Decoder.instance { c =>
      for {
        x <- c.downField("xBin").as[GmosXBinning]
        y <- c.downField("yBin").as[GmosYBinning]
        n <- c.downField("ampCount").as[GmosAmpCount]
        g <- c.downField("ampGain").as[GmosAmpGain]
        m <- c.downField("ampReadMode").as[GmosAmpReadMode]
      } yield GmosCcdMode(x, y, n, g, m)
    }

  // This needs an explicit name because the default, given_Decoder_Custom
  // would clash with the default for the Flamingos2 custom mask.
  given given_Decoder_GmosFpuMask_Custom: Decoder[GmosFpuMask.Custom] =
    Decoder.instance { c =>
      for {
        f <- c.downField("filename").as[String].flatMap { s =>
          NonEmptyString.from(s).leftMap { m => DecodingFailure(s"GMOS custom mask file name cannot be empty", c.history) }
        }
        s <- c.downField("slitWidth").as[GmosCustomSlitWidth]
      } yield GmosFpuMask.Custom(f, s)
    }

  given [A](using Decoder[A]): Decoder[GmosFpuMask[A]] =
    Decoder.instance[GmosFpuMask[A]] { c =>
      c.downField("builtin").as[A].map { a =>
        GmosFpuMask.Builtin[A](a)
      } orElse
        c.downField("customMask").as[GmosFpuMask.Custom]
    }

  given given_Decoder_GmosGratingConfig_North: Decoder[GmosGratingConfig.North] =
    Decoder.instance { c =>
      for {
        g <- c.downField("grating").as[GmosNorthGrating]
        o <- c.downField("order").as[GmosGratingOrder]
        w <- c.downField("wavelength").as[Wavelength]
      } yield GmosGratingConfig.North(g, o, w)
    }

  given given_Decoder_GmosGratingConfig_South: Decoder[GmosGratingConfig.South] =
    Decoder.instance { c =>
      for {
        g <- c.downField("grating").as[GmosSouthGrating]
        o <- c.downField("order").as[GmosGratingOrder]
        w <- c.downField("wavelength").as[Wavelength]
      } yield GmosGratingConfig.South(g, o, w)
    }

  given given_Decoder_DynamicConfig_GmosNorth: Decoder[DynamicConfig.GmosNorth] =
    Decoder.instance { c =>
      for {
        e <- c.downField("exposure").as[TimeSpan]
        r <- c.downField("readout").as[GmosCcdMode]
        x <- c.downField("dtax").as[GmosDtax]
        i <- c.downField("roi").as[GmosRoi]
        g <- c.downField("gratingConfig").as[Option[GmosGratingConfig.North]]
        f <- c.downField("filter").as[Option[GmosNorthFilter]]
        u <- c.downField("fpu").as[Option[GmosFpuMask[GmosNorthFpu]]]
      } yield DynamicConfig.GmosNorth(e, r, x, i, g, f, u)
    }

  given given_Decoder_DynamicConfig_GmosSouth: Decoder[DynamicConfig.GmosSouth] =
    Decoder.instance { c =>
      for {
        e <- c.downField("exposure").as[TimeSpan]
        r <- c.downField("readout").as[GmosCcdMode]
        x <- c.downField("dtax").as[GmosDtax]
        i <- c.downField("roi").as[GmosRoi]
        g <- c.downField("gratingConfig").as[Option[GmosGratingConfig.South]]
        f <- c.downField("filter").as[Option[GmosSouthFilter]]
        u <- c.downField("fpu").as[Option[GmosFpuMask[GmosSouthFpu]]]
      } yield DynamicConfig.GmosSouth(e, r, x, i, g, f, u)
    }

  given Encoder[GmosCcdMode] =
    Encoder.instance { (a: GmosCcdMode) =>
      Json.obj(
        "xBin"        -> a.xBin.asJson,
        "yBin"        -> a.yBin.asJson,
        "ampCount"    -> a.ampCount.asJson,
        "ampGain"     -> a.ampGain.asJson,
        "ampReadMode" -> a.ampReadMode.asJson
      )
    }

  given given_Encoder_GmosFpuMask_Custom: Encoder[GmosFpuMask.Custom] =
    Encoder.instance { (a: GmosFpuMask.Custom) =>
      Json.obj(
        "filename"  -> a.filename.value.asJson,
        "slitWidth" -> a.slitWidth.asJson
      )
    }

  given [A](using Encoder[A]): Encoder[GmosFpuMask[A]] =
    Encoder.instance[GmosFpuMask[A]] {
      case GmosFpuMask.Builtin(v)       =>
        Json.obj(
          "builtin"    -> v.asJson,
          "customMask" -> Json.Null
        )
      case c @ GmosFpuMask.Custom(_, _) =>
        Json.obj(
          "builtin"    -> Json.Null,
          "customMask" -> c.asJson
        )
    }

  given given_Encoder_GratingConfig_North(using Encoder[Wavelength]): Encoder[GmosGratingConfig.North] =
    Encoder.instance { (a: GmosGratingConfig.North) =>
      Json.obj(
        "grating"    -> a.grating.asJson,
        "order"      -> a.order.asJson,
        "wavelength" -> a.wavelength.asJson
      )
    }

  given given_Encoder_GratingConfig_South(using Encoder[Wavelength]): Encoder[GmosGratingConfig.South] =
    Encoder.instance { (a: GmosGratingConfig.South) =>
      Json.obj(
        "grating"    -> a.grating.asJson,
        "order"      -> a.order.asJson,
        "wavelength" -> a.wavelength.asJson
      )
    }

  given given_Encoder_DynamicConfig_GmosNorth(using Encoder[TimeSpan], Encoder[Wavelength]): Encoder[DynamicConfig.GmosNorth] =
    Encoder.instance { (a: DynamicConfig.GmosNorth) =>
      Json.obj(
        "exposure"          -> a.exposure.asJson,
        "readout"           -> a.readout.asJson,
        "dtax"              -> a.dtax.asJson,
        "roi"               -> a.roi.asJson,
        "gratingConfig"     -> a.gratingConfig.asJson,
        "filter"            -> a.filter.asJson,
        "fpu"               -> a.fpu.asJson,
        "centralWavelength" -> a.centralWavelength.asJson
      )
    }

  given given_Encoder_DynamicConfig_GmosSouth(using Encoder[TimeSpan], Encoder[Wavelength]): Encoder[DynamicConfig.GmosSouth] =
    Encoder.instance { (a: DynamicConfig.GmosSouth) =>
      Json.obj(
        "exposure"          -> a.exposure.asJson,
        "readout"           -> a.readout.asJson,
        "dtax"              -> a.dtax.asJson,
        "roi"               -> a.roi.asJson,
        "gratingConfig"     -> a.gratingConfig.asJson,
        "filter"            -> a.filter.asJson,
        "fpu"               -> a.fpu.asJson,
        "centralWavelength" -> a.centralWavelength.asJson
      )
    }

}

object gmos extends GmosCodec
