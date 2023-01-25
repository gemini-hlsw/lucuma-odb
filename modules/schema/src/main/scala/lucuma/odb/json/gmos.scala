// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.syntax.either.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Codec
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.refined._
import io.circe.syntax._
import lucuma.core.enums.GmosAmpCount
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosCustomSlitWidth
import lucuma.core.enums.GmosDtax
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.DynamicConfig.GmosSouth
import lucuma.core.model.sequence.GmosCcdMode
import lucuma.core.model.sequence.GmosFpuMask
import lucuma.core.model.sequence.GmosGratingConfig
import lucuma.core.util.TimeSpan


trait GmosCodec {

  import time.decoder.given
  import wavelength.decoder.given

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

  given Decoder[GmosFpuMask.Custom] =
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

  given Decoder[GmosGratingConfig.North] =
    Decoder.instance { c =>
      for {
        g <- c.downField("grating").as[GmosNorthGrating]
        o <- c.downField("order").as[GmosGratingOrder]
        w <- c.downField("wavelength").as[Wavelength]
      } yield GmosGratingConfig.North(g, o, w)
    }

  given Decoder[GmosGratingConfig.South] =
    Decoder.instance { c =>
      for {
        g <- c.downField("grating").as[GmosSouthGrating]
        o <- c.downField("order").as[GmosGratingOrder]
        w <- c.downField("wavelength").as[Wavelength]
      } yield GmosGratingConfig.South(g, o, w)
    }

  given Decoder[GmosNorth] =
    Decoder.instance { c =>
      for {
        e <- c.downField("exposure").as[TimeSpan]
        r <- c.downField("readout").as[GmosCcdMode]
        x <- c.downField("dtax").as[GmosDtax]
        i <- c.downField("roi").as[GmosRoi]
        g <- c.downField("gratingConfig").as[Option[GmosGratingConfig.North]]
        f <- c.downField("filter").as[Option[GmosNorthFilter]]
        u <- c.downField("fpu").as[Option[GmosFpuMask[GmosNorthFpu]]]
      } yield GmosNorth(e, r, x, i, g, f, u)
    }

  given Decoder[GmosSouth] =
    Decoder.instance { c =>
      for {
        e <- c.downField("exposure").as[TimeSpan]
        r <- c.downField("readout").as[GmosCcdMode]
        x <- c.downField("dtax").as[GmosDtax]
        i <- c.downField("roi").as[GmosRoi]
        g <- c.downField("gratingConfig").as[Option[GmosGratingConfig.South]]
        f <- c.downField("filter").as[Option[GmosSouthFilter]]
        u <- c.downField("fpu").as[Option[GmosFpuMask[GmosSouthFpu]]]
      } yield GmosSouth(e, r, x, i, g, f, u)
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

  given Encoder[GmosFpuMask.Custom] =
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
          "builtin"    -> v.asJson
        )
      case c @ GmosFpuMask.Custom(_, _) =>
        Json.obj(
          "customMask" -> c.asJson
        )
    }

  given (using Encoder[Wavelength]): Encoder[GmosGratingConfig.North] =
    Encoder.instance { (a: GmosGratingConfig.North) =>
      Json.obj(
        "grating"    -> a.grating.asJson,
        "order"      -> a.order.asJson,
        "wavelength" -> a.wavelength.asJson
      )
    }

  given (using Encoder[Wavelength]): Encoder[GmosGratingConfig.South] =
    Encoder.instance { (a: GmosGratingConfig.South) =>
      Json.obj(
        "grating"    -> a.grating.asJson,
        "order"      -> a.order.asJson,
        "wavelength" -> a.wavelength.asJson
      )
    }

  given (using Encoder[TimeSpan], Encoder[Wavelength]): Encoder[GmosNorth] =
    Encoder.instance { (a: GmosNorth) =>
      Json.obj(
        "exposure"      -> a.exposure.asJson,
        "readout"       -> a.readout.asJson,
        "dtax"          -> a.dtax.asJson,
        "roi"           -> a.roi.asJson,
        "gratingConfig" -> a.gratingConfig.asJson,
        "filter"        -> a.filter.asJson,
        "fpu"           -> a.fpu.asJson
      )
    }

  given (using Encoder[TimeSpan], Encoder[Wavelength]): Encoder[GmosSouth] =
    Encoder.instance { (a: GmosSouth) =>
      Json.obj(
        "exposure"      -> a.exposure.asJson,
        "readout"       -> a.readout.asJson,
        "dtax"          -> a.dtax.asJson,
        "roi"           -> a.roi.asJson,
        "gratingConfig" -> a.gratingConfig.asJson,
        "filter"        -> a.filter.asJson,
        "fpu"           -> a.fpu.asJson
      )
    }

}

object gmos extends GmosCodec