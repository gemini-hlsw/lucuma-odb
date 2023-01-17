// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.syntax.either.*
import io.circe.Codec
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.refined._
import io.circe.syntax._
import lucuma.core.enums.GcalArc
import lucuma.core.enums.GcalContinuum
import lucuma.core.enums.GcalDiffuser
import lucuma.core.enums.GcalFilter
import lucuma.core.enums.GcalShutter
import lucuma.core.enums.StepType
import lucuma.core.math.Offset
import lucuma.core.model.sequence.StepConfig

trait StepConfigCodec {

  import offset.decoder.given

  given Decoder[StepConfig.Gcal] =
    Decoder.instance { c =>
      for {
        u <- c.downField("continuum").as[Option[GcalContinuum]]
        a <- c.downField("arcs").as[List[GcalArc]]
        f <- c.downField("filter").as[GcalFilter]
        d <- c.downField("diffuser").as[GcalDiffuser]
        s <- c.downField("shutter").as[GcalShutter]
      } yield StepConfig.Gcal(u, a, f, d, s)
    }

  given Encoder[StepConfig.Gcal] =
    Encoder.instance { (a: StepConfig.Gcal) =>
      Json.obj(
        "continuum" -> a.continuum.asJson,
        "arcs"      -> a.arcs.asJson,
        "filter"    -> a.filter.asJson,
        "diffuser"  -> a.diffuser.asJson,
        "shutter"   -> a.shutter.asJson
      )
    }

  given Decoder[StepConfig.Science] =
    Decoder.instance { c =>
      c.downField("offset").as[Offset].map { o =>
        StepConfig.Science(o)
      }
    }

  given (using Encoder[Offset]): Encoder[StepConfig.Science] =
    Encoder.instance { (a: StepConfig.Science) =>
      Json.obj(
        "offset" -> a.offset.asJson
      )
    }

  given Decoder[StepConfig] =
    Decoder.instance { c =>
      c.downField("stepType").as[StepType].flatMap {
        case StepType.Bias      => StepConfig.Bias.asRight[DecodingFailure]
        case StepType.Dark      => StepConfig.Dark.asRight[DecodingFailure]
        case StepType.Gcal      => c.as[StepConfig.Gcal]
        case StepType.Science   => c.as[StepConfig.Science]
        case StepType.SmartGcal => DecodingFailure("SmartGcal not implemented", c.history).asLeft[StepConfig]
      }
    }

  given (using Encoder[Offset]): Encoder[StepConfig] =
    Encoder.instance { (a: StepConfig) =>
      (a match {
        case _: StepConfig.Bias.type => Json.obj()
        case _: StepConfig.Dark.type => Json.obj()
        case s: StepConfig.Science   => s.asJson
        case g: StepConfig.Gcal      => g.asJson
      }).mapObject(("stepType" -> a.stepType.asJson) +: _)
    }

}

object stepconfig extends StepConfigCodec

