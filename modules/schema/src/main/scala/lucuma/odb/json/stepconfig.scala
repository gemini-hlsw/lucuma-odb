// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.syntax.either.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax._
import lucuma.core.enums.GcalArc
import lucuma.core.enums.GcalContinuum
import lucuma.core.enums.GcalDiffuser
import lucuma.core.enums.GcalFilter
import lucuma.core.enums.GcalShutter
import lucuma.core.enums.SmartGcalType
import lucuma.core.enums.StepGuideState
import lucuma.core.enums.StepType
import lucuma.core.math.Offset
import lucuma.core.model.sequence.StepConfig

trait StepConfigCodec {

  import offset.decoder.given

  given given_Decoder_Lamp: Decoder[StepConfig.Gcal.Lamp] =
    Decoder.instance { c =>
      for {
        u <- c.downField("continuum").as[Option[GcalContinuum]]
        a <- c.downField("arcs").as[List[GcalArc]]
        r <- StepConfig.Gcal.Lamp.fromContinuumOrArcs(u, a).leftMap(msg => DecodingFailure(msg, c.history))
      } yield r
    }

  given given_Encoder_Lamp: Encoder[StepConfig.Gcal.Lamp] =
    Encoder.instance { (a: StepConfig.Gcal.Lamp) =>
      Json.obj(
        "continuum" -> a.continuum.asJson,
        "arcs"      -> a.toArcsSortedSet.asJson
      )
    }

  given Decoder[StepConfig.Gcal] =
    Decoder.instance { c =>
      for {
        l <- given_Decoder_Lamp(c)
        f <- c.downField("filter").as[GcalFilter]
        d <- c.downField("diffuser").as[GcalDiffuser]
        s <- c.downField("shutter").as[GcalShutter]
      } yield StepConfig.Gcal(l, f, d, s)
    }

  given Encoder[StepConfig.Gcal] =
    Encoder.instance { (a: StepConfig.Gcal) =>
      given_Encoder_Lamp(a.lamp).mapObject { jo =>
        jo.add("filter",   a.filter.asJson)
          .add("diffuser", a.diffuser.asJson)
          .add("shutter",  a.shutter.asJson)
      }
    }

  given Decoder[StepConfig.Science] =
    Decoder.instance { c =>
      for {
        o <- c.downField("offset").as[Offset]
        g <- c.downField("guiding").as[StepGuideState]
      } yield StepConfig.Science(o, g)
    }

  given (using Encoder[Offset]): Encoder[StepConfig.Science] =
    Encoder.instance { (a: StepConfig.Science) =>
      Json.obj(
        "offset"  -> a.offset.asJson,
        "guiding" -> a.guiding.asJson
      )
    }

  given Decoder[StepConfig.SmartGcal] =
    Decoder.instance { c =>
      c.downField("smartGcalType").as[SmartGcalType].map { sgt =>
        StepConfig.SmartGcal(sgt)
      }
    }

  given Encoder[StepConfig.SmartGcal] =
    Encoder.instance { (a: StepConfig.SmartGcal) =>
      Json.obj(
        "smartGcalType" -> a.smartGcalType.asJson
      )
    }

  given Decoder[StepConfig] =
    Decoder.instance { c =>
      c.downField("stepType").as[StepType].flatMap {
        case StepType.Bias      => StepConfig.Bias.asRight[DecodingFailure]
        case StepType.Dark      => StepConfig.Dark.asRight[DecodingFailure]
        case StepType.Gcal      => c.as[StepConfig.Gcal]
        case StepType.Science   => c.as[StepConfig.Science]
        case StepType.SmartGcal => c.as[StepConfig.SmartGcal]
      }
    }

  given (using Encoder[Offset]): Encoder[StepConfig] =
    Encoder.instance { (a: StepConfig) =>
      (a match {
        case _: StepConfig.Bias.type => Json.obj()
        case _: StepConfig.Dark.type => Json.obj()
        case s: StepConfig.Science   => s.asJson
        case g: StepConfig.Gcal      => g.asJson
        case m: StepConfig.SmartGcal => m.asJson
      }).mapObject(("stepType" -> a.stepType.asJson) +: _)
    }

}

object stepconfig extends StepConfigCodec

