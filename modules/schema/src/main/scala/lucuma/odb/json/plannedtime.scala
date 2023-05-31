// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import eu.timepit.refined.types.numeric.NonNegInt
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.JsonObject
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.data.Zipper
import lucuma.core.enums.ChargeClass
import lucuma.core.model.sequence.ConfigChangeEstimate
import lucuma.core.model.sequence.DatasetEstimate
import lucuma.core.model.sequence.DetectorEstimate
import lucuma.core.model.sequence.PlannedTime
import lucuma.core.model.sequence.SetupTime
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.util.TimeSpan


trait PlannedTimeCodec {

  import time.decoder.given
  import zipper.given

  given Decoder[SetupTime] =
    Decoder.instance { c =>
      for {
        f <- c.downField("full").as[TimeSpan]
        r <- c.downField("reacquisition").as[TimeSpan]
      } yield SetupTime(f, r)
    }

  given (using Encoder[TimeSpan]): Encoder[SetupTime] =
    Encoder.instance { (a: SetupTime) =>
      Json.obj(
        "full"          -> a.full.asJson,
        "reacquisition" -> a.reacquisition.asJson
      )
    }

  given Decoder[DatasetEstimate] =
    Decoder.instance { c =>
      for {
        e <- c.downField("exposure").as[TimeSpan]
        r <- c.downField("readout").as[TimeSpan]
        w <- c.downField("write").as[TimeSpan]
      } yield DatasetEstimate(e, r, w)
    }

  given (using Encoder[TimeSpan]): Encoder[DatasetEstimate] =
    Encoder.instance { (a: DatasetEstimate) =>
      Json.obj(
        "exposure" -> a.exposure.asJson,
        "readout"  -> a.readout.asJson,
        "write"    -> a.write.asJson,
        "estimate" -> a.estimate.asJson
      )
    }

  given Decoder[DetectorEstimate] =
    Decoder.instance { c =>
      for {
        n <- c.downField("name").as[String]
        d <- c.downField("description").as[String]
        s <- c.downField("dataset").as[DatasetEstimate]
        u <- c.downField("count").as[NonNegInt]
      } yield DetectorEstimate(n, d, s, u)
    }

  given (using Encoder[TimeSpan]): Encoder[DetectorEstimate] =
    Encoder.instance { (a: DetectorEstimate) =>
      Json.obj(
        "name"         -> a.name.asJson,
        "description"  -> a.description.asJson,
        "dataset"      -> a.dataset.asJson,
        "count"        -> a.count.asJson,
        "estimate"     -> a.estimate.asJson
      )
    }

  private val DecoderAllDetectorEstimates: Decoder[Zipper[DetectorEstimate]] =
    Decoder.instance(_.as[Zipper[DetectorEstimate]])

  private def EncoderAllDetectorEstimates(using Encoder[TimeSpan]): Encoder[Zipper[DetectorEstimate]] =
    Encoder.instance { (a: Zipper[DetectorEstimate]) =>
      Encoder[Zipper[DetectorEstimate]]
        .apply(a)
        .mapObject(_.add("estimate", a.focus.estimate.asJson))
    }

  // ConfigChangeDetailEstimate in the schema. ConfigChangeEstimate in the
  // schema is a Zipper[ConfigChangeEstimate] in Scala.
  given Decoder[ConfigChangeEstimate] =
    Decoder.instance { c =>
      for {
        nm <- c.downField("name").as[String]
        ds <- c.downField("description").as[String]
        es <- c.downField("estimate").as[TimeSpan]
      } yield ConfigChangeEstimate(nm, ds, es)
    }

  given (using Encoder[TimeSpan]): Encoder[ConfigChangeEstimate] =
    Encoder.instance { (a: ConfigChangeEstimate) =>
      Json.obj(
        "name"        -> a.name.asJson,
        "description" -> a.description.asJson,
        "estimate"    -> a.estimate.asJson
      )
    }

  private val DecoderAllConfigChangeEstimates: Decoder[Zipper[ConfigChangeEstimate]] =
    Decoder.instance(_.as[Zipper[ConfigChangeEstimate]])

  private def EncoderAllConfigChangeEstimates(using Encoder[TimeSpan]): Encoder[Zipper[ConfigChangeEstimate]] =
    Encoder.instance { (a: Zipper[ConfigChangeEstimate]) =>
      Encoder[Zipper[ConfigChangeEstimate]]
        .apply(a)
        .mapObject(_.add("estimate", a.focus.estimate.asJson))
    }

  given Decoder[StepEstimate] = {
    given ce: Decoder[Zipper[ConfigChangeEstimate]] = DecoderAllConfigChangeEstimates
    given de: Decoder[Zipper[DetectorEstimate]]     = DecoderAllDetectorEstimates

    Decoder.instance { c =>
      for {
        h <- c.downField("configChange").as[Option[Zipper[ConfigChangeEstimate]]]
        d <- c.downField("detector").as[Option[Zipper[DetectorEstimate]]]
      } yield StepEstimate(h, d)
    }
  }

  given (using Encoder[TimeSpan]): Encoder[StepEstimate] =
    Encoder.instance { (a: StepEstimate) =>
      Json.fromFields(
        "total" -> a.total.asJson ::
        a.configChange.toList.map { z => "configChange" -> EncoderAllConfigChangeEstimates.apply(z) } ++
        a.detector.toList.map { z => "detector" -> EncoderAllDetectorEstimates.apply(z) }
      )
    }

  // PlannedTimeCharge in the schema
  private given Decoder[(ChargeClass, TimeSpan)] =
    Decoder.instance { c =>
      for {
        s <- c.downField("chargeClass").as[ChargeClass]
        t <- c.downField("time").as[TimeSpan]
      } yield (s, t)
    }

  // PlannedTimeCharge in the schema
  private given (using Encoder[TimeSpan]): Encoder[(ChargeClass, TimeSpan)] =
    Encoder.instance { (a: (ChargeClass, TimeSpan)) =>
      Json.obj(
        "chargeClass" -> a._1.asJson,
        "time"        -> a._2.asJson
      )
    }

  given Decoder[PlannedTime] =
    Decoder.instance { c =>
      c.downField("charges").as[List[(ChargeClass, TimeSpan)]].map(PlannedTime.from)
    }

  given (using Encoder[TimeSpan]): Encoder[PlannedTime] =
    Encoder.instance { (a: PlannedTime) =>
      Json.obj(
        "charges" -> a.charges.asJson,
        "total"   -> a.sum.asJson
      )
    }
}

object plannedtime extends PlannedTimeCodec


