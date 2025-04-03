// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import eu.timepit.refined.types.numeric.NonNegInt
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.data.Zipper
import lucuma.core.data.ZipperCodec
import lucuma.core.enums.ChargeClass
import lucuma.core.enums.ScienceBand
import lucuma.core.model.User
import lucuma.core.model.sequence.BandedTime
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.CategorizedTimeRange
import lucuma.core.model.sequence.ConfigChangeEstimate
import lucuma.core.model.sequence.DatasetEstimate
import lucuma.core.model.sequence.DetectorEstimate
import lucuma.core.model.sequence.SetupTime
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.model.sequence.TimeChargeCorrection
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.sso.client.codec.user.*

trait TimeAccountingCodec {

  import time.decoder.given
  import ZipperCodec.given

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
        "total"        -> a.total.asJson ::
        "configChange" -> Json.Null      :: // may be replaced below
        "detector"     -> Json.Null      :: // may be replaced below
        a.configChange.toList.map { z => "configChange" -> EncoderAllConfigChangeEstimates.apply(z) } ++
        a.detector.toList.map { z => "detector" -> EncoderAllDetectorEstimates.apply(z) }
      )
    }

  given Decoder[CategorizedTime] =
    Decoder.instance { c =>
      for {
        p <- c.downField("program").as[TimeSpan]
        n <- c.downField("nonCharged").as[TimeSpan]
      } yield CategorizedTime(ChargeClass.Program -> p, ChargeClass.NonCharged -> n)
    }

  given (using Encoder[TimeSpan]): Encoder[CategorizedTime] =
    Encoder.instance { (a: CategorizedTime) =>
      Json.obj(
        "program"    -> a(ChargeClass.Program).asJson,
        "nonCharged" -> a(ChargeClass.NonCharged).asJson,
        "total"      -> a.sum.asJson
      )
    }

  given Decoder[BandedTime] =
    Decoder.instance { c =>
      for {
        b <- c.downField("band").as[Option[ScienceBand]]
        t <- c.downField("time").as[CategorizedTime]
      } yield BandedTime(b, t)
    }

  given (using Encoder[TimeSpan]): Encoder[BandedTime] =
    Encoder.instance { (a: BandedTime) =>
      Json.obj(
        "band" -> a.band.asJson,
        "time" -> a.time.asJson
      )
    }

  given Decoder[CategorizedTimeRange] =
    Decoder.instance { c =>
      for {
        n <- c.downField("minimum").as[CategorizedTime]
        x <- c.downField("maximum").as[CategorizedTime]
      } yield CategorizedTimeRange.from(n, x)
    }

  given (using Encoder[TimeSpan]): Encoder[CategorizedTimeRange] =
    Encoder.instance { (a: CategorizedTimeRange) =>
      Json.obj(
        "minimum" -> a.min.asJson,
        "maximum" -> a.max.asJson
      )
    }

  given Decoder[TimeChargeCorrection] =
    Decoder.instance { c =>
      for {
        t <- c.downField("timestamp").as[Timestamp]
        u <- c.downField("user").as[User]
        z <- c.downField("chargeClass").as[ChargeClass]
        o <- c.downField("op").as[TimeChargeCorrection.Op]
        a <- c.downField("amount").as[TimeSpan]
        m <- c.downField("comment").as[Option[String]]
      } yield TimeChargeCorrection(t, u, z, o, a, m)
    }

  given (using Encoder[TimeSpan]): Encoder[TimeChargeCorrection] =
    Encoder.instance { (a: TimeChargeCorrection) =>
      Json.obj(
        "timestamp"   -> a.timestamp.asJson,
        "user"        -> a.user.asJson,
        "chargeClass" -> a.chargeClass.asJson,
        "op"          -> a.op.asJson,
        "amount"      -> a.amount.asJson,
        "comment"     -> a.comment.asJson
      )
    }

}

object timeaccounting extends TimeAccountingCodec


