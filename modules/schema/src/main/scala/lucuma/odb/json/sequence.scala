// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.syntax.either.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.Instrument
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.DynamicConfig.GmosSouth
import lucuma.core.model.sequence.ExecutionSequence
import lucuma.core.model.sequence.FutureExecutionConfig
import lucuma.core.model.sequence.StaticConfig
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepTime
import lucuma.core.util.TimeSpan
import lucuma.core.util.Uid

trait SequenceCodec {

  import gmos.given
  import stepconfig.given
  import time.decoder.given

  given Decoder[StepTime] =
    Decoder.instance { c =>
      for {
        h <- c.downField("configChange").as[TimeSpan]
        e <- c.downField("exposure").as[TimeSpan]
        r <- c.downField("readout").as[TimeSpan]
        w <- c.downField("write").as[TimeSpan]
        t <- c.downField("total").as[TimeSpan]
      } yield StepTime(h, e, r, w, t)
    }

  given (using Encoder[TimeSpan]): Encoder[StepTime] =
    Encoder.instance { (a: StepTime) =>
      Json.obj(
        "configChange" -> a.configChange.asJson,
        "exposure"     -> a.exposure.asJson,
        "readout"      -> a.readout.asJson,
        "write"        -> a.write.asJson,
        "total"        -> a.total.asJson
      )
    }

  given given_Decoder_Step_GmosNorth: Decoder[Step.GmosNorth] =
    Decoder.instance { c =>
      for {
        i <- c.downField("id").as[Step.Id]
        g <- c.downField("instrumentConfig").as[GmosNorth]
        s <- c.downField("stepConfig").as[StepConfig]
        t <- c.downField("time").as[StepTime]
        b <- c.downField("breakpoint").as[Breakpoint]
      } yield Step.GmosNorth(i, g, s, t, b)
    }

  given given_Encoder_Step_GmosNorth(using Encoder[Offset], Encoder[TimeSpan], Encoder[Wavelength]): Encoder[Step.GmosNorth] =
    Encoder.instance { (a: Step.GmosNorth) =>
      Json.obj(
        "id"               -> a.id.asJson,
        "instrumentConfig" -> a.instrumentConfig.asJson,
        "stepConfig"       -> a.stepConfig.asJson,
        "time"             -> a.time.asJson,
        "breakpoint"       -> a.breakpoint.asJson
      )
    }

  given given_Decoder_Step_GmosSouth: Decoder[Step.GmosSouth] =
    Decoder.instance { c =>
      for {
        i <- c.downField("id").as[Step.Id]
        g <- c.downField("instrumentConfig").as[GmosSouth]
        s <- c.downField("stepConfig").as[StepConfig]
        t <- c.downField("time").as[StepTime]
        b <- c.downField("breakpoint").as[Breakpoint]
      } yield Step.GmosSouth(i, g, s, t, b)
    }

  given given_Encoder_Step_GmosSouth(using Encoder[Offset], Encoder[TimeSpan], Encoder[Wavelength]): Encoder[Step.GmosSouth] =
    Encoder.instance { (a: Step.GmosSouth) =>
      Json.obj(
        "id"               -> a.id.asJson,
        "instrumentConfig" -> a.instrumentConfig.asJson,
        "stepConfig"       -> a.stepConfig.asJson,
        "time"             -> a.time.asJson,
        "breakpoint"       -> a.breakpoint.asJson
      )
    }

  given given_Decoder_Atom_GmosNorth: Decoder[Atom.GmosNorth] =
    Decoder.instance { c =>
      for {
        i <- c.downField("id").as[Atom.Id]
        s <- c.downField("steps").as[List[Step.GmosNorth]]
      } yield Atom.GmosNorth(i, s)
    }

  given given_Encoder_Atom_GmosNorth(using Encoder[Offset], Encoder[TimeSpan], Encoder[Wavelength]): Encoder[Atom.GmosNorth] =
    Encoder.instance { (a: Atom.GmosNorth) =>
      Json.obj(
        "id"    -> a.id.asJson,
        "steps" -> a.steps.asJson
      )
    }

  given given_Decoder_Atom_GmosSouth: Decoder[Atom.GmosSouth] =
    Decoder.instance { c =>
      for {
        i <- c.downField("id").as[Atom.Id]
        s <- c.downField("steps").as[List[Step.GmosSouth]]
      } yield Atom.GmosSouth(i, s)
    }

  given given_Encoder_Atom_GmosSouth(using Encoder[Offset], Encoder[TimeSpan], Encoder[Wavelength]): Encoder[Atom.GmosSouth] =
    Encoder.instance { (a: Atom.GmosSouth) =>
      Json.obj(
        "id"    -> a.id.asJson,
        "steps" -> a.steps.asJson
      )
    }

  given given_Decoder_ExecutionSequence_GmosNorth: Decoder[ExecutionSequence.GmosNorth] =
    Decoder.instance { c =>
      for {
        n <- c.downField("nextAtom").as[Atom.GmosNorth]
        f <- c.downField("possibleFuture").as[List[Atom.GmosNorth]]
      } yield ExecutionSequence.GmosNorth(n, f)
    }

  given given_Encoder_ExecutionSequence_GmosNorth(using Encoder[Offset], Encoder[TimeSpan], Encoder[Wavelength]): Encoder[ExecutionSequence.GmosNorth] =
    Encoder.instance { (a: ExecutionSequence.GmosNorth) =>
      Json.obj(
        "nextAtom"       -> a.nextAtom.asJson,
        "possibleFuture" -> a.possibleFuture.asJson
      )
    }

  given given_Decoder_ExecutionSequence_GmosSouth: Decoder[ExecutionSequence.GmosSouth] =
    Decoder.instance { c =>
      for {
        n <- c.downField("nextAtom").as[Atom.GmosSouth]
        f <- c.downField("possibleFuture").as[List[Atom.GmosSouth]]
      } yield ExecutionSequence.GmosSouth(n, f)
    }

  given given_Encoder_ExecutionSequence_GmosSouth(using Encoder[Offset], Encoder[TimeSpan], Encoder[Wavelength]): Encoder[ExecutionSequence.GmosSouth] =
    Encoder.instance { (a: ExecutionSequence.GmosSouth) =>
      Json.obj(
        "nextAtom"       -> a.nextAtom.asJson,
        "possibleFuture" -> a.possibleFuture.asJson
      )
    }

  given given_Decoder_FutureExecutionConfig_GmosNorth: Decoder[FutureExecutionConfig.GmosNorth] =
    Decoder.instance { c =>
      for {
        t <- c.downField("static").as[StaticConfig.GmosNorth]
        a <- c.downField("acquisition").as[ExecutionSequence.GmosNorth]
        s <- c.downField("science").as[ExecutionSequence.GmosNorth]
      } yield FutureExecutionConfig.GmosNorth(t, a, s)
    }

  given given_Encoder_FutureExecutionConfig_GmosNorth(using Encoder[Offset], Encoder[TimeSpan], Encoder[Wavelength]): Encoder[FutureExecutionConfig.GmosNorth] =
    Encoder.instance { (a: FutureExecutionConfig.GmosNorth) =>
      Json.obj(
        "instrument"  -> (Instrument.GmosNorth: Instrument).asJson,
        "static"      -> a.static.asJson,
        "acquisition" -> a.acquisition.asJson,
        "science"     -> a.science.asJson
      )
    }

  given given_Decoder_FutureExecutionConfig_GmosSouth: Decoder[FutureExecutionConfig.GmosSouth] =
    Decoder.instance { c =>
      for {
        t <- c.downField("static").as[StaticConfig.GmosSouth]
        a <- c.downField("acquisition").as[ExecutionSequence.GmosSouth]
        s <- c.downField("science").as[ExecutionSequence.GmosSouth]
      } yield FutureExecutionConfig.GmosSouth(t, a, s)
    }

  given given_Encoder_FutureExecutionConfig_GmosSouth(using Encoder[Offset], Encoder[TimeSpan], Encoder[Wavelength]): Encoder[FutureExecutionConfig.GmosSouth] =
    Encoder.instance { (a: FutureExecutionConfig.GmosSouth) =>
      Json.obj(
        "instrument"  -> (Instrument.GmosSouth: Instrument).asJson,
        "static"      -> a.static.asJson,
        "acquisition" -> a.acquisition.asJson,
        "science"     -> a.science.asJson
      )
    }

  given given_Decoder_FutureExecutionConfig: Decoder[FutureExecutionConfig] =
    Decoder.instance { c =>
      for {
        i <- c.downField("instrument").as[Instrument]
        x <- i match {
          case Instrument.GmosNorth => Decoder[FutureExecutionConfig.GmosNorth].apply(c)
          case Instrument.GmosSouth => Decoder[FutureExecutionConfig.GmosSouth].apply(c)
          case _                    => DecodingFailure(s"Instrument not yet supported: ${i.tag}", c.history).asLeft
        }
      } yield x
    }

  given given_Encoder_FutureExecutionConfig(using Encoder[Offset], Encoder[TimeSpan], Encoder[Wavelength]): Encoder[FutureExecutionConfig] =
    Encoder.instance {
      case a: FutureExecutionConfig.GmosNorth =>
        Encoder[FutureExecutionConfig.GmosNorth].apply(a)
      case a: FutureExecutionConfig.GmosSouth =>
        Encoder[FutureExecutionConfig.GmosSouth].apply(a)
    }

}

object sequence extends SequenceCodec