// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.JsonObject
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.ChargeClass
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.ExecutionConfig
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.PlannedTime
import lucuma.core.model.sequence.Sequence
import lucuma.core.model.sequence.SetupTime
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.util.TimeSpan
import lucuma.core.util.Uid

// (using Encoder[Offset], Encoder[TimeSpan], Encoder[Wavelength])

trait SequenceCodec {

  import stepconfig.given
  import time.decoder.given
  import zipper.given
  import plannedtime.given

  given [D: Decoder]: Decoder[Step[D]] =
    Decoder.instance { c =>
      for {
        i <- c.downField("id").as[Step.Id]
        d <- c.downField("instrumentConfig").as[D]
        n <- c.downField("stepConfig").as[StepConfig]
        e <- c.downField("estimate").as[StepEstimate]
        o <- c.downField("observeClass").as[ObserveClass]
        b <- c.downField("breakpoint").as[Breakpoint]
      } yield Step(i, d, n, e, o, b)
    }

  given [D: Encoder](using Encoder[Offset], Encoder[TimeSpan]): Encoder[Step[D]] =
    Encoder.instance { (a: Step[D]) =>
      Json.obj(
        "id"               -> a.id.asJson,
        "instrumentConfig" -> a.instrumentConfig.asJson,
        "stepConfig"       -> a.stepConfig.asJson,
        "estimate"         -> a.estimate.asJson,
        "observeClass"     -> a.observeClass.asJson,
        "breakpoint"       -> a.breakpoint.asJson
      )
    }

  given [D: Decoder]: Decoder[Atom[D]] =
    Decoder.instance { c =>
      for {
        i <- c.downField("id").as[Atom.Id]
        d <- c.downField("description").as[Option[String]]
        s <- c.downField("steps").as[List[Step[D]]]
        n <- NonEmptyList.fromList(s).toRight(DecodingFailure("At least one step is required in the `steps` array of an atom", c.history))
      } yield Atom(i, d, n)
    }

  given [D: Encoder](using Encoder[Offset], Encoder[TimeSpan]): Encoder[Atom[D]] =
    Encoder.instance { (a: Atom[D]) =>
      Json.obj(
        "id"           -> a.id.asJson,
        "description"  -> a.description.asJson,
        "steps"        -> a.steps.toList.asJson,
        "observeClass" -> a.observeClass.asJson
      )
    }

  given [D: Decoder]: Decoder[Sequence[D]] =
    Decoder.instance { c =>
      for {
        n <- c.downField("nextAtom").as[Atom[D]]
        f <- c.downField("possibleFuture").as[List[Atom[D]]]
      } yield Sequence(NonEmptyList(n, f))
    }

  given [D: Encoder](using Encoder[Offset], Encoder[TimeSpan]): Encoder[Sequence[D]] =
    Encoder.instance { (a: Sequence[D]) =>
      Json.obj(
        "nextAtom"       -> a.atoms.head.asJson,
        "possibleFuture" -> a.atoms.tail.asJson,
        "observeClass"   -> a.observeClass.asJson,
        "plannedTime"    -> a.plannedTime.asJson
      )
    }

  given [S: Decoder, D: Decoder]: Decoder[ExecutionConfig[S, D]] =
    Decoder.instance { c =>
      for {
        t <- c.downField("static").as[S]
        a <- c.downField("acquisition").as[Option[Sequence[D]]]
        s <- c.downField("science").as[Option[Sequence[D]]]
        u <- c.downField("setup").as[SetupTime]
      } yield ExecutionConfig(t, a, s, u)
    }

  given [S: Encoder, D: Encoder](using Encoder[Offset], Encoder[TimeSpan]): Encoder[ExecutionConfig[S, D]] =
    Encoder.instance { (a: ExecutionConfig[S, D]) =>
      Json.obj(
        "static"        -> a.static.asJson,
        "acquisition"   -> a.acquisition.asJson,
        "science"       -> a.science.asJson,
        "setup"         -> a.setup.asJson,
        "observeClass"  -> a.observeClass.asJson
      )
    }

  import lucuma.odb.json.gmos.given

  private def rootDecoder[R, S: Decoder, D: Decoder](name: String)(instrumentExecutionConfig: ExecutionConfig[S, D] => R): Decoder[R] =
    Decoder.instance { c =>
      for {
        _ <- c.downField(name).as[Boolean]
        r <- c.as[ExecutionConfig[S, D]]
      } yield instrumentExecutionConfig(r)
    }

  given Decoder[InstrumentExecutionConfig.GmosNorth] =
    rootDecoder("gmosNorth")(InstrumentExecutionConfig.GmosNorth.apply)

  given Decoder[InstrumentExecutionConfig.GmosSouth] =
    rootDecoder("gmosSouth")(InstrumentExecutionConfig.GmosSouth.apply)

  private def rootEncoder[R, S: Encoder, D: Encoder](using Encoder[Offset], Encoder[TimeSpan])(
    name:       String,
    instrument: Instrument
  )(root: R => ExecutionConfig[S, D]): Encoder[R] =
    Encoder.instance { (a: R) =>
      root(a).asJson.mapObject { obj =>
        ("instrument", instrument.asJson) +: (name, true.asJson) +: obj
      }
    }

  given (using Encoder[Offset], Encoder[TimeSpan], Encoder[Wavelength]): Encoder[InstrumentExecutionConfig.GmosNorth] =
    rootEncoder("gmosNorth", Instrument.GmosNorth)(_.executionConfig)

  given (using Encoder[Offset], Encoder[TimeSpan], Encoder[Wavelength]): Encoder[InstrumentExecutionConfig.GmosSouth] =
    rootEncoder("gmosSouth", Instrument.GmosSouth)(_.executionConfig)

  given Decoder[InstrumentExecutionConfig] =
    Decoder.instance { c =>
      for {
        i <- c.downField("instrument").as[Instrument]
        r <- i match {
          case Instrument.GmosNorth => c.as[InstrumentExecutionConfig.GmosNorth]
          case Instrument.GmosSouth => c.as[InstrumentExecutionConfig.GmosSouth]
          case _                    => DecodingFailure(s"Unexpected instrument $i", c.history).asLeft[InstrumentExecutionConfig]
        }
      } yield r
    }

  given (using Encoder[Offset], Encoder[TimeSpan], Encoder[Wavelength]): Encoder[InstrumentExecutionConfig] =
    Encoder.instance {
      case i@InstrumentExecutionConfig.GmosNorth(_) => i.asJson
      case i@InstrumentExecutionConfig.GmosSouth(_) => i.asJson
    }

}

object sequence extends SequenceCodec