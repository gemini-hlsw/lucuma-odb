// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.Order.catsKernelOrderingForOrder
import cats.data.NonEmptyList
import cats.syntax.either.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.ExecutionState
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.ExecutionConfig
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.model.sequence.ExecutionSequence
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.SequenceDigest
import lucuma.core.model.sequence.SetupTime
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.util.TimeSpan

import scala.collection.immutable.SortedSet

trait SequenceCodec {

  import offset.decoder.given
  import stepconfig.given
  import timeaccounting.given

  given Decoder[Dataset.Filename] =
    Decoder[String].emap { s =>
      Dataset.Filename.FromString.getOption(s).toRight(s"Invalid dataset filename: $s")
    }

  given Encoder[Dataset.Filename] =
    Encoder[String].contramap[Dataset.Filename](_.format)

  given [D: Decoder]: Decoder[Step[D]] =
    Decoder.instance { c =>
      for {
        i <- c.downField("id").as[Step.Id]
        d <- c.downField("instrumentConfig").as[D]
        n <- c.downField("stepConfig").as[StepConfig]
        t <- c.downField("telescopeConfig").as[TelescopeConfig]
        e <- c.downField("estimate").as[StepEstimate]
        o <- c.downField("observeClass").as[ObserveClass]
        b <- c.downField("breakpoint").as[Breakpoint]
      } yield Step(i, d, n, t, e, o, b)
    }

  given [D: Encoder](using Encoder[Offset], Encoder[TimeSpan]): Encoder[Step[D]] =
    Encoder.instance { (a: Step[D]) =>
      Json.obj(
        "id"               -> a.id.asJson,
        "instrumentConfig" -> a.instrumentConfig.asJson,
        "stepConfig"       -> a.stepConfig.asJson,
        "telescopeConfig"  -> a.telescopeConfig.asJson,
        "estimate"         -> a.estimate.asJson,
        "observeClass"     -> a.observeClass.asJson,
        "breakpoint"       -> a.breakpoint.asJson
      )
    }

  given [D: Decoder]: Decoder[Atom[D]] =
    Decoder.instance { c =>
      for {
        i <- c.downField("id").as[Atom.Id]
        d <- c.downField("description").as[Option[NonEmptyString]]
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

  given Decoder[SequenceDigest] =
    Decoder.instance: c =>
      for
        o <- c.downField("observeClass").as[ObserveClass]
        t <- c.downField("timeEstimate").as[CategorizedTime]
        f <- c.downField("offsets").as[List[Offset]].map(SortedSet.from)
        n <- c.downField("atomCount").as[NonNegInt]
        e <- c.downField("executionState").as[ExecutionState]
      yield SequenceDigest(o, t, f, n, e)

  given (using Encoder[Offset], Encoder[TimeSpan]): Encoder[SequenceDigest] =
    Encoder.instance: (a: SequenceDigest) =>
      Json.obj(
        "observeClass"   -> a.observeClass.asJson,
        "timeEstimate"   -> a.timeEstimate.asJson,
        "offsets"        -> a.offsets.toList.asJson,
        "atomCount"      -> a.atomCount.asJson,
        "executionState" -> a.executionState.asJson
      )

  given Decoder[ExecutionDigest] =
    Decoder.instance { c =>
      for {
        t <- c.downField("setup").as[SetupTime]
        a <- c.downField("acquisition").as[SequenceDigest]
        s <- c.downField("science").as[SequenceDigest]
      } yield ExecutionDigest(t, a, s)
    }

  given (using Encoder[Offset], Encoder[TimeSpan]): Encoder[ExecutionDigest] =
    Encoder.instance { (a: ExecutionDigest) =>
      Json.obj(
        "setup"       -> a.setup.asJson,
        "acquisition" -> a.acquisition.asJson,
        "science"     -> a.science.asJson
      )
    }

  given [D: Decoder]: Decoder[ExecutionSequence[D]] =
    Decoder.instance { c =>
      for {
        n <- c.downField("nextAtom").as[Atom[D]]
        f <- c.downField("possibleFuture").as[List[Atom[D]]]
        m <- c.downField("hasMore").as[Boolean]
      } yield ExecutionSequence(n, f, m)
    }

  given [D: Encoder](using Encoder[Offset], Encoder[TimeSpan]): Encoder[ExecutionSequence[D]] =
    Encoder.instance { (a: ExecutionSequence[D]) =>
      Json.obj(
        "nextAtom"       -> a.nextAtom.asJson,
        "possibleFuture" -> a.possibleFuture.asJson,
        "hasMore"        -> a.hasMore.asJson
      )
    }

  given [S: Decoder, D: Decoder]: Decoder[ExecutionConfig[S, D]] =
    Decoder.instance { c =>
      for {
        t <- c.downField("static").as[S]
        a <- c.downField("acquisition").as[Option[ExecutionSequence[D]]]
        s <- c.downField("science").as[Option[ExecutionSequence[D]]]
      } yield ExecutionConfig(t, a, s)
    }

  given [S: Encoder, D: Encoder](using Encoder[Offset], Encoder[TimeSpan]): Encoder[ExecutionConfig[S, D]] =
    Encoder.instance { (a: ExecutionConfig[S, D]) =>
      Json.obj(
        "static"        -> a.static.asJson,
        "acquisition"   -> a.acquisition.asJson,
        "science"       -> a.science.asJson
      )
    }

  import lucuma.odb.json.flamingos2.given
  import lucuma.odb.json.gmos.given

  private def rootDecoder[R, S: Decoder, D: Decoder](instrumentExecutionConfig: ExecutionConfig[S, D] => R): Decoder[R] =
    Decoder.instance { _.as[ExecutionConfig[S, D]].map(instrumentExecutionConfig) }

  given Decoder[InstrumentExecutionConfig.Flamingos2] =
    rootDecoder(InstrumentExecutionConfig.Flamingos2.apply)

  given Decoder[InstrumentExecutionConfig.GmosNorth] =
    rootDecoder(InstrumentExecutionConfig.GmosNorth.apply)

  given Decoder[InstrumentExecutionConfig.GmosSouth] =
    rootDecoder(InstrumentExecutionConfig.GmosSouth.apply)

  private def rootEncoder[R, S: Encoder, D: Encoder](using Encoder[Offset], Encoder[TimeSpan])(
    root: R => ExecutionConfig[S, D]
  ): Encoder[R] =
    Encoder.instance { (a: R) => root(a).asJson }

  given (using Encoder[Offset], Encoder[TimeSpan], Encoder[Wavelength]): Encoder[InstrumentExecutionConfig.Flamingos2] =
    rootEncoder(_.executionConfig)

  given (using Encoder[Offset], Encoder[TimeSpan], Encoder[Wavelength]): Encoder[InstrumentExecutionConfig.GmosNorth] =
    rootEncoder(_.executionConfig)

  given (using Encoder[Offset], Encoder[TimeSpan], Encoder[Wavelength]): Encoder[InstrumentExecutionConfig.GmosSouth] =
    rootEncoder(_.executionConfig)

  given Decoder[InstrumentExecutionConfig] =
    Decoder.instance { c =>
      for {
        i <- c.downField("instrument").as[Instrument]
        r <- i match {
          case Instrument.Flamingos2 => c.downField("flamingos2").as[InstrumentExecutionConfig.Flamingos2]
          case Instrument.GmosNorth  => c.downField("gmosNorth").as[InstrumentExecutionConfig.GmosNorth]
          case Instrument.GmosSouth  => c.downField("gmosSouth").as[InstrumentExecutionConfig.GmosSouth]
          case _                     => DecodingFailure(s"Unexpected instrument $i", c.history).asLeft[InstrumentExecutionConfig]
        }
      } yield r
    }

  given (using Encoder[Offset], Encoder[TimeSpan], Encoder[Wavelength]): Encoder[InstrumentExecutionConfig] =
    Encoder.instance: (a: InstrumentExecutionConfig) =>
      Json.obj(
        "instrument" -> a.instrument.asJson,
        "flamingos2" -> Json.Null, // one of these will be replaced
        "gmosNorth"  -> Json.Null, // one of these will be replaced
        "gmosSouth"  -> Json.Null, // one of these will be replaced
        a match
          case i@InstrumentExecutionConfig.Flamingos2(_) => "flamingos2" -> i.asJson
          case i@InstrumentExecutionConfig.GmosNorth(_)  => "gmosNorth"  -> i.asJson
          case i@InstrumentExecutionConfig.GmosSouth(_)  => "gmosSouth"  -> i.asJson
      )

}

object sequence extends SequenceCodec
