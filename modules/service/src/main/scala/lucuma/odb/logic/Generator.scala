// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.Functor
import cats.MonadThrow
import cats.data.EitherT
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.data.NonEmptyVector
import cats.syntax.bifunctor.*
import cats.syntax.either.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import io.circe.Encoder
import lucuma.core.data.Zipper
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.ConfigChangeEstimate
import lucuma.core.model.sequence.DetectorEstimate
import lucuma.core.model.sequence.ExecutionConfig
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.Sequence
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.itc.client.ItcClient
import lucuma.itc.client.SpectroscopyIntegrationTimeInput
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoExecutionConfig
import lucuma.odb.sequence.data.ProtoSequence
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.gmos
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.sequence.util.SequenceIds
import lucuma.odb.service.GeneratorParamsService
import lucuma.odb.service.SmartGcalService

import java.io.ObjectOutputStream
import java.util.UUID

sealed trait Generator[F[_]] {

  def generate(
    programId:     Program.Id,
    observationId: Observation.Id,
    useCache:      Boolean
  ): F[Generator.Result]

}

object Generator {

  private val SequenceLengthLimit: Int = 1000

  sealed trait Result
  sealed trait Error extends Result {
    def format: String
  }

  object Result {

    case class ObservationNotFound(
      programId:     Program.Id,
      observationId: Observation.Id
    ) extends Error {
      def format: String =
        s"Observation '$observationId' in program '$programId' not found."
    }

    case class MissingParams(
      missing: NonEmptyList[GeneratorParamsService.MissingData]
    ) extends Error {
      def format: String = {
        val params = missing.map { m =>
          s"${m.targetId.fold("") { tid => s"(target $tid) " }}${m.paramName}"
        }.intercalate(", ")
        s"ITC cannot be queried until the following parameters are defined: $params"
      }
    }

    case class ItcServiceError(
      errors: NonEmptyList[Itc.Error]
    ) extends Error {
      def format: String =
        s"ITC service errors: ${errors.map(_.format).intercalate(", ")}"
    }

    case class SequenceTooLong(
      observationId: Observation.Id,
      length:        Int
    ) extends Error {
      def format: String =
        s"Sequence length is temporarily limited to $SequenceLengthLimit steps, but observation '$observationId' would require $length."
    }

    case class InvalidData(
      message: String
    ) extends Error {
      def format: String =
        s"Could not generate a sequence from the observation: $message"
    }

    case class MissingSmartGcalDef(
      key: String
    ) extends Error {
      def format: String =
        s"Could not generate a sequence, missing Smart GCAL mapping: $key"
    }

    case class Success(
      observationId: Observation.Id,
      itc:           Itc.ResultSet,
      value:         InstrumentExecutionConfig
    ) extends Result
  }

  def fromClientAndServices[F[_]: MonadThrow](
    commitHash:   CommitHash,
    itcClient:    ItcClient[F],
    paramsSrv:    GeneratorParamsService[F],
    smartGcalSrv: SmartGcalService[F],
    calculator:   PlannedTimeCalculator.ForInstrumentMode
  ): Generator[F] =
    new Generator[F] {

      import Result.*

      private val itc = Itc.fromClientAndServices(itcClient, paramsSrv)
      private val exp = SmartGcalExpander.fromService(smartGcalSrv)

      override def generate(
        programId:     Program.Id,
        observationId: Observation.Id,
        useCache:      Boolean
      ): F[Result] =
        (for {
          params <- selectParams(programId, observationId)
          res    <- generateSequence(observationId, params, useCache)
        } yield res).merge

      private def selectParams(
        pid: Program.Id,
        oid: Observation.Id
      ): EitherT[F, Error, GeneratorParams] =
        EitherT(
          paramsSrv
            .select(pid, oid)
            .map {
              case None                => ObservationNotFound(pid, oid).asLeft
              case Some(Left(missing)) => MissingParams(missing).asLeft
              case Some(Right(params)) => params.asRight
            }
        )

      // Generate a sequence for the observation, which will depend on the
      // observation's observing mode.
      private def generateSequence(
        oid:       Observation.Id,
        params:    GeneratorParams,
        useCache:  Boolean
      ): EitherT[F, Error, Success] = {
        val namespace = SequenceIds.namespace(commitHash, oid, params)

        params match {
          case GeneratorParams.GmosNorthLongSlit(itcInput, config) =>
            for {
              tup <- gmosLongSlit(oid, itcInput, config, gmos.longslit.Generator.GmosNorth, useCache)
              (rs, p0) = tup
              p1 <- expandAndEstimate(p0, exp.gmosNorth, _.format, calculator.gmosNorth)
            } yield Success(oid, rs, InstrumentExecutionConfig.GmosNorth(execConfig(namespace, p1, calculator.gmosNorth)))

          case GeneratorParams.GmosSouthLongSlit(itcInput, config) =>
            for {
              tup <- gmosLongSlit(oid, itcInput, config, gmos.longslit.Generator.GmosSouth, useCache)
              (rs, p0) = tup
              p1  <- expandAndEstimate(p0, exp.gmosSouth, _.format, calculator.gmosSouth)
            } yield Success(oid, rs, InstrumentExecutionConfig.GmosSouth(execConfig(namespace, p1, calculator.gmosSouth)))

        }
      }

      // Generates the initial GMOS LongSlit sequences, without smart-gcal expansion
      // or planned time calculation.
      private def gmosLongSlit[S, D, G, L, U](
        oid:       Observation.Id,
        itcInput:  NonEmptyList[(Target.Id, SpectroscopyIntegrationTimeInput)],
        config:    gmos.longslit.Config[G, L, U],
        generator: gmos.longslit.Generator[S, D, G, L, U],
        useCache:  Boolean
      ): EitherT[F, Error, (Itc.ResultSet, ProtoExecutionConfig[S, ProtoStep[D]])] = {

        def generate(s: Itc.Result.Success): Either[Error, ProtoExecutionConfig[S, ProtoStep[D]]] =
          generator.generate(
            s.value,
            s.input.sourceProfile,
            s.input.constraints.imageQuality,
            config
          ) match {
            case Left(msg)    => InvalidData(msg).asLeft
            case Right(proto) => proto.asRight
          }

        EitherT(
          itc
            .spectroscopy(itcInput, useCache)
            .map {
              case Left(errors) =>
                ItcServiceError(errors).asLeft
              case Right(rs)    =>
                val success = rs.value.focus
                success.value.exposures.value match {
                  case v if v > SequenceLengthLimit =>
                    SequenceTooLong(oid, v).asLeft
                  case _                            =>
                    generate(success).tupleLeft(rs)
                }
            }
        )
      }

      // Performs smart-gcal expansion and planned time calculation.
      private def expandAndEstimate[K, S, D](
        proto:     ProtoExecutionConfig[S, ProtoStep[D]],
        expander:  SmartGcalExpander[F, K, D],
        keyFormat: K => String,
        calc:      PlannedTimeCalculator[S, D]
      ): EitherT[F, Error, ProtoExecutionConfig[S, ProtoStep[(D, StepEstimate)]]] =
        EitherT(SmartGcalExpander.expandExecutionConfig(proto, expander))
          .bimap(
            k => MissingSmartGcalDef(keyFormat(k)),
            _.mapSequences(calc.estimateSequence(proto.static, _))
          )

      // Converts to lucuma-core ExecutionConfig, adding atom and step ids.
      private def execConfig[S, D](
        namespace: UUID,
        proto:     ProtoExecutionConfig[S, ProtoStep[(D, StepEstimate)]],
        calc:      PlannedTimeCalculator[S, D]
      ): ExecutionConfig[S, D] = {

        def toSequence(
          sequence:     ProtoSequence[ProtoStep[(D, StepEstimate)]],
          sequenceType: SequenceType
        ): Sequence[D] =
          Sequence(
            sequence
              .atoms
              .zipWithIndex
              .map(_.map(SequenceIds.atomId(namespace, sequenceType, _)))
              .map { case (atom, atomId) =>
                val steps = atom.steps.zipWithIndex.map { case (ProtoStep((d, e), sc, oc, bp), j) =>
                  Step(
                    SequenceIds.stepId(namespace, sequenceType, atomId, j),
                    d,
                    sc,
                    e,
                    oc,
                    bp
                  )
                }
                Atom(atomId, atom.description, steps)
              }
          )

        ExecutionConfig(
          proto.static,
          toSequence(proto.acquisition, SequenceType.Acquisition).some,
          toSequence(proto.science,     SequenceType.Science).some,
          calc.estimateSetup
        )
      }


    }

}
