// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.Applicative
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.syntax.bifunctor.*
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.option.*
import io.circe.Encoder
import lucuma.core.data.Zipper
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.ExecutionSequence
import lucuma.core.model.sequence.FutureExecutionConfig
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepTime
import lucuma.core.util.TimeSpan
import lucuma.itc.client.ItcClient
import lucuma.itc.client.ItcResult
import lucuma.itc.client.SpectroscopyModeInput
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoExecution
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.sequence.util.SequenceIds

import java.io.ObjectOutputStream
import java.util.UUID

sealed trait Generator[F[_]] {

  def generate(
    observationId: Observation.Id,
    params:        GeneratorParams,
    useCache:      Boolean
  ): F[Generator.Result]

}

object Generator {

  sealed trait Result
  sealed trait Error extends Result

  object Result {

    case class ItcServiceError(
      errors: NonEmptyList[Itc.Error]
    ) extends Error

    case class InvalidData(
      message: String
    ) extends Error

    case class Success(
      observationId: Observation.Id,
      itc:           Itc.ResultSet,
      value:         FutureExecutionConfig
    ) extends Result
  }

  def fromClient[F[_]: Applicative](
    commitHash: CommitHash,
    itcClient:  ItcClient[F]
  ): Generator[F] =
    new Generator[F] {

      private val itc = Itc.fromClient(itcClient)
      private val StepTimeZero: StepTime = StepTime(
        TimeSpan.Zero,
        TimeSpan.Zero,
        TimeSpan.Zero,
        TimeSpan.Zero,
        TimeSpan.Zero
      )

      override def generate(
        observationId: Observation.Id,
        params:        GeneratorParams,
        useCache:      Boolean
      ): F[Result] = {

        val namespace = SequenceIds.namespace(commitHash, observationId, params)

        params match {
          case GeneratorParams.GmosNorthLongSlit(itcInput, config) =>
            gmosLongSlit(itcInput, config, gmos.longslit.Generator.GmosNorth, useCache).map(
              _.map { case (rs, proto) =>
                Result.Success(
                  observationId,
                  rs,
                  futureExecutionConfig(
                    namespace,
                    proto,
                    FutureExecutionConfig.GmosNorth.apply,
                    ExecutionSequence.GmosNorth.apply,
                    Atom.GmosNorth.apply,
                    Step.GmosNorth.apply
                  )
                )
              }.merge
            )

          case GeneratorParams.GmosSouthLongSlit(itcInput, config) =>
            gmosLongSlit(itcInput, config, gmos.longslit.Generator.GmosSouth, useCache).map(
              _.map { case (rs, proto) =>
                Result.Success(
                  observationId,
                  rs,
                  futureExecutionConfig(
                    namespace,
                    proto,
                    FutureExecutionConfig.GmosSouth.apply,
                    ExecutionSequence.GmosSouth.apply,
                    Atom.GmosSouth.apply,
                    Step.GmosSouth.apply
                  )
                )
              }.merge
            )

        }

      }

      def gmosLongSlit[S, D, G, L, U](
        itcInput:  NonEmptyList[(Target.Id, SpectroscopyModeInput)],
        config:    gmos.longslit.Config[G, L, U],
        generator: gmos.longslit.Generator[S, D, G, L, U],
        useCache:  Boolean
      ): F[Either[Error, (Itc.ResultSet, ProtoExecution[S, D])]] =

        itc
          .spectroscopy(itcInput, useCache)
          .map {
            case Left(errors) =>
              Result.ItcServiceError(errors).asLeft
            case Right(rs)    =>
              val success = rs.value.focus
              generator.generate(
                success.value,
                success.input.sourceProfile,
                success.input.constraints.imageQuality,
                config
              ) match {
                case Left(msg)    => Result.InvalidData(msg).asLeft
                case Right(proto) => (rs, proto).asRight
              }
          }

      def futureExecutionConfig[X <: ExecutionSequence, A <: Atom, T <: Step, S, D](
        namespace:  UUID,
        proto:      ProtoExecution[S, D],
        mkExec:     (S, X, X) => FutureExecutionConfig,
        mkSequence: (A, List[A]) => X,
        mkAtom:     (Atom.Id, List[T]) => A,
        mkStep:     (Step.Id, D, StepConfig, StepTime, Breakpoint) => T
      ): FutureExecutionConfig = {

        def executionSequence(
          sequence:     NonEmptyList[ProtoAtom[D]],
          sequenceType: SequenceType
        ): X = {
          val NonEmptyList(h, t) =
            sequence
              .zipWithIndex
              .map(_.map(SequenceIds.atomId(namespace, sequenceType, _)))
              .map { case (atom, atomId) =>
                mkAtom(
                  atomId,
                  atom.steps.zipWithIndex.map { case (s, j) =>
                    mkStep(
                      SequenceIds.stepId(namespace, sequenceType, atomId, j),
                      s.instrumentConfig,
                      s.stepConfig,
                      StepTimeZero,
                      Breakpoint.Disabled
                    )
                  }.toList
                )
              }
          mkSequence(h, t)
        }

        mkExec(
          proto.static,
          executionSequence(proto.acquisition, SequenceType.Acquisition),
          executionSequence(proto.science,     SequenceType.Science)
        )
      }


    }

}
