// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.Functor
import cats.Monoid
import cats.data.EitherT
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.data.NonEmptyVector
import cats.effect.Concurrent
import cats.syntax.bifunctor.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.monoid.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import fs2.Pure
import fs2.Stream
import io.circe.Encoder
import lucuma.core.data.Zipper
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.math.Offset
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.ConfigChangeEstimate
import lucuma.core.model.sequence.DetectorEstimate
import lucuma.core.model.sequence.ExecutionConfig
import lucuma.core.model.sequence.ExecutionSequence
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.PlannedTime
import lucuma.core.model.sequence.SequenceDigest
import lucuma.core.model.sequence.SetupTime
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

  // Max atoms to appear in the possibleFuture + 1 for nextAtom
  // Hardcode for now, parameter later ?
  private val AtomLimit = 25

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
      value:         InstrumentExecutionConfig,
      scienceDigest: Option[SequenceDigest]
    ) extends Result
  }

  def fromClientAndServices[F[_]: Concurrent](
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
              tup <- gmosLongSlit(itcInput, config, gmos.longslit.Generator.GmosNorth, useCache)
              (itc, p0) = tup
              p1  <- expandAndEstimate(p0, exp.gmosNorth, calculator.gmosNorth)
              res <- EitherT.right(toExecutionConfig(namespace, p1, calculator.gmosNorth.estimateSetup))
            } yield Success(oid, itc, InstrumentExecutionConfig.GmosNorth(res), res.science.map(_.digest))

          case GeneratorParams.GmosSouthLongSlit(itcInput, config) =>
            for {
              tup <- gmosLongSlit(itcInput, config, gmos.longslit.Generator.GmosSouth, useCache)
              (itc, p0) = tup
              p1  <- expandAndEstimate(p0, exp.gmosSouth, calculator.gmosSouth)
              res <- EitherT.right(toExecutionConfig(namespace, p1, calculator.gmosSouth.estimateSetup))
            } yield Success(oid, itc, InstrumentExecutionConfig.GmosSouth(res), res.science.map(_.digest))
        }
      }

      // Generates the initial GMOS LongSlit sequences, without smart-gcal expansion
      // or planned time calculation.
      private def gmosLongSlit[S, D, G, L, U](
        itcInput:  NonEmptyList[(Target.Id, SpectroscopyIntegrationTimeInput)],
        config:    gmos.longslit.Config[G, L, U],
        generator: gmos.longslit.Generator[S, D, G, L, U],
        useCache:  Boolean
      ): EitherT[F, Error, (Itc.ResultSet, ProtoExecutionConfig[Pure, S, ProtoAtom[ProtoStep[D]]])] = {

        def generate(s: Itc.Result.Success): Either[Error, ProtoExecutionConfig[Pure, S, ProtoAtom[ProtoStep[D]]]] =
          generator.generate(
            s.value,
            s.input.sourceProfile,
            s.input.constraints.imageQuality,
            config
          ) match {
            case Left(msg)    => InvalidData(msg).asLeft
            case Right(proto) => proto.mapSequences(_.take(1), _.take(s.value.exposures.value)).asRight[Error]
          }

        EitherT(
          itc
            .spectroscopy(itcInput, useCache)
            .map {
              case Left(errors) => ItcServiceError(errors).asLeft
              case Right(rs)    => generate(rs.value.focus).tupleLeft(rs)
            }
        )
      }

      // Performs smart-gcal expansion and planned time calculation.
      private def expandAndEstimate[S, K, D](
        proto:    ProtoExecutionConfig[Pure, S, ProtoAtom[ProtoStep[D]]],
        expander: SmartGcalExpander[F, K, D],
        calc:     PlannedTimeCalculator[S, D]
      ): EitherT[F, Error, ProtoExecutionConfig[F, S, ProtoAtom[ProtoStep[(D, StepEstimate)]]]] =
        for {
          _ <- EitherT(expander.validate(proto.acquisition).map(_.leftMap(MissingSmartGcalDef.apply)))
          c <- EitherT(expander.validate(proto.science).map(_.leftMap(MissingSmartGcalDef.apply)))
          s <- EitherT.rightT(
                 proto.mapBothSequences[F, ProtoAtom[ProtoStep[(D, StepEstimate)]]](
                   _.through(expander.expandWithCache(c))
                    .through(calc.estimateSequence[F](proto.static))
                 )
               )
        } yield s

      private val offset = StepConfig.science.andThen(StepConfig.Science.offset)

      private case class SequenceSummary[D](
        initialAtoms: Vector[ProtoAtom[ProtoStep[(D, StepEstimate)]]],
        hasMore:      Boolean,
        atomCount:    Int,
        digest:       SequenceDigest
      ) {

        def addAtom(a: ProtoAtom[ProtoStep[(D, StepEstimate)]]): SequenceSummary[D] = {
          val digestʹ    = a.steps.foldLeft(digest) { (d, s) =>
            val dʹ = d.add(s.observeClass).add(PlannedTime.fromStep(s.observeClass, s.value._2))
            offset.getOption(s.stepConfig).fold(dʹ)(dʹ.add)
          }
          val appendAtom = initialAtoms.lengthIs < AtomLimit
          copy(
            initialAtoms = if (appendAtom) initialAtoms.appended(a) else initialAtoms,
            hasMore      = !appendAtom,
            atomCount    = atomCount + 1,
            digest       = digestʹ
          )
        }
      }

      private object SequenceSummary {
        def zero[D]: SequenceSummary[D] =
          SequenceSummary(
            Vector.empty,
            false,
            0,
            SequenceDigest.Zero
          )

        def summarize[D](
          s: Stream[F, ProtoAtom[ProtoStep[(D, StepEstimate)]]]
        ): F[SequenceSummary[D]] =
          s.fold(zero[D]) { (sum, a) => sum.addAtom(a) }.compile.onlyOrError
      }

      private def toExecutionConfig[S, D](
        namespace: UUID,
        proto:     ProtoExecutionConfig[F, S, ProtoAtom[ProtoStep[(D, StepEstimate)]]],
        setupTime: SetupTime
      ): F[ExecutionConfig[S, D]] = {

        def toExecutionSequence(
          sequence:     SequenceSummary[D],
          sequenceType: SequenceType
        ): Option[ExecutionSequence[D]] =
          NonEmptyList.fromFoldable(
            sequence
              .initialAtoms
              .zipWithIndex
              .map(_.map(SequenceIds.atomId(namespace, sequenceType, _)))
              .map { case (atom, atomId) =>
                val steps = atom.steps.zipWithIndex.map { case (ProtoStep((d, e), sc, oc, bp), j) =>
                  Step(SequenceIds.stepId(namespace, sequenceType, atomId, j), d, sc, e, oc, bp)
                }
                Atom(atomId, atom.description, steps)
              }
        ).map(nel => ExecutionSequence(nel.head, nel.tail, sequence.hasMore, PosInt.unsafeFrom(sequence.atomCount), sequence.digest))

        for {
          acq <- SequenceSummary.summarize(proto.acquisition)
          sci <- SequenceSummary.summarize(proto.science)
        } yield
          ExecutionConfig(
            proto.static,
            toExecutionSequence(acq, SequenceType.Acquisition),
            toExecutionSequence(sci, SequenceType.Science),
            setupTime
          )
      }
    }
}