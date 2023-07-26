// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.data.EitherT
import cats.effect.Concurrent
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.RefinedTypeOps
import eu.timepit.refined.numeric.Interval
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.SequenceType
import lucuma.core.math.Offset
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.ExecutionConfig
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.model.sequence.ExecutionSequence
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.PlannedTime
import lucuma.core.model.sequence.SequenceDigest
import lucuma.core.model.sequence.SetupTime
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepEstimate
import lucuma.itc.client.ItcClient
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoExecutionConfig
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.gmos
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.sequence.util.SequenceIds
import lucuma.odb.service.ItcService
import lucuma.odb.service.NoTransaction
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*

import java.security.MessageDigest
import java.util.HexFormat
import java.util.UUID

import Generator.FutureLimit

sealed trait Generator[F[_]] {

  def digest(
    programId:     Program.Id,
    observationId: Observation.Id
  )(using NoTransaction[F]): F[Either[Generator.Error, ExecutionDigest]]

  def generate(
    programId:     Program.Id,
    observationId: Observation.Id,
    futureLimit:   FutureLimit = FutureLimit.Default
  )(using NoTransaction[F]): F[Either[Generator.Error, InstrumentExecutionConfig]]

}

object Generator {

  type FutureLimit = Int Refined Interval.Closed[0, 100]

  object FutureLimit extends RefinedTypeOps[FutureLimit, Int] {
    val Default: FutureLimit = unsafeFrom( 25)
    val Min: FutureLimit     = unsafeFrom(  0)
    val Max: FutureLimit     = unsafeFrom(100)

    val Binding: lucuma.odb.graphql.binding.Matcher[FutureLimit] =
      lucuma.odb.graphql.binding.IntBinding.emap { v =>
        from(v).leftMap { _ =>
          s"Future limit must range from ${Min.value} to ${Max.value}, but was $v."
        }
      }
  }

  sealed trait Error {
    def format: String
  }

  object Error {

    case class ItcError(error: ItcService.Error) extends Error {
      def format: String =
        error.format
    }

    case class InvalidData(message: String) extends Error {
      def format: String =
        s"Could not generate a sequence from the observation: $message"
    }

    case class MissingSmartGcalDef(key: String) extends Error {
      def format: String =
        s"Could not generate a sequence, missing Smart GCAL mapping: $key"
    }

    case object SequenceTooLong extends Error {
      def format: String =
        s"The generated sequence is too long (more than ${Int.MaxValue} atoms)"
    }

    def missingSmartGcalDef(key: String): Error =
      MissingSmartGcalDef(key)
  }


  def instantiate[F[_]: Concurrent](
    commitHash:   CommitHash,
    itcClient:    ItcClient[F],
    calculator:   PlannedTimeCalculator.ForInstrumentMode,
  )(using Services[F]): Generator[F] =
    new Generator[F] {

      import Error.*

      private val exp = SmartGcalExpander.fromService(smartGcalService)

      private case class Context(
        pid:    Program.Id,
        oid:    Observation.Id,
        itcRes: ItcService.AsterismResult,
        params: GeneratorParams
      ) {

        def namespace: UUID =
           SequenceIds.namespace(commitHash, oid, params)

        val integrationTime: ItcService.TargetResult =
          itcRes.value.focus

        val hash: String = {
          val zero = 0.toByte
          val md5  = MessageDigest.getInstance("MD5")

          // Observing Mode
          md5.update(params.observingMode.hashBytes)

          // Integration Time
          val ing = itcRes.value.focus.value
          md5.update(BigInt(ing.exposureTime.toMicroseconds).toByteArray.reverse.padTo(8, zero))
          md5.update(BigInt(ing.exposures.value).toByteArray.reverse.padTo(4, zero))

          // Commit Hash
          md5.update(commitHash.toByteArray)

          HexFormat.of.formatHex(md5.digest())
        }

        def checkCache(using NoTransaction[F]): EitherT[F, Error, Option[ExecutionDigest]] =
          EitherT.right(services.transactionally {
            executionDigestService.select(pid, oid, hash)
          })

        def cache(digest: ExecutionDigest)(using NoTransaction[F]): EitherT[F, Error, Unit] =
          EitherT.right(services.transactionally {
            executionDigestService.insertOrUpdate(pid, oid, hash, digest)
          })
      }

      private object Context {

        def lookup(
          pid: Program.Id,
          oid: Observation.Id
        )(using NoTransaction[F]): EitherT[F, Error, Context] =
          EitherT(
            itcService(itcClient)
              .lookup(pid, oid)
              .map(_.bimap(
                error   => ItcError(error),
                success => Context(pid, oid, success.result, success.params)
              ))
          )

      }

      override def digest(
        pid: Program.Id,
        oid: Observation.Id
      )(using NoTransaction[F]): F[Either[Error, ExecutionDigest]] =
        (for {
          c <- Context.lookup(pid, oid)
          o <- c.checkCache
          d <- o.fold(calcExecutionDigest(c).flatTap(c.cache))(d => EitherT.pure(d))
        } yield d).value

      override def generate(
        pid: Program.Id,
        oid: Observation.Id,
        lim: FutureLimit = FutureLimit.Default
      )(using NoTransaction[F]): F[Either[Error, InstrumentExecutionConfig]] =
        (for {
          c <- Context.lookup(pid, oid)
          x <- calcExecutionConfig(c, lim)
        } yield x).value

      private def calcExecutionDigest(
        ctx: Context
      )(using NoTransaction[F]): EitherT[F, Error, ExecutionDigest] =
        ctx.params match {
          case GeneratorParams.GmosNorthLongSlit(_, config) =>
            for {
              p <- gmosLongSlit(ctx.integrationTime, config, gmos.longslit.Generator.GmosNorth)
              r <- executionDigest(expandAndEstimate(p, exp.gmosNorth, calculator.gmosNorth), calculator.gmosNorth.estimateSetup)
            } yield r

          case GeneratorParams.GmosSouthLongSlit(_, config) =>
            for {
              p <- gmosLongSlit(ctx.integrationTime, config, gmos.longslit.Generator.GmosSouth)
              r <- executionDigest(expandAndEstimate(p, exp.gmosSouth, calculator.gmosSouth), calculator.gmosSouth.estimateSetup)
            } yield r
        }

      private def calcExecutionConfig(
        ctx: Context,
        lim: FutureLimit
      )(using NoTransaction[F]): EitherT[F, Error, InstrumentExecutionConfig] =
        ctx.params match {
          case GeneratorParams.GmosNorthLongSlit(_, config) =>
            for {
              p <- gmosLongSlit(ctx.integrationTime, config, gmos.longslit.Generator.GmosNorth)
              r <- executionConfig(expandAndEstimate(p, exp.gmosNorth, calculator.gmosNorth), ctx.namespace, lim)
            } yield InstrumentExecutionConfig.GmosNorth(r)

          case GeneratorParams.GmosSouthLongSlit(_, config) =>
            for {
              p <- gmosLongSlit(ctx.integrationTime, config, gmos.longslit.Generator.GmosSouth)
              r <- executionConfig(expandAndEstimate(p, exp.gmosSouth, calculator.gmosSouth), ctx.namespace, lim)
            } yield InstrumentExecutionConfig.GmosSouth(r)
        }


      // Generates the initial GMOS LongSlit sequences, without smart-gcal expansion
      // or planned time calculation.
      private def gmosLongSlit[S, D, G, L, U](
        itcResult: ItcService.TargetResult,
        config:    gmos.longslit.Config[G, L, U],
        generator: gmos.longslit.Generator[S, D, G, L, U]
      ): EitherT[F, Error, ProtoExecutionConfig[Pure, S, ProtoAtom[ProtoStep[D]]]] =
        EitherT.fromEither[F](
          generator.generate(itcResult.value, config) match {
            case Left(msg)    => InvalidData(msg).asLeft
            case Right(proto) => proto.mapSequences(_.take(1), _.take(itcResult.value.exposures.value)).asRight[Error]
          }
        )

      // Performs smart-gcal expansion and planned time calculation.
      private def expandAndEstimate[S, K, D](
        proto:    ProtoExecutionConfig[Pure, S, ProtoAtom[ProtoStep[D]]],
        expander: SmartGcalExpander[F, K, D],
        calc:     PlannedTimeCalculator[S, D]
      ): ProtoExecutionConfig[F, S, Either[String, ProtoAtom[ProtoStep[(D, StepEstimate)]]]] =
        proto.mapBothSequences[F, Either[String, ProtoAtom[ProtoStep[(D, StepEstimate)]]]](
          _.through(expander.expand)
           .through(calc.estimateSequence[F](proto.static))
        )


      private val offset = StepConfig.science.andThen(StepConfig.Science.offset)

      private def executionDigest[S, D](
        proto:     ProtoExecutionConfig[F, S, Either[String, ProtoAtom[ProtoStep[(D, StepEstimate)]]]],
        setupTime: SetupTime
      ): EitherT[F, Error, ExecutionDigest] = {

        // Compute the sequence digest from the stream by folding over the steps
        // if possible. Missing smart gcal definitions may prevent it.
        def sequenceDigest(
          s: Stream[F, Either[String, ProtoAtom[ProtoStep[(D, StepEstimate)]]]]
        ): F[Either[Error, SequenceDigest]] =
          s.fold(SequenceDigest.Zero.asRight[Error]) { (eDigest, eAtom) =>
            eDigest.flatMap { digest =>
              digest.incrementAtomCount.toRight(SequenceTooLong).flatMap { incDigest =>
                eAtom.bimap(
                  missingSmartGcalDef,
                  _.steps.foldLeft(incDigest) { (d, s) =>
                    val dʹ = d.add(s.observeClass).add(PlannedTime.fromStep(s.observeClass, s.value._2))
                    offset.getOption(s.stepConfig).fold(dʹ)(dʹ.add)
                  }
                )
              }
            }
          }.compile.onlyOrError

        for {
          a <- EitherT(sequenceDigest(proto.acquisition))
          s <- EitherT(sequenceDigest(proto.science))
        } yield ExecutionDigest(setupTime, a, s)

      }

      private def executionConfig[S, D](
        proto:       ProtoExecutionConfig[F, S, Either[String, ProtoAtom[ProtoStep[(D, StepEstimate)]]]],
        namespace:   UUID,
        futureLimit: FutureLimit
      ): EitherT[F, Error, ExecutionConfig[S, D]] = {

        def executionSequence(
          s: Stream[F, Either[String, ProtoAtom[ProtoStep[(D, StepEstimate)]]]],
          t: SequenceType
        ): F[Either[Error, Option[ExecutionSequence[D]]]] =
          s.zipWithIndex
           .map(_.map(SequenceIds.atomId(namespace, t, _)))
           .map { case (eAtom, atomId) =>
             eAtom.bimap(
               missingSmartGcalDef,
               atom =>
                 val steps = atom.steps.zipWithIndex.map { case (ProtoStep((d, e), sc, oc, bp), j) =>
                   Step(SequenceIds.stepId(namespace, t, atomId, j), d, sc, e, oc, bp)
                 }
                 Atom(atomId, atom.description, steps)
             )
           }
           .zipWithNext
           .map { case (e, n) => e.tupleRight(n.isDefined) } // Either[Error, (atom, has more)]
           .take(1 + futureLimit.value) // 1 (nextAtom) + futureLimit (possibleFuture)
           .compile
           .toList
           .map(_.sequence.map { atoms =>
              atoms.headOption.map { case (head, _) =>
                val future  = atoms.tail.map(_._1)
                val hasMore = atoms.last._2
                ExecutionSequence(head, future, hasMore)
              }
           })

        for {
          a <- EitherT(executionSequence(proto.acquisition, SequenceType.Acquisition))
          s <- EitherT(executionSequence(proto.science, SequenceType.Science))
        } yield ExecutionConfig(proto.static, a, s)
      }
    }
}
