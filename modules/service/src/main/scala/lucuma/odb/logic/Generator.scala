// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.data.EitherT
import cats.effect.Concurrent
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.RefinedTypeOps
import eu.timepit.refined.numeric.Interval
import fs2.Pipe
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.SequenceType
import lucuma.core.math.Offset
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.ExecutionConfig
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.model.sequence.ExecutionSequence
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.SequenceDigest
import lucuma.core.model.sequence.SetupTime
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepEstimate
import lucuma.itc.IntegrationTime
import lucuma.itc.client.ItcClient
import lucuma.odb.data.Md5Hash
import lucuma.odb.sequence.data.CompletedAtomMap
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoExecutionConfig
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.gmos
import lucuma.odb.sequence.syntax.hash.*
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.sequence.util.SequenceIds
import lucuma.odb.service.ItcService
import lucuma.odb.service.NoTransaction
import lucuma.odb.service.SequenceService.CompletionState
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*

import java.security.MessageDigest
import java.util.UUID

import Generator.Error
import Generator.FutureLimit

sealed trait Generator[F[_]] {

  /**
   * Looks up the parameters required to calculate the ExecutionDigest, and checks
   * the cache. If not in the cache, it performs the calculation and caches the 
   * results. If the observation is not completely defined (e.g., if missing the 
   * observing mode), an Error is produced.
   */
  def digest(
    programId:     Program.Id,
    observationId: Observation.Id
  )(using NoTransaction[F]): F[Either[Error, ExecutionDigest]]

  /**
   * The same is `digest()`, but it also returns the GeneratorParms and the hash used
   * to determine if the digest needed to be recalculated. This is useful in things
   * like the guide star availability calculations which depend on the digest and are
   * also cached.
   */
  def digestWithParamsAndHash(
    programId:     Program.Id,
    observationId: Observation.Id
  )(using NoTransaction[F]): F[Either[Error, (ExecutionDigest, GeneratorParams, Md5Hash)]]

  /**
   * Calculates the ExecutionDigest given the AsterismResults from the ITC
   * along with the GeneratorParams. This method always performs the calculation
   * and does not attempt to use cached results nor call the ITC.  It will
   * cache the calculation once performed.
   */
  def calculateDigest(
    programId:      Program.Id,
    observationId:  Observation.Id,
    asterismResult: ItcService.AsterismResult,
    params:         GeneratorParams
  )(using NoTransaction[F]): F[Either[Error, ExecutionDigest]]

  /**
   * Generates the execution config if the observation is found and defined
   * well enough to perform the calculation.
   */
  def generate(
    programId:     Program.Id,
    observationId: Observation.Id,
    futureLimit:   FutureLimit = FutureLimit.Default
  )(using NoTransaction[F]): F[Either[Error, InstrumentExecutionConfig]]

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

    case class InvalidData(
      observationId: Observation.Id,
      message:       String
    ) extends Error {
      def format: String =
        s"Could not generate a sequence from the observation $observationId: $message"
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

  private type SimpleAtom[D]    = ProtoAtom[ProtoStep[D]]
  private type EstimatedAtom[D] = ProtoAtom[ProtoStep[(D, StepEstimate)]]

  def instantiate[F[_]: Concurrent](
    commitHash:   CommitHash,
    itcClient:    ItcClient[F],
    calculator:   TimeEstimateCalculator.ForInstrumentMode,
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

        val acquisitionIntegrationTime: IntegrationTime =
          itcRes.acquisitionResult.focus.value

        val scienceIntegrationTime: IntegrationTime =
          itcRes.scienceResult.focus.value

        val hash: Md5Hash = {
          val md5 = MessageDigest.getInstance("MD5")

          // Observing Mode
          md5.update(params.observingMode.hashBytes)

          // Integration Time
          List(acquisitionIntegrationTime, scienceIntegrationTime).foreach { ing =>
            md5.update(ing.exposureTime.hashBytes)
            md5.update(ing.exposures.hashBytes)
          }

          // Commit Hash
          md5.update(commitHash.hashBytes)

          Md5Hash.unsafeFromByteArray(md5.digest())
        }

        def checkCache(using NoTransaction[F]): EitherT[F, Error, Option[ExecutionDigest]] =
          EitherT.right(services.transactionally {
            executionDigestService.selectOne(pid, oid, hash)
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
        )(using NoTransaction[F]): EitherT[F, Error, Context] = {
          val itc = itcService(itcClient)

          val opc: F[Either[Error, (GeneratorParams, Option[ItcService.AsterismResult])]] =
            services.transactionally {
              (for {
                p <- EitherT(generatorParamsService.selectOne(pid, oid).map(_.leftMap(es => InvalidData(oid, es.map(_.format).intercalate(", ")))))
                c <- EitherT.liftF(itc.selectOne(pid, oid, p))
              } yield (p, c)).value
            }

          def callItc(p: GeneratorParams): EitherT[F, Error, ItcService.AsterismResult] =
            EitherT(itc.callRemote(pid, oid, p)).leftMap(ItcError(_): Error)

          for {
            pc <- EitherT(opc)
            (params, cached) = pc
            as <- cached.fold(callItc(params))(EitherT.pure(_))
          } yield Context(pid, oid, as, params)

        }
      }

      override def digest(
        programId:     Program.Id,
        observationId: Observation.Id
      )(using NoTransaction[F]): F[Either[Error, ExecutionDigest]] =
        digestWithParamsAndHash(programId, observationId).map(_.map(_._1))

      override def digestWithParamsAndHash(
        pid: Program.Id,
        oid: Observation.Id
      )(using NoTransaction[F]): F[Either[Error, (ExecutionDigest, GeneratorParams, Md5Hash)]] =
        (for {
          c <- Context.lookup(pid, oid)
          d <- c.checkCache.flatMap(_.fold(calcDigestThenCache(c))(EitherT.pure(_)))
          r  = (d, c.params, c.hash)
        } yield r).value

      override def calculateDigest(
        pid:            Program.Id,
        oid:            Observation.Id,
        asterismResult: ItcService.AsterismResult,
        params:         GeneratorParams
      )(using NoTransaction[F]): F[Either[Error, ExecutionDigest]] =
        calcDigestThenCache(Context(pid, oid, asterismResult, params)).value

      private def calcDigestThenCache(
        ctx: Context
      )(using NoTransaction[F]): EitherT[F, Error, ExecutionDigest] =
        calcDigestFromContext(ctx).flatTap(ctx.cache)

      private def calcDigestFromContext(
        ctx: Context
      )(using NoTransaction[F]): EitherT[F, Error, ExecutionDigest] =
        ctx.params match {
          case GeneratorParams.GmosNorthLongSlit(_, config) =>
            for {
              p <- gmosLongSlit(ctx.oid, ctx.acquisitionIntegrationTime, ctx.scienceIntegrationTime, config, gmos.longslit.Generator.GmosNorth)
              m <- EitherT.liftF(services.transactionally { services.sequenceService.selectGmosNorthCompletionState(ctx.oid) })
              r <- executionDigest(expandAndEstimate(p, exp.gmosNorth, calculator.gmosNorth, m), calculator.gmosNorth.estimateSetup)
            } yield r

          case GeneratorParams.GmosSouthLongSlit(_, config) =>
            for {
              p <- gmosLongSlit(ctx.oid, ctx.acquisitionIntegrationTime, ctx.scienceIntegrationTime, config, gmos.longslit.Generator.GmosSouth)
              m <- EitherT.liftF(services.transactionally { services.sequenceService.selectGmosSouthCompletionState(ctx.oid) })
              r <- executionDigest(expandAndEstimate(p, exp.gmosSouth, calculator.gmosSouth, m), calculator.gmosSouth.estimateSetup)
            } yield r
        }

      override def generate(
        pid: Program.Id,
        oid: Observation.Id,
        lim: FutureLimit = FutureLimit.Default
      )(using NoTransaction[F]): F[Either[Error, InstrumentExecutionConfig]] =
        (for {
          c <- Context.lookup(pid, oid)
          x <- calcExecutionConfigFromContext(c, lim)
        } yield x).value

      private def calcExecutionConfigFromContext(
        ctx: Context,
        lim: FutureLimit
      )(using NoTransaction[F]): EitherT[F, Error, InstrumentExecutionConfig] =
        ctx.params match {
          case GeneratorParams.GmosNorthLongSlit(_, config) =>
            for {
              p <- gmosLongSlit(ctx.oid, ctx.acquisitionIntegrationTime, ctx.scienceIntegrationTime, config, gmos.longslit.Generator.GmosNorth)
              m <- EitherT.liftF(services.transactionally { services.sequenceService.selectGmosNorthCompletionState(ctx.oid) })
              r <- executionConfig(expandAndEstimate(p, exp.gmosNorth, calculator.gmosNorth, m), ctx.namespace, lim)
            } yield InstrumentExecutionConfig.GmosNorth(r)

          case GeneratorParams.GmosSouthLongSlit(_, config) =>
            for {
              p <- gmosLongSlit(ctx.oid, ctx.acquisitionIntegrationTime, ctx.scienceIntegrationTime, config, gmos.longslit.Generator.GmosSouth)
              m <- EitherT.liftF(services.transactionally { services.sequenceService.selectGmosSouthCompletionState(ctx.oid) })
              r <- executionConfig(expandAndEstimate(p, exp.gmosSouth, calculator.gmosSouth, m), ctx.namespace, lim)
            } yield InstrumentExecutionConfig.GmosSouth(r)
        }


      // Generates the initial GMOS LongSlit sequences, without smart-gcal expansion
      // or time estimate calculation.
      private def gmosLongSlit[S, D, G, L, U](
        oid:            Observation.Id,
        acquisitionItc: IntegrationTime,
        scienceItc:     IntegrationTime,
        config:         gmos.longslit.Config[G, L, U],
        generator:      gmos.longslit.Generator[S, D, G, L, U]
      ): EitherT[F, Error, ProtoExecutionConfig[Pure, S, SimpleAtom[D]]] =
        EitherT.fromEither[F](
          generator.generate(acquisitionItc, scienceItc, config) match {
            case Left(msg)    => InvalidData(oid, msg).asLeft
            case Right(proto) => proto.mapSequences(_.take(1), _.take(scienceItc.exposures.value)).asRight[Error]
          }
        )

      // Performs smart-gcal expansion and time estimate calculation.
      private def expandAndEstimate[S, K, D](
        proto:    ProtoExecutionConfig[Pure, S, SimpleAtom[D]],
        expander: SmartGcalExpander[F, K, D],
        calc:     TimeEstimateCalculator[S, D],
        comState: CompletionState[D]
      ): ProtoExecutionConfig[F, S, Either[String, (EstimatedAtom[D], Long)]] = {

        // Given a SequenceType produces a Pipe from a SimpleAtom to a smart-gcal
        // expanded, estimated, indexed atom with executed steps filtered out.
        def pipe(
          sequenceType: SequenceType,
          compMap:      CompletedAtomMap[D]
        ): Pipe[F, SimpleAtom[D], Either[String, (EstimatedAtom[D], Long)]] =
            // Do smart-gcal expansion
          _.through(expander.expand)

            // Number the atoms, because executed atoms will be filtered out but
            // we need the correct index to always calculate the same Atom.Id.
            .zipWithIndex.map { case (e, index) => e.tupleRight(index) }

            // Strip executed atoms
            .mapAccumulate(compMap) { case (state, eAtom) =>
              eAtom.fold(
                error => (state, error.asLeft),
                { case (atom, index) =>
                  // Add executed flag and return updated map.
                  val (stateʹ, executed) = state.matchAtom(atom)
                  (stateʹ, (atom, index, executed).asRight)
                }
              )
            }
            .collect {
              case (_, Left(error))               => Left(error)
              case (_, Right(atom, index, false)) => Right((atom, index)) // keep only un-executed atoms
            }

            // Add step estimates
           .through(calc.estimateSequence[F](proto.static))

        proto.mapSequences(pipe(SequenceType.Acquisition, comState.completedAcq), pipe(SequenceType.Science, comState.completedSci))
      }


      private val offset = StepConfig.science.andThen(StepConfig.Science.offset)

      private def executionDigest[S, D](
        proto:     ProtoExecutionConfig[F, S, Either[String, (EstimatedAtom[D], Long)]],
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
                    val dʹ = d.add(s.observeClass).add(CategorizedTime.fromStep(s.observeClass, s.value._2))
                    offset.getOption(s.stepConfig).fold(dʹ)(dʹ.add)
                  }
                )
              }
            }
          }.compile.onlyOrError

        for {
          a <- EitherT(sequenceDigest(proto.acquisition.map(_.map(_._1)))) // strip the atom index and compute seq digest
          s <- EitherT(sequenceDigest(proto.science.map(_.map(_._1))))
        } yield ExecutionDigest(setupTime, a, s)

      }

      private def executionConfig[S, D](
        proto:       ProtoExecutionConfig[F, S, Either[String, (EstimatedAtom[D], Long)]],
        namespace:   UUID,
        futureLimit: FutureLimit
      ): EitherT[F, Error, ExecutionConfig[S, D]] = {

        def executionSequence(
          s: Stream[F, Either[String, (EstimatedAtom[D], Long)]],
          t: SequenceType
        ): F[Either[Error, Option[ExecutionSequence[D]]]] =
          s.map(_.map(_.map(SequenceIds.atomId(namespace, t, _)))) // turn the Long into an AtomId
           .map {
             _.bimap(
               missingSmartGcalDef,
               atomI =>
                 val (atom, atomId) = atomI
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
