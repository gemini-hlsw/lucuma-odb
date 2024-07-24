// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.data.EitherT
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.RefinedTypeOps
import eu.timepit.refined.numeric.Interval
import fs2.Stream
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
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
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth as GmosNorthDynamic
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth as GmosSouthDynamic
import lucuma.core.model.sequence.gmos.StaticConfig.GmosNorth as GmosNorthStatic
import lucuma.core.model.sequence.gmos.StaticConfig.GmosSouth as GmosSouthStatic
import lucuma.itc.IntegrationTime
import lucuma.itc.client.ItcClient
import lucuma.odb.data.CalibrationRole
import lucuma.odb.data.Md5Hash
import lucuma.odb.sequence.data.Completion
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.data.IdBase
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoExecutionConfig
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.gmos
import lucuma.odb.sequence.syntax.hash.*
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.sequence.util.SequenceIds
import lucuma.odb.service.ItcService
import lucuma.odb.service.NoTransaction
import lucuma.odb.service.SequenceService
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
import skunk.Transaction

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

  // The digest calculation needs to go through every step in the sequence.
  // The ITC sometimes returns `Int.MaxValue`, which leads to timeouts.  This is
  // a reasonable upper limit on the number of atoms in a sequence.
  val SequenceAtomLimit = 1000

  // This is a user-specifiable limit on how many `possibleFuture` steps should
  // be returned by the sequence generation.  It doesn't limit the overall
  // length of the sequence the way that the SequenceAtomLimit above does.
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
        s"The generated sequence is too long (more than $SequenceAtomLimit atoms)."
    }

    case class NotImplemented(
      instrument:      Instrument,
      calibrationRole: Option[CalibrationRole]
    ) extends Error {
      def format: String =
        s"${instrument.longName} ${calibrationRole.fold("science")(_.tag)} observation sequence generation not supported."
    }

    val sequenceTooLong: Error = SequenceTooLong

    def missingSmartGcalDef(key: String): Error =
      MissingSmartGcalDef(key)

    def notImplemented(instrument: Instrument, calibrationRole: Option[CalibrationRole]): Error =
      NotImplemented(instrument, calibrationRole)
  }

  private type EstimatedAtom[D] = ProtoAtom[ProtoStep[(D, StepEstimate)]]

  def instantiate[F[_]: Concurrent](
    commitHash:   CommitHash,
    itcClient:    ItcClient[F],
    calculator:   TimeEstimateCalculator.ForInstrumentMode,
  )(using Services[F]): Generator[F] =
    new Generator[F] {

      import Error.*

      private val exp = SmartGcalImplementation.fromService(smartGcalService)

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

      type ExpandedAndEstimatedProtoExecutionConfig[F[_], S, D] =
        ProtoExecutionConfig[F, S, Either[String, (EstimatedAtom[D], Long)]]

      type GmosNorth[F[_]] =
        ExpandedAndEstimatedProtoExecutionConfig[F, GmosNorthStatic, GmosNorthDynamic]

      type GmosSouth[F[_]] =
        ExpandedAndEstimatedProtoExecutionConfig[F, GmosSouthStatic, GmosSouthDynamic]

      private def protoExecutionConfig[S, D](
        gen:  lucuma.odb.sequence.Generator[F, S, D],
        ctx:  Context,
        calc: TimeEstimateCalculator[S, D]
      )(
        comp: Transaction[F] ?=> SequenceService[F] => F[Completion.Matcher[D]]
      ): EitherT[F, Error, (ExpandedAndEstimatedProtoExecutionConfig[F, S, D], IdBase.Acq, IdBase.Sci)] =
        EitherT.liftF(services.transactionally { comp(sequenceService) }).map { m =>
          val p = gen.generate(ctx.acquisitionIntegrationTime, ctx.scienceIntegrationTime, m)
          (
            p.mapBothSequences(calc.estimateSequence(p.static)),
            IdBase.Acq(m.acq.idBase),
            IdBase.Sci(m.sci.idBase)
          )
        }

      private def gmosNorthLongSlit(
        ctx:    Context,
        config: gmos.longslit.Config.GmosNorth
      ): EitherT[F, Error, (GmosNorth[F], IdBase.Acq, IdBase.Sci)] =
        val gen = gmos.longslit.LongSlit.gmosNorth(config, exp.gmosNorth, None)
                      .toRight(Error.notImplemented(Instrument.GmosNorth, None))
        for {
          g <- EitherT.fromEither(gen)
          t <- protoExecutionConfig(g, ctx, calculator.gmosNorth)(_.selectGmosNorthCompletionState(ctx.oid))
        } yield t

      private def gmosSouthLongSlit(
        ctx:    Context,
        config: gmos.longslit.Config.GmosSouth
      ): EitherT[F, Error, (GmosSouth[F], IdBase.Acq, IdBase.Sci)] =
        val gen = gmos.longslit.LongSlit.gmosSouth(config, exp.gmosSouth, None)
                      .toRight(Error.notImplemented(Instrument.GmosSouth, None))
        for {
          g <- EitherT.fromEither(gen)
          t <- protoExecutionConfig(g, ctx, calculator.gmosSouth)(_.selectGmosSouthCompletionState(ctx.oid))
        } yield t

      private def calcDigestFromContext(
        ctx: Context
      )(using NoTransaction[F]): EitherT[F, Error, ExecutionDigest] =
        EitherT
          .fromEither(Error.sequenceTooLong.asLeft[ExecutionDigest])
          .unlessA(ctx.scienceIntegrationTime.exposures.value <= SequenceAtomLimit) *>
        (ctx.params.observingMode match {
          case config: gmos.longslit.Config.GmosNorth =>
            gmosNorthLongSlit(ctx, config).flatMap { (p, _, _) => executionDigest(p, calculator.gmosNorth.estimateSetup) }

          case config: gmos.longslit.Config.GmosSouth =>
            gmosSouthLongSlit(ctx, config).flatMap { (p, _, _) => executionDigest(p, calculator.gmosSouth.estimateSetup) }
        })

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
        ctx.params.observingMode match {
          case config: gmos.longslit.Config.GmosNorth =>
            for {
              (p, a, s) <- gmosNorthLongSlit(ctx, config)
              r         <- executionConfig(p, ctx.namespace, a, s, lim)
            } yield InstrumentExecutionConfig.GmosNorth(r)

          case config: gmos.longslit.Config.GmosSouth =>
            for {
              (p, a, s) <- gmosSouthLongSlit(ctx, config)
              r         <- executionConfig(p, ctx.namespace, a, s, lim)
            } yield InstrumentExecutionConfig.GmosSouth(r)
        }

      private val offset = StepConfig.science.andThen(StepConfig.Science.offset)

      private def executionDigest[S, D](
        proto:     ProtoExecutionConfig[F, S, Either[String, (EstimatedAtom[D], Long)]],
        setupTime: SetupTime
      ): EitherT[F, Error, ExecutionDigest] = {

        // Compute the sequence digest from the stream by folding over the steps
        // if possible. Missing smart gcal definitions may prevent it.
        def sequenceDigest(
          s: Stream[F, Either[String, EstimatedAtom[D]]]
        ): F[Either[Error, SequenceDigest]] =
          s.fold(SequenceDigest.Zero.asRight[Error]) { (eDigest, eAtom) =>
            eDigest.flatMap { digest =>
              digest
                .incrementAtomCount
                .filter(_.atomCount.value <= SequenceAtomLimit)
                .toRight(SequenceTooLong)
                .flatMap { incDigest =>
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
          // Compute the SequenceDigests.  We don't need the atom indices for
          // this so we drop them, keeping only the EstimatedAtom.  For the
          // acquisition sequence (which is infinite) we assume only the next
          // step will be executed.
          a <- EitherT(sequenceDigest(proto.acquisition.take(1).map(_.map(_._1))))
          s <- EitherT(sequenceDigest(proto.science.map(_.map(_._1))))
        } yield ExecutionDigest(setupTime, a, s)

      }

      private def executionConfig[S, D](
        proto:       ProtoExecutionConfig[F, S, Either[String, (EstimatedAtom[D], Long)]],
        namespace:   UUID,
        acqBase:     IdBase.Acq,
        sciBase:     IdBase.Sci,
        futureLimit: FutureLimit
      ): EitherT[F, Error, ExecutionConfig[S, D]] = {

        def executionSequence(
          s: Stream[F, Either[String, (EstimatedAtom[D], Long)]],
          t: SequenceType,
          c: Int
        ): F[Either[Error, Option[ExecutionSequence[D]]]] =
          s.map(_.map(_.map(SequenceIds.atomId(namespace, t, c, _)))) // turn the Long into an AtomId
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
          // For acquisitions, take only the first step of the infinite sequence.
          // We always assume we only need one more step.
          a <- EitherT(executionSequence(proto.acquisition.take(1), SequenceType.Acquisition, acqBase.value))
          s <- EitherT(executionSequence(proto.science, SequenceType.Science, sciBase.value))
        } yield ExecutionConfig(proto.static, a, s)
      }
  }
}
