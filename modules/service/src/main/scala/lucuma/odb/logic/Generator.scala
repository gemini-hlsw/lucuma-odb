// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.Eq
import cats.data.EitherT
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.RefinedTypeOps
import eu.timepit.refined.numeric.Interval
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.CalibrationRole
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
import lucuma.core.util.Timestamp
import lucuma.itc.IntegrationTime
import lucuma.itc.client.ItcClient
import lucuma.odb.data.Md5Hash
import lucuma.odb.sequence.ExecutionConfigGenerator
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoExecutionConfig
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.StepRecord
import lucuma.odb.sequence.gmos
import lucuma.odb.sequence.gmos.longslit.LongSlit
import lucuma.odb.sequence.syntax.hash.*
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.sequence.util.SequenceIds
import lucuma.odb.service.ItcService
import lucuma.odb.service.NoTransaction
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.Codecs.core_timestamp
import skunk.*
import skunk.implicits.*
import skunk.codec.temporal.timestamptz

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
    asterismResult: ItcService.AsterismResults,
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

    case class MissingDefinition(format: String) extends Error

    case class MissingSmartGcalDef(key: String) extends Error {
      def format: String =
        s"Could not generate a sequence, missing Smart GCAL mapping: $key"
    }

    case object SequenceTooLong extends Error {
      def format: String =
        s"The generated sequence is too long (more than $SequenceAtomLimit atoms)."
    }

    val sequenceTooLong: Error = SequenceTooLong

  }

  def instantiate[F[_]: Concurrent](
    commitHash:   CommitHash,
    itcClient:    ItcClient[F],
    calculator:   TimeEstimateCalculator.ForInstrumentMode
  )(using Services[F]): Generator[F] =
    new Generator[F] {

      import Error.*

      private val exp = SmartGcalImplementation.fromService(smartGcalService)

      private case class Context(
        pid:    Program.Id,
        oid:    Observation.Id,
        itcRes: ItcService.AsterismResults,
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
            md5.update(ing.exposureCount.hashBytes)
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

          val opc: F[Either[Error, (GeneratorParams, Option[ItcService.AsterismResults])]] =
            services.transactionally {
              (for {
                p <- EitherT(generatorParamsService.selectOne(pid, oid).map(_.leftMap(es => InvalidData(oid, es.map(_.format).intercalate(", ")))))
                c <- EitherT.liftF(itc.selectOne(pid, oid, p))
              } yield (p, c)).value
            }

          def callItc(p: GeneratorParams): EitherT[F, Error, ItcService.AsterismResults] =
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
        pid:             Program.Id,
        oid:             Observation.Id,
        asterismResults: ItcService.AsterismResults,
        params:          GeneratorParams
      )(using NoTransaction[F]): F[Either[Error, ExecutionDigest]] =
        calcDigestThenCache(Context(pid, oid, asterismResults, params)).value

      private def calcDigestThenCache(
        ctx: Context
      )(using NoTransaction[F]): EitherT[F, Error, ExecutionDigest] =
        calcDigestFromContext(ctx).flatTap(ctx.cache)

      private val tz: Codec[Timestamp] =
        timestamptz.eimap(
          odt => Timestamp.fromInstantTruncated(odt.toInstant).toRight(s"Invalid Timestamp: $odt"))(
          ts  => java.time.OffsetDateTime.ofInstant(ts.toInstant, java.time.ZoneOffset.UTC)
        )

      val CurrentTimestamp: Query[Void, Timestamp] =
        sql"select current_timestamp".query(tz)

      type ProtoGmosNorth = ProtoExecutionConfig[GmosNorthStatic, (ProtoAtom[(ProtoStep[GmosNorthDynamic], Int, StepEstimate)], Int)]
      type ProtoGmosSouth = ProtoExecutionConfig[GmosSouthStatic, (ProtoAtom[(ProtoStep[GmosSouthDynamic], Int, StepEstimate)], Int)]

      private def protoExecutionConfig[S, D](
        gen:   ExecutionConfigGenerator[S, D],
        calc:  TimeEstimateCalculator[S, D],
        steps: Stream[F, StepRecord[D]]
      )(using Eq[D]): EitherT[F, Error, ProtoExecutionConfig[S, (ProtoAtom[(ProtoStep[D], Int, StepEstimate)], Int)]] =
        EitherT.liftF(services.transactionally {
          for {
            t <- session.unique(CurrentTimestamp)
            p <- gen.executionConfig(steps, t)
          } yield p
        }).map(p => p.pipeBothSequences(calc.estimateSequence(p.static)))

      private def gmosNorthLongSlit(
        ctx:    Context,
        config: lucuma.odb.sequence.gmos.longslit.Config.GmosNorth,
        role:   Option[CalibrationRole]
      ): EitherT[F, Error, ProtoGmosNorth] =
        val gen = LongSlit.gmosNorth(exp.gmosNorth, config, ctx.acquisitionIntegrationTime, ctx.scienceIntegrationTime, role)
        val srs = services.gmosSequenceService.selectGmosNorthStepRecords(ctx.oid)
        for {
          g <- EitherT(gen).leftMap(m => Error.MissingDefinition(m))
          p <- protoExecutionConfig(g, calculator.gmosNorth, srs)
        } yield p

      private def gmosSouthLongSlit(
        ctx:    Context,
        config: lucuma.odb.sequence.gmos.longslit.Config.GmosSouth,
        role:   Option[CalibrationRole]
      ): EitherT[F, Error, ProtoGmosSouth] =
        val gen = LongSlit.gmosSouth(exp.gmosSouth, config, ctx.acquisitionIntegrationTime, ctx.scienceIntegrationTime, role)
        val srs = services.gmosSequenceService.selectGmosSouthStepRecords(ctx.oid)
        for {
          g <- EitherT(gen).leftMap(m => Error.MissingDefinition(m))
          p <- protoExecutionConfig(g, calculator.gmosSouth, srs)
        } yield p

      private def calcDigestFromContext(
        ctx: Context
      )(using NoTransaction[F]): EitherT[F, Error, ExecutionDigest] =
        EitherT
          .fromEither(Error.sequenceTooLong.asLeft[ExecutionDigest])
          .unlessA(ctx.scienceIntegrationTime.exposureCount.value <= SequenceAtomLimit) *>
        (ctx.params match {
          case GeneratorParams(_, config: gmos.longslit.Config.GmosNorth, role) =>
            gmosNorthLongSlit(ctx, config, role).flatMap { p =>
              EitherT.fromEither[F](executionDigest(p, calculator.gmosNorth.estimateSetup))
            }

          case GeneratorParams(_, config: gmos.longslit.Config.GmosSouth, role) =>
            gmosSouthLongSlit(ctx, config, role).flatMap { p =>
              EitherT.fromEither[F](executionDigest(p, calculator.gmosSouth.estimateSetup))
            }
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
        ctx.params match {
          case GeneratorParams(_, config: gmos.longslit.Config.GmosNorth, role) =>
            gmosNorthLongSlit(ctx, config, role).map { p =>
              InstrumentExecutionConfig.GmosNorth(
                executionConfig(p, ctx.namespace, lim)
              )
            }

          case GeneratorParams(_, config: gmos.longslit.Config.GmosSouth, role) =>
            gmosSouthLongSlit(ctx, config, role).map { p =>
              InstrumentExecutionConfig.GmosSouth(
                executionConfig(p, ctx.namespace, lim)
              )
            }
        }

      private val offset = StepConfig.science.andThen(StepConfig.Science.offset)

      private def executionDigest[S, D](
        proto:     ProtoExecutionConfig[S, (ProtoAtom[(ProtoStep[D], Int, StepEstimate)], Int)],
        setupTime: SetupTime
      ): Either[Error, ExecutionDigest] = {

        // Compute the sequence digest from the stream by folding over the steps
        // if possible. Missing smart gcal definitions may prevent it.
        def sequenceDigest(
          s: Stream[Pure, (ProtoAtom[(ProtoStep[D], Int, StepEstimate)], Int)]
        ): Either[Error, SequenceDigest] =
          s.fold(SequenceDigest.Zero.asRight[Error]) { case (eDigest, (atom, _)) =>
            eDigest.flatMap { digest =>
              digest
                .incrementAtomCount
                .filter(_.atomCount.value <= SequenceAtomLimit)
                .toRight(SequenceTooLong)
                .map { incDigest =>
                  atom.steps.foldLeft(incDigest) { case (d, (s, _, est)) =>
                    val dʹ = d.add(s.observeClass).add(CategorizedTime.fromStep(s.observeClass, est))
                    offset.getOption(s.stepConfig).fold(dʹ)(dʹ.add)
                  }
                }
            }
          }.toList.head

        for {
          // Compute the SequenceDigests.
          a <- sequenceDigest(proto.acquisition)
          s <- sequenceDigest(proto.science)
        } yield ExecutionDigest(setupTime, a, s)

      }

      private def executionConfig[S, D](
        proto:       ProtoExecutionConfig[S, (ProtoAtom[(ProtoStep[D], Int, StepEstimate)], Int)],
        namespace:   UUID,
        futureLimit: FutureLimit
      ): ExecutionConfig[S, D] =
        def executionSequence(
          s: Stream[Pure, (ProtoAtom[(ProtoStep[D], Int, StepEstimate)], Int)],
          t: SequenceType
        ): Option[ExecutionSequence[D]] =
          val atoms: List[(Atom[D], Boolean)] = s.map { case (atom, aix) =>
            val atomId = SequenceIds.atomId(namespace, t, aix)
            val steps  = atom.steps.map { case (ProtoStep(d, sc, oc, bp), six, est) =>
              Step(SequenceIds.stepId(namespace, t, atomId, six), d, sc, est, oc, bp)
            }
            Atom(atomId, atom.description, steps)
          }.zipWithNext
           .map(_.map(_.isDefined))
           .take(1 + futureLimit.value) // 1 (nextAtom) + futureLimit (possibleFuture)
           .toList

          atoms.headOption.map { case (head, _) =>
            ExecutionSequence(head, atoms.tail.map(_._1), atoms.last._2)
          }

        ExecutionConfig(
          proto.static,
          executionSequence(proto.acquisition, SequenceType.Acquisition),
          executionSequence(proto.science,     SequenceType.Science)
        )

  }
}
