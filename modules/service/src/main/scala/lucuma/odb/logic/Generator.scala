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
import lucuma.core.model.sequence.StepConfig
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
import lucuma.odb.sequence.data.ItcInput
import lucuma.odb.sequence.data.ProtoExecutionConfig
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
import skunk.*
import skunk.codec.temporal.timestamptz
import skunk.implicits.*

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
   *
   * Because the sequences that are generated may differ depending on when
   * requested, a 'when' parameter option is provided.  By default this will be
   * "now".
   */
  def digest(
    programId:     Program.Id,
    observationId: Observation.Id,
    when:          Option[Timestamp] = None
  )(using NoTransaction[F]): F[Either[Error, ExecutionDigest]]

  /**
   * The same is `digest()`, but it also returns the GeneratorParms and the hash used
   * to determine if the digest needed to be recalculated. This is useful in things
   * like the guide star availability calculations which depend on the digest and are
   * also cached.
   *
   * Because the sequences that are generated may differ depending on when
   * requested, a 'when' parameter option is provided.  By default this will be
   * "now".
   */
  def digestWithParamsAndHash(
    programId:     Program.Id,
    observationId: Observation.Id,
    when:          Option[Timestamp] = None
  )(using NoTransaction[F]): F[Either[Error, (ExecutionDigest, GeneratorParams, Md5Hash)]]

  /**
   * Calculates the ExecutionDigest given the AsterismResults from the ITC
   * along with the GeneratorParams. This method always performs the calculation
   * and does not attempt to use cached results nor call the ITC.  It will
   * cache the calculation once performed.
   *
   * Because the sequences that are generated may differ depending on when
   * requested, a 'when' parameter option is provided.  By default this will be
   * "now".
   */
  def calculateDigest(
    programId:      Program.Id,
    observationId:  Observation.Id,
    asterismResult: Either[ItcInput.Missing, ItcService.AsterismResults],
    params:         GeneratorParams,
    when:           Option[Timestamp] = None
  )(using NoTransaction[F]): F[Either[Error, ExecutionDigest]]

  /**
   * Generates the execution config if the observation is found and defined
   * well enough to perform the calculation.  Because the sequences that are
   * generated may differ depending on when requested, a 'when' parameter option
   * is provided.  By default this will be "now".
   *
   * @param futureLimit cap to place on the number of atoms that map appear in
   *                    the possibleFuture
   * @param when perform the generation as if requested at this time (by default
   *             the sequence is requested generated at the current time)
   */
  def generate(
    programId:     Program.Id,
    observationId: Observation.Id,
    futureLimit:   FutureLimit = FutureLimit.Default,
    when:          Option[Timestamp] = None
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

    case class MissingDefinition(msg: String) extends Error {
      def format: String =
        s"Could not generate a sequence: $msg"
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
    calculator:   TimeEstimateCalculatorImplementation.ForInstrumentMode
  )(using Services[F]): Generator[F] =
    new Generator[F] {

      import Error.*

      private val exp = SmartGcalImplementation.fromService(smartGcalService)

      private case class Context(
        pid:    Program.Id,
        oid:    Observation.Id,
        itcRes: Either[ItcInput.Missing, ItcService.AsterismResults],
        params: GeneratorParams
      ) {

        def namespace: UUID =
          SequenceIds.namespace(commitHash, oid, params)

        val acquisitionIntegrationTime: Either[ItcInput.Missing, IntegrationTime] =
          itcRes.map(_.acquisitionResult.focus.value)

        val scienceIntegrationTime: Either[ItcInput.Missing, IntegrationTime] =
          itcRes.map(_.scienceResult.focus.value)

        val hash: Md5Hash = {
          val md5 = MessageDigest.getInstance("MD5")

          // Observing Mode
          md5.update(params.observingMode.hashBytes)

          // Integration Time
          List(acquisitionIntegrationTime, scienceIntegrationTime).foreach { ving =>
            ving.fold(_ => Array.emptyByteArray, ing => md5.update(ing.exposureTime.hashBytes))
            ving.fold(_ => Array.emptyByteArray, ing => md5.update(ing.exposureCount.hashBytes))
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
            EitherT(itc.callRemote(pid, oid, p)).leftMap {
              case e@ItcService.Error.ObservationDefinitionError(_) => MissingDefinition(e.format)
              case e@ItcService.Error.RemoteServiceErrors(_)        => ItcError(e)
              case e@ItcService.Error.TargetMismatch                => ItcError(e)
            }

          for {
            pc <- EitherT(opc)
            (params, cached) = pc
            // This will be confusing, but the idea here is that if the observation
            // definition is missing target information we just record that in the
            // Context.  On the other hand if there is an error calling the ITC then
            // we cannot create the Context.
            // EitherT[F, Error, Either[ItcInput.Missing, ItcService.AsterismResults]]
            as <- params.itcInput.fold(
              m => EitherT.pure(Either.left[ItcInput.Missing, ItcService.AsterismResults](m)),
              _ => cached.fold(callItc(params))(EitherT.pure(_)).map(Either.right[ItcInput.Missing, ItcService.AsterismResults](_))
            )
          } yield Context(pid, oid, as, params)

        }
      }

      override def digest(
        pid:  Program.Id,
        oid:  Observation.Id,
        when: Option[Timestamp] = None
      )(using NoTransaction[F]): F[Either[Error, ExecutionDigest]] =
        digestWithParamsAndHash(pid, oid, when).map(_.map(_._1))

      override def digestWithParamsAndHash(
        pid:  Program.Id,
        oid:  Observation.Id,
        when: Option[Timestamp] = None
      )(using NoTransaction[F]): F[Either[Error, (ExecutionDigest, GeneratorParams, Md5Hash)]] =
        (for {
          c <- Context.lookup(pid, oid)
          d <- c.checkCache.flatMap(_.fold(calcDigestThenCache(c, when))(EitherT.pure(_)))
          r  = (d, c.params, c.hash)
        } yield r).value

      override def calculateDigest(
        pid:             Program.Id,
        oid:             Observation.Id,
        asterismResults: Either[ItcInput.Missing, ItcService.AsterismResults],
        params:          GeneratorParams,
        when:            Option[Timestamp] = None
      )(using NoTransaction[F]): F[Either[Error, ExecutionDigest]] =
        calcDigestThenCache(Context(pid, oid, asterismResults, params), when).value

      private def calcDigestThenCache(
        ctx:  Context,
        when: Option[Timestamp]
      )(using NoTransaction[F]): EitherT[F, Error, ExecutionDigest] =
        calcDigestFromContext(ctx, when).flatTap(ctx.cache)

      private val tz: Codec[Timestamp] =
        timestamptz.eimap(
          odt => Timestamp.fromInstantTruncated(odt.toInstant).toRight(s"Invalid Timestamp: $odt"))(
          ts  => java.time.OffsetDateTime.ofInstant(ts.toInstant, java.time.ZoneOffset.UTC)
        )

      val CurrentTimestamp: Query[Void, Timestamp] =
        sql"select current_timestamp".query(tz)

      type ProtoGmosNorth = ProtoExecutionConfig[GmosNorthStatic, Atom[GmosNorthDynamic]]
      type ProtoGmosSouth = ProtoExecutionConfig[GmosSouthStatic, Atom[GmosSouthDynamic]]

      private def protoExecutionConfig[S, D](
        oid:   Observation.Id,
        gen:   ExecutionConfigGenerator[S, D],
        steps: Stream[F, StepRecord[D]],
        when:  Option[Timestamp]
      )(using Eq[D]): EitherT[F, Error, ProtoExecutionConfig[S, Atom[D]]] =
        val visits = services.visitService.selectAll(oid)
        EitherT.liftF(services.transactionally {
          for {
            t <- when.fold(session.unique(CurrentTimestamp))(_.pure[F])
            p <- gen.executionConfig(visits, steps, t)
          } yield p
        })

      private def gmosNorthLongSlit(
        ctx:    Context,
        config: lucuma.odb.sequence.gmos.longslit.Config.GmosNorth,
        role:   Option[CalibrationRole],
        when:   Option[Timestamp]
      ): EitherT[F, Error, ProtoGmosNorth] =
        val gen = LongSlit.gmosNorth(calculator.gmosNorth, ctx.namespace, exp.gmosNorth, config, ctx.acquisitionIntegrationTime, ctx.scienceIntegrationTime, role)
        val srs = services.gmosSequenceService.selectGmosNorthStepRecords(ctx.oid)
        for {
          g <- EitherT(gen).leftMap(m => Error.MissingDefinition(m))
          p <- protoExecutionConfig(ctx.oid, g, srs, when)
        } yield p

      private def gmosSouthLongSlit(
        ctx:    Context,
        config: lucuma.odb.sequence.gmos.longslit.Config.GmosSouth,
        role:   Option[CalibrationRole],
        when:   Option[Timestamp]
      ): EitherT[F, Error, ProtoGmosSouth] =
        val gen = LongSlit.gmosSouth(calculator.gmosSouth, ctx.namespace, exp.gmosSouth, config, ctx.acquisitionIntegrationTime, ctx.scienceIntegrationTime, role)
        val srs = services.gmosSequenceService.selectGmosSouthStepRecords(ctx.oid)
        for {
          g <- EitherT(gen).leftMap(m => Error.MissingDefinition(m))
          p <- protoExecutionConfig(ctx.oid, g, srs, when)
        } yield p

      private def calcDigestFromContext(
        ctx:  Context,
        when: Option[Timestamp]
      )(using NoTransaction[F]): EitherT[F, Error, ExecutionDigest] =
        EitherT
          .fromEither(Error.sequenceTooLong.asLeft[ExecutionDigest])
          .unlessA(ctx.scienceIntegrationTime.toOption.forall(_.exposureCount.value <= SequenceAtomLimit)) *>
        (ctx.params match {
          case GeneratorParams(_, config: gmos.longslit.Config.GmosNorth, role) =>
            gmosNorthLongSlit(ctx, config, role, when).flatMap { p =>
              EitherT.fromEither[F](executionDigest(p, calculator.gmosNorth.estimateSetup))
            }

          case GeneratorParams(_, config: gmos.longslit.Config.GmosSouth, role) =>
            gmosSouthLongSlit(ctx, config, role, when).flatMap { p =>
              EitherT.fromEither[F](executionDigest(p, calculator.gmosSouth.estimateSetup))
            }
        })

      override def generate(
        pid: Program.Id,
        oid: Observation.Id,
        lim: FutureLimit = FutureLimit.Default,
        when: Option[Timestamp] = None
      )(using NoTransaction[F]): F[Either[Error, InstrumentExecutionConfig]] =
        (for {
          c <- Context.lookup(pid, oid)
          x <- calcExecutionConfigFromContext(c, lim, when)
        } yield x).value

      private def calcExecutionConfigFromContext(
        ctx: Context,
        lim: FutureLimit,
        when: Option[Timestamp]
      )(using NoTransaction[F]): EitherT[F, Error, InstrumentExecutionConfig] =
        ctx.params match {
          case GeneratorParams(_, config: gmos.longslit.Config.GmosNorth, role) =>
            gmosNorthLongSlit(ctx, config, role, when).map { p =>
              InstrumentExecutionConfig.GmosNorth(executionConfig(p, lim))
            }

          case GeneratorParams(_, config: gmos.longslit.Config.GmosSouth, role) =>
            gmosSouthLongSlit(ctx, config, role, when).map { p =>
              InstrumentExecutionConfig.GmosSouth(executionConfig(p, lim))
            }
        }

      private val offset = StepConfig.science.andThen(StepConfig.Science.offset)

      private def executionDigest[S, D](
        proto:     ProtoExecutionConfig[S, Atom[D]],
        setupTime: SetupTime
      ): Either[Error, ExecutionDigest] = {

        // Compute the sequence digest from the stream by folding over the steps.
        def sequenceDigest(s: Stream[Pure, Atom[D]]): Either[Error, SequenceDigest] =
          s.fold(SequenceDigest.Zero.asRight[Error]) { case (eDigest, atom) =>
            eDigest.flatMap { digest =>
              digest
                .incrementAtomCount
                .filter(_.atomCount.value <= SequenceAtomLimit)
                .toRight(SequenceTooLong)
                .map { incDigest =>
                  atom.steps.foldLeft(incDigest) { case (d, s) =>
                    val dʹ = d.add(s.observeClass).add(CategorizedTime.fromStep(s.observeClass, s.estimate))
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
        proto:       ProtoExecutionConfig[S, Atom[D]],
        futureLimit: FutureLimit
      ): ExecutionConfig[S, D] =
        def executionSequence(s: Stream[Pure, Atom[D]], t: SequenceType): Option[ExecutionSequence[D]] =
          val atoms: List[(Atom[D], Boolean)] =
            s.zipWithNext
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
