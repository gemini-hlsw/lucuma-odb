// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.Eq
import cats.data.EitherT
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.RefinedTypeOps
import eu.timepit.refined.numeric.Interval
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ExecutionState
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
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth as GmosNorthDynamic
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth as GmosSouthDynamic
import lucuma.core.model.sequence.gmos.StaticConfig.GmosNorth as GmosNorthStatic
import lucuma.core.model.sequence.gmos.StaticConfig.GmosSouth as GmosSouthStatic
import lucuma.core.util.Timestamp
import lucuma.itc.IntegrationTime
import lucuma.itc.client.ItcClient
import lucuma.odb.data.Md5Hash
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.ExecutionConfigGenerator
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.data.ProtoExecutionConfig
import lucuma.odb.sequence.data.StepRecord
import lucuma.odb.sequence.flamingos2
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

import java.security.MessageDigest
import java.util.UUID

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
  )(using NoTransaction[F]): F[Either[OdbError, ExecutionDigest]]

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
  )(using NoTransaction[F]): F[Either[OdbError, (ExecutionDigest, GeneratorParams, Md5Hash)]]

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
    asterismResult: Either[OdbError, ItcService.AsterismResults],
    params:         GeneratorParams,
    when:           Option[Timestamp] = None
  )(using NoTransaction[F]): F[Either[OdbError, ExecutionDigest]]

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
  )(using NoTransaction[F]): F[Either[OdbError, InstrumentExecutionConfig]]

  /**
   * Determines the execution state of the given observation by looking at the
   * remaining science steps (if any) and past visits.
   */
  def executionState(
    programId:     Program.Id,
    observationId: Observation.Id
  )(using NoTransaction[F]): F[ExecutionState]

  /**
   * Equivalent to executionState above, but uses provided asterism results and
   * generator params rather than computing them.
   */
  def executionState(
    pid:    Program.Id,
    oid:    Observation.Id,
    itcRes: ItcService.AsterismResults,
    params: GeneratorParams,
  ): F[ExecutionState]

  /** Equivalent to executionState above, but computes many results. */
  def executionStates(
    input: Map[Observation.Id, (Program.Id, ItcService.AsterismResults, GeneratorParams)]
  )(using NoTransaction[F]): F[Map[Observation.Id, ExecutionState]]

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

  object Error:
    def sequenceUnavailable(oid: Observation.Id, message: String): OdbError =
      OdbError.SequenceUnavailable(oid, s"Could not generate a sequence for $oid: $message".some)

    def sequenceTooLong(oid: Observation.Id): OdbError =
      sequenceUnavailable(oid, s"The generated sequence is too long (more than $SequenceAtomLimit atoms).")

  def instantiate[F[_]: Concurrent](
    commitHash:   CommitHash,
    itcClient:    ItcClient[F],
    calculator:   TimeEstimateCalculatorImplementation.ForInstrumentMode
  )(using Services[F]): Generator[F] =
    new Generator[F] {

      private val exp = SmartGcalImplementation.fromService(smartGcalService)

      private case class Context(
        pid:    Program.Id,
        oid:    Observation.Id,
        itcRes: Either[OdbError, ItcService.AsterismResults],
        params: GeneratorParams
      ) {

        def namespace: UUID =
          SequenceIds.namespace(commitHash, oid, params)

        val acquisitionIntegrationTime: Either[OdbError, IntegrationTime] =
          itcRes.map(_.acquisitionResult.focus.value)

        val scienceIntegrationTime: Either[OdbError, IntegrationTime] =
          itcRes.map(_.scienceResult.focus.value)

        val hash: Md5Hash = {
          val md5 = MessageDigest.getInstance("MD5")

          // Generator Params
          md5.update(params.hashBytes)

          // Integration Time
          List(acquisitionIntegrationTime, scienceIntegrationTime).foreach { ving =>
            ving.fold(_ => Array.emptyByteArray, ing => md5.update(ing.exposureTime.hashBytes))
            ving.fold(_ => Array.emptyByteArray, ing => md5.update(ing.exposureCount.hashBytes))
          }

          // Commit Hash
          md5.update(commitHash.hashBytes)

          Md5Hash.unsafeFromByteArray(md5.digest())
        }

        @annotation.nowarn("msg=unused implicit parameter")
        def checkCache(using NoTransaction[F]): EitherT[F, OdbError, Option[ExecutionDigest]] =
          EitherT.right(services.transactionally {
            executionDigestService.selectOne(oid, hash)
          })

        @annotation.nowarn("msg=unused implicit parameter")
        def cache(digest: ExecutionDigest)(using NoTransaction[F]): EitherT[F, OdbError, Unit] =
          EitherT.right(services.transactionally {
            executionDigestService.insertOrUpdate(pid, oid, hash, digest)
          })
      }

      private object Context {

        @annotation.nowarn("msg=unused implicit parameter")
        def lookup(
          pid: Program.Id,
          oid: Observation.Id
        )(using NoTransaction[F]): EitherT[F, OdbError, Context] = {
          val itc = itcService(itcClient)

          val opc: F[Either[OdbError, (GeneratorParams, Option[ItcService.AsterismResults])]] =
            services.transactionally:
              (for
                p <- EitherT(generatorParamsService.selectOne(pid, oid).map(_.leftMap(e => Error.sequenceUnavailable(oid, e.format))))
                c <- EitherT.liftF(itc.selectOne(pid, oid, p))
              yield (p, c)).value

          def callItc(p: GeneratorParams): EitherT[F, OdbError, ItcService.AsterismResults] =
            EitherT(itc.callRemote(pid, oid, p))

          for {
            pc <- EitherT(opc)
            (params, cached) = pc
            // This will be confusing, but the idea here is that if the observation
            // definition is missing target information we just record that in the
            // Context.  On the other hand if there is an error calling the ITC then
            // we cannot create the Context.
            as <- params.itcInput.fold(
              m => EitherT.pure(Error.sequenceUnavailable(oid, s"Missing parameters: ${m.format}").asLeft),
              _ => cached.fold(callItc(params))(EitherT.pure(_)).map(_.asRight)
            )
          } yield Context(pid, oid, as, params)

        }


        def checkCache(contexts: List[Context])(using Transaction[F]): F[Map[Observation.Id, ExecutionDigest]] =
          executionDigestService.selectMany(contexts.map(c => (c.oid, c.hash)))

      }

      override def digest(
        pid:  Program.Id,
        oid:  Observation.Id,
        when: Option[Timestamp] = None
      )(using NoTransaction[F]): F[Either[OdbError, ExecutionDigest]] =
        digestWithParamsAndHash(pid, oid, when).map(_.map(_._1))

      @annotation.nowarn("msg=unused implicit parameter")
      private def digestWithParamsAndHash(
        context: Context,
        when: Option[Timestamp]
      )(using NoTransaction[F]): EitherT[F, OdbError, (ExecutionDigest, GeneratorParams, Md5Hash)] =
        context
          .checkCache
          .flatMap:
            case Some(digest) => digest.pure
            case None => calcDigestThenCache(context, when)
          .map: digest =>
            (digest, context.params, context.hash)

      def digestWithParamsAndHash(
        pid:  Program.Id,
        oid:  Observation.Id,
        when: Option[Timestamp] = None
      )(using NoTransaction[F]): F[Either[OdbError, (ExecutionDigest, GeneratorParams, Md5Hash)]] =
        Context.lookup(pid, oid).flatMap(digestWithParamsAndHash(_, when)).value

      override def calculateDigest(
        pid:             Program.Id,
        oid:             Observation.Id,
        asterismResults: Either[OdbError, ItcService.AsterismResults],
        params:          GeneratorParams,
        when:            Option[Timestamp] = None
      )(using NoTransaction[F]): F[Either[OdbError, ExecutionDigest]] =
        calcDigestThenCache(Context(pid, oid, asterismResults, params), when).value

      @annotation.nowarn("msg=unused implicit parameter")
      private def calcDigestThenCache(
        ctx:  Context,
        when: Option[Timestamp]
      )(using NoTransaction[F]): EitherT[F, OdbError, ExecutionDigest] =
        calcDigestFromContext(ctx, when).flatTap(ctx.cache)

      type ProtoFlamingos2 = ProtoExecutionConfig[Flamingos2StaticConfig,  Atom[Flamingos2DynamicConfig]]
      type ProtoGmosNorth  = ProtoExecutionConfig[GmosNorthStatic, Atom[GmosNorthDynamic]]
      type ProtoGmosSouth  = ProtoExecutionConfig[GmosSouthStatic, Atom[GmosSouthDynamic]]

      private def protoExecutionConfig[S, D](
        ctx:      Context,
        gen:      ExecutionConfigGenerator[S, D],
        steps:    Stream[F, StepRecord[D]],
        when:     Option[Timestamp]
      )(using Eq[D]): EitherT[F, OdbError, (ProtoExecutionConfig[S, Atom[D]], ExecutionState)] =
        if ctx.params.declaredComplete then
          EitherT.pure:
            (ProtoExecutionConfig(gen.static, Stream.empty, Stream.empty), ExecutionState.DeclaredComplete)
        else
          val visits = services.visitService.selectAll(ctx.oid)
          val events = services.executionEventService.selectSequenceEvents(ctx.oid)
          EitherT.liftF:
            services.transactionally:
              for
                t <- when.fold(timeService.transactionTime)(_.pure[F])
                p <- gen.executionConfig(visits, events, steps, t)
              yield p

      private def flamingos2LongSlit(
        ctx:    Context,
        config: lucuma.odb.sequence.flamingos2.longslit.Config,
        when:   Option[Timestamp]
      ): EitherT[F, OdbError, (ProtoFlamingos2, ExecutionState)] =
        import lucuma.odb.sequence.flamingos2.longslit.LongSlit
        val gen = LongSlit.instantiate(ctx.oid, calculator.flamingos2, ctx.namespace, exp.flamingos2, config, ctx.acquisitionIntegrationTime, ctx.scienceIntegrationTime, ctx.params.acqResetTime)
        val srs = services.flamingos2SequenceService.selectStepRecords(ctx.oid)
        for
          g <- EitherT(gen)
          p <- protoExecutionConfig(ctx, g, srs, when)
        yield p

      private def gmosNorthLongSlit(
        ctx:    Context,
        config: lucuma.odb.sequence.gmos.longslit.Config.GmosNorth,
        role:   Option[CalibrationRole],
        when:   Option[Timestamp]
      ): EitherT[F, OdbError, (ProtoGmosNorth, ExecutionState)] =
        val gen = LongSlit.gmosNorth(ctx.oid, calculator.gmosNorth, ctx.namespace, exp.gmosNorth, config, ctx.acquisitionIntegrationTime, ctx.scienceIntegrationTime, role, ctx.params.acqResetTime)
        val srs = services.gmosSequenceService.selectGmosNorthStepRecords(ctx.oid)
        for
          g <- EitherT(gen)
          p <- protoExecutionConfig(ctx, g, srs, when)
        yield p

      private def gmosSouthLongSlit(
        ctx:    Context,
        config: lucuma.odb.sequence.gmos.longslit.Config.GmosSouth,
        role:   Option[CalibrationRole],
        when:   Option[Timestamp]
      ): EitherT[F, OdbError, (ProtoGmosSouth, ExecutionState)] =
        val gen = LongSlit.gmosSouth(ctx.oid, calculator.gmosSouth, ctx.namespace, exp.gmosSouth, config, ctx.acquisitionIntegrationTime, ctx.scienceIntegrationTime, role, ctx.params.acqResetTime)
        val srs = services.gmosSequenceService.selectGmosSouthStepRecords(ctx.oid)
        for
          g <- EitherT(gen)
          p <- protoExecutionConfig(ctx, g, srs, when)
        yield p

      @annotation.nowarn("msg=unused implicit parameter")
      private def calcDigestFromContext(
        ctx:  Context,
        when: Option[Timestamp]
      )(using NoTransaction[F]): EitherT[F, OdbError, ExecutionDigest] =
        EitherT
          .fromEither(Error.sequenceTooLong(ctx.oid).asLeft[ExecutionDigest])
          .unlessA(ctx.scienceIntegrationTime.toOption.forall(_.exposureCount.value <= SequenceAtomLimit)) *>
        (ctx.params match
          case GeneratorParams(_, _, config: flamingos2.longslit.Config, role, declaredComplete, _) =>
            flamingos2LongSlit(ctx, config, when).flatMap: (p, e) =>
              EitherT.fromEither[F](executionDigest(ctx.oid, p, e, calculator.flamingos2.estimateSetup))

          case GeneratorParams(_, _, config: gmos.longslit.Config.GmosNorth, role, declaredComplete, _) =>
            gmosNorthLongSlit(ctx, config, role, when).flatMap: (p, e) =>
              EitherT.fromEither[F](executionDigest(ctx.oid, p, e, calculator.gmosNorth.estimateSetup))

          case GeneratorParams(_, _, config: gmos.longslit.Config.GmosSouth, role, declaredComplete, _) =>
            gmosSouthLongSlit(ctx, config, role, when).flatMap: (p, e) =>
              EitherT.fromEither[F](executionDigest(ctx.oid, p, e, calculator.gmosSouth.estimateSetup))

          case GeneratorParams(_, _, config: gmos.imaging.Config.GmosNorth, _, _, _) =>
            EitherT.leftT[F, ExecutionDigest](OdbError.SequenceUnavailable(ctx.oid, "GMOS North imaging sequence generation is not yet implemented".some))

          case GeneratorParams(_, _, config: gmos.imaging.Config.GmosSouth, _, _, _) =>
            EitherT.leftT[F, ExecutionDigest](OdbError.SequenceUnavailable(ctx.oid, "GMOS South imaging sequence generation is not yet implemented".some))
        )

      override def generate(
        pid:  Program.Id,
        oid:  Observation.Id,
        lim:  FutureLimit = FutureLimit.Default,
        when: Option[Timestamp] = None
      )(using NoTransaction[F]): F[Either[OdbError, InstrumentExecutionConfig]] =
        (for {
          c <- Context.lookup(pid, oid)
          x <- calcExecutionConfigFromContext(c, lim, when)
        } yield x).value

      @annotation.nowarn("msg=unused implicit parameter")
      private def calcExecutionConfigFromContext(
        ctx:      Context,
        lim:      FutureLimit,
        when:     Option[Timestamp]
      )(using NoTransaction[F]): EitherT[F, OdbError, InstrumentExecutionConfig] =
        ctx.params match
          case GeneratorParams(_, _, config: flamingos2.longslit.Config, role, _, _) =>
            flamingos2LongSlit(ctx, config, when).map: (p, _) =>
              InstrumentExecutionConfig.Flamingos2(executionConfig(p, lim))

          case GeneratorParams(_, _, config: gmos.longslit.Config.GmosNorth, role, _, _) =>
            gmosNorthLongSlit(ctx, config, role, when).map: (p, _) =>
              InstrumentExecutionConfig.GmosNorth(executionConfig(p, lim))

          case GeneratorParams(_, _, config: gmos.longslit.Config.GmosSouth, role, _, _) =>
            gmosSouthLongSlit(ctx, config, role, when).map: (p, _) =>
              InstrumentExecutionConfig.GmosSouth(executionConfig(p, lim))

          case GeneratorParams(_, _, config: gmos.imaging.Config.GmosNorth, _, _, _) =>
            EitherT.leftT[F, InstrumentExecutionConfig](OdbError.SequenceUnavailable(ctx.oid, "GMOS North imaging execution config generation is not yet implemented".some))

          case GeneratorParams(_, _, config: gmos.imaging.Config.GmosSouth, _, _, _) =>
            EitherT.leftT[F, InstrumentExecutionConfig](OdbError.SequenceUnavailable(ctx.oid, "GMOS South imaging execution config generation is not yet implemented".some))

      private def executionDigest[S, D](
        oid:       Observation.Id,
        proto:     ProtoExecutionConfig[S, Atom[D]],
        execState: ExecutionState,
        setupTime: SetupTime
      ): Either[OdbError, ExecutionDigest] =

        if execState === ExecutionState.DeclaredComplete then
          ExecutionDigest(
            SetupTime.Zero,
            SequenceDigest.Zero.copy(executionState = execState),
            SequenceDigest.Zero.copy(executionState = execState)
          ).asRight
        else
          // Compute the sequence digest from the stream by folding over the steps.
          def sequenceDigest(s: Stream[Pure, Atom[D]]): Either[OdbError, SequenceDigest] =
            s.fold(SequenceDigest.Zero.copy(executionState = ExecutionState.Completed).asRight[OdbError]) { case (eDigest, atom) =>
              eDigest.flatMap: digest =>
                digest
                  .incrementAtomCount
                  .filter(_.atomCount.value <= SequenceAtomLimit)
                  .toRight(Error.sequenceTooLong(oid))
                  .map: incDigest =>
                    atom.steps.foldLeft(incDigest) { case (d, s) =>
                      d.add(s.observeClass)
                       .add(CategorizedTime.fromStep(s.observeClass, s.estimate))
                       .add(s.telescopeConfig.offset)
                       .copy(executionState = execState)
                    }
            }.toList.head

          // Compute the SequenceDigests.
          for
            a <- sequenceDigest(proto.acquisition)
            s <- sequenceDigest(proto.science)
          yield ExecutionDigest(setupTime, a, s)
        end if

      private def executionConfig[S, D](
        proto:       ProtoExecutionConfig[S, Atom[D]],
        futureLimit: FutureLimit
      ): ExecutionConfig[S, D] =
        def executionSequence(s: Stream[Pure, Atom[D]]): Option[ExecutionSequence[D]] =
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
          executionSequence(proto.acquisition),
          executionSequence(proto.science)
        )

      def executionState(
        programId:     Program.Id,
        observationId: Observation.Id
      )(using NoTransaction[F]): F[ExecutionState] =
        digest(programId, observationId)
          .map(_.fold(
            _ => ExecutionState.NotDefined,
            _.science.executionState
          ))

      override def executionState(
        pid:    Program.Id,
        oid:    Observation.Id,
        itcRes: ItcService.AsterismResults,
        params: GeneratorParams,
      ): F[ExecutionState] =
        digestWithParamsAndHash(Context(pid, oid, Right(itcRes), params), None)
          .map(_._1)
          .value
          .map:
            case Left(_)  => ExecutionState.NotDefined
            case Right(d) => d.science.executionState

      override def executionStates(
        input: Map[Observation.Id, (Program.Id, ItcService.AsterismResults, GeneratorParams)]
      )(using NoTransaction[F]): F[Map[Observation.Id, ExecutionState]] =
        services
          .transactionally:
            Context.checkCache:
              input
                .toList
                .map:
                  case (oid, (pid, itc, gps)) =>
                    Context(pid, oid, Right(itc), gps)
          .flatMap: cached =>
            input
              .view
              .filterKeys(a => !cached.contains(a))     // remove cached subset
              .toList
              .traverse:
                case (oid, (pid, itc, gps)) =>
                  calculateDigest(pid, oid, Right(itc), gps, None).map(oid -> _)  // compute one by one
              .map: list =>
                list
                  .collect:
                    case (oid, Right(d)) => (oid, d)    // filter out failures
                  .toMap ++ cached                      // combine with cached results
              .map: all =>
                all
                  .view
                  .mapValues(_.science.executionState)  // narrow to just the execution state
                  .toMap

  }
}
