// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.data.EitherT
import cats.effect.Async
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.RefinedTypeOps
import eu.timepit.refined.numeric.Interval
import fs2.Stream
import lucuma.core.enums.ExecutionState
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.AtomDigest
import lucuma.core.model.sequence.ExecutionConfig
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.model.sequence.ExecutionSequence
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.SequenceDigest
import lucuma.core.model.sequence.SetupTime
import lucuma.odb.data.Itc
import lucuma.odb.data.Md5Hash
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.ObservingMode.Syntax.*
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.data.StreamingExecutionConfig
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.NoTransaction
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
import skunk.*

import Generator.FutureLimit

sealed trait Generator[F[_]]:

  /**
   * Looks up the parameters required to calculate the ExecutionDigest, and checks
   * the cache. If not in the cache, it performs the calculation and caches the
   * results. If the observation is not completely defined (e.g., if missing the
   * observing mode), an Error is produced.
   */
  def digest(
    observationId: Observation.Id
  )(using NoTransaction[F]): F[Either[OdbError, ExecutionDigest]]

  /**
   * The same is `digest`, but it also returns the GeneratorParms and the hash used
   * to determine if the digest needed to be recalculated. This is useful in things
   * like the guide star availability calculations which depend on the digest and are
   * also cached.
   */
  def digestWithParamsAndHash(
    observationId: Observation.Id
  )(using NoTransaction[F]): F[Either[OdbError, (ExecutionDigest, GeneratorParams, Md5Hash)]]

  /**
   * Calculates the ExecutionDigest and AtomDigests (for the obscalc service).
   * This method always performs the calculation and does not attempt to use
   * cached results nor call the ITC.  It will cache the calculation once
   * performed.
   */
  def obscalc(
    observationId: Observation.Id,
    itcResult:     Either[OdbError, Itc]
  )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, (ExecutionDigest, Stream[F, AtomDigest])]]

  /**
   * Generates the execution config if the observation is found and defined
   * well enough to perform the calculation.
   *
   * @param futureLimit cap to place on the number of atoms that map appear in
   *                    the possibleFuture
   */
  def generate(
    observationId: Observation.Id,
    futureLimit:   FutureLimit = FutureLimit.Default
  )(using NoTransaction[F]): F[Either[OdbError, InstrumentExecutionConfig]]

  def resetAcquisition(
    observationId: Observation.Id
  )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, Unit]]

  def materialize(
    observationId: Observation.Id
  )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, Unit]]

  def materializeAndThen[A](
    oid:  Observation.Id
  )(
    f: Transaction[F] ?=> F[Either[OdbError, A]]
  )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, A]]

object Generator:

  // The digest calculation needs to go through every step in the sequence.
  // The ITC sometimes returns `Int.MaxValue`, which leads to timeouts.  This is
  // a reasonable upper limit on the number of atoms in a sequence.
  val SequenceAtomLimit = 1000

  // This is a user-specifiable limit on how many `possibleFuture` steps should
  // be returned by the sequence generation.  It doesn't limit the overall
  // length of the sequence the way that the SequenceAtomLimit above does.
  type FutureLimit = Int Refined Interval.Closed[0, 100]

  object FutureLimit extends RefinedTypeOps[FutureLimit, Int]:
    val Default: FutureLimit = unsafeFrom( 25)
    val Min: FutureLimit     = unsafeFrom(  0)
    val Max: FutureLimit     = unsafeFrom(100)

    val Binding: lucuma.odb.graphql.binding.Matcher[FutureLimit] =
      lucuma.odb.graphql.binding.IntBinding.emap: v =>
        from(v).leftMap: _ =>
          s"Future limit must range from ${Min.value} to ${Max.value}, but was $v."

  def instantiate[F[_]: Async: Services](
    commitHash: CommitHash,
    calculator: TimeEstimateCalculatorImplementation.ForInstrumentMode
  ): Generator[F] =
    new Generator[F]:
      val streaming = GeneratorStreaming.instantiate(commitHash, calculator)

      object ExecutionDigestCache:
        def lookupOne(ctx: GeneratorContext)(using Transaction[F]): EitherT[F, OdbError, Option[ExecutionDigest]] =
          EitherT.right(executionDigestService.selectOne(ctx.oid, ctx.hash))

        def lookupMany(contexts: List[GeneratorContext])(using Transaction[F]): F[Map[Observation.Id, ExecutionDigest]] =
          executionDigestService.selectMany(contexts.map(c => (c.oid, c.hash)))

        def store(ctx: GeneratorContext, digest: ExecutionDigest)(using Transaction[F]): EitherT[F, OdbError, Unit] =
          EitherT.right(executionDigestService.insertOrUpdate(ctx.oid, ctx.hash, digest))

      private def transactionallyEitherT[A](
        fa: (Transaction[F], Services[F]) ?=> EitherT[F, OdbError, A]
      )(using NoTransaction[F]): EitherT[F, OdbError, A] =
        EitherT(services.transactionally { val x = fa; x.value})

      private def transactionallyWithContext[A](
        oid:        Observation.Id,
        commitHash: CommitHash,
        itcResult:  Option[Either[OdbError, Itc]] = None
      )(
        f: GeneratorContext => Transaction[F] ?=> EitherT[F, OdbError, A]
      )(using NoTransaction[F], Services[F]): F[Either[OdbError, A]] =
        EitherT(GeneratorContext.lookup(oid, commitHash, itcResult))
          .flatMap(ctx => transactionallyEitherT(f(ctx)))
          .value

      override def digest(
        oid: Observation.Id
      )(using NoTransaction[F]): F[Either[OdbError, ExecutionDigest]] =
        digestWithParamsAndHash(oid).map(_.map(_._1))

      override def digestWithParamsAndHash(
        oid: Observation.Id
      )(using NoTransaction[F]): F[Either[OdbError, (ExecutionDigest, GeneratorParams, Md5Hash)]] =
        transactionallyWithContext(oid, commitHash): ctx =>
          for
            d0  <- ExecutionDigestCache.lookupOne(ctx)
            d1  <- d0.fold(calcDigestThenCache(ctx))(d => EitherT.pure(d))
          yield (d1, ctx.params, ctx.hash)

      private def calcDigestThenCache(
        ctx: GeneratorContext
      )(using Transaction[F]): EitherT[F, OdbError, ExecutionDigest] =
        for
          d <- calcDigestFromContext(ctx)
          _ <- ExecutionDigestCache.store(ctx, d)
        yield d

      private def calcDigestFromContext(
        ctx: GeneratorContext
      )(using Transaction[F]): EitherT[F, OdbError, ExecutionDigest] =

        def digest[S, D](
          stream: StreamingExecutionConfig[F, S, D],
          setup:  SetupTime
        ): EitherT[F, OdbError, ExecutionDigest] =

          def sequenceDigest(s: Stream[F, Atom[D]]): F[Either[OdbError, SequenceDigest]] =
            s.fold(SequenceDigest.Zero.copy(executionState = ExecutionState.Completed).asRight[OdbError]) { case (eDigest, atom) =>
              eDigest.flatMap: digest =>
                Either.cond(
                  digest.atomCount.value < SequenceAtomLimit,
                  digest.add(atom).copy(executionState = ctx.params.executionState),
                  GeneratorError.sequenceTooLong(ctx.oid)
                )
            }.compile.onlyOrError

          // Compute the SequenceDigests.
          for
            a <- EitherT(sequenceDigest(stream.acquisition))
            s <- EitherT(sequenceDigest(stream.science))
          yield ExecutionDigest(setup, a, s)

        if ctx.params.declaredComplete then
          EitherT.pure:
            ExecutionDigest(
              SetupTime.Zero,
              SequenceDigest.Zero.copy(executionState = ExecutionState.DeclaredComplete),
              SequenceDigest.Zero.copy(executionState = ExecutionState.DeclaredComplete)
            )
        else
          ctx.params.observingMode.modeType match
            case ObservingModeType.Flamingos2LongSlit =>
              EitherT(streaming.selectOrGenerateFlamingos2LongSlit(ctx)).flatMap(digest(_, calculator.flamingos2.estimateSetup))
            case ObservingModeType.GmosNorthImaging   =>
              EitherT(streaming.selectOrGenerateGmosNorthImaging(ctx)).flatMap(digest(_, calculator.gmosNorth.estimateSetup))
            case ObservingModeType.GmosNorthLongSlit  =>
              EitherT(streaming.selectOrGenerateGmosNorthLongSlit(ctx)).flatMap(digest(_, calculator.gmosNorth.estimateSetup))
            case ObservingModeType.GmosSouthImaging   =>
              EitherT(streaming.selectOrGenerateGmosSouthImaging(ctx)).flatMap(digest(_, calculator.gmosSouth.estimateSetup))
            case ObservingModeType.GmosSouthLongSlit  =>
              EitherT(streaming.selectOrGenerateGmosSouthLongSlit(ctx)).flatMap(digest(_, calculator.gmosSouth.estimateSetup))
            case ObservingModeType.Igrins2LongSlit    =>
              EitherT.leftT(OdbError.InvalidObservation(ctx.oid, s"IGRINS2 is not yet supported".some))

      private def calculateScienceAtomDigests(
        ctx: GeneratorContext
      )(using Transaction[F]): EitherT[F, OdbError, Stream[F, AtomDigest]] =

        val checkSequence =
          EitherT
            .fromEither(GeneratorError.sequenceTooLong(ctx.oid).asLeft[ExecutionDigest])
            .unlessA(ctx.itcRes.toOption.forall(_.scienceExposureCount.value <= SequenceAtomLimit))

        val atomDigests = ctx.params.observingMode.modeType match
          case ObservingModeType.Flamingos2LongSlit =>
            EitherT(streaming.selectOrGenerateFlamingos2LongSlit(ctx)).map(_.science.map(AtomDigest.fromAtom))
          case ObservingModeType.GmosNorthImaging   =>
            EitherT(streaming.selectOrGenerateGmosNorthImaging(ctx)).map(_.science.map(AtomDigest.fromAtom))
          case ObservingModeType.GmosNorthLongSlit  =>
            EitherT(streaming.selectOrGenerateGmosNorthLongSlit(ctx)).map(_.science.map(AtomDigest.fromAtom))
          case ObservingModeType.GmosSouthImaging   =>
            EitherT(streaming.selectOrGenerateGmosSouthImaging(ctx)).map(_.science.map(AtomDigest.fromAtom))
          case ObservingModeType.GmosSouthLongSlit  =>
            EitherT(streaming.selectOrGenerateGmosSouthLongSlit(ctx)).map(_.science.map(AtomDigest.fromAtom))
          case ObservingModeType.Igrins2LongSlit    =>
            EitherT.leftT(OdbError.InvalidObservation(ctx.oid, s"IGRINS2 is not yet supported".some))

        checkSequence *> atomDigests

      override def obscalc(
        observationId: Observation.Id,
        itcResult:     Either[OdbError, Itc]
      )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, (ExecutionDigest, Stream[F, AtomDigest])]] =
        transactionallyWithContext(observationId, commitHash, itcResult.some): ctx =>
          for
            d <- calcDigestThenCache(ctx)
            a <- calculateScienceAtomDigests(ctx)
          yield (d, a)

      override def generate(
        oid:  Observation.Id,
        lim:  FutureLimit = FutureLimit.Default
      )(using NoTransaction[F]): F[Either[OdbError, InstrumentExecutionConfig]] =

        def executionConfig[S, D](
          stream: StreamingExecutionConfig[F, S, D],
        ): F[ExecutionConfig[S, D]] =
          def executionSequence(s: Stream[F, Atom[D]]): F[Option[ExecutionSequence[D]]] =
            val atoms: F[List[(Atom[D], Boolean)]] =
              s.zipWithNext
               .map(_.map(_.isDefined))
               .take(1 + lim.value) // 1 (nextAtom) + futureLimit (possibleFuture)
               .compile
               .toList

            atoms.map: lst =>
              lst.headOption.map { case (head, _) =>
                ExecutionSequence(head, lst.tail.map(_._1), lst.last._2)
              }

          for
            a <- executionSequence(stream.acquisition)
            s <- executionSequence(stream.science)
          yield ExecutionConfig(stream.static, a, s)

        def instrumentExecutionConfig(
          ctx: GeneratorContext
        )(using Transaction[F]): EitherT[F, OdbError, InstrumentExecutionConfig] =
          ctx.params.observingMode.modeType match

            case ObservingModeType.Flamingos2LongSlit =>
              EitherT(streaming.selectOrGenerateFlamingos2LongSlit(ctx))
                .flatMap(s => EitherT.liftF(executionConfig(s)))
                .map(InstrumentExecutionConfig.Flamingos2.apply)

            case ObservingModeType.GmosNorthImaging   =>
              EitherT(streaming.selectOrGenerateGmosNorthImaging(ctx))
                .flatMap(s => EitherT.liftF(executionConfig(s)))
                .map(InstrumentExecutionConfig.GmosNorth.apply)

            case ObservingModeType.GmosNorthLongSlit  =>
              EitherT(streaming.selectOrGenerateGmosNorthLongSlit(ctx))
                .flatMap(s => EitherT.liftF(executionConfig(s)))
                .map(InstrumentExecutionConfig.GmosNorth.apply)

            case ObservingModeType.GmosSouthImaging   =>
              EitherT(streaming.selectOrGenerateGmosSouthImaging(ctx))
                .flatMap(s => EitherT.liftF(executionConfig(s)))
                .map(InstrumentExecutionConfig.GmosSouth.apply)

            case ObservingModeType.GmosSouthLongSlit  =>
              EitherT(streaming.selectOrGenerateGmosSouthLongSlit(ctx))
                .flatMap(s => EitherT.liftF(executionConfig(s)))
                .map(InstrumentExecutionConfig.GmosSouth.apply)

            case ObservingModeType.Igrins2LongSlit    =>
              EitherT.leftT(OdbError.InvalidObservation(ctx.oid, s"IGRINS2 is not yet supported".some))

        transactionallyWithContext(oid, commitHash): ctx =>
          instrumentExecutionConfig(ctx)

      override def resetAcquisition(
        observationId: Observation.Id
      )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, Unit]] =
        transactionallyWithContext(observationId, commitHash): ctx =>
          ctx.params.observingMode.modeType match

            case ObservingModeType.Flamingos2LongSlit =>
              EitherT(streaming.generateFlamingos2LongSlit(ctx))
                .flatMap(s => EitherT.liftF(sequenceService.resetFlamingos2Acquisition(observationId, s.acquisition)))

            // N.B. there is no imaging acquisition, but it should not blow up.
            case ObservingModeType.GmosNorthImaging =>
              EitherT(streaming.generateGmosNorthImaging(ctx))
                .flatMap(s => EitherT.liftF(sequenceService.resetGmosNorthAcquisition(observationId, s.acquisition)))

            case ObservingModeType.GmosNorthLongSlit =>
              EitherT(streaming.generateGmosNorthLongSlit(ctx))
                .flatMap(s => EitherT.liftF(sequenceService.resetGmosNorthAcquisition(observationId, s.acquisition)))

            case ObservingModeType.GmosSouthImaging =>
              EitherT(streaming.generateGmosSouthImaging(ctx))
                .flatMap(s => EitherT.liftF(sequenceService.resetGmosSouthAcquisition(observationId, s.acquisition)))

            case ObservingModeType.GmosSouthLongSlit =>
              EitherT(streaming.generateGmosSouthLongSlit(ctx))
                .flatMap(s => EitherT.liftF(sequenceService.resetGmosSouthAcquisition(observationId, s.acquisition)))

            case ObservingModeType.Igrins2LongSlit =>
              EitherT.leftT(OdbError.InvalidObservation(ctx.oid, s"IGRINS2 is not yet supported".some))

      override def materializeAndThen[A](
        oid:  Observation.Id
      )(
        f: Transaction[F] ?=> F[Either[OdbError, A]]
      )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, A]] =

        def materializeExecutionConfig(
          ctx: GeneratorContext
        )(using Transaction[F]): EitherT[F, OdbError, Unit] =
          ctx.params.observingMode.modeType match

            case ObservingModeType.Flamingos2LongSlit =>
              EitherT(streaming.generateFlamingos2LongSlit(ctx))
                .flatMap(s => EitherT.liftF(sequenceService.materializeFlamingos2ExecutionConfig(oid, s)))

            case ObservingModeType.GmosNorthImaging =>
              EitherT(streaming.generateGmosNorthImaging(ctx))
                .flatMap(s => EitherT.liftF(sequenceService.materializeGmosNorthExecutionConfig(oid, s)))

            case ObservingModeType.GmosNorthLongSlit =>
              EitherT(streaming.generateGmosNorthLongSlit(ctx))
                .flatMap(s => EitherT.liftF(sequenceService.materializeGmosNorthExecutionConfig(oid, s)))

            case ObservingModeType.GmosSouthImaging =>
              EitherT(streaming.generateGmosSouthImaging(ctx))
                .flatMap(s => EitherT.liftF(sequenceService.materializeGmosSouthExecutionConfig(oid, s)))

            case ObservingModeType.GmosSouthLongSlit =>
              EitherT(streaming.generateGmosSouthLongSlit(ctx))
                .flatMap(s => EitherT.liftF(sequenceService.materializeGmosSouthExecutionConfig(oid, s)))

            case ObservingModeType.Igrins2LongSlit =>
              EitherT.leftT(OdbError.InvalidObservation(ctx.oid, s"IGRINS2 is not yet supported".some))

        transactionallyWithContext(oid, commitHash): ctx =>
          materializeExecutionConfig(ctx) *> EitherT(f)


      override def materialize(
        oid:  Observation.Id,
      )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, Unit]] =
        materializeAndThen(oid)(().asRight[OdbError].pure[F])