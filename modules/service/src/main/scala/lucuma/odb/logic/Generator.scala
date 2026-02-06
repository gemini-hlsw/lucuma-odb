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
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.ExecutionState
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
  )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, ExecutionDigest]]

  /**
   * The same is `digest`, but it also returns the GeneratorParms and the hash used
   * to determine if the digest needed to be recalculated. This is useful in things
   * like the guide star availability calculations which depend on the digest and are
   * also cached.
   */
  def digestWithParamsAndHash(
    observationId: Observation.Id
  )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, (ExecutionDigest, GeneratorParams, Md5Hash)]]

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
   * well enough to perform the calculation.  Because the sequences that are
   * generated may differ depending on when requested, a 'when' parameter option
   * is provided.  By default this will be "now".
   *
   * @param futureLimit cap to place on the number of atoms that map appear in
   *                    the possibleFuture
   */
  def generate(
    observationId: Observation.Id,
    futureLimit:   FutureLimit = FutureLimit.Default
  )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, InstrumentExecutionConfig]]

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
        def lookupOne(ctx: GeneratorContext)(using NoTransaction[F]): EitherT[F, OdbError, Option[ExecutionDigest]] =
          EitherT.right:
            services.transactionally:
              executionDigestService.selectOne(ctx.oid, ctx.hash)

        def lookupMany(contexts: List[GeneratorContext])(using Transaction[F]): F[Map[Observation.Id, ExecutionDigest]] =
          executionDigestService.selectMany(contexts.map(c => (c.oid, c.hash)))

        def store(ctx: GeneratorContext, digest: ExecutionDigest)(using NoTransaction[F]): EitherT[F, OdbError, Unit] =
          EitherT.right:
            services.transactionally:
              executionDigestService.insertOrUpdate(ctx.oid, ctx.hash, digest)

      override def digest(
        oid: Observation.Id
      )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, ExecutionDigest]] =
        digestWithParamsAndHash(oid).map(_.map(_._1))

      override def digestWithParamsAndHash(
        oid: Observation.Id
      )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, (ExecutionDigest, GeneratorParams, Md5Hash)]] =
        (for
          ctx <- EitherT(GeneratorContext.lookup(oid, commitHash))
          d0  <- ExecutionDigestCache.lookupOne(ctx)
          d1  <- d0.fold(calcDigestThenCache(ctx))(d => EitherT.pure(d))
        yield (d1, ctx.params, ctx.hash)).value

      private def calcDigestThenCache(
        ctx: GeneratorContext
      )(using NoTransaction[F], Services.ServiceAccess): EitherT[F, OdbError, ExecutionDigest] =
        for
          d <- calcDigestFromContext(ctx)
          _ <- ExecutionDigestCache.store(ctx, d)
        yield d

      private def calcDigestFromContext(
        ctx: GeneratorContext
      )(using NoTransaction[F], Services.ServiceAccess): EitherT[F, OdbError, ExecutionDigest] =

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
              EitherT(streaming.flamingos2LongSlit(ctx)).flatMap(digest(_, calculator.flamingos2.estimateSetup))
            case ObservingModeType.GmosNorthImaging   =>
              EitherT(streaming.gmosNorthImaging(ctx)).flatMap(digest(_, calculator.gmosNorth.estimateSetup))
            case ObservingModeType.GmosNorthLongSlit  =>
              EitherT(streaming.gmosNorthLongSlit(ctx)).flatMap(digest(_, calculator.gmosNorth.estimateSetup))
            case ObservingModeType.GmosSouthImaging   =>
              EitherT(streaming.gmosSouthImaging(ctx)).flatMap(digest(_, calculator.gmosSouth.estimateSetup))
            case ObservingModeType.GmosSouthLongSlit  =>
              EitherT(streaming.gmosSouthLongSlit(ctx)).flatMap(digest(_, calculator.gmosSouth.estimateSetup))

      private def calculateDigest(
        ctx: GeneratorContext
      )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, ExecutionDigest]] =
        calcDigestThenCache(ctx).value

      private def calculateScienceAtomDigests(
        ctx: GeneratorContext
      )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, Stream[F, AtomDigest]]] =

        val checkSequence =
          EitherT
            .fromEither(GeneratorError.sequenceTooLong(ctx.oid).asLeft[ExecutionDigest])
            .unlessA(ctx.itcRes.toOption.forall(_.scienceExposureCount.value <= SequenceAtomLimit))

        val atomDigests = ctx.params.observingMode.modeType match
          case ObservingModeType.Flamingos2LongSlit =>
            EitherT(streaming.flamingos2LongSlit(ctx)).map(_.science.map(AtomDigest.fromAtom))
          case ObservingModeType.GmosNorthImaging   =>
            EitherT(streaming.gmosNorthImaging(ctx)).map(_.science.map(AtomDigest.fromAtom))
          case ObservingModeType.GmosNorthLongSlit  =>
            EitherT(streaming.gmosNorthLongSlit(ctx)).map(_.science.map(AtomDigest.fromAtom))
          case ObservingModeType.GmosSouthImaging   =>
            EitherT(streaming.gmosSouthImaging(ctx)).map(_.science.map(AtomDigest.fromAtom))
          case ObservingModeType.GmosSouthLongSlit  =>
            EitherT(streaming.gmosSouthLongSlit(ctx)).map(_.science.map(AtomDigest.fromAtom))

        (checkSequence *> atomDigests).value

      override def obscalc(
        observationId: Observation.Id,
        itcResult:     Either[OdbError, Itc]
      )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, (ExecutionDigest, Stream[F, AtomDigest])]] =
        (for
          c <- EitherT(GeneratorContext.lookup(observationId, commitHash, itcResult.some))
          d <- EitherT(calculateDigest(c))
          a <- EitherT(calculateScienceAtomDigests(c))
        yield (d, a)).value

      override def generate(
        oid:  Observation.Id,
        lim:  FutureLimit = FutureLimit.Default
      )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, InstrumentExecutionConfig]] =

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
        ): EitherT[F, OdbError, InstrumentExecutionConfig] =
          ctx.params.observingMode.modeType match

            case ObservingModeType.Flamingos2LongSlit =>
              EitherT(streaming.flamingos2LongSlit(ctx))
                .flatMap(s => EitherT.liftF(executionConfig(s)))
                .map(InstrumentExecutionConfig.Flamingos2.apply)

            case ObservingModeType.GmosNorthImaging   =>
              EitherT(streaming.gmosNorthImaging(ctx))
                .flatMap(s => EitherT.liftF(executionConfig(s)))
                .map(InstrumentExecutionConfig.GmosNorth.apply)

            case ObservingModeType.GmosNorthLongSlit  =>
              EitherT(streaming.gmosNorthLongSlit(ctx))
                .flatMap(s => EitherT.liftF(executionConfig(s)))
                .map(InstrumentExecutionConfig.GmosNorth.apply)

            case ObservingModeType.GmosSouthImaging   =>
              EitherT(streaming.gmosSouthImaging(ctx))
                .flatMap(s => EitherT.liftF(executionConfig(s)))
                .map(InstrumentExecutionConfig.GmosSouth.apply)

            case ObservingModeType.GmosSouthLongSlit  =>
              EitherT(streaming.gmosSouthLongSlit(ctx))
                .flatMap(s => EitherT.liftF(executionConfig(s)))
                .map(InstrumentExecutionConfig.GmosSouth.apply)

        (for
          ctx <- EitherT(GeneratorContext.lookup(oid, commitHash))
          iec <- instrumentExecutionConfig(ctx)
        yield iec).value