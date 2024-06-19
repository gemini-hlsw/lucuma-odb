// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.data.EitherT
import cats.data.NonEmptyList
import cats.data.State
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.RefinedTypeOps
import eu.timepit.refined.numeric.Interval
import fs2.Pipe
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.GcalLampType
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepType
import lucuma.core.math.Offset
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Visit
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
import lucuma.odb.data.Md5Hash
import lucuma.odb.sequence.data.CalLocation
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

    val sequenceTooLong: Error = SequenceTooLong

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

      type ExpandedAndEstimatedProtoExecutionSequence[F[_], S, D] =
        ProtoExecutionConfig[F, S, Either[String, (EstimatedAtom[D], Long)]]

      type GmosNorthProtoExecutionConfig[F[_]] =
        ExpandedAndEstimatedProtoExecutionSequence[F, GmosNorthStatic, GmosNorthDynamic]

      type GmosSouthProtoExecutionConfig[F[_]] =
        ExpandedAndEstimatedProtoExecutionSequence[F, GmosSouthStatic, GmosSouthDynamic]

      private def gmosNorthLongSlit(
        ctx:    Context,
        config: gmos.longslit.Config.GmosNorth
      ): EitherT[F, Error, (GmosNorthProtoExecutionConfig[F], IdBase.Acq, IdBase.Sci)] =
        for {
          p <- gmosLongSlit(ctx.oid, ctx.acquisitionIntegrationTime, ctx.scienceIntegrationTime, config, gmos.longslit.Generator.GmosNorth)
          m <- EitherT.liftF(services.transactionally { services.sequenceService.selectGmosNorthCompletionState(ctx.oid) })
        } yield (gmosLongslitExpandAndEstimate(p, exp.gmosNorth, calculator.gmosNorth, m), IdBase.Acq(m.acq.idBase), IdBase.Sci(m.sci.idBase))

      private def gmosSouthLongSlit(
        ctx:    Context,
        config: gmos.longslit.Config.GmosSouth
      ): EitherT[F, Error, (GmosSouthProtoExecutionConfig[F], IdBase.Acq, IdBase.Sci)] =
        for {
          p <- gmosLongSlit(ctx.oid, ctx.acquisitionIntegrationTime, ctx.scienceIntegrationTime, config, gmos.longslit.Generator.GmosSouth)
          m <- EitherT.liftF(services.transactionally { services.sequenceService.selectGmosSouthCompletionState(ctx.oid) })
        } yield (gmosLongslitExpandAndEstimate(p, exp.gmosSouth, calculator.gmosSouth, m), IdBase.Acq(m.acq.idBase), IdBase.Sci(m.sci.idBase))

      private def calcDigestFromContext(
        ctx: Context
      )(using NoTransaction[F]): EitherT[F, Error, ExecutionDigest] =
        EitherT
          .fromEither(Error.sequenceTooLong.asLeft[ExecutionDigest])
          .unlessA(ctx.scienceIntegrationTime.exposures.value <= SequenceAtomLimit) *>
        (ctx.params match {
          case GeneratorParams.GmosNorthLongSlit(_, config) =>
            gmosNorthLongSlit(ctx, config).flatMap { (p, _, _) => executionDigest(p, calculator.gmosNorth.estimateSetup) }

          case GeneratorParams.GmosSouthLongSlit(_, config) =>
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
        ctx.params match {
          case GeneratorParams.GmosNorthLongSlit(_, config) =>
            for {
              (p, a, s) <- gmosNorthLongSlit(ctx, config)
              r         <- executionConfig(p, ctx.namespace, a, s, lim)
            } yield InstrumentExecutionConfig.GmosNorth(r)

          case GeneratorParams.GmosSouthLongSlit(_, config) =>
            for {
              (p, a, s) <- gmosSouthLongSlit(ctx, config)
              r         <- executionConfig(p, ctx.namespace, a, s, lim)
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
          generator
            .generate(acquisitionItc, scienceItc, config)
            .leftMap(s => InvalidData(oid, s))
        )

      // Performs smart-gcal expansion and time estimate calculation.
      private def simpleExpandAndEstimatePipe[S, K, D](
        static:   S,
        expander: SmartGcalExpander[F, K, D],
        calc:     TimeEstimateCalculator[S, D],
        comp:     Completion.SequenceMatch[D]
      ): Pipe[F, SimpleAtom[D], Either[String, (EstimatedAtom[D], Long)]] = {

        type MatchState[A] = State[Completion.SequenceMatch[D], A]
        import Completion.SequenceMatch.matchAny

          // Do smart-gcal expansion
        _.through(expander.expand)

          // Number the atoms, because executed atoms will be filtered out but
          // we need the correct index to always calculate the same Atom.Id.
          .zipWithIndex.map { case (e, index) => e.tupleRight(index) }

          // Mark the atoms with true (excuted) or false (not executed)
          .mapAccumulate(comp) { case (state, eAtom) =>
            val nextAtom = for {
              (atom, index) <- EitherT.fromEither[MatchState](eAtom)
              visit         <- EitherT.liftF(matchAny(atom))
            } yield (atom, index, visit.isDefined)

            nextAtom.value.run(state).value
          }

          // dump the state and keep only un-executed atoms
          .collect {
            case (_, Left(error))               => Left(error)
            case (_, Right(atom, index, false)) => Right((atom, index))
          }

          // Add step estimates
         .through(calc.estimateSequence[F](static))
      }

      private def gmosLongslitScienceExpandAndEstimatePipe[S, K, D](
        static:   S,
        expander: SmartGcalExpander[F, K, D],
        calc:     TimeEstimateCalculator[S, D],
        comp:     Completion.SequenceMatch[D]
      ): Pipe[F, SimpleAtom[D], Either[String, (EstimatedAtom[D], Long)]] = { (s: Stream[F, SimpleAtom[D]]) =>

        def smartArc(description: String, d: D): ProtoAtom[ProtoStep[D]] =
          ProtoAtom.one(description, ProtoStep.smartArc(d))

        val matchedArcs: Set[D] = comp.current.toList.flatMap(_._2.toList.flatMap(_._1)).collect {
          case (d, StepConfig.Gcal(lamp,_,_,_)) if lamp.lampType === GcalLampType.Arc => d
        }.toSet

        case class Accumulator(
          comp:          Completion.SequenceMatch[D],
          index:         Long,
          generatedArcs: Set[D]
        ) {
          def shouldGenerateArc(d: D): Boolean =
            !(matchedArcs(d) || generatedArcs(d))
        }

        val init = Accumulator(comp, 1L, Set.empty)

        type MatchState[A] = State[Completion.SequenceMatch[D], A]
        import Completion.SequenceMatch.matchCurrent
        import Completion.SequenceMatch.matchPast
        import Completion.SequenceMatch.contains

        case class Arc[D](atom: ProtoAtom[ProtoStep[D]], location: CalLocation)

        val zipWithArc: Pipe[
          F,
          Either[String, ProtoAtom[ProtoStep[D]]],
          Either[String, (ProtoAtom[ProtoStep[D]], Arc[D])]
        ] =
          _.evalMapAccumulate(Map.empty[D, ProtoAtom[ProtoStep[D]]]) { (cache, eAtom) =>
            eAtom.flatTraverse { atom =>
              val arcTitle = atom.description.fold("Arc")(s => s"Arc: $s")

              val scienceConfig: Either[String, (D, CalLocation)] =
                atom
                  .steps
                  .zipWithIndex
                  .find(_._1.stepConfig.stepType === StepType.Science)
                  .map((s, l) => (s.value, if (l === 0) CalLocation.After else CalLocation.Before))
                  .toRight("Atom contains no science step!")

              scienceConfig.flatTraverse { (d, loc) =>
                cache
                  .get(d)
                  .fold(expander.expandAtom(smartArc(arcTitle, d)))(_.asRight.pure)
                  .map(_.map { arc => (cache.updated(d, arc), atom, Arc(arc, loc)) })
              }
            }
            .map {
              case Left(error)             => (cache, error.asLeft)
              case Right(cache, atom, arc) => (cache, (atom, arc).asRight)
            }
          }
          .map(_._2)

          // Do smart-gcal expansion
        s.through(expander.expand)

          // Add corresponding arcs
         .through(zipWithArc)

          // Strip executed atoms
         .mapAccumulate(init) {
           case (acc, Left(error))        =>
             (acc, error.asLeft)

           case (acc, Right((atom, arc))) =>
             val arcConfig = arc.atom.steps.head.value

             val pastMatch: MatchState[(Int, Option[D], List[(ProtoAtom[ProtoStep[D]], Long)])] =
               (0, none[D], List.empty[(ProtoAtom[ProtoStep[D]], Long)]).pure[MatchState]

             val currentMatch: MatchState[(Int, Option[D], List[(ProtoAtom[ProtoStep[D]], Long)])] =
               matchCurrent(atom).map { vid =>
                 val genAtom = Option.unless(vid.isDefined)(atom)
                 val genArc  = Option.when(acc.shouldGenerateArc(arcConfig))(arc.atom)
                 val atoms = arc.location match {
                   case CalLocation.Before => genArc.tupleRight(acc.index + 1).toList ++ genAtom.tupleRight(acc.index + 2).toList
                   case CalLocation.After  => genAtom.tupleRight(acc.index + 1).toList ++ genArc.tupleRight(acc.index + 2).toList
                 }
                 (
                   1 + (if (acc.generatedArcs(arcConfig)) 0 else 1),
                   arcConfig.some,
                   atoms//.zipWithIndex.map(_.map(_ + acc.index))
                 )
               }

             val update = for {
               vid  <- matchPast(atom)
               skip <- vid.fold(false.pure[MatchState])(contains(arc.atom)) // same visit must contain the arc
               m    <- if (skip) pastMatch else currentMatch
             } yield m

             val (compʹ, (inc, genArc, steps)) = update.run(acc.comp).value
             val accʹ = Accumulator(compʹ, acc.index + inc, acc.generatedArcs ++ genArc)

             (accʹ, steps.asRight)
         }
         .flatMap {
           case (_, Left(error))  => Stream.emit(Left(error))
           case (_, Right(steps)) => Stream.emits(steps.map(_.asRight))
         }

         // Add step estimates
         .through(calc.estimateSequence[F](static))
      }

      // Performs smart-gcal expansion and time estimate calculation.
      private def gmosLongslitExpandAndEstimate[S, K, D](
        proto:    ProtoExecutionConfig[Pure, S, SimpleAtom[D]],
        expander: SmartGcalExpander[F, K, D],
        calc:     TimeEstimateCalculator[S, D],
        comState: Completion.Matcher[D]
      ): ProtoExecutionConfig[F, S, Either[String, (EstimatedAtom[D], Long)]] =
        proto.mapSequences(
          simpleExpandAndEstimatePipe(proto.static, expander, calc, comState.acq),
          gmosLongslitScienceExpandAndEstimatePipe(proto.static, expander, calc, comState.sci)
        )

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
