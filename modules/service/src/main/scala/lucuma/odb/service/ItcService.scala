// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Order
import cats.Parallel
import cats.data.EitherT
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.effect.Async
import cats.effect.Concurrent
import cats.effect.Resource
import cats.effect.syntax.spawn.*
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.order.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import clue.ResponseException
import fs2.Stream
import io.circe.syntax.*
import lucuma.core.data.Zipper
import lucuma.core.enums.Band
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GnirsAcquisitionType
import lucuma.core.enums.GnirsFilter
import lucuma.core.math.SignalToNoise
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMode
import lucuma.core.util.TimeSpan
import lucuma.itc.AsterismIntegrationTimes
import lucuma.itc.IntegrationTime
import lucuma.itc.ItcGhostDetector
import lucuma.itc.TargetIntegrationTime
import lucuma.itc.client.ClientCalculationResult
import lucuma.itc.client.ImagingInput
import lucuma.itc.client.ImagingParameters
import lucuma.itc.client.InstrumentMode
import lucuma.itc.client.ItcClient
import lucuma.itc.client.SpectroscopyInput
import lucuma.itc.client.SpectroscopyParameters
import lucuma.odb.data.Itc
import lucuma.odb.data.ItcAcquisition
import lucuma.odb.data.ItcResult
import lucuma.odb.data.ItcScience
import lucuma.odb.data.Md5Hash
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.data.ItcInput
import lucuma.odb.sequence.data.ItcInputDerivation
import lucuma.odb.sequence.data.MissingParamSet
import lucuma.odb.sequence.flamingos2
import lucuma.odb.sequence.gmos
import lucuma.odb.sequence.gnirs
import lucuma.odb.sequence.syntax.hash.*
import lucuma.odb.service.NoTransaction
import lucuma.odb.service.Services.SuperUserAccess
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.Codecs.*
import lucuma.refined.*
import org.typelevel.log4cats.Logger
import skunk.*
import skunk.circe.codec.json.*
import skunk.codec.boolean.bool
import skunk.codec.text.text
import skunk.implicits.*

import scala.concurrent.duration.*

sealed trait ItcService[F[_]] {

  /**
   * Obtains the ITC results for a single target, first checking the cache and
   * then performing a query to the remote ITC service if necessary.
   */
  def lookup(
    programId:     Program.Id,
    observationId: Observation.Id
  )(using NoTransaction[F]): F[Either[OdbError, Itc]]

  /**
   * Using the provided generator parameters, calls the remote ITC service to
   * obtain the AsterismResult, if possible, then caches it for future reference.
   */
  def callRemote(
    programId:     Program.Id,
    observationId: Observation.Id,
    params:        GeneratorParams
  )(using NoTransaction[F]): F[Either[OdbError, Itc]]

  /**
   * Selects the cached ITC result for a single observation, if available and
   * still valid.
   * Returns Some(Right) for a cached success, Some(Left) for a
   * cached deterministic failure, and None if not cached.
   * Does not perform a remote ITC service call.
   */
  def selectOne(
    programId:    Program.Id,
    observatinId: Observation.Id,
    params:       GeneratorParams
  )(using Transaction[F]): F[Option[Either[OdbError, Itc]]]

  /**
   * Selects the cached ITC results for a program, for those observations where
   * it is available and still valid.  Does not perform a remote ITC service
   * call if not available.
   */
  def selectAll(
    programId: Program.Id,
    params:    Map[Observation.Id, GeneratorParams]
  )(using Transaction[F]): F[Map[Observation.Id, Itc]]

  def selectAll(
    params: Map[Observation.Id, GeneratorParams]
  )(using Transaction[F], SuperUserAccess): F[Map[Observation.Id, Itc]]

  /**
   * Freezes the ITC result for an observation, making it durable and
   * authoritative.  A frozen result is exempt from the wholesale wipe on an ITC
   * version bump and is returned by all lookups regardless of the current input
   * hash, so that an executing observation no longer depends on the live ITC
   * service or on the disposable cache.  Intended to be called when execution
   * begins (the sequence is materialized), passing the same ITC result that fed
   * sequence generation.  Idempotent: the first frozen value wins and is never
   * overwritten by a later call.
   */
  def freeze(
    observationId: Observation.Id,
    input:         ItcInput,
    result:        Itc
  )(using Transaction[F]): F[Unit]

  /**
   * Performs a fresh remote ITC call for the ACQUISITION portion of an
   * observation only, bypassing the cache and any frozen result.  This is the
   * one point at which acquisition is re-derived during execution (via
   * resetAcquisition), so that an edited acquisition exposure-time mode takes
   * effect.  Unlike [[lookup]], ANY acquisition ITC failure — deterministic or
   * transient — is fatal (a Left): resetAcquisition must not overwrite a working
   * acquisition with a failure.  Returns NotApplicable for modes that have no
   * acquisition sequence (in which case resetAcquisition is a no-op).
   */
  def callRemoteAcquisition(
    observationId: Observation.Id
  )(using NoTransaction[F]): F[Either[OdbError, ItcAcquisition]]

  /**
   * Overwrites just the acquisition portion of an observation's stored ITC
   * result, leaving the science part and the frozen flag untouched.  Used by
   * resetAcquisition to keep the cached/frozen snapshot consistent with the
   * newly generated acquisition sequence.  A no-op if there is no stored row.
   */
  def updateAcquisition(
    observationId: Observation.Id,
    acquisition:   ItcAcquisition
  )(using Transaction[F]): F[Unit]

}

object ItcService {

  object Error:
    def invalidObservation(
      oid:   Observation.Id,
      error: GeneratorParamsService.Error
    ): OdbError =
      val msg = error match
        case GeneratorParamsService.Error.MissingData(p) =>
          s"ITC cannot be queried until the following parameters are defined: ${p.params.map(_.name).intercalate(", ")}"
        case _                                           =>
          error.format
      OdbError.InvalidObservation(oid, msg.some)

    def itcError(
      problems: NonEmptyList[(Option[Target.Id], String)]
    ): OdbError =
      val ps = problems.map:
        case (None, msg)      => s"Asterism: $msg"
        case (Some(tid), msg) => s"Target '$tid': $msg"
      OdbError.ItcError(s"ITC returned errors: ${ps.intercalate(", ")}".some)

    def targetMismatch: OdbError =
      OdbError.ItcError("ITC provided conflicting results".some)

  end Error

  opaque type Result = Either[OdbError, Itc]

  object Result:
    def apply(e: Either[OdbError, Itc]): Result =
      e

  extension (r: Result)
    def toEither: Either[OdbError, Itc] =
      r

  // Recombines the two acquisition columns into the three-way ItcAcquisition:
  // a results blob is Available, a bare error is Failed, both absent is
  // NotApplicable (a mode with no acquisition sequence).
  private def assembleAcquisition(
    acquisitionResults: Option[ItcAcquisition.Available],
    acquisitionError:   Option[String]
  ): ItcAcquisition =
    acquisitionResults.getOrElse:
      acquisitionError.fold(ItcAcquisition.NotApplicable)(ItcAcquisition.Failed(_))

  // Recombines the four result columns into an Itc.  Science is required: a
  // science error means the whole result is unavailable (a Left); an
  // acquisition error is folded into the (Right) composite as a Failed.
  private def assembleItc(
    scienceResults:     Option[ItcScience],
    scienceError:       Option[String],
    acquisitionResults: Option[ItcAcquisition.Available],
    acquisitionError:   Option[String]
  ): Either[OdbError, Itc] =
    scienceResults match
      case Some(sci) => Itc(assembleAcquisition(acquisitionResults, acquisitionError), sci).asRight
      case None      => OdbError.ItcError(scienceError).asLeft

  // Splits an ItcAcquisition into its two storage columns (results, error).
  private def splitAcquisition(
    acquisition: ItcAcquisition
  ): (Option[ItcAcquisition.Available], Option[String]) =
    acquisition match
      case ItcAcquisition.NotApplicable       => (none, none)
      case ItcAcquisition.Failed(msg)         => (none, msg.some)
      case a @ ItcAcquisition.Available(_, _) => (a.some, none)

  def pollVersionsForever[F[_]: Async: Logger](
    client:     Resource[F, ItcClient[F]],
    session:    Resource[F, Session[F]],
    pollPeriod: FiniteDuration
  ): F[Unit] = {
    val pollOnce: F[Unit] =
      for {
        v <- client.use(_.versions)
        _ <- session.use { s => s.transaction.use(_ => s.execute(Statements.UpdateItcVersion)(v.serverVersion.some, v.dataVersion)) }
      } yield ()

    val pollOnceLogError: F[Unit] =
      pollOnce.handleErrorWith { t =>
        Logger[F].info(t)(s"Error while polling ITC versions: ${t.getMessage}")
      }

    Stream
      .fixedDelay(pollPeriod)
      .zip(Stream.repeatEval(pollOnceLogError))
      .compile
      .drain
      .start
      .void

  }

  def instantiate[F[_]: Concurrent as F: Parallel: Logger as L: Services](client: ItcClient[F]): ItcService[F] =
    new ItcService[F] {

      override def lookup(
        pid: Program.Id,
        oid: Observation.Id
      )(using NoTransaction[F]): F[Either[OdbError, Itc]] =
        // A frozen result is authoritative once execution has begun: return it
        // without consulting the params, the input hash, or the remote service.
        services.transactionally(selectFrozen(pid, oid)).flatMap:
          case Some(itc) => itc.asRight.pure[F]
          case None      =>
            (for
              pr          <- EitherT(attemptLookup(pid, oid))
              (params, oa) = pr
              result      <- oa.fold(EitherT(callRemote(pid, oid, params)))(EitherT.fromEither(_))
            yield result).value

      override def callRemote(
        pid:    Program.Id,
        oid:    Observation.Id,
        params: GeneratorParams
      )(using NoTransaction[F]): F[Either[OdbError, Itc]] =
        def missing(m: MissingParamSet): F[Either[OdbError, Itc]] =
          Error.invalidObservation(oid, GeneratorParamsService.Error.MissingData(m)).asLeft[Itc].pure[F]

        params.itcInput match
          case ItcInputDerivation.Ready(p)      =>
            callRemoteItc(oid, p).flatTap:
              case Right(r)                       => services.transactionally(insertOrUpdate(pid, oid, p, r))
              case Left(e @ OdbError.ItcError(_)) => services.transactionally(insertOrUpdateFailure(pid, oid, p, e))
              case Left(_)                        => F.unit
          case ItcInputDerivation.Incomplete(m) => missing(m)
          // No ITC applies to this mode; callers gate on Ready, so this is defensive.
          case ItcInputDerivation.NotApplicable =>
            OdbError.InvalidObservation(oid, "ITC is not applicable for this observing mode".some).asLeft[Itc].pure[F]

      // Selects the parameters then checks the cache
      // Returns None if not cached (call remote), Some(Right) for cached success,
      // Some(Left) for cached deterministic failure.
      private def attemptLookup(
        pid: Program.Id,
        oid: Observation.Id
      )(using NoTransaction[F]): F[Either[OdbError, (GeneratorParams, Option[Either[OdbError, Itc]])]] =
        services.transactionally {
          (for {
            p <- EitherT(generatorParamsService.selectOne(pid, oid).map(_.leftMap(Error.invalidObservation(oid, _))))
            r <- EitherT.liftF(selectOneCached(pid, oid, p))
          } yield (p, r)).value
        }

      private def selectOneCached(
        pid:    Program.Id,
        oid:    Observation.Id,
        params: GeneratorParams
      ): F[Option[Either[OdbError, Itc]]] =
        params.itcInput.toOption.flatTraverse: ps =>
          val inputHash = Md5Hash.unsafeFromByteArray(ps.md5)
          session
            .option(Statements.SelectOneCachedResult)(pid, oid)
            .map: rowOpt =>
              for
                (h, sciOpt, sciErr, acqOpt, acqErr, frozen) <- rowOpt
                // A frozen result is returned regardless of the input hash.
                if frozen || h === inputHash
              yield assembleItc(sciOpt, sciErr, acqOpt, acqErr)

      // Selects the frozen (durable, authoritative) result for an observation,
      // if one exists.  Ignores the input hash.  A frozen row always carries
      // science results (it is only ever frozen from a successful Itc), so this
      // returns the Itc directly; the acquisition part may still be Failed.
      private def selectFrozen(
        pid: Program.Id,
        oid: Observation.Id
      ): F[Option[Itc]] =
        session.option(Statements.SelectFrozenResult)(pid, oid).map: rowOpt =>
          rowOpt.map: (sci, acqOpt, acqErr) =>
            Itc(assembleAcquisition(acqOpt, acqErr), sci)

      override def selectOne(
        pid:    Program.Id,
        oid:    Observation.Id,
        params: GeneratorParams
      )(using Transaction[F]): F[Option[Either[OdbError, Itc]]] =
        selectOneCached(pid, oid, params)

      override def selectAll(
        pid:    Program.Id,
        params: Map[Observation.Id, GeneratorParams]
      )(using Transaction[F]): F[Map[Observation.Id, Itc]] =
        session
          .execute(Statements.SelectAllItcResults)(pid)
          .map: rows =>
            rows.flatMap: (oid, hash, sci, acqOpt, acqErr, frozen) =>
              val results = Itc(assembleAcquisition(acqOpt, acqErr), sci)
              // A frozen result is returned regardless of the input hash.
              if frozen then (oid -> results).some
              else params.get(oid).flatMap(_.itcInput.toOption).flatMap: ps =>
                val inputHash = Md5Hash.unsafeFromByteArray(ps.md5)
                Option.when(hash === inputHash)(oid -> results)
            .toMap

      override def selectAll(
        params: Map[Observation.Id, GeneratorParams]
      )(using Transaction[F], SuperUserAccess): F[Map[Observation.Id, Itc]] =
        NonEmptyList.fromList(params.keys.toList).fold(Map.empty.pure[F]): nel =>
          val enc = observation_id.nel(nel)
          session
            .execute(Statements.selectAllItcResults(enc))(nel)
            .map: rows =>
              rows
                .flatMap: (oid, hash, sci, acqOpt, acqErr, frozen) =>
                  val results = Itc(assembleAcquisition(acqOpt, acqErr), sci)
                  // A frozen result is returned regardless of the input hash.
                  if frozen then (oid -> results).some
                  else
                    for
                      params <- params.get(oid)
                      input  <- params.itcInput.toOption
                      inhash  = Md5Hash.unsafeFromByteArray(input.md5)
                      pair   <- Option.when(hash === inhash)(oid -> results)
                    yield pair
                .toMap


      // According to the spec we default if the target is too bright
      // https://app.shortcut.com/lucuma/story/1999/determine-exposure-time-for-acquisition-images
      //
      // The returned acquisition type is set only on the GNIRS S/N-mode two-pass path
      // (see below); it pins the mode the sequence uses.
      private def safeAcquisitionCall(
        oid:          Observation.Id,
        input:        ImagingInput,
        targets:      NonEmptyList[ItcInput.TargetDefinition],
        autoClassify: Boolean
      ): EitherT[F, OdbError, (Zipper[ItcResult], Option[GnirsAcquisitionType])] =
        def go(imInput: ImagingInput, min: TimeSpan, max: TimeSpan): EitherT[F, OdbError, Zipper[ItcResult]] =
          EitherT:
            client
              .imaging(imInput, useCache = false)
              .map: ccr =>
                val modifiedTargetTimes =
                  ccr.targetTimes.modifyValue:
                    _.map:
                      _.modifyValue:
                        case Left(lucuma.itc.Error.SourceTooBright(_))    =>
                          TargetIntegrationTime(
                            Zipper.one(IntegrationTime(min, 1.refined)),
                            Band.R.asLeft, // Band is meaningless here, but we need to provide one
                            None, // Imaging doesn't return signal-to-noise at
                            Nil // No ccd data for this case
                          ).asRight
                        case Right(r) if r.times.focus.exposureTime > max =>
                          r.copy(times = r.times.map(_.copy(exposureTime = max))).asRight
                        case Right(r) if r.times.focus.exposureTime < min =>
                          r.copy(times = r.times.map(_.copy(exposureTime = min))).asRight
                        case other => other

                val modifiedCcr = ClientCalculationResult(ccr.versions, modifiedTargetTimes)
                toTargetResults(targets, NonEmptyList.one(modifiedCcr)).map(_.head)
              .handleErrorWith:
                case e: ResponseException[?] =>
                  OdbError.ItcError(e.getMessage.some).asLeft.pure
                case t =>
                  OdbError.RemoteServiceCallError(s"Error calling ITC service: ${t.getMessage}".some).asLeft.pure

        def noType(z: EitherT[F, OdbError, Zipper[ItcResult]]): EitherT[F, OdbError, (Zipper[ItcResult], Option[GnirsAcquisitionType])] =
          z.map((_, Option.empty[GnirsAcquisitionType]))

        input.mode match
          case InstrumentMode.GmosNorthImaging(_, _, _, _) |
               InstrumentMode.GmosSouthImaging(_, _, _, _) =>
            noType(go(input, gmos.MinAcquisitionExposureTime, gmos.MaxAcquisitionExposureTime))
          case InstrumentMode.Flamingos2Imaging(_, _, _, _) =>
            noType(go(input, flamingos2.MinAcquisitionExposureTime, flamingos2.MaxAcquisitionExposureTime))
          case InstrumentMode.GnirsImaging(etm, filter, camera, readMode, wellDepth, coadds, port) =>
            val min: TimeSpan = gnirs.MinAcquisitionExposureTime
            val max: TimeSpan = gnirs.MaxAcquisitionExposureTime
            etm match
              case _ if autoClassify =>
                // Two-pass. The acquisition mode (Very Bright / Bright / Faint) is a
                // function of exposure time, but the exposure time depends on the filter
                // (Very Bright images in H2) which depends on the mode — and, in S/N mode,
                // a low requested S/N would shorten the exposure and misclassify a Bright
                // target as Very Bright. So classify from a fixed brightness measurement
                // (broadband filter, classification S/N) first, then compute the real
                // exposure at the user's ETM in the mode-appropriate filter. The
                // classification must not depend on the user's acquisition ETM, so it
                // always runs at the classification S/N — the user ETM's own wavelength
                // (`etm.at`, present on both S/N and time-and-count modes) locates it.
                def gnirsInput(f: GnirsFilter, e: ExposureTimeMode): ImagingInput =
                  ImagingInput.parameters
                    .andThen(ImagingParameters.mode)
                    .replace(InstrumentMode.GnirsImaging(e, f, camera, readMode, wellDepth, coadds, port))(input)

                val classifySN:  SignalToNoise    = gnirs.AcquisitionClassificationSignalToNoise
                val classifyEtm: ExposureTimeMode = ExposureTimeMode.SignalToNoiseMode(classifySN, etm.at)
                for
                  z1                                <- go(gnirsInput(filter, classifyEtm), min, max)
                  t1:      IntegrationTime           = z1.focus.value
                  acqType: GnirsAcquisitionType      = GnirsAcquisitionMode.defaultFor(t1.exposureTime, t1.exposureCount).acquisitionType
                  // Very Bright images through the acquisition filter in H2; the other
                  // classifications keep the broadband filter (already `filter`).
                  f2:      GnirsFilter               = if acqType === GnirsAcquisitionType.VeryBright then GnirsFilter.H2 else filter
                  // Skip the second call only when it would be identical to the first:
                  // same filter and the user's ETM is itself the classification S/N
                  // request. Time-and-count always needs the second call (the first ran
                  // at the classification S/N, not the user's time/count).
                  skip:    Boolean                   = f2 === filter && (etm match
                                                         case ExposureTimeMode.SignalToNoiseMode(sn, _) => sn === classifySN
                                                         case _                                         => false)
                  z2                                <- if skip then EitherT.pure[F, OdbError](z1)
                                                       else go(gnirsInput(f2, etm), min, max)
                yield (z2, acqType.some)
              case _ =>
                noType(go(input, min, max))
          case m                                                      =>
            EitherT.leftT:
              OdbError.InvalidObservation(oid, s"Acquisition is not supported for ${m.displayName}".some)

      // Runs the acquisition ITC call and folds its outcome into the three-way
      // ItcAcquisition.  A deterministic failure (ItcError) becomes a non-fatal
      // Failed carried inside the (successful) composite — science is unaffected.
      // A transient failure (RemoteServiceCallError) or an invalid observation
      // stays a fatal Left so it is not cached and the obscalc worker retries.
      private def acquisitionResult(
        oid:          Observation.Id,
        input:        ImagingInput,
        targets:      NonEmptyList[ItcInput.TargetDefinition],
        autoClassify: Boolean
      ): EitherT[F, OdbError, ItcAcquisition] =
        EitherT:
          safeAcquisitionCall(oid, input, targets, autoClassify).value.map:
            case Right((z, t))                  => ItcAcquisition.Available(z, t).asRight
            case Left(e @ OdbError.ItcError(_)) => ItcAcquisition.Failed(e.detail.getOrElse("")).asRight
            case Left(other)                    => other.asLeft

      private def toTargetResults(
        targets: NonEmptyList[ItcInput.TargetDefinition],
        results: NonEmptyList[ClientCalculationResult],
        signalToNoiseTargetId: Option[Target.Id] = none
      ): Either[OdbError, NonEmptyList[Zipper[ItcResult]]] =

        def convertErrors(
          itcErrors: NonEmptyChain[(lucuma.itc.Error, Int)]
        ): OdbError =
          Error.itcError:
            itcErrors.map { case (e, i) =>
              targets.get(i).map(_.targetId) -> e.message
            }.toNonEmptyList

        results.traverse: r =>
          r.targetTimes
           .partitionErrors
           .leftMap(convertErrors)
           .map: a =>
             val z = a.value.zipWithIndex.map { case (intTime, index) =>
               val t = targets.get(index).get
               ItcResult(t.targetId, intTime.times.focus, intTime.signalToNoiseAt)
             }
             // Pin the "selected" result to the user's signal-to-noise target,
             // if one is set and present; otherwise keep the automatic
             // (brightest) focus.
             signalToNoiseTargetId
               .flatMap(id => z.findFocus(_.targetId === id))
               .getOrElse(z)

      sealed trait Imaging[A]:
        def pf: PartialFunction[InstrumentMode, A]
        def wrap(nem: NonEmptyMap[A, Zipper[ItcResult]]): ItcScience

      object Imaging:
        case object Flamingos2Imaging extends Imaging[Flamingos2Filter]:
          override def pf: PartialFunction[InstrumentMode, Flamingos2Filter] =
            case InstrumentMode.Flamingos2Imaging(filter = f) => f

          override def wrap(nem: NonEmptyMap[Flamingos2Filter, Zipper[ItcResult]]): ItcScience =
            ItcScience.Flamingos2Imaging(nem)

        case object GmosNorthImaging extends Imaging[GmosNorthFilter]:
          override def pf: PartialFunction[InstrumentMode, GmosNorthFilter] =
            case InstrumentMode.GmosNorthImaging(filter = f) => f

          override def wrap(nem: NonEmptyMap[GmosNorthFilter, Zipper[ItcResult]]): ItcScience =
            ItcScience.GmosNorthImaging(nem)

        case object GmosSouthImaging extends Imaging[GmosSouthFilter]:
          override def pf: PartialFunction[InstrumentMode, GmosSouthFilter] =
               case InstrumentMode.GmosSouthImaging(filter = f) => f

          override def wrap(nem: NonEmptyMap[GmosSouthFilter, Zipper[ItcResult]]): ItcScience =
            ItcScience.GmosSouthImaging(nem)

        case object GnirsImaging extends Imaging[GnirsFilter]:
          override def pf: PartialFunction[InstrumentMode, GnirsFilter] =
            case InstrumentMode.GnirsImaging(filter = f) => f

          override def wrap(nem: NonEmptyMap[GnirsFilter, Zipper[ItcResult]]): ItcScience =
            ItcScience.GnirsImaging(nem)

      private def callRemoteImagingItc[A: Order](
        oid:   Observation.Id,
        input: ItcInput.Imaging,
        im:    Imaging[A]
      ): EitherT[F, OdbError, Itc] =

        def extractFilters(
          remaining: List[InstrumentMode],
          filters:   List[A]
        ): Either[OdbError, NonEmptyList[A]] =
          remaining match
            case Nil    =>
              // remaining originally comes from a NonEmptyList
              NonEmptyList.fromListUnsafe(filters.reverse).asRight
            case h :: t =>
              if im.pf.isDefinedAt(h) then extractFilters(t, im.pf(h) :: filters)
              else OdbError.InvalidObservation(oid, s"Mixed instrument mode observations are not supported.".some).asLeft

        val clientCalculationResults: F[Either[OdbError, NonEmptyList[ClientCalculationResult]]] =
          input
            .scienceInput
            .parTraverse: in =>
              client.imaging(in, useCache = false)
            .map(_.asRight)
            .handleErrorWith:
              case e: ResponseException[?] =>
                OdbError.ItcError(e.getMessage.some).asLeft.pure
              case t =>
                OdbError.RemoteServiceCallError(s"Error calling ITC service: ${t.getMessage}".some).asLeft.pure

        for
          fs <- EitherT.fromEither(extractFilters(input.science.map(_.mode).toList, Nil))
          cs <- EitherT(clientCalculationResults)
          ts <- EitherT.fromEither(toTargetResults(input.targets, cs, input.signalToNoiseTargetId))
        yield Itc(ItcAcquisition.NotApplicable, im.wrap(fs.zip(ts).toNem))


      private def callRemoteItc(
        oid:   Observation.Id,
        input: ItcInput
      )(using NoTransaction[F]): F[Either[OdbError, Itc]] =

        def callSpectroscopy(si: SpectroscopyInput): EitherT[F, OdbError, ClientCalculationResult] =
          EitherT:
            client
              .spectroscopy(si, useCache = false)
              .map(_.asRight)
              .handleErrorWith:
                case e: ResponseException[?] =>
                  OdbError.ItcError(e.getMessage.some).asLeft.pure
                case t =>
                  OdbError.RemoteServiceCallError(s"Error calling ITC service: ${t.getMessage}".some).asLeft.pure

        // A placeholder result for GHOST, at least for now.
        //
        // * The ITC accepts the red and blue exposure time mode data, but
        //   only returns a single result set?  It isn't clear how to map this
        //   back onto the red and blue channels.
        //
        // * The ITC only handles Time And Count mode and we don't use the S/N
        //   value so we'll just short-circuit the call and make a fake result
        //   that has the time and count we need per channel.
        def ghost(
          ghost:   InstrumentMode.GhostSpectroscopy,
          targets: NonEmptyList[ItcInput.TargetDefinition]
        ): EitherT[F, OdbError, Itc] =
          def resultSet(d: ItcGhostDetector): Zipper[ItcResult] =
            Zipper.fromNel:
              targets.map: t =>
                val it = IntegrationTime(d.timeAndCount.time, d.timeAndCount.count)
                ItcResult(t.targetId, it, none)

          EitherT.pure:
            Itc(
              ItcAcquisition.NotApplicable,
              ItcScience.GhostIfu(
                resultSet(ghost.redDetector),
                resultSet(ghost.blueDetector)
              )
            )

        def imaging(im: ItcInput.Imaging): EitherT[F, OdbError, Itc] =
          im.science.head.mode match
            case InstrumentMode.Flamingos2Imaging(_, _, _, _) =>
              callRemoteImagingItc(oid, im, Imaging.Flamingos2Imaging)

            case InstrumentMode.GmosNorthImaging(_, _, _, _) =>
              callRemoteImagingItc(oid, im, Imaging.GmosNorthImaging)

            case InstrumentMode.GmosSouthImaging(_, _, _, _) =>
              callRemoteImagingItc(oid, im, Imaging.GmosSouthImaging)

            case InstrumentMode.GnirsImaging(_, _, _, _, _, _, _) =>
              callRemoteImagingItc(oid, im, Imaging.GnirsImaging)

            case m                                     =>
              EitherT.leftT:
                OdbError.InvalidObservation(oid, s"Imaging ITC lookup is not supported for ${m.displayName}.".some)

        def spectroscopy(sp: ItcInput.Spectroscopy): EitherT[F, OdbError, Itc] =
          for
            cr  <- callSpectroscopy(sp.scienceInput)
            sci <- EitherT.fromEither(toTargetResults(sp.targets, NonEmptyList.one(cr), sp.signalToNoiseTargetId).map(_.head))
            acq <- acquisitionResult(oid, sp.acquisitionInput, sp.acquisitionTargets, sp.gnirsAcqAutoClassify)
          yield Itc(acq, ItcScience.Spectroscopy(sci))

        def igrins2Spectroscopy(sp: ItcInput.ScienceOnlySpectroscopy): EitherT[F, OdbError, Itc] =
          for
            cr  <- callSpectroscopy(sp.scienceInput)
            sci <- EitherT.fromEither(toTargetResults(sp.targets, NonEmptyList.one(cr), sp.signalToNoiseTargetId).map(_.head))
          yield Itc(ItcAcquisition.NotApplicable, ItcScience.Spectroscopy(sci))

        (input match
          case im @ ItcInput.Imaging(_, _, _) =>
            imaging(im)
          case sp @ ItcInput.Spectroscopy(_, _, _, _, _, _) =>
            spectroscopy(sp)
          case sp @ ItcInput.ScienceOnlySpectroscopy(SpectroscopyParameters(_, gh @ InstrumentMode.GhostSpectroscopy(_, _, _, _)), targets, _) =>
            ghost(gh, targets)
          case sp @ ItcInput.ScienceOnlySpectroscopy(SpectroscopyParameters(_, InstrumentMode.Igrins2Spectroscopy(_, _)), _, _) =>
            igrins2Spectroscopy(sp)
          case _ =>
            EitherT.leftT(OdbError.InvalidObservation(oid, s"Unrecognized ItcInput: $input".some))
        ).value

      private def insertOrUpdate(
        pid:     Program.Id,
        oid:     Observation.Id,
        input:   ItcInput,
        results: Itc
      ): F[Unit] =
        val h = Md5Hash.unsafeFromByteArray(input.md5)
        session.execute(Statements.InsertOrUpdateItcResult)(pid, oid, h, results)
          .void
          .recoverWith {
            case SqlState.ForeignKeyViolation(ex) =>
              L.info(ex)(s"Failed to insert or update ITC result for program $pid, observation $oid. Probably due to a deleted calibration observation.")
          }

      override def freeze(
        oid:    Observation.Id,
        input:  ItcInput,
        result: Itc
      )(using Transaction[F]): F[Unit] =
        val h = Md5Hash.unsafeFromByteArray(input.md5)
        session.execute(Statements.FreezeItcResult)(oid, h, result)
          .void
          .recoverWith:
            case SqlState.ForeignKeyViolation(ex) =>
              L.info(ex)(s"Failed to freeze ITC result for observation $oid. Probably due to a deleted observation.")

      override def callRemoteAcquisition(
        oid: Observation.Id
      )(using NoTransaction[F]): F[Either[OdbError, ItcAcquisition]] =
        val params: EitherT[F, OdbError, GeneratorParams] =
          EitherT:
            services.transactionally:
              (for
                pid <- EitherT(observationService.selectProgram(oid).map(_.toOption.toRight(OdbError.InvalidObservation(oid, s"Program for observation $oid not found.".some))))
                prm <- EitherT(generatorParamsService.selectOne(pid, oid)).leftMap(e => Error.invalidObservation(oid, e))
              yield prm).value

        val NotApplicable: EitherT[F, OdbError, ItcAcquisition] =
          EitherT.pure(ItcAcquisition.NotApplicable: ItcAcquisition)

        params
          .flatMap: params =>
            params.itcInput match
              case ItcInputDerivation.Ready(sp: ItcInput.Spectroscopy)           =>
                safeAcquisitionCall(oid, sp.acquisitionInput, sp.acquisitionTargets, sp.gnirsAcqAutoClassify)
                  .map((z, t) => ItcAcquisition.Available(z, t): ItcAcquisition)

              // Imaging has no acquisition sequence.
              case ItcInputDerivation.Ready(_: ItcInput.Imaging)                 =>
                NotApplicable

              // GHOST and IGRINS-2 spectroscopy have no acquisition sequence.
              case ItcInputDerivation.Ready(_: ItcInput.ScienceOnlySpectroscopy) =>
                NotApplicable

              // Incomplete parameters mean the acquisition ITC cannot be derived at
              // all: report that here rather than leaving it for a caller to notice.
              case ItcInputDerivation.Incomplete(m)                              =>
                EitherT.leftT:
                  Error.invalidObservation(oid, GeneratorParamsService.Error.MissingData(m))

              // No ITC at all (exchange / visitor): there is no acquisition sequence.
              case ItcInputDerivation.NotApplicable                              =>
                NotApplicable
          .value

      override def updateAcquisition(
        oid:         Observation.Id,
        acquisition: ItcAcquisition
      )(using Transaction[F]): F[Unit] =
        session.execute(Statements.UpdateItcAcquisition)(oid, acquisition)
          .void
          .recoverWith:
            case SqlState.ForeignKeyViolation(ex) =>
              L.info(ex)(s"Failed to update ITC acquisition for observation $oid. Probably due to a deleted observation.")

      private def insertOrUpdateFailure(
        pid:   Program.Id,
        oid:   Observation.Id,
        input: ItcInput,
        error: OdbError.ItcError
      ): F[Unit] =
        val h   = Md5Hash.unsafeFromByteArray(input.md5)
        val msg = error.detail.getOrElse("")
        session.execute(Statements.InsertOrUpdateItcFailure)(pid, oid, h, msg)
          .void
          .recoverWith:
            case SqlState.ForeignKeyViolation(ex) =>
              L.info(ex)(s"Failed to insert or update ITC failure for program $pid, observation $oid. Probably due to a deleted calibration observation.")
    }

  object Statements {
    import lucuma.odb.json.itc.given
    import lucuma.odb.json.time.transport.given
    import lucuma.odb.json.wavelength.transport.given

    private val science: Codec[ItcScience] =
      jsonb.eimap(
        _.as[ItcScience].leftMap(f => s"Could not decode ITC science results: ${f.message}")
      )(_.asJson)

    private val acquisition: Codec[ItcAcquisition.Available] =
      jsonb.eimap(
        _.as[ItcAcquisition.Available].leftMap(f => s"Could not decode ITC acquisition results: ${f.message}")
      )(_.asJson)

    val UpdateItcVersion: Command[(
      Option[String],
      Option[String]
    )] =
      sql"""
        UPDATE t_itc_version
           SET c_version = ${text.opt},
               c_data    = ${text.opt}
      """.command

    val SelectOneCachedResult: Query[(
      Program.Id,
      Observation.Id,
    ), (Md5Hash, Option[ItcScience], Option[String], Option[ItcAcquisition.Available], Option[String], Boolean)] =
      sql"""
        SELECT
          c_hash,
          c_science_results,
          c_science_error,
          c_acquisition_results,
          c_acquisition_error,
          c_is_frozen
        FROM t_itc_result
        WHERE c_program_id     = $program_id     AND
              c_observation_id = $observation_id
      """.query(md5_hash *: science.opt *: text.opt *: acquisition.opt *: text.opt *: bool)

    val SelectFrozenResult: Query[(
      Program.Id,
      Observation.Id,
    ), (ItcScience, Option[ItcAcquisition.Available], Option[String])] =
      sql"""
        SELECT
          c_science_results,
          c_acquisition_results,
          c_acquisition_error
        FROM t_itc_result
        WHERE c_program_id      = $program_id     AND
              c_observation_id  = $observation_id AND
              c_is_frozen                         AND
              c_science_results IS NOT NULL
      """.query(science *: acquisition.opt *: text.opt)

    val SelectAllItcResults: Query[Program.Id, (Observation.Id, Md5Hash, ItcScience, Option[ItcAcquisition.Available], Option[String], Boolean)] =
      sql"""
        SELECT
          c_observation_id,
          c_hash,
          c_science_results,
          c_acquisition_results,
          c_acquisition_error,
          c_is_frozen
        FROM t_itc_result
        WHERE c_program_id      = $program_id AND
              c_science_results IS NOT NULL
      """.query(observation_id *: md5_hash *: science *: acquisition.opt *: text.opt *: bool)

    def selectAllItcResults[A <: NonEmptyList[Observation.Id]](enc: skunk.Encoder[A]): Query[A, (Observation.Id, Md5Hash, ItcScience, Option[ItcAcquisition.Available], Option[String], Boolean)] =
      sql"""
        SELECT
          c_observation_id,
          c_hash,
          c_science_results,
          c_acquisition_results,
          c_acquisition_error,
          c_is_frozen
        FROM t_itc_result
        WHERE c_observation_id  IN ($enc) AND
              c_science_results IS NOT NULL
      """.query(observation_id *: md5_hash *: science *: acquisition.opt *: text.opt *: bool)

    val InsertOrUpdateItcResult: Command[(
      Program.Id,
      Observation.Id,
      Md5Hash,
      Itc
    )] =
      sql"""
        INSERT INTO t_itc_result (
          c_program_id,
          c_observation_id,
          c_hash,
          c_science_results,
          c_acquisition_results,
          c_acquisition_error
        ) VALUES (
          $program_id,
          $observation_id,
          $md5_hash,
          $science,
          ${acquisition.opt},
          ${text.opt}
        )
        ON CONFLICT ON CONSTRAINT t_itc_result_pkey DO UPDATE
          SET c_hash                = EXCLUDED.c_hash,
              c_science_results     = EXCLUDED.c_science_results,
              c_science_error       = NULL,
              c_acquisition_results = EXCLUDED.c_acquisition_results,
              c_acquisition_error   = EXCLUDED.c_acquisition_error
          WHERE NOT t_itc_result.c_is_frozen
      """.command
        .contramap { case (pid, oid, h, itc) =>
          val (acqResults, acqError) = splitAcquisition(itc.acquisition)
          (pid, oid, h, itc.science, acqResults, acqError)
        }

    // Promotes an observation's ITC result to frozen/authoritative.  Derives the
    // program id from t_observation (per the mode-table convention).  The first
    // frozen value wins: the WHERE guard on the conflict clause prevents a later
    // call from overwriting an already-frozen row.
    val FreezeItcResult: Command[(
      Observation.Id,
      Md5Hash,
      Itc
    )] =
      sql"""
        INSERT INTO t_itc_result (
          c_program_id,
          c_observation_id,
          c_hash,
          c_science_results,
          c_acquisition_results,
          c_acquisition_error,
          c_is_frozen
        )
        SELECT
          o.c_program_id,
          $observation_id,
          $md5_hash,
          $science,
          ${acquisition.opt},
          ${text.opt},
          true
        FROM t_observation o
        WHERE o.c_observation_id = $observation_id
        ON CONFLICT ON CONSTRAINT t_itc_result_pkey DO UPDATE
          SET c_hash                = EXCLUDED.c_hash,
              c_science_results     = EXCLUDED.c_science_results,
              c_science_error       = NULL,
              c_acquisition_results = EXCLUDED.c_acquisition_results,
              c_acquisition_error   = EXCLUDED.c_acquisition_error,
              c_is_frozen           = true
          WHERE NOT t_itc_result.c_is_frozen
      """.command
        .contramap { case (oid, h, itc) =>
          val (acqResults, acqError) = splitAcquisition(itc.acquisition)
          (oid, h, itc.science, acqResults, acqError, oid)
        }

    val InsertOrUpdateItcFailure: Command[(
      Program.Id,
      Observation.Id,
      Md5Hash,
      String
    )] =
      sql"""
        INSERT INTO t_itc_result (
          c_program_id,
          c_observation_id,
          c_hash,
          c_science_error
        ) VALUES (
          $program_id,
          $observation_id,
          $md5_hash,
          $text
        )
        ON CONFLICT ON CONSTRAINT t_itc_result_pkey DO UPDATE
          SET c_hash                = EXCLUDED.c_hash,
              c_science_results     = NULL,
              c_science_error       = EXCLUDED.c_science_error,
              c_acquisition_results = NULL,
              c_acquisition_error   = NULL
          WHERE NOT t_itc_result.c_is_frozen
      """.command

    // Overwrites only the acquisition columns, leaving science and the frozen
    // flag untouched — deliberately not guarded by NOT c_is_frozen, since
    // resetAcquisition updates the acquisition of a frozen (executing) row.
    val UpdateItcAcquisition: Command[(
      Observation.Id,
      ItcAcquisition
    )] =
      sql"""
        UPDATE t_itc_result
           SET c_acquisition_results = ${acquisition.opt},
               c_acquisition_error   = ${text.opt}
         WHERE c_observation_id = $observation_id
      """.command
        .contramap { case (oid, acq) =>
          val (acqResults, acqError) = splitAcquisition(acq)
          (acqResults, acqError, oid)
        }

  }

}
