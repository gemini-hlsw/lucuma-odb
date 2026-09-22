// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.data.EitherT
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import grackle.Result
import lucuma.ags.GuideStarName
import lucuma.core.data.Zipper
import lucuma.core.enums.AltairMode
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.itc.AltairParameters
import lucuma.odb.data.AltairConfiguration
import lucuma.odb.data.Itc
import lucuma.odb.data.ItcAcquisition
import lucuma.odb.data.ItcResult
import lucuma.odb.data.ItcScience
import lucuma.odb.data.Md5Hash
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.data.ItcInput
import lucuma.odb.sequence.data.ItcInputDerivation
import lucuma.odb.sequence.syntax.hash.*
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.sequence.util.HashBytes
import lucuma.odb.service.GuideService
import lucuma.odb.service.NoTransaction
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
import org.typelevel.log4cats.Logger

import java.security.MessageDigest

/**
 * `altairProblem` records why the Altair guide star could not be resolved against the sequence
 * that was generated for it. It is deliberately not a failure of the lookup: the guide star
 * calculations themselves need a sequence to select a star against, so they read the context as it
 * is, while everything else reports the problem. See `Generator`.
 */
case class GeneratorContext(
  oid:           Observation.Id,
  itcRes:        Either[OdbError, Itc],
  params:        GeneratorParams,
  commitHash:    CommitHash,
  altairProblem: Option[OdbError]
):
  val hash: Md5Hash =
    val md5 = MessageDigest.getInstance("MD5")

    // Generator Params
    md5.update(params.hashBytes)

    def addResultSet(z: Zipper[ItcResult]): Unit =
      md5.update(z.focus.value.exposureTime.hashBytes)
      md5.update(z.focus.value.exposureCount.hashBytes)

    def addKeyedResultSet[A: HashBytes](kv: (A, Zipper[ItcResult])): Unit =
      md5.update(kv._1.hashBytes)
      addResultSet(kv._2)

    // ITC
    itcRes.foreach: itc =>
      itc.science match
        case ItcScience.Flamingos2Imaging(m) =>
          m.toNel.toList.foreach(addKeyedResultSet)
        case ItcScience.GhostIfu(r, b)       =>
          addResultSet(r)
          addResultSet(b)
        case ItcScience.GmosNorthImaging(m)  =>
          m.toNel.toList.foreach(addKeyedResultSet)
        case ItcScience.GmosSouthImaging(m)  =>
          m.toNel.toList.foreach(addKeyedResultSet)
        case ItcScience.GnirsImaging(m)      =>
          m.toNel.toList.foreach(addKeyedResultSet)
        case ItcScience.GnirsSpectroscopy(l) =>
          l.toList.foreach(addKeyedResultSet)
        case ItcScience.Spectroscopy(sci)    =>
          addResultSet(sci)

      itc.acquisition match
        case ItcAcquisition.Available(times, _) => addResultSet(times)
        case ItcAcquisition.Failed(_)           => ()
        case ItcAcquisition.NotApplicable       => ()

    // Commit Hash
    md5.update(commitHash.hashBytes)

    Md5Hash.unsafeFromByteArray(md5.digest())

  /** The hash that guide star validity and availability caching key on. */
  def guideStarHash: Md5Hash =
    GeneratorContext.guideStarHash(params, commitHash)

object GeneratorContext:

  /**
   * How many times the sequence may be generated while the Altair guide star settles. A pass is
   * one generation: the first with the star picked before anything was generated, each one after
   * it with the star the previous pass resolved.
   */
  val MaxAltairGuideStarPasses: Int = 3

  /**
   * The hash guide star validity and the availability cache key on: the generator parameters,
   * which hold every user-editable input, and the code version. The ITC result is deliberately
   * left out. Behind Altair it is computed from the very guide star whose validity is being
   * checked, so folding it in would be circular, and the parameters already move whenever an
   * input the ITC reads moves. The price is that redeploying the ITC alone no longer invalidates
   * a stored guide star.
   */
  def guideStarHash(params: GeneratorParams, commitHash: CommitHash): Md5Hash =
    val md5 = MessageDigest.getInstance("MD5")
    md5.update(params.hashBytes)
    md5.update(commitHash.hashBytes)
    Md5Hash.unsafeFromByteArray(md5.digest())

  private def noUsableStar(oid: Observation.Id): OdbError =
    GeneratorError.sequenceUnavailable(oid, "Altair needs a guide star but none is usable.")

  private def selectedStarUnusable(oid: Observation.Id, name: GuideStarName): OdbError =
    GeneratorError.sequenceUnavailable(
      oid,
      s"The selected guide star ${name.value.value} is no longer usable; choose another or return to automatic selection."
    )

  private def unsettled(oid: Observation.Id): String =
    s"$oid: The Altair guide star selection did not settle after $MaxAltairGuideStarPasses passes; using the last selection."

  // Without a guide star only LGS+P1 has ITC parameters, and it has none of its own.
  private def altairItcParameters(
    configuration: AltairConfiguration,
    star:          Option[GuideService.ResolvedGuideStar]
  ): Option[AltairParameters] =
    star.fold(Option.when(configuration.mode === AltairMode.LgsP1)(AltairParameters.LgsP1)): resolved =>
      configuration.itcParameters(resolved.separation, resolved.rBrightness)

  /**
   * Rewrites the Altair parameters of every GNIRS ITC input in `params` for the given guide star.
   * Only GNIRS observes behind Altair, so every other mode is left alone.
   */
  def withAltairGuideStar(
    params: GeneratorParams,
    star:   Option[GuideService.ResolvedGuideStar]
  ): GeneratorParams =
    params.altair.fold(params): configuration =>
      val altair: Option[AltairParameters] = altairItcParameters(configuration, star)

      params.itcInput match
        case ItcInputDerivation.Ready(input) =>
          params.copy(itcInput = ItcInputDerivation.Ready(ItcInput.withAltairParameters(input, altair)))
        case _                               =>
          params

  private def selectParams[F[_]: Concurrent](
    oid: Observation.Id
  )(using NoTransaction[F], Services[F]): F[Either[OdbError, (Program.Id, GeneratorParams)]] =
    services.transactionally:
      (for
        pid <- EitherT:
                 observationService
                   .selectProgram(oid)
                   .map(_.toOption.toRight(GeneratorError.programNotFound(oid)))
        prm <- EitherT:
                 generatorParamsService
                   .selectOne(pid, oid)
                   .map(_.leftMap(e => GeneratorError.sequenceUnavailable(oid, e.format)))
      yield (pid, prm)).value

  /**
   * One generation's worth of context: the cached ITC result when there is one for these exact
   * parameters, otherwise a remote call, which happens outside any transaction.
   */
  private def contextFor[F[_]: Concurrent](
    pid:        Program.Id,
    oid:        Observation.Id,
    params:     GeneratorParams,
    commitHash: CommitHash
  )(using NoTransaction[F], Services[F]): F[Either[OdbError, GeneratorContext]] =
    val itc = itcService

    (for
      c  <- EitherT.liftF(services.transactionally(itc.selectOne(pid, oid, params)))
      // This will be confusing, but the idea here is that if the observation
      // definition is missing target information we just record that in the
      // Context.  On the other hand if there is an error calling the ITC then
      // we shortcircuit / cannot create the GeneratorContext at all.  So,
      // we use EitherT for short-circuiting control flow, but the payload is
      // also an Either value which may be a Left (for example when the target
      // is missing a SED).
      as <- params.itcInput match
              case ItcInputDerivation.Ready(_)      =>
                c.fold(EitherT(itc.callRemote(pid, oid, params)).map(_.asRight))(EitherT.pure(_))
              case ItcInputDerivation.Incomplete(m) =>
                // A frozen result outlives its inputs, so an observation edited after execution
                // began still generates from the result it is executing against.
                EitherT.pure(c.getOrElse(GeneratorError.sequenceUnavailable(oid, s"Missing parameters: ${m.format}").asLeft[Itc]))
              case ItcInputDerivation.NotApplicable =>
                // Exchange / visitor modes have no ITC; this result is never consumed
                // for them, but a Left keeps the payload type honest.
                EitherT.pure(GeneratorError.sequenceUnavailable(oid, "ITC is not applicable for this observing mode").asLeft[Itc])
    yield GeneratorContext(oid, as, params, commitHash, none)).value

  /**
   * Behind Altair the ITC is modelled from the guide star, and the guide star is selected against
   * the sequence the ITC sizes. The star therefore comes first, picked from the database alone
   * before anything is generated: a sequence sized without Altair answers a question nobody asked,
   * since the requested signal to noise may well be out of reach without it. The star is then
   * verified against the sequence it produced, and the sequence generated again for as long as the
   * verification keeps returning a different star, up to [[MaxAltairGuideStarPasses]] generations.
   * LGS+P1 needs no pick, its ITC parameters being the same whatever the star, but it is still
   * verified once so that an unusable selection is reported rather than ignored. The loop lives
   * here so that every caller of the generator sees the same digest and hash.
   */
  private def settleAltairGuideStar[F[_]: Concurrent: Logger](
    pid:        Program.Id,
    oid:        Observation.Id,
    commitHash: CommitHash,
    params:     GeneratorParams
  )(using NoTransaction[F], Services[F]): F[Either[OdbError, GeneratorContext]] =

    // Each pass caches its own digest, so the one the caller asks for afterwards is a lookup.
    def digestOf(ctx: GeneratorContext): EitherT[F, OdbError, ExecutionDigest] =
      EitherT:
        services.transactionally:
          executionDigestService.selectOne(oid, ctx.hash).flatMap:
            case Some(d) => d.asRight[OdbError].pure[F]
            case None    => generator.calculateDigest(ctx).flatTap(_.traverse(executionDigestService.insertOrUpdate(oid, ctx.hash, _)))

    def resolution(
      fr: Services.SuperUserAccess ?=> F[Result[GuideService.GuideStarResolution]]
    ): F[Either[OdbError, GuideService.GuideStarResolution]] =
      Services.asSuperUser(fr).map:
        _.toEither.leftMap: failure =>
          GeneratorError.sequenceUnavailable(oid, failure.fold(_.getMessage, _.toChain.toList.map(_.message).mkString("; ")))

    // The pick, from the observation as the database holds it.
    val pick: F[Either[OdbError, GuideService.GuideStarResolution]] =
      resolution(guideService.resolveGuideStar(oid, params))

    // A Gaia or geometry failure is reported the same way an unusable star is: the sequence that
    // was generated stands and the problem travels with it.
    def verify(
      ctx:    GeneratorContext,
      digest: ExecutionDigest
    ): F[Either[OdbError, GuideService.GuideStarResolution]] =
      resolution:
        guideService.resolveGuideStar(oid, GuideService.GeneratorInfo.fromDigest(digest, ctx.params, ctx.guideStarHash))

    def generate(star: Option[GuideService.ResolvedGuideStar]): EitherT[F, OdbError, GeneratorContext] =
      EitherT(contextFor(pid, oid, withAltairGuideStar(params, star), commitHash))

    def go(number: Int, ctx: GeneratorContext): EitherT[F, OdbError, GeneratorContext] =
      for
        digest   <- digestOf(ctx)
        verified <- EitherT.liftF(verify(ctx, digest))
        settled  <- verified match
                      case Right(GuideService.GuideStarResolution.Resolved(guideStar = resolved)) =>
                        val next: GeneratorParams = withAltairGuideStar(params, resolved.some)
                        if next === ctx.params then EitherT.pure(ctx)
                        else if number >= MaxAltairGuideStarPasses then EitherT.liftF(Logger[F].warn(unsettled(oid))).as(ctx)
                        else generate(resolved.some).flatMap(go(number + 1, _))
                      case Right(GuideService.GuideStarResolution.NotGuided)                      =>
                        EitherT.pure(ctx)
                      case Right(GuideService.GuideStarResolution.NoUsableStar)                   =>
                        EitherT.pure(ctx.copy(altairProblem = noUsableStar(oid).some))
                      case Right(GuideService.GuideStarResolution.SelectedStarUnusable(name = n)) =>
                        EitherT.pure(ctx.copy(altairProblem = selectedStarUnusable(oid, n).some))
                      case Left(problem)                                                          =>
                        EitherT.pure(ctx.copy(altairProblem = problem.some))
      yield settled

    val settled: EitherT[F, OdbError, GeneratorContext] =
      if params.altair.exists(_.mode === AltairMode.LgsP1) then generate(none).flatMap(go(1, _))
      else
        EitherT(pick).flatMap:
          case GuideService.GuideStarResolution.Resolved(guideStar = star)     =>
            generate(star.some).flatMap(go(1, _))
          // A calibration that does not guide is generated without Altair parameters.
          case GuideService.GuideStarResolution.NotGuided                      =>
            generate(none)
          case GuideService.GuideStarResolution.NoUsableStar                   =>
            EitherT.leftT(noUsableStar(oid))
          case GuideService.GuideStarResolution.SelectedStarUnusable(name = n) =>
            EitherT.leftT(selectedStarUnusable(oid, n))

    settled.value

  def lookup[F[_]: Concurrent: Logger](
    oid:        Observation.Id,
    commitHash: CommitHash
  )(using NoTransaction[F], Services[F]): F[Either[OdbError, GeneratorContext]] =
    (for
      pp           <- EitherT(selectParams(oid))
      (pid, params) = pp
      ctx          <- EitherT:
                        // Only Altair feeds the guide star back into the sequence; everything else
                        // is generated once.
                        if params.altair.isDefined then settleAltairGuideStar(pid, oid, commitHash, params)
                        else contextFor(pid, oid, params, commitHash)
    yield ctx).value
