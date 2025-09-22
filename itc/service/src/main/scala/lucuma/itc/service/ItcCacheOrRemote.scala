// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import boopickle.DefaultBasic.*
import buildinfo.BuildInfo
import cats.*
import cats.syntax.all.*
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition.BandNormalized
import lucuma.core.model.UnnormalizedSED
import lucuma.itc.*
import lucuma.itc.cache.BinaryEffectfulCache
import lucuma.itc.input.customSed.CustomSed
import lucuma.itc.service.config.Config
import lucuma.itc.service.redis.given
import lucuma.itc.service.requests.*
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.*

/**
 * Methods to check if a values is on the cache and if not retrieve them from old itc and store them
 * in the cache
 */
trait ItcCacheOrRemote extends Version:
  // Manual version to manually force a flush
  private val ManualCacheVersion = 1
  private val CacheRootPrefix    = "itc"
  val VersionKeyRoot: String     = "version"

  private val versionKey: String =
    s"$ManualCacheVersion-${BuildInfo.ocslibHash}"

  // Time to live for entries. The idea is that eviction is based on LRU and cache size restriction.
  // Redis should be configured to use the `volatile-lru` eviction policy. This will work better
  // than `allkeys-lru` since it will prevent the version key from being evicted, which will
  // cause the cache to flush.
  private def TTL(config: Config): Option[FiniteDuration] =
    FiniteDuration(config.cacheTtlDays, DAYS).some

  private def customSedCotent(targetData: TargetData): Option[String] =
    def describeSed(sedOpt: Option[UnnormalizedSED]): Option[String] =
      sedOpt.collect:
        case UnnormalizedSED.UserDefined(fluxDensities) =>
          val header = s"UserDefined SED with ${fluxDensities.size} flux density points"
          val lines  = fluxDensities.toNel.map: (wv, f) =>
            f"${Wavelength.nanometers.reverseGet(wv)}%.1f $f%.3f"
          (header :: lines).toList.mkString("\n")

        case UnnormalizedSED.UserDefinedAttachment(id) =>
          s"UserDefinedAttachment SED with attachment ID: $id"

    val profile = targetData.sourceProfile

    val integratedSed = SourceProfile.integratedBandNormalizedSpectralDefinition
      .andThen(BandNormalized.sed)
      .getOption(profile)
      .map(describeSed)
      .flatten
    val surfaceSed    = SourceProfile.surfaceBandNormalizedSpectralDefinition
      .andThen(BandNormalized.sed)
      .getOption(profile)
      .map(describeSed)
      .flatten

    integratedSed.orElse(surfaceSed)

  private def logSedOnFailure[F[_]: MonadThrow: Logger](
    request: ServiceRequest
  ): PartialFunction[Throwable, F[Unit]] =
    case error =>
      customSedCotent(request.target)
        .map: msg =>
          Logger[F].error(s"""|
            |ITC Calculation Failed for Custom SED:
            |Request: ${request}
            |Custom SED Data:
            |$msg
            |Error: ${error.getMessage}""".stripMargin)
        .getOrElse(Applicative[F].unit)

  private def requestGraphs[F[_]: MonadThrow: Logger](
    itc: Itc[F]
  )(request: TargetGraphRequest): F[TargetGraphsCalcResult] =
    itc
      .calculateGraphs(
        request.target,
        request.atWavelength,
        request.specMode,
        request.constraints,
        request.expTime,
        request.exp
      )
      .onError:
        logSedOnFailure(request)

  /**
   * Request a graph
   */
  def graphsFromCacheOrRemote[F[_]: MonadThrow: Parallel: Logger: CustomSed.Resolver](
    request: TargetGraphRequest
  )(
    itc:     Itc[F],
    cache:   BinaryEffectfulCache[F],
    config:  Config
  ): F[TargetGraphsCalcResult] =
    CustomSed // We must resolve CustomSed before caching.
      .resolveTargetGraphRequest(request)
      .flatMap: r =>
        cache.getOrInvokeBinary(r,
                                requestGraphs(itc)(r),
                                TTL(config),
                                s"$CacheRootPrefix:graph:spec"
        )

  private def requestSpecSNCalc[F[_]: MonadThrow: Logger](itc: Itc[F])(
    calcRequest: TargetSpectroscopyTimeRequest,
    mode:        ExposureTimeMode.TimeAndCountMode
  ): F[TargetIntegrationTime] =
    itc
      .calculateSignalToNoise(
        calcRequest.target,
        mode.at,
        calcRequest.specMode,
        calcRequest.constraints,
        mode.time,
        mode.count
      )
      .onError:
        logSedOnFailure(calcRequest)

  private def requestSpecTimeCalc[F[_]: MonadThrow: Logger](itc: Itc[F])(
    calcRequest: TargetSpectroscopyTimeRequest,
    mode:        ExposureTimeMode.SignalToNoiseMode
  ): F[TargetIntegrationTime] =
    itc
      .calculateIntegrationTime(
        calcRequest.target,
        mode.at,
        calcRequest.specMode,
        calcRequest.constraints,
        mode.value
      )
      .onError:
        logSedOnFailure(calcRequest)

  /**
   * Request exposure time calculation for spectroscopy
   */
  def spectroscopyFromCacheOrRemote[F[
    _
  ]: MonadThrow: Parallel: Logger: CustomSed.Resolver](
    calcRequest: TargetSpectroscopyTimeRequest
  )(
    itc:         Itc[F],
    cache:       BinaryEffectfulCache[F],
    config:      Config
  ): F[TargetIntegrationTime] =
    CustomSed // We must resolve CustomSed before caching.
      .resolveTargetSpectroscopyTimeRequest(calcRequest)
      .flatMap: r =>
        r.exposureTimeMode match
          case m @ ExposureTimeMode.SignalToNoiseMode(_, _)   =>
            cache.getOrInvokeBinary(r,
                                    requestSpecTimeCalc(itc)(r, m),
                                    TTL(config),
                                    s"$CacheRootPrefix:calc:spec:sn"
            )
          case m @ ExposureTimeMode.TimeAndCountMode(_, _, _) =>
            cache.getOrInvokeBinary(r,
                                    requestSpecSNCalc(itc)(r, m),
                                    TTL(config),
                                    s"$CacheRootPrefix:calc:spec:tc"
            )

  private def requestImgTimeCalc[F[_]: MonadThrow: Logger](itc: Itc[F])(
    calcRequest: TargetImagingTimeRequest,
    mode:        ExposureTimeMode.SignalToNoiseMode
  ): F[TargetIntegrationTime] =
    itc
      .calculateIntegrationTime(
        calcRequest.target,
        mode.at,
        calcRequest.imagingMode,
        calcRequest.constraints,
        mode.value
      )
      .onError:
        logSedOnFailure(calcRequest)

  private def requestImgSNCalc[F[_]: MonadThrow: Logger](itc: Itc[F])(
    calcRequest: TargetImagingTimeRequest,
    mode:        ExposureTimeMode.TimeAndCountMode
  ): F[TargetIntegrationTime] =
    itc
      .calculateSignalToNoise(
        calcRequest.target,
        mode.at,
        calcRequest.imagingMode,
        calcRequest.constraints,
        mode.time,
        mode.count
      )
      .onError:
        logSedOnFailure(calcRequest)

  /**
   * Request exposure time calculation for imaging
   */
  def imagingFromCacheOrRemote[
    F[_]: MonadThrow: Parallel: Logger: CustomSed.Resolver
  ](
    calcRequest: TargetImagingTimeRequest
  )(
    itc:         Itc[F],
    cache:       BinaryEffectfulCache[F],
    config:      Config
  ): F[TargetIntegrationTime] =
    CustomSed // We must resolve CustomSed before caching.
      .resolveTargetImagingTimeRequest(calcRequest)
      .flatMap: r =>
        r.exposureTimeMode match
          case m @ ExposureTimeMode.SignalToNoiseMode(_, _)   =>
            cache.getOrInvokeBinary(r,
                                    requestImgTimeCalc(itc)(r, m),
                                    TTL(config),
                                    s"$CacheRootPrefix:calc:img:sn"
            )
          case m @ ExposureTimeMode.TimeAndCountMode(_, _, _) =>
            cache.getOrInvokeBinary(r,
                                    requestImgSNCalc(itc)(r, m),
                                    TTL(config),
                                    s"$CacheRootPrefix:calc:img:tc"
            )

  /**
   * This method will get the version from the remote itc and compare it with the one on the cache.
   * If there is none in the cache we just store it. If the remote is different than the local then
   * flush the cache.
   */
  def checkVersionToPurge[F[_]: MonadThrow: Logger](
    cache: BinaryEffectfulCache[F]
  ): F[Unit] = {
    val L      = Logger[F]
    val result = for
      _              <- L.info("Check for stale cache")
      versionOnCache <-
        cache.readBinary[String, String](VersionKeyRoot, s"$CacheRootPrefix:version")
      _              <- L.info(s"itc data checksum on cache ${versionOnCache.orEmpty}, version $versionKey")
      _              <-
        (L.info( // if the version changes or is missing, flush cache
          s"Flush cache on missing or changed ITC version key, set to [$versionKey]"
        ) *> cache.flush)
          .whenA(versionOnCache.forall(_ =!= versionKey))
      _              <- cache.writeBinary(VersionKeyRoot, versionKey, none, s"$CacheRootPrefix:version")
    yield ()
    result.handleErrorWith(e => L.error(e)("Error doing version check to purge"))
  }
