// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gnirs

import cats.Eq
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GnirsAcquisitionType
import lucuma.core.enums.GnirsFilter
import lucuma.core.math.Offset
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMode
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.sequence.syntax.all.*

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream
import scala.math.BigDecimal.RoundingMode

/**
 * Acquisition customization shared by the GNIRS observing modes. Every field is an
 * override of an otherwise automatic choice, except the exposure time mode and coadds
 * which always have a value.
 *
 * `explicitAcqMode` of `None` means the brightness classification is used; when it is
 * `Faint` it carries its own sky offset (whose default differs per mode). An
 * `explicitFilter` of `None` means the filter is derived per mode.
 *
 * `exposureTimeMode` is the *effective* mode: the user's when `explicitExposureTimeMode`
 * is set, otherwise the one derived from the brightness classification and maintained by
 * the ITC service. Because a derived value is a function of an ITC result, it must not
 * take part in configuration identity — see `itcExposureTimeMode`, which `hashBytes` and
 * `Eq` use in its place.
 */
case class AcquisitionConfig(
  explicitAcqMode:          Option[GnirsAcquisitionMode],
  explicitFilter:           Option[GnirsFilter],
  exposureTimeMode:         ExposureTimeMode,
  explicitExposureTimeMode: Boolean,
  coadds:                   PosInt
):

  /**
   * True when the acquisition signal-to-noise is derived from the ITC brightness
   * classification, i.e. the user set neither the exposure time mode nor the
   * acquisition type (an explicit type determines the S/N on its own, with no ITC).
   */
  def autoSignalToNoise: Boolean =
    !explicitExposureTimeMode && explicitAcqMode.isEmpty

  /**
   * The acquisition exposure time mode as presented to the ITC.
   *
   * When the S/N is derived, the effective value is whatever the last classification
   * produced — so putting it here would make the ITC input hash a function of the ITC
   * output, and every write would invalidate the result that produced it. A fixed
   * placeholder at the classification S/N stands in instead, and the real S/N is chosen
   * inside `ItcService.safeAcquisitionCall`'s second pass from the classification it just
   * computed. Do not "fix" this by using `exposureTimeMode`.
   */
  def itcExposureTimeMode: ExposureTimeMode =
    if autoSignalToNoise then
      ExposureTimeMode.SignalToNoiseMode(AcquisitionClassificationSignalToNoise, exposureTimeMode.at)
    else
      exposureTimeMode

  /**
   * The acquisition mode: the explicit choice if set, else the brightness
   * classification, carrying the given sky offset when that classification is Faint
   * (the default offset differs between long slit, IFU and imaging).
   *
   * The classification is `pinnedType` when the ITC resolved it (the two-pass
   * acquisition path), otherwise it is derived here from the integration time. Pinning
   * matters because the final (user-S/N) exposure time can misclassify — e.g. a Bright
   * target whose short exposure would otherwise read as Very Bright.
   */
  def resolvedMode(
    time:                  IntegrationTime,
    defaultFaintSkyOffset: Offset,
    pinnedType:            Option[GnirsAcquisitionType] = None
  ): GnirsAcquisitionMode =
    explicitAcqMode.getOrElse:
      val tpe: GnirsAcquisitionType = pinnedType.getOrElse:
        GnirsAcquisitionMode.defaultFor(time.exposureTime, classificationCoadds(time)).acquisitionType
      GnirsAcquisitionMode.forTypeAndOffset(tpe, defaultFaintSkyOffset)

  /**
   * The selected acquisition filter: the explicit filter if set, otherwise H2 for
   * VeryBright (low transmission), or the mode-supplied automatic choice for
   * Bright/Faint.
   */
  def selectedFilter(
    mode: GnirsAcquisitionMode,
    auto: => GnirsFilter
  ): GnirsFilter =
    explicitFilter match
      case Some(f) => f
      case None    =>
        mode match
          case GnirsAcquisitionMode.VeryBright => GnirsFilter.H2
          case _                               => auto

  /**
   * The exposure count the brightness classification sees: in S/N mode every single
   * exposure the ITC asked for (its frames times its coadds), in time-and-count mode the
   * explicit coadds. Unlike `resolvedCoadds` this is not raised to the integration floor,
   * which would otherwise hide a Very Bright target (0.3s x 1 is Very Bright, 0.3s x 17 is
   * not).
   */
  def classificationCoadds(time: IntegrationTime): PosInt =
    exposureTimeMode match
      case ExposureTimeMode.SignalToNoiseMode(_, _)   => time.totalExposureCount
      case ExposureTimeMode.TimeAndCountMode(_, _, _) => coadds

  /**
   * Coadds for the acquisition steps after the FPU image. In S/N mode every single exposure
   * the ITC asked for (its frames times its coadds) is folded into the coadds of one frame,
   * so the acquisition takes a single step (see
   * https://app.shortcut.com/lucuma/story/9189/gnirs-acquisition-coadds), and the count is
   * then raised if needed so that the step integrates for at least
   * `AcquisitionConfig.MinimumStepIntegration` (see
   * https://app.shortcut.com/lucuma/story/10747/augment-gnirs-coadding-in-acquisitions).
   * In time-and-count mode the user controls the acquisition directly, so the explicit
   * coadds are used as is.
   */
  def resolvedCoadds(time: IntegrationTime): PosInt =
    exposureTimeMode match
      case ExposureTimeMode.SignalToNoiseMode(_, _)   =>
        AcquisitionConfig.withMinimumIntegration(time.exposureTime, time.totalExposureCount)
      case ExposureTimeMode.TimeAndCountMode(_, _, _) =>
        coadds

  def hashBytes: Array[Byte] =
    val bao = new ByteArrayOutputStream(128)
    val out = new DataOutputStream(bao)
    // explicit acquisition mode (None => AUTO): tag byte + offset (Faint only)
    explicitAcqMode match
      case None                                  => out.writeByte(0)
      case Some(GnirsAcquisitionMode.VeryBright) => out.writeByte(1)
      case Some(GnirsAcquisitionMode.Bright)     => out.writeByte(2)
      case Some(GnirsAcquisitionMode.Faint(o))   =>
        out.writeByte(3)
        out.write(o.hashBytes)
    out.writeChars(explicitFilter.fold("")(_.tag))
    out.write(coadds.value.hashBytes)
    // The ITC-facing mode, not the effective one: a derived S/N must not change the hash.
    out.write(itcExposureTimeMode.hashBytes)
    out.close()
    bao.toByteArray

object AcquisitionConfig:

  /**
   * The least an acquisition step after the FPU image integrates for (coadds times
   * exposure), so that it averages over the image motion caused by atmospheric turbulence.
   */
  val MinimumStepIntegration: TimeSpan = 5.secTimeSpan

  /**
   * Raises the coadds of a step with the given exposure to `round(MinimumStepIntegration /
   * exposure)` when the step would integrate for less than `MinimumStepIntegration`. Rounding
   * (rather than ceiling) is the specified behavior: 3.3s x 1 becomes 2 x 3.3s while 3.4s x 1
   * stays 1 x 3.4s. The rounded quotient cannot fall below the given coadds, which the floor
   * check already found short of it.
   */
  def withMinimumIntegration(exposure: TimeSpan, coadds: PosInt): PosInt =
    val exposureµs: Long = exposure.toMicroseconds
    val floorµs: Long    = MinimumStepIntegration.toMicroseconds
    if exposureµs <= 0 || exposureµs * coadds.value >= floorµs then coadds
    else
      val rounded: Int = (BigDecimal(floorµs) / exposureµs).setScale(0, RoundingMode.HALF_UP).toInt
      PosInt.from(rounded).getOrElse(coadds)

  /**
   * Compares the ITC-facing exposure time mode rather than the effective one, so that two
   * configurations differing only in a derived signal-to-noise are equal. This keeps
   * `CalibrationConfigSubset` from re-syncing telluric standards every time a
   * classification lands.
   */
  given Eq[AcquisitionConfig] =
    Eq.by: a =>
      (a.explicitAcqMode, a.explicitFilter, a.coadds.value, a.itcExposureTimeMode)
