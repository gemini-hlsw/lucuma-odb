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
import lucuma.itc.IntegrationTime
import lucuma.odb.sequence.syntax.all.*

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream

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
        GnirsAcquisitionMode.defaultFor(time.exposureTime, resolvedCoadds(time)).acquisitionType
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
   * Coadds for the acquisition steps. In S/N mode the ITC sizes the acquisition, so we
   * use its exposure count — the number of exposures needed to reach the target S/N — as
   * the coadds. In time-and-count mode the user controls the acquisition directly, so the
   * explicit coadds are used.
   */
  def resolvedCoadds(time: IntegrationTime): PosInt =
    exposureTimeMode match
      case ExposureTimeMode.SignalToNoiseMode(_, _)   => time.exposureCount
      case ExposureTimeMode.TimeAndCountMode(_, _, _) => coadds

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
   * Compares the ITC-facing exposure time mode rather than the effective one, so that two
   * configurations differing only in a derived signal-to-noise are equal. This keeps
   * `CalibrationConfigSubset` from re-syncing telluric standards every time a
   * classification lands.
   */
  given Eq[AcquisitionConfig] =
    Eq.by: a =>
      (a.explicitAcqMode, a.explicitFilter, a.coadds.value, a.itcExposureTimeMode)
