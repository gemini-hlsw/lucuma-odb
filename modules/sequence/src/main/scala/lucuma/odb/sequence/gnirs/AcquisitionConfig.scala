// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
 */
case class AcquisitionConfig(
  explicitAcqMode:  Option[GnirsAcquisitionMode],
  explicitFilter:   Option[GnirsFilter],
  exposureTimeMode: ExposureTimeMode,
  coadds:           PosInt
):

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
    out.write(exposureTimeMode.hashBytes)
    out.close()
    bao.toByteArray

object AcquisitionConfig:
  given Eq[AcquisitionConfig] =
    Eq.by: a =>
      (a.explicitAcqMode, a.explicitFilter, a.coadds.value, a.exposureTimeMode)
