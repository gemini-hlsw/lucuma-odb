// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.flamingos2.spectroscopy

import cats.Eq
import lucuma.core.enums.Flamingos2AcquisitionType
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.math.Offset
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.sequence.flamingos2.Flamingos2AcquisitionMode
import lucuma.odb.sequence.syntax.hash.*

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream


/**
 * Acquisition parameters shared by the Flamingos 2 long slit and MOS modes.
 *
 * An `explicitMode` of `None` leaves the choice between a Bright and a Faint
 * acquisition to the ITC's classification of the acquired targets.
 */
final case class AcquisitionConfig(
  exposureTimeMode: ExposureTimeMode,
  defaultFilter:    Flamingos2Filter,
  explicitFilter:   Option[Flamingos2Filter],
  explicitMode:     Option[Flamingos2AcquisitionMode] = None
):

  def filter: Flamingos2Filter =
    explicitFilter.getOrElse(defaultFilter)

  /**
   * The acquisition mode: the explicit choice if set, else the type the ITC
   * pinned when it classified the target, with the default sky offset when that
   * is Faint. `unclassified` covers results that predate the classification.
   */
  def resolvedMode(
    pinnedType:   Option[Flamingos2AcquisitionType],
    unclassified: => Flamingos2AcquisitionMode
  ): Flamingos2AcquisitionMode =
    explicitMode.getOrElse:
      pinnedType.fold(unclassified):
        Flamingos2AcquisitionMode.forTypeAndOffset(_, Flamingos2AcquisitionMode.Faint.DefaultSkyOffset)

  def hashBytes: Array[Byte] =
    val bao: ByteArrayOutputStream = new ByteArrayOutputStream(256)
    val out: DataOutputStream      = new DataOutputStream(bao)

    out.writeChars(filter.tag)
    out.write(exposureTimeMode.hashBytes)
    // Nothing is written for an automatic mode, which keeps the hash of every
    // configuration that predates the explicit mode.
    explicitMode.foreach: mode =>
      out.writeChars(mode.acquisitionType.tag)
      Flamingos2AcquisitionMode.skyOffset.getOption(mode).foreach(o => out.write(o.hashBytes))

    out.close()
    bao.toByteArray

object AcquisitionConfig:

  /**
   * The explicit mode for an explicit acquisition type. A Faint type always
   * comes with a sky offset; the default only guards against a missing one.
   */
  def explicitMode(
    acquisitionType: Option[Flamingos2AcquisitionType],
    skyOffset:       Option[Offset]
  ): Option[Flamingos2AcquisitionMode] =
    acquisitionType.map: t =>
      Flamingos2AcquisitionMode.forTypeAndOffset(t, skyOffset.getOrElse(Flamingos2AcquisitionMode.Faint.DefaultSkyOffset))

  given Eq[AcquisitionConfig] =
    Eq.by: a =>
      (
        a.exposureTimeMode,
        a.defaultFilter,
        a.explicitFilter,
        a.explicitMode
      )
