// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gmos.ifu

import cats.Eq
import cats.derived.*
import lucuma.core.enums.GmosIfuAcquisitionRoi
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.Enumerated
import lucuma.odb.sequence.syntax.hash.*

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream

/**
 * Configuration for the GMOS IFU acquisition.
 *
 * As for MOS, the default filter is the acquisition filter nearest the central
 * wavelength; it is computed in the database view and passed in here, as is the
 * default ROI, which depends on the observation's calibration role.
 *
 * @tparam L filter type
 */
sealed trait AcquisitionConfig[L: Enumerated] extends Product with Serializable:

  def exposureTimeMode: ExposureTimeMode

  def filter: L =
    explicitFilter.getOrElse(defaultFilter)

  def defaultFilter: L

  def explicitFilter: Option[L]

  def roi: GmosIfuAcquisitionRoi =
    explicitRoi.getOrElse(defaultRoi)

  def defaultRoi: GmosIfuAcquisitionRoi

  def explicitRoi: Option[GmosIfuAcquisitionRoi]

  def hashBytes: Array[Byte] =
    val bao: ByteArrayOutputStream = new ByteArrayOutputStream(256)
    val out: DataOutputStream      = new DataOutputStream(bao)

    out.writeChars(Enumerated[L].tag(filter))
    out.writeChars(roi.tag)
    out.write(exposureTimeMode.hashBytes)

    out.close()
    bao.toByteArray

object AcquisitionConfig:


  final case class GmosNorth(
    exposureTimeMode: ExposureTimeMode,
    defaultFilter:    GmosNorthFilter,
    explicitFilter:   Option[GmosNorthFilter],
    defaultRoi:       GmosIfuAcquisitionRoi,
    explicitRoi:      Option[GmosIfuAcquisitionRoi]
  ) extends AcquisitionConfig[GmosNorthFilter] derives Eq

  final case class GmosSouth(
    exposureTimeMode: ExposureTimeMode,
    defaultFilter:    GmosSouthFilter,
    explicitFilter:   Option[GmosSouthFilter],
    defaultRoi:       GmosIfuAcquisitionRoi,
    explicitRoi:      Option[GmosIfuAcquisitionRoi]
  ) extends AcquisitionConfig[GmosSouthFilter] derives Eq
