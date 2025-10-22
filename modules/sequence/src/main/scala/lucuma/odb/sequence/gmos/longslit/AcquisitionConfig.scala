// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gmos.longslit

import cats.Eq
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.Enumerated
import lucuma.odb.sequence.syntax.hash.*

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream

/**
 * Configuration for the GMOS Long Slit acquisition.
 *
 * @tparam L filter type
 */
sealed trait AcquisitionConfig[L: Enumerated] extends Product with Serializable:

  def exposureTimeMode: ExposureTimeMode

  def filter: L =
    explicitFilter.getOrElse(defaultFilter)

  def defaultFilter: L

  def explicitFilter: Option[L]

  def hashBytes: Array[Byte] =
    val bao: ByteArrayOutputStream = new ByteArrayOutputStream(256)
    val out: DataOutputStream      = new DataOutputStream(bao)

    out.writeChars(Enumerated[L].tag(filter))
    out.write(exposureTimeMode.hashBytes)

    out.close()
    bao.toByteArray

object AcquisitionConfig:

  final case class GmosNorth(
    exposureTimeMode: ExposureTimeMode,
    defaultFilter:    GmosNorthFilter,
    explicitFilter:    Option[GmosNorthFilter]
  ) extends AcquisitionConfig[GmosNorthFilter]

  object GmosNorth:

    given Eq[GmosNorth] =
      Eq.by: a =>
        (
          a.exposureTimeMode,
          a.defaultFilter,
          a.explicitFilter
        )

  final case class GmosSouth(
    exposureTimeMode: ExposureTimeMode,
    defaultFilter:    GmosSouthFilter,
    explicitFilter:    Option[GmosSouthFilter]
  ) extends AcquisitionConfig[GmosSouthFilter]

  object GmosSouth:

    given Eq[GmosSouth] =
      Eq.by: a =>
        (
          a.exposureTimeMode,
          a.defaultFilter,
          a.explicitFilter
        )