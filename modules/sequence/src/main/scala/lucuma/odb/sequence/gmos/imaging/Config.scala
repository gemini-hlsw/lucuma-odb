// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos.imaging

import cats.Eq
import cats.data.NonEmptyList
import lucuma.core.enums.GmosAmpCount
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosBinning
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.MultipleFiltersMode
import lucuma.core.math.Offset
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.util.Enumerated
import lucuma.odb.sequence.util.HashBytes

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream

/**
 * Configuration for the GMOS Imaging science mode.
 * @tparam L filter type
 */
sealed trait Config[L: Enumerated]:

  def filters: NonEmptyList[L]

  def multipleFiltersMode: MultipleFiltersMode =
    explicitMultipleFiltersMode.getOrElse(Config.DefaultMultipleFiltersMode)

  def explicitMultipleFiltersMode: Option[MultipleFiltersMode]

  def bin: GmosBinning =
    explicitBin.getOrElse(defaultBin)

  def defaultBin: GmosBinning

  def explicitBin: Option[GmosBinning]

  def ampReadMode: GmosAmpReadMode =
    explicitAmpReadMode.getOrElse(Config.DefaultAmpReadMode)

  def explicitAmpReadMode: Option[GmosAmpReadMode]

  def ampGain: GmosAmpGain =
    explicitAmpGain.getOrElse(Config.DefaultAmpGain)

  def explicitAmpGain: Option[GmosAmpGain]

  def roi: GmosRoi =
    explicitRoi.getOrElse(Config.DefaultRoi)

  def explicitRoi: Option[GmosRoi]

  def offsets: List[Offset]

  def ccdMode: GmosCcdMode =
    GmosCcdMode(
      GmosXBinning(bin),
      GmosYBinning(bin),
      Config.DefaultAmpCount,
      ampGain,
      ampReadMode
    )

  def hashBytes: Array[Byte] = {
    val bao: ByteArrayOutputStream = new ByteArrayOutputStream(256)
    val out: DataOutputStream      = new DataOutputStream(bao)

    filters.toList.foreach(f => out.writeChars(Enumerated[L].tag(f)))
    out.writeChars(bin.tag)
    out.writeChars(ampGain.tag)
    out.writeChars(ampReadMode.tag)
    out.writeChars(roi.tag)
    offsets.foreach { o =>
      out.writeLong(o.p.toAngle.toMicroarcseconds)
      out.writeLong(o.q.toAngle.toMicroarcseconds)
    }

    out.close()
    bao.toByteArray
  }

object Config:

  private val DefaultMultipleFiltersMode = MultipleFiltersMode.Grouped
  private val DefaultAmpReadMode         = GmosAmpReadMode.Slow
  private val DefaultAmpGain             = GmosAmpGain.Low
  private val DefaultRoi                 = GmosRoi.FullFrame
  private val DefaultAmpCount            = GmosAmpCount.Twelve

  final case class Common(
    defaultBin:                  GmosBinning,
    explicitBin:                 Option[GmosBinning]         = None,
    explicitMultipleFiltersMode: Option[MultipleFiltersMode] = None,
    explicitAmpReadMode:         Option[GmosAmpReadMode]     = None,
    explicitAmpGain:             Option[GmosAmpGain]         = None,
    explicitRoi:                 Option[GmosRoi]             = None,
    offsets:                     List[Offset]                = Nil
  )

  object Common:
    given Eq[Common] =
      Eq.by: a =>
        (
          a.defaultBin,
          a.explicitBin,
          a.explicitMultipleFiltersMode,
          a.explicitAmpReadMode,
          a.explicitAmpGain,
          a.explicitRoi,
          a.offsets
        )

  val DefaultOffsets: List[Offset] = Nil

  final case class GmosNorth (
    filters: NonEmptyList[GmosNorthFilter],
    common:  Common
  ) extends Config[GmosNorthFilter]:

    override def explicitMultipleFiltersMode: Option[MultipleFiltersMode] =
      common.explicitMultipleFiltersMode

    override def defaultBin: GmosBinning =
      common.defaultBin

    override def explicitBin: Option[GmosBinning] =
      common.explicitBin

    override def explicitAmpReadMode: Option[GmosAmpReadMode] =
      common.explicitAmpReadMode

    override def explicitAmpGain: Option[GmosAmpGain] =
      common.explicitAmpGain

    override def explicitRoi: Option[GmosRoi] =
      common.explicitRoi

    override def offsets: List[Offset] =
      common.offsets

  object GmosNorth:

    given Eq[GmosNorth] =
      Eq.by: a =>
        (
          a.filters,
          a.common
        )

    given HashBytes[GmosNorth] = _.hashBytes

  final case class GmosSouth(
    filters: NonEmptyList[GmosSouthFilter],
    common:  Common
  ) extends Config[GmosSouthFilter]:

    override def explicitMultipleFiltersMode: Option[MultipleFiltersMode] =
      common.explicitMultipleFiltersMode

    override def defaultBin: GmosBinning =
      common.defaultBin

    override def explicitBin: Option[GmosBinning] =
      common.explicitBin

    override def explicitAmpReadMode: Option[GmosAmpReadMode] =
      common.explicitAmpReadMode

    override def explicitAmpGain: Option[GmosAmpGain] =
      common.explicitAmpGain

    override def explicitRoi: Option[GmosRoi] =
      common.explicitRoi

    override def offsets: List[Offset] =
      common.offsets

  object GmosSouth:

    given Eq[GmosSouth] =
      Eq.by: a =>
        (
          a.filters,
          a.common
        )

    given HashBytes[GmosSouth] = _.hashBytes