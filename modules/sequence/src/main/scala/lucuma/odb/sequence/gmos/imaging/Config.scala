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
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.util.Enumerated
import lucuma.odb.sequence.syntax.hash.*
import lucuma.odb.sequence.util.HashBytes

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream

/**
 * Configuration for the GMOS Imaging science mode.
 */
sealed trait Config[L: Enumerated]:

  def variant: Variant

  def filters: NonEmptyList[Filter[L]]

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

  def ccdMode: GmosCcdMode =
    GmosCcdMode(
      GmosXBinning(bin),
      GmosYBinning(bin),
      Config.DefaultAmpCount,
      ampGain,
      ampReadMode
    )

  def hashBytes: Array[Byte] =
    val bao: ByteArrayOutputStream = new ByteArrayOutputStream(256)
    val out: DataOutputStream      = new DataOutputStream(bao)

    out.write(variant.hashBytes)
    out.write(filters.hashBytes)
    out.writeChars(bin.tag)
    out.writeChars(ampGain.tag)
    out.writeChars(ampReadMode.tag)
    out.writeChars(roi.tag)

    out.close()
    bao.toByteArray

object Config:

  private val DefaultAmpReadMode         = GmosAmpReadMode.Slow
  private val DefaultAmpGain             = GmosAmpGain.Low
  private val DefaultRoi                 = GmosRoi.FullFrame
  private val DefaultAmpCount            = GmosAmpCount.Twelve

  final case class Common(
    defaultBin:          GmosBinning,
    explicitBin:         Option[GmosBinning]     = None,
    explicitAmpReadMode: Option[GmosAmpReadMode] = None,
    explicitAmpGain:     Option[GmosAmpGain]     = None,
    explicitRoi:         Option[GmosRoi]         = None
  )

  object Common:
    given Eq[Common] =
      Eq.by: a =>
        (
          a.defaultBin,
          a.explicitBin,
          a.explicitAmpReadMode,
          a.explicitAmpGain,
          a.explicitRoi
        )

  final case class GmosNorth (
    variant: Variant,
    filters: NonEmptyList[Filter[GmosNorthFilter]],
    common:  Common
  ) extends Config[GmosNorthFilter]:

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

  object GmosNorth:
    given Eq[GmosNorth] =
      Eq.by(a => (a.variant, a.filters, a.common))

    given HashBytes[GmosNorth] = _.hashBytes

  final case class GmosSouth(
    variant: Variant,
    filters: NonEmptyList[Filter[GmosSouthFilter]],
    common:  Common
  ) extends Config[GmosSouthFilter]:

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

  object GmosSouth:

    given Eq[GmosSouth] =
      Eq.by(a => (a.variant, a.filters, a.common))

    given HashBytes[GmosSouth] = _.hashBytes