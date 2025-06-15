// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos.imaging

import cats.Eq
import cats.syntax.eq.*
import cats.syntax.option.*
import lucuma.core.enums.GmosAmpCount
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.util.Enumerated
import lucuma.odb.sequence.ObservingMode
import lucuma.odb.sequence.util.HashBytes

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream
import cats.data.NonEmptyList
import lucuma.core.enums.GmosBinning

/**
 * Configuration for the GMOS Imaging science mode.
 * @tparam L filter type
 */
sealed trait Config[L: Enumerated] {

  def filters: NonEmptyList[L]

  def bin: GmosBinning =
    explicitBin.getOrElse(defaultBin)

  def defaultBin: GmosBinning =
    Config.DefaultBin

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

  def hashBytes: Array[Byte] = {
    val bao: ByteArrayOutputStream = new ByteArrayOutputStream(256)
    val out: DataOutputStream      = new DataOutputStream(bao)

    filters.toList.foreach(f => out.writeChars(Enumerated[L].tag(f)))
    out.writeChars(bin.tag)
    out.writeChars(ampGain.tag)
    out.writeChars(ampReadMode.tag)
    out.writeChars(roi.tag)

    out.close()
    bao.toByteArray
  }

}

object Config {

  private val DefaultBin         = GmosBinning.Two
  private val DefaultAmpReadMode = GmosAmpReadMode.Slow
  private val DefaultAmpGain     = GmosAmpGain.Low
  private val DefaultRoi         = GmosRoi.FullFrame
  private val DefaultAmpCount    = GmosAmpCount.Twelve

  final case class GmosNorth(
    filters:             NonEmptyList[GmosNorthFilter],
    explicitBin:         Option[GmosBinning],
    explicitAmpReadMode: Option[GmosAmpReadMode],
    explicitAmpGain:     Option[GmosAmpGain],
    explicitRoi:         Option[GmosRoi]
  ) extends Config[GmosNorthFilter]

  object GmosNorth {

    def apply(
      filters:             NonEmptyList[GmosNorthFilter],
      explicitBin:         Option[GmosBinning]     = None,
      explicitAmpReadMode: Option[GmosAmpReadMode] = None,
      explicitAmpGain:     Option[GmosAmpGain]     = None,
      explicitRoi:         Option[GmosRoi]         = None
    ): GmosNorth =
      GmosNorth(
        filters,
        explicitBin,
        explicitAmpReadMode,
        explicitAmpGain,
        explicitRoi
      )

    def reconcile(a: GmosNorth, modes: List[ObservingMode]): Option[GmosNorth] =
      modes.headOption match {
        case None =>
          a.some

        case Some(b: GmosNorth) =>
          if (a === b)
            reconcile(a, modes.tail)
          else
            none

        case _ =>
          none
      }

    given Eq[GmosNorth] =
      Eq.by { a => (
        a.filters,
        a.explicitBin,
        a.explicitAmpReadMode,
        a.explicitAmpGain,
        a.explicitRoi
      )}

    given HashBytes[GmosNorth] with {
      def hashBytes(a: GmosNorth): Array[Byte] = a.hashBytes
    }

  }

  final case class GmosSouth(
    filters:             NonEmptyList[GmosSouthFilter],
    explicitBin:         Option[GmosBinning],
    explicitAmpReadMode: Option[GmosAmpReadMode],
    explicitAmpGain:     Option[GmosAmpGain],
    explicitRoi:         Option[GmosRoi]
  ) extends Config[GmosSouthFilter]

  object GmosSouth {

    def apply(
      filters:             NonEmptyList[GmosSouthFilter],
      explicitBin:         Option[GmosBinning]     = None,
      explicitAmpReadMode: Option[GmosAmpReadMode] = None,
      explicitAmpGain:     Option[GmosAmpGain]     = None,
      explicitRoi:         Option[GmosRoi]         = None
    ): GmosSouth =
      GmosSouth(
        filters,
        explicitBin,
        explicitAmpReadMode,
        explicitAmpGain,
        explicitRoi
      )

    def reconcile(a: GmosSouth, modes: List[ObservingMode]): Option[GmosSouth] =
      modes.headOption match {
        case None =>
          a.some

        case Some(b: GmosSouth) =>
          if (a === b)
            reconcile(a, modes.tail)
          else
            none

        case _ =>
          none
      }

    given Eq[GmosSouth] =
      Eq.by { a => (
        a.filters,
        a.explicitBin,
        a.explicitAmpReadMode,
        a.explicitAmpGain,
        a.explicitRoi
      )}

    given HashBytes[GmosSouth] with {
      def hashBytes(a: GmosSouth): Array[Byte] = a.hashBytes
    }

  }

}
