// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gmos

import lucuma.core.enums.*
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.model.sequence.gmos.GmosGratingConfig
import lucuma.core.util.TimeSpan
import monocle.Iso
import monocle.Lens
import monocle.Optional

/**
 * Abstracts the optics for updating GMOS dynamic (changing) configurations,
 * enabling sequence state changes from step to step to be written once for
 * GMOS north and south.
 *
 * @tparam D dynamic configuration type (i.e., GMOS North or GMOS South)
 * @tparam G grating type
 * @tparam F filter type
 * @tparam U FPU type
 */
trait DynamicOptics[D, G, F, U] {
  def exposure: Lens[D, TimeSpan]
  def readout: Lens[D, GmosCcdMode]

  lazy val xBin: Lens[D, GmosXBinning] =
    readout andThen GmosCcdMode.xBin

  lazy val yBin: Lens[D, GmosYBinning] =
    readout andThen GmosCcdMode.yBin

  lazy val ampReadMode: Lens[D, GmosAmpReadMode] =
    readout andThen GmosCcdMode.ampReadMode

  lazy val ampGain: Lens[D, GmosAmpGain] =
    readout andThen GmosCcdMode.ampGain

  def dtax: Lens[D, GmosDtax]

  def roi: Lens[D, GmosRoi]

  def grating: Lens[D, Option[(G, GmosGratingOrder, Wavelength)]]

  def wavelength: Optional[D, Wavelength]

  def filter: Lens[D, Option[F]]

  def fpu: Lens[D, Option[GmosFpuMask[U]]]

}

object DynamicOptics {

  val North: DynamicOptics[GmosNorth, GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] =
    new DynamicOptics[GmosNorth, GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] {
      val exposure: Lens[GmosNorth, TimeSpan]    = GmosNorth.exposure
      val readout:  Lens[GmosNorth, GmosCcdMode] = GmosNorth.readout
      val dtax:     Lens[GmosNorth, GmosDtax]    = GmosNorth.dtax
      val roi:      Lens[GmosNorth, GmosRoi]     = GmosNorth.roi

      private val isoGrating: Iso[GmosGratingConfig.North, (GmosNorthGrating, GmosGratingOrder, Wavelength)] =
        // GenIso.fields[GmosGratingConfig.North] (deprecated, seemingly no replacement)
        Iso[GmosGratingConfig.North, (GmosNorthGrating, GmosGratingOrder, Wavelength)] { s =>
          (s.grating, s.order, s.wavelength)
        } { case (g, o, w) => GmosGratingConfig.North(g, o, w) }

      val grating: Lens[GmosNorth, Option[(GmosNorthGrating, GmosGratingOrder, Wavelength)]] =
        Lens[GmosNorth, Option[(GmosNorthGrating, GmosGratingOrder, Wavelength)]](
          gn   => GmosNorth.gratingConfig.get(gn).map(isoGrating.get)
        )(
          otup => GmosNorth.gratingConfig.replace(otup.map(isoGrating.reverseGet))
        )

      val wavelength: Optional[GmosNorth, Wavelength] =
        GmosNorth.gratingConfig.some andThen GmosGratingConfig.North.wavelength

      val filter: Lens[GmosNorth, Option[GmosNorthFilter]]        = GmosNorth.filter
      val fpu: Lens[GmosNorth, Option[GmosFpuMask[GmosNorthFpu]]] = GmosNorth.fpu
    }

  val South: DynamicOptics[GmosSouth, GmosSouthGrating, GmosSouthFilter, GmosSouthFpu] =
    new DynamicOptics[GmosSouth, GmosSouthGrating, GmosSouthFilter, GmosSouthFpu] {
      val exposure: Lens[GmosSouth, TimeSpan]    = GmosSouth.exposure
      val readout:  Lens[GmosSouth, GmosCcdMode] = GmosSouth.readout
      val dtax:     Lens[GmosSouth, GmosDtax]    = GmosSouth.dtax
      val roi:      Lens[GmosSouth, GmosRoi]     = GmosSouth.roi

      private val isoGrating: Iso[GmosGratingConfig.South, (GmosSouthGrating, GmosGratingOrder, Wavelength)] =
        // GenIso.fields[GmosGratingConfig.South]  (deprecated, seemingly no replacement)
        Iso[GmosGratingConfig.South, (GmosSouthGrating, GmosGratingOrder, Wavelength)] { s =>
          (s.grating, s.order, s.wavelength)
        } { case (g, o, w) => GmosGratingConfig.South(g, o, w) }


      val grating: Lens[GmosSouth, Option[(GmosSouthGrating, GmosGratingOrder, Wavelength)]] =
        Lens[GmosSouth, Option[(GmosSouthGrating, GmosGratingOrder, Wavelength)]](
          gs   => GmosSouth.gratingConfig.get(gs).map(isoGrating.get)
        )(
          otup => GmosSouth.gratingConfig.replace(otup.map(isoGrating.reverseGet))
        )

      val wavelength: Optional[GmosSouth, Wavelength] =
        GmosSouth.gratingConfig.some andThen GmosGratingConfig.South.wavelength

      val filter: Lens[GmosSouth, Option[GmosSouthFilter]]        = GmosSouth.filter
      val fpu: Lens[GmosSouth, Option[GmosFpuMask[GmosSouthFpu]]] = GmosSouth.fpu
    }

}
