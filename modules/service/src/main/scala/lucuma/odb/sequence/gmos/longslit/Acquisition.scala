// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos
package longslit

import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.math.Wavelength
import lucuma.core.math.syntax.int.*
import lucuma.core.model.NonNegDuration
import lucuma.core.model.sequence.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.DynamicConfig.GmosSouth
import lucuma.core.model.sequence.GmosFpuMask
import lucuma.core.model.syntax.nonnegduration.*
import lucuma.core.optics.syntax.lens.*
import lucuma.core.optics.syntax.optional.*
import lucuma.core.syntax.time.*
import lucuma.odb.sequence.SequenceState
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.AcqExposureTime

import scala.collection.immutable.LazyList

sealed trait Acquisition[D, G, F, U] extends SequenceState[D] {

  def optics: DynamicOptics[D, G, F, U]

  def wavelength(f: F): Wavelength

  def compute(
    acqFilters:   NonEmptyList[F],
    fpu:          U,
    exposureTime: AcqExposureTime,
    λ:            Wavelength
  ): Acquisition.Steps[D] = {

    def filter: F = acqFilters.toList.minBy { f =>
      (λ.toPicometers.value.value - wavelength(f).toPicometers.value.value).abs
    }

    eval {
      for {
        _  <- optics.exposure      := exposureTime.duration
        _  <- optics.filter        := filter.some
        _  <- optics.fpu           := none[GmosFpuMask[U]]
        _  <- optics.grating       := none[(G, GmosGratingOrder, Wavelength)]
        _  <- optics.xBin          := GmosXBinning.Two
        _  <- optics.yBin          := GmosYBinning.Two
        _  <- optics.roi           := GmosRoi.Ccd2
        s0 <- scienceStep(0.arcsec, 0.arcsec)

        _  <- optics.exposure      := NonNegDuration.unsafeFrom(20.seconds)
        _  <- optics.fpu           := GmosFpuMask.Builtin(fpu).some
        _  <- optics.xBin          := GmosXBinning.One
        _  <- optics.yBin          := GmosYBinning.One
        _  <- optics.roi           := GmosRoi.CentralStamp
        s1 <- scienceStep(10.arcsec, 0.arcsec)

        _  <- optics.exposure      := (exposureTime * NonNegInt.unsafeFrom(4)).duration
        s2 <- scienceStep(0.arcsec, 0.arcsec)

      } yield Acquisition.Steps(s0, s1, s2)
    }
  }

}

object Acquisition {

  /**
   * Unique step configurations used to form an acquisition sequence.
   *
   * @param ccd2 image, 2x2 using CCD2 ROI
   * @param p10  20 second exposure, 1x1 Central Stamp, 10 arcsec offset in p
   * @param slit image through the slit
   */
  final case class Steps[D](
    ccd2: ProtoStep[D],
    p10:  ProtoStep[D],
    slit: ProtoStep[D]
  ) {

    val initialAtom: NonEmptyList[ProtoStep[D]] =
      NonEmptyList.of(ccd2, p10, slit)

    val repeatingAtom: NonEmptyList[ProtoStep[D]] =
      NonEmptyList.of(slit)

  }

  object GmosNorth extends GmosNorthInitialDynamicConfig
                      with Acquisition[GmosNorth, GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] {

    override def optics: DynamicOptics[GmosNorth, GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] =
      DynamicOptics.North

    override def wavelength(f: GmosNorthFilter): Wavelength =
      f.wavelength

  }

  object GmosSouth extends GmosSouthInitialDynamicConfig
                      with Acquisition[GmosSouth, GmosSouthGrating, GmosSouthFilter, GmosSouthFpu] {

    override def optics: DynamicOptics[GmosSouth, GmosSouthGrating, GmosSouthFilter, GmosSouthFpu] =
      DynamicOptics.South

    override def wavelength(f: GmosSouthFilter): Wavelength =
      f.wavelength

  }
}
