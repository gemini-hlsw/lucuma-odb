// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos
package longslit

import cats.Order.catsKernelOrderingForOrder
import cats.data.NonEmptyList
import cats.syntax.option.*
import cats.syntax.order.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.math.syntax.int.*
import lucuma.core.model.Observation
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.optics.syntax.lens.*
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.gmos.spectroscopy.AcquisitionAtoms
import lucuma.odb.sequence.gmos.spectroscopy.AcquisitionSteps
import lucuma.odb.sequence.util.AtomBuilder

import java.util.UUID

object Acquisition:
  val AcquisitionSN: SignalToNoise =
    SignalToNoise.FromBigDecimalExact.getOption(10).get

  val MaxExpTimeLastStep = 360.secondTimeSpan

  val RepeatingAtomCount: Int = 10

  private val ModeName: String =
    "GMOS Long Slit"

  def filter[L](acqFilters: NonEmptyList[L], λ: Wavelength, wavelength: L => Wavelength): L =
    acqFilters.toList.minBy(filter => λ.diff(wavelength(filter)).abs)

  /**
   * Unique step configurations used to form an acquisition sequence.
   *
   * @param ccd2 image, 2x2 using CCD2 ROI
   * @param p10  20 second exposure, 1x1 Central Stamp, 10 arcsec offset in p
   * @param slit image through the slit
   */
  case class Steps[D](
    ccd2: ProtoStep[D],
    p10:  ProtoStep[D],
    slit: ProtoStep[D]
  ):
    def acquisitionSteps: AcquisitionSteps[D] =
      AcquisitionSteps(
        NonEmptyList.of(ccd2, p10, slit),
        NonEmptyList.of(slit)
      )

  private sealed trait StepComputer[D, G, L, U] extends GmosSequenceState[D, G, L, U]:

    def compute(
      acqConfig:    AcquisitionConfig[L],
      fpu:          U,
      exposureTime: TimeSpan
    ): Acquisition.Steps[D] =

      // Last step, max 360s
      // https://app.shortcut.com/lucuma/story/1999/determine-exposure-time-for-acquisition-images#activity-2516
      def lastExpTime(exposureTime: TimeSpan): TimeSpan =
        Acquisition.MaxExpTimeLastStep min
          TimeSpan.unsafeFromMicroseconds(exposureTime.toMicroseconds * 3)

      val readMode = AcquisitionAtoms.readMode(exposureTime)

      eval:
        for
          _  <- optics.exposure      := exposureTime
          _  <- optics.filter        := acqConfig.filter.some
          _  <- optics.fpu           := none[GmosFpuMask[U]]
          _  <- optics.grating       := none[(G, GmosGratingOrder, Wavelength)]
          _  <- optics.xBin          := GmosXBinning.Two
          _  <- optics.yBin          := GmosYBinning.Two
          _  <- optics.ampReadMode   := readMode
          _  <- optics.roi           := acqConfig.roi.imagingRoi
          s0 <- scienceStep(0.arcsec, 0.arcsec, ObserveClass.Acquisition)

          _  <- optics.exposure      := 20.secondTimeSpan
          _  <- optics.fpu           := GmosFpuMask.Builtin(fpu).some
          _  <- optics.xBin          := GmosXBinning.One
          _  <- optics.yBin          := GmosYBinning.One
          _  <- optics.ampReadMode   := GmosAmpReadMode.Fast
          _  <- optics.roi           := acqConfig.roi.slitRoi
          s1 <- scienceStep(10.arcsec, 0.arcsec, ObserveClass.Acquisition)

          _  <- optics.exposure      := lastExpTime(exposureTime)
          _  <- optics.ampReadMode   := readMode
          s2 <- scienceStep(0.arcsec, 0.arcsec, ObserveClass.Acquisition)
        yield Acquisition.Steps(s0, s1, s2)

    end compute

  end StepComputer

  private object StepComputer:

    object North extends GmosNorthSequenceState
                    with StepComputer[GmosNorth, GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]

    object South extends GmosSouthSequenceState
                    with StepComputer[GmosSouth, GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]

  end StepComputer

  def gmosNorth(
    observationId: Observation.Id,
    estimator:     StepTimeEstimateCalculator[StaticConfig.GmosNorth, GmosNorth],
    static:        StaticConfig.GmosNorth,
    namespace:     UUID,
    config:        Config.GmosNorth,
    time:          Either[OdbError, IntegrationTime],
    calRole:       Option[CalibrationRole]
  ): Either[OdbError, SequenceGenerator[GmosNorth]] =
    AcquisitionAtoms.instantiate(
      observationId,
      time,
      calRole,
      AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Acquisition),
      ModeName,
      t => StepComputer.North.compute(config.acquisition, config.fpu, t).acquisitionSteps,
      RepeatingAtomCount
    )

  def gmosSouth(
    observationId: Observation.Id,
    estimator:     StepTimeEstimateCalculator[StaticConfig.GmosSouth, GmosSouth],
    static:        StaticConfig.GmosSouth,
    namespace:     UUID,
    config:        Config.GmosSouth,
    time:          Either[OdbError, IntegrationTime],
    calRole:       Option[CalibrationRole]
  ): Either[OdbError, SequenceGenerator[GmosSouth]] =
    AcquisitionAtoms.instantiate(
      observationId,
      time,
      calRole,
      AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Acquisition),
      ModeName,
      t => StepComputer.South.compute(config.acquisition, config.fpu, t).acquisitionSteps,
      RepeatingAtomCount
    )

end Acquisition
