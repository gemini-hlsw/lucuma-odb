// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos
package ifu

import cats.data.NonEmptyList
import cats.syntax.option.*
import lucuma.core.enums.CalibrationRole
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
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.math.Wavelength
import lucuma.core.math.syntax.int.*
import lucuma.core.model.Observation
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.optics.syntax.lens.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.gmos.spectroscopy.AcquisitionAtoms
import lucuma.odb.sequence.gmos.spectroscopy.AcquisitionSteps
import lucuma.odb.sequence.util.AtomBuilder

import java.util.UUID

/**
 * GMOS IFU acquisition sequence generation.
 *
 * An unmasked 2x2 field step on CCD2 to find the target, then an unbinned Full
 * Frame step through the IFU to confirm it lands on the science field.  Both
 * take the acquisition filter and exposure time, with no grating and no offset;
 * only the through-IFU step is repeated.
 *
 * Unlike the long slit there is no centering step: the IFU field is 7"x5"
 * rather than a slit width, so there is nothing to walk the target into.
 */
object Acquisition:

  private val ModeName: String =
    "GMOS IFU"

  val RepeatingAtomCount: Int = 10

  /**
   * The two step configurations a GMOS IFU acquisition uses.
   *
   * @param field the unmasked field image
   * @param ifu   the image taken through the IFU
   */
  case class Steps[D](
    field: ProtoStep[D],
    ifu:   ProtoStep[D]
  ):
    /** The step grouping, without the breakpoint (placed by `AcquisitionAtoms`). */
    def acquisitionSteps: AcquisitionSteps[D] =
      AcquisitionSteps(NonEmptyList.of(field, ifu), NonEmptyList.of(ifu))

  private sealed trait StepComputer[D, G, L, U] extends GmosSequenceState[D, G, L, U]:

    def compute(
      acqConfig:    AcquisitionConfig[L],
      fpu:          U,
      exposureTime: TimeSpan
    ): Acquisition.Steps[D] =
      val readMode = AcquisitionAtoms.readMode(exposureTime)

      eval:
        for
          _  <- optics.exposure    := exposureTime
          _  <- optics.filter      := acqConfig.filter.some
          _  <- optics.fpu         := none[GmosFpuMask[U]]
          _  <- optics.grating     := none[(G, GmosGratingOrder, Wavelength)]
          _  <- optics.xBin        := GmosXBinning.Two
          _  <- optics.yBin        := GmosYBinning.Two
          _  <- optics.ampReadMode := readMode
          _  <- optics.roi         := GmosRoi.Ccd2
          s0 <- scienceStep(0.arcsec, 0.arcsec, ObserveClass.Acquisition)

          _  <- optics.fpu         := GmosFpuMask.Builtin(fpu).some
          _  <- optics.xBin        := GmosXBinning.One
          _  <- optics.yBin        := GmosYBinning.One
          _  <- optics.roi         := GmosRoi.FullFrame
          s1 <- scienceStep(0.arcsec, 0.arcsec, ObserveClass.Acquisition)
        yield Acquisition.Steps(s0, s1)

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
      t => StepComputer.North.compute(config.acquisition, config.builtinFpu, t).acquisitionSteps,
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
      t => StepComputer.South.compute(config.acquisition, config.builtinFpu, t).acquisitionSteps,
      RepeatingAtomCount
    )

end Acquisition
