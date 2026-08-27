// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.flamingos2.longslit

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.foldable.*
import lucuma.core.enums.Flamingos2Decker
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.Flamingos2ReadoutMode
import lucuma.core.enums.Flamingos2Reads
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.TelluricType
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.TelescopeConfigAlongSlit
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.odb.sequence.flamingos2.spectroscopy
import lucuma.odb.sequence.flamingos2.spectroscopy.AcquisitionConfig
import lucuma.odb.sequence.flamingos2.spectroscopy.Config.Common
import lucuma.odb.sequence.syntax.all.*

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream

/**
 * Configuration for the Flamingos2 Long Slit science mode.  Using these parameters, a
 * F2 long slit sequence may be generated.
 */
case class Config(
  disperser:           Flamingos2Disperser,
  filter:              Flamingos2Filter,
  fpu:                 Flamingos2Fpu,
  acquisition:         AcquisitionConfig,
  common:              Common,
  telluricScienceMode: Option[ObservingModeType]
) extends spectroscopy.Config derives Eq:

  override def fpuMask: Flamingos2FpuMask =
    Flamingos2FpuMask.Builtin(fpu)

  override def gcalFpu: Flamingos2Fpu =
    fpu

  def hashBytes: Array[Byte] =
    val bao: ByteArrayOutputStream = new ByteArrayOutputStream(256)
    val out: DataOutputStream      = new DataOutputStream(bao)

    out.writeChars(disperser.tag)
    out.writeChars(filter.tag)
    out.writeChars(fpu.tag)
    out.write(exposureTimeMode.hashBytes)
    out.write(acquisition.hashBytes)
    out.writeChars(explicitReadMode.foldMap(_.tag))
    out.writeChars(explicitReads.foldMap(_.tag))
    out.writeChars(decker.tag)
    out.writeChars(readoutMode.tag)
    out.writeChars(telluricScienceMode.foldMap(_.tag))

    telescopeConfigs.toList.foreach: tc =>
      out.write(tc.hashBytes)

    out.write(telluricType.hashBytes)

    out.close()
    bao.toByteArray


object Config:

  /**
   * Nod pattern for the telluric standard of a MOS observation.
   * Move to lucuma-core
   */
  val MosTelluricTelescopeConfigs: SlitTelescopeConfigs =
    SlitTelescopeConfigs.AlongSlit(
      NonEmptyList
        .of(60, 40, 20, -20, 40, 60)
        .map(q => TelescopeConfigAlongSlit(Offset.Q(q.arcsec), StepGuideState.Enabled))
    )

  def apply(
    disperser: Flamingos2Disperser,
    filter: Flamingos2Filter,
    fpu: Flamingos2Fpu,
    exposureTimeMode: ExposureTimeMode,
    acquisition: AcquisitionConfig,
    telescopeConfigs: NonEmptyList[TelescopeConfig],
    explicitReadMode: Option[Flamingos2ReadMode] = None,
    explicitReads: Option[Flamingos2Reads] = None,
    explicitDecker: Option[Flamingos2Decker] = None,
    explicitReadoutMode: Option[Flamingos2ReadoutMode] = None,
    telluricType: TelluricType = TelluricType.Hot,
    telluricScienceMode: Option[ObservingModeType] = None
  ): Config =
    new Config(
      disperser,
      filter,
      fpu,
      acquisition,
      Common(
        exposureTimeMode,
        explicitReadMode,
        explicitReads,
        explicitDecker,
        DefaultFlamingos2ReadoutMode,
        explicitReadoutMode,
        telescopeConfigs,
        telluricType
      ),
      telluricScienceMode
    )
