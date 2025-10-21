// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.flamingos2.longslit

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import lucuma.core.enums.Flamingos2Decker
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.Flamingos2ReadoutMode
import lucuma.core.enums.Flamingos2Reads
import lucuma.core.math.Offset
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.TelluricType
import lucuma.core.syntax.all.*
import lucuma.odb.sequence.syntax.all.*

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream

/**
 * Configuration for the Flamingos2 Long Slit science mode.  Using these parameters, a
 * F2 long slit sequence may be generated.
 */
case class Config private[longslit](
  disperser: Flamingos2Disperser,
  filter: Flamingos2Filter,
  fpu: Flamingos2Fpu,
  acquisitionExposureTimeMode: ExposureTimeMode,
  scienceExposureTimeMode: ExposureTimeMode,
  explicitReadMode: Option[Flamingos2ReadMode],
  explicitReads: Option[Flamingos2Reads],
  defaultDecker: Flamingos2Decker,
  explicitDecker: Option[Flamingos2Decker],
  defaultReadoutMode: Flamingos2ReadoutMode,
  explicitReadoutMode: Option[Flamingos2ReadoutMode],
  explicitSpatialOffsets: Option[List[Offset]],
  telluricType: TelluricType
) derives Eq:

  def decker: Flamingos2Decker =
    explicitDecker.getOrElse(defaultDecker)

  def readoutMode: Flamingos2ReadoutMode =
    explicitReadoutMode.getOrElse(defaultReadoutMode)

  def offsets: List[Offset] =
    explicitSpatialOffsets.getOrElse(Config.DefaultSpatialOffsets)

  def hashBytes: Array[Byte] =
    val bao: ByteArrayOutputStream = new ByteArrayOutputStream(256)
    val out: DataOutputStream      = new DataOutputStream(bao)

    out.writeChars(disperser.tag)
    out.writeChars(filter.tag)
    out.writeChars(fpu.tag)
    out.write(acquisitionExposureTimeMode.hashBytes)
    out.write(scienceExposureTimeMode.hashBytes)
    out.writeChars(explicitReadMode.foldMap(_.tag))
    out.writeChars(explicitReads.foldMap(_.tag))
    out.writeChars(decker.tag)
    out.writeChars(readoutMode.tag)
    val off: Array[Byte] = explicitSpatialOffsets.foldMap(_.map(_.hashBytes)).flatten.toArray
    out.write(off, 0, off.length)
    val tt: Array[Byte] = telluricType.hashBytes
    out.write(tt, 0, tt.length)

    out.close()
    bao.toByteArray


object Config:

  val DefaultSpatialOffsets: List[Offset] =
    List(
      Offset.Zero.copy(q =  15.arcseconds.q),
      Offset.Zero.copy(q = -15.arcseconds.q),
      Offset.Zero.copy(q = -15.arcseconds.q),
      Offset.Zero.copy(q =  15.arcseconds.q)
    )

  def apply(
    disperser: Flamingos2Disperser,
    filter: Flamingos2Filter,
    fpu: Flamingos2Fpu,
    acquisitionExposureTimeMode: ExposureTimeMode,
    scienceExposureTimeMode: ExposureTimeMode,
    explicitReadMode: Option[Flamingos2ReadMode] = None,
    explicitReads: Option[Flamingos2Reads] = None,
    explicitDecker: Option[Flamingos2Decker] = None,
    explicitReadoutMode: Option[Flamingos2ReadoutMode] = None,
    explicitSpatialOffsets: Option[List[Offset]] = None,
    telluricType: TelluricType = TelluricType.Hot
  ): Config =
    new Config(
      disperser,
      filter,
      fpu,
      acquisitionExposureTimeMode,
      scienceExposureTimeMode,
      explicitReadMode,
      explicitReads,
      Flamingos2Decker.LongSlit,
      explicitDecker,
      DefaultFlamingos2ReadoutMode,
      explicitReadoutMode,
      explicitSpatialOffsets,
      telluricType
    )
