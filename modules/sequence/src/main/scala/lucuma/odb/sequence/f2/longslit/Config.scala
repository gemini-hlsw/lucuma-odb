// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.f2.longslit

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import lucuma.core.enums.F2Decker
import lucuma.core.enums.F2Disperser
import lucuma.core.enums.F2Filter
import lucuma.core.enums.F2Fpu
import lucuma.core.enums.F2ReadMode
import lucuma.core.enums.F2ReadoutMode
import lucuma.core.enums.F2Reads
import lucuma.core.enums.F2WindowCover

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream

/**
 * Configuration for the Flamingos2 Long Slit science mode.  Using these parameters, a
 * F2 long slit sequence may be generated.
 */
case class Config(
  grating: F2Disperser,
  filter: Option[F2Filter],
  fpu: F2Fpu,
  explicitReadMode: Option[F2ReadMode]             = None,
  explicitDecker: Option[F2Decker]                 = None,
  explicitReadoutMode: Option[F2ReadoutMode]       = None,
  explicitReads: Option[F2Reads]                   = None,
  explicitWindowCover: Option[F2WindowCover]       = None,
  explicitUseElectronicOffsetting: Option[Boolean] = None
) derives Eq {

  def readMode: F2ReadMode =
    explicitReadMode.getOrElse(defaultReadMode)

  def defaultReadMode: F2ReadMode =
    DefaultF2ReadMode

  def decker: F2Decker =
    explicitDecker.getOrElse(defaultDecker)

  def defaultDecker: F2Decker =
    DefaultF2Decker

  def readoutMode: Option[F2ReadoutMode] = explicitReadoutMode

  def reads: Option[F2Reads] = explicitReads

  def windowCover: Option[F2WindowCover] = explicitWindowCover

  def useElectronicOffseting: Boolean =
    explicitUseElectronicOffsetting.getOrElse(defaultUseElectronicOffsetting)

  def defaultUseElectronicOffsetting: Boolean =
    DefaultF2UseElectronicOffsetting

  def defaultMOSPreImaging: Boolean =
    DefaultF2MOSPreImaging

  def hashBytes: Array[Byte] = {
    val bao: ByteArrayOutputStream = new ByteArrayOutputStream(256)
    val out: DataOutputStream      = new DataOutputStream(bao)

    out.writeChars(grating.tag)
    out.writeChars(filter.foldMap(_.tag))
    out.writeChars(fpu.tag)
    out.writeChars(readMode.tag)
    out.writeChars(decker.tag)
    out.writeChars(readoutMode.foldMap(_.tag))
    out.writeChars(reads.foldMap(_.tag))
    out.writeChars(windowCover.foldMap(_.tag))
    out.writeBoolean(useElectronicOffseting)

    out.close()
    bao.toByteArray
  }

}

