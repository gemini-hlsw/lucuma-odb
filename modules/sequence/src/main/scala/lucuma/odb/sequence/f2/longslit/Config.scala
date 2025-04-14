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

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream

/**
 * Configuration for the Flamingos2 Long Slit science mode.  Using these parameters, a
 * F2 long slit sequence may be generated.
 */
case class Config private[longslit](
  grating: F2Disperser,
  filter: Option[F2Filter],
  fpu: F2Fpu,
  defaultReadMode: F2ReadMode,
  explicitReadMode: Option[F2ReadMode],
  defaultReads: F2Reads,
  explicitReads: Option[F2Reads],
  explicitDecker: Option[F2Decker],
  explicitReadoutMode: Option[F2ReadoutMode]
) derives Eq {

  def decker: Option[F2Decker] = explicitDecker

  def readoutMode: Option[F2ReadoutMode] = explicitReadoutMode

  def reads: F2Reads =
    explicitReads.getOrElse(defaultReads)

  def readMode: F2ReadMode =
    explicitReadMode.getOrElse(defaultReadMode)

  def hashBytes: Array[Byte] = {
    val bao: ByteArrayOutputStream = new ByteArrayOutputStream(256)
    val out: DataOutputStream      = new DataOutputStream(bao)

    out.writeChars(grating.tag)
    out.writeChars(filter.foldMap(_.tag))
    out.writeChars(fpu.tag)
    out.writeChars(readMode.tag)
    out.writeChars(reads.tag)
    out.writeChars(decker.foldMap(_.tag))
    out.writeChars(readoutMode.foldMap(_.tag))

    out.close()
    bao.toByteArray
  }

}

object Config:

  def apply(
    grating: F2Disperser,
    filter: Option[F2Filter],
    fpu: F2Fpu,
    explicitReadMode: Option[F2ReadMode] = None,
    explicitReads: Option[F2Reads] = None,
    explicitDecker: Option[F2Decker] = None,
    explicitReadoutMode: Option[F2ReadoutMode] = None,
  ): Config =
    val rm = explicitReadMode.getOrElse(DefaultF2ReadMode)
    new Config(
      grating,
      filter,
      fpu,
      rm,
      explicitReadMode,
      rm.readCount,
      explicitReads,
      explicitDecker,
      explicitReadoutMode
    )
