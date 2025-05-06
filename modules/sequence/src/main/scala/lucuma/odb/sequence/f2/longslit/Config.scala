// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.odb.sequence.ObservingMode

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream

/**
 * Configuration for the Flamingos2 Long Slit science mode.  Using these parameters, a
 * F2 long slit sequence may be generated.
 */
case class Config private[longslit](
  disperser: F2Disperser,
  filter: F2Filter,
  fpu: F2Fpu,
  explicitReadMode: Option[F2ReadMode],
  explicitReads: Option[F2Reads],
  defaultDecker: F2Decker,
  explicitDecker: Option[F2Decker],
  defaultReadoutMode: F2ReadoutMode,
  explicitReadoutMode: Option[F2ReadoutMode]
) derives Eq {

  def decker: F2Decker =
    explicitDecker.getOrElse(defaultDecker)

  def readoutMode: F2ReadoutMode =
    explicitReadoutMode.getOrElse(defaultReadoutMode)

  def hashBytes: Array[Byte] = {
    val bao: ByteArrayOutputStream = new ByteArrayOutputStream(256)
    val out: DataOutputStream      = new DataOutputStream(bao)

    out.writeChars(disperser.tag)
    out.writeChars(filter.tag)
    out.writeChars(fpu.tag)
    out.writeChars(explicitReadMode.foldMap(_.tag))
    out.writeChars(explicitReads.foldMap(_.tag))
    out.writeChars(decker.tag)
    out.writeChars(readoutMode.tag)

    out.close()
    bao.toByteArray
  }

}

object Config:

  def apply(
    disperser: F2Disperser,
    filter: F2Filter,
    fpu: F2Fpu,
    explicitReadMode: Option[F2ReadMode] = None,
    explicitReads: Option[F2Reads] = None,
    explicitDecker: Option[F2Decker] = None,
    explicitReadoutMode: Option[F2ReadoutMode] = None,
  ): Config =
    new Config(
      disperser,
      filter,
      fpu,
      explicitReadMode,
      explicitReads,
      F2Decker.LongSlit,
      explicitDecker,
      DefaultF2ReadoutMode,
      explicitReadoutMode
    )

  def reconcile(a: Config, modes: List[ObservingMode]): Option[Config] =
    modes.headOption match {
      case None                                                  =>
        a.some

      case Some(b: Config) =>
        Option.when(a === b)(
          reconcile(a, modes.tail)
        ).flatten

      case _                                                     =>
        none
    }
