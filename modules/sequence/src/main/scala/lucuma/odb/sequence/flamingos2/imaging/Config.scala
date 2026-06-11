// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.flamingos2.imaging

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import lucuma.core.enums.Flamingos2Decker
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.Flamingos2ReadoutMode
import lucuma.core.enums.Flamingos2Reads
import lucuma.odb.sequence.imaging.Variant
import lucuma.odb.sequence.syntax.all.*

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream

/**
 * Configuration for the Flamingos2 imaging science mode.
 */
case class Config(
  variant:             Variant,
  filters:             NonEmptyList[Filter],
  defaultReadMode:     Flamingos2ReadMode,
  explicitReadMode:    Option[Flamingos2ReadMode],
  defaultReads:        Flamingos2Reads,
  explicitReads:       Option[Flamingos2Reads],
  defaultDecker:       Flamingos2Decker,
  explicitDecker:      Option[Flamingos2Decker],
  defaultReadoutMode:  Flamingos2ReadoutMode,
  explicitReadoutMode: Option[Flamingos2ReadoutMode]
) derives Eq:

  def readMode: Flamingos2ReadMode =
    explicitReadMode.getOrElse(defaultReadMode)

  def reads: Flamingos2Reads =
    explicitReads.getOrElse(defaultReads)

  def decker: Flamingos2Decker =
    explicitDecker.getOrElse(defaultDecker)

  def readoutMode: Flamingos2ReadoutMode =
    explicitReadoutMode.getOrElse(defaultReadoutMode)

  def hashBytes: Array[Byte] =
    val bao: ByteArrayOutputStream = new ByteArrayOutputStream(256)
    val out: DataOutputStream      = new DataOutputStream(bao)

    out.write(variant.hashBytes)
    out.write(filters.hashBytes)

    out.writeChars(readMode.tag)
    out.writeChars(reads.tag)
    out.writeChars(decker.tag)
    out.writeChars(readoutMode.tag)

    out.close()
    bao.toByteArray
