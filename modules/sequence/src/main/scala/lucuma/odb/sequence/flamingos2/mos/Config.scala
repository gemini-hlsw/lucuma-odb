// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.flamingos2.mos

import cats.Eq
import cats.derived.*
import cats.syntax.foldable.*
import lucuma.core.enums.Flamingos2CustomSlitWidth
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.model.Defined
import lucuma.core.model.ToBeDefined
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.odb.sequence.flamingos2.spectroscopy
import lucuma.odb.sequence.flamingos2.spectroscopy.Config.Common
import lucuma.odb.sequence.syntax.all.*

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream

/**
 * Configuration for Flamingos 2 MOS science mode.
 *
 * This is Flamingos 2 long slit with the builtin FPU replaced by a custom mask
 * and no acquisition (yet).
 */
case class Config private (
  disperser:     Flamingos2Disperser,
  filter:        Flamingos2Filter,
  customMask:    Flamingos2FpuMask.Custom,
  equivalentFpu: Flamingos2Fpu,
  common:        Common
) extends spectroscopy.Config derives Eq:

  override def fpuMask: Flamingos2FpuMask =
    customMask

  /**
   * The builtin long slit FPU whose aperture matches the custom mask's slit
   * width.  Every accepted `Flamingos2CustomSlitWidth` has one, which is what
   * lets a MOS observation be calibrated as a long slit.
   */
  override def gcalFpu: Flamingos2Fpu =
    equivalentFpu

  def hashBytes: Array[Byte] =
    val bao: ByteArrayOutputStream = new ByteArrayOutputStream(256)
    val out: DataOutputStream      = new DataOutputStream(bao)

    out.writeChars(disperser.tag)
    out.writeChars(filter.tag)
    out.writeChars(customMask.slitWidth.tag)
    customMask.mask match
      case ToBeDefined => ()
      case Defined(id) => out.writeLong(id.value.value)
    out.write(exposureTimeMode.hashBytes)
    out.writeChars(explicitReadMode.foldMap(_.tag))
    out.writeChars(explicitReads.foldMap(_.tag))
    out.writeChars(decker.tag)
    out.writeChars(readoutMode.tag)

    telescopeConfigs.toList.foreach: tc =>
      out.write(tc.hashBytes)

    out.write(telluricType.hashBytes)

    out.close()
    bao.toByteArray

object Config:

  /**
   * `Flamingos2CustomSlitWidth.Other` carries no width at all, so neither the
   * ITC nor the equivalent-long-slit calibration can be derived from it.
   * It will be implemented later as we need a way to input or calculate an approximate
   * slit width.
   */
  val OtherSlitWidthMessage: String =
    "Flamingos 2 MOS does not support the 'OTHER' custom slit width."

  def apply(
    disperser:  Flamingos2Disperser,
    filter:     Flamingos2Filter,
    customMask: Flamingos2FpuMask.Custom,
    common:     Common
  ): Either[String, Config] =
    equivalentFpu(customMask.slitWidth).map: fpu =>
      new Config(disperser, filter, customMask, fpu, common)

  def equivalentFpu(slitWidth: Flamingos2CustomSlitWidth): Either[String, Flamingos2Fpu] =
    slitWidth.fpu.toRight(OtherSlitWidthMessage)
