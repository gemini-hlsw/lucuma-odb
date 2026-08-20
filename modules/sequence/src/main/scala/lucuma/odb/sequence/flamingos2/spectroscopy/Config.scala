// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.flamingos2.spectroscopy

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import lucuma.core.enums.Flamingos2Decker
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.Flamingos2ReadoutMode
import lucuma.core.enums.Flamingos2Reads
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.TelluricType
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask

/**
 * The science-mode parameters shared by the Flamingos 2 long slit and MOS modes.
 *
 * What varies between them is the aperture: long slit carries a builtin FPU,
 * MOS a custom mask.  Everything a Flamingos 2 spectroscopy sequence reads that
 * does not depend on the aperture is declared here.
 *
 * `gcalFpu` is the builtin FPU the smart gcal tables are keyed on.  For long
 * slit it is the aperture itself; for MOS it is the builtin long slit FPU whose
 * width matches the custom mask's slit width.
 */
trait Config extends Product with Serializable:

  import Config.Common

  def common: Common

  def disperser: Flamingos2Disperser

  def filter: Flamingos2Filter

  /** The aperture the generated steps carry. */
  def fpuMask: Flamingos2FpuMask

  /** The builtin FPU used to key the smart gcal search. */
  def gcalFpu: Flamingos2Fpu

  def exposureTimeMode: ExposureTimeMode =
    common.exposureTimeMode

  def explicitReadMode: Option[Flamingos2ReadMode] =
    common.explicitReadMode

  def explicitReads: Option[Flamingos2Reads] =
    common.explicitReads

  /** Long slit and MOS differ here: the decker follows the aperture. */
  def defaultDecker: Flamingos2Decker =
    fpuMask.defaultDecker

  def explicitDecker: Option[Flamingos2Decker] =
    common.explicitDecker

  def decker: Flamingos2Decker =
    explicitDecker.getOrElse(defaultDecker)

  def defaultReadoutMode: Flamingos2ReadoutMode =
    common.defaultReadoutMode

  def explicitReadoutMode: Option[Flamingos2ReadoutMode] =
    common.explicitReadoutMode

  def readoutMode: Flamingos2ReadoutMode =
    explicitReadoutMode.getOrElse(defaultReadoutMode)

  def telescopeConfigs: NonEmptyList[TelescopeConfig] =
    common.telescopeConfigs

  def telluricType: TelluricType =
    common.telluricType

object Config:

  final case class Common(
    exposureTimeMode:    ExposureTimeMode,
    explicitReadMode:    Option[Flamingos2ReadMode],
    explicitReads:       Option[Flamingos2Reads],
    explicitDecker:      Option[Flamingos2Decker],
    defaultReadoutMode:  Flamingos2ReadoutMode,
    explicitReadoutMode: Option[Flamingos2ReadoutMode],
    telescopeConfigs:    NonEmptyList[TelescopeConfig],
    telluricType:        TelluricType
  ) derives Eq
