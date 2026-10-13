// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.GnirsReadMode
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig

/**
 * What a smart GCAL expansion must know about an instrument beyond its lookup
 * table.  A `SmartGcalExpander` applies these rules to every step it expands,
 * so that callers supply the configuration of the step being calibrated and
 * receive correctly configured GCAL steps, wherever the expansion happens.
 *
 * @tparam D dynamic instrument configuration
 */
trait SmartGcalRules[D]:

  /**
   * The configuration with which to search the smart GCAL table for a step
   * configured as `d`, when it isn't `d` itself.
   */
  def lookupConfig(d: D): D

  /**
   * Completes the configuration of an expanded GCAL step, `d`, which already
   * carries what the lookup prescribes (e.g., the exposure time).
   */
  def calibrationConfig(d: D): D

object SmartGcalRules:

  private def identityRules[D]: SmartGcalRules[D] =
    new SmartGcalRules[D]:
      def lookupConfig(d: D): D      = d
      def calibrationConfig(d: D): D = d

  /**
   * The smart GCAL tables are keyed on builtin FPUs, so a custom mask is
   * calibrated as the builtin long slit of the same width.  A calibration
   * step's exposure time comes from the lookup, so its read mode is recomputed
   * rather than inherited from the step being calibrated.
   */
  val flamingos2: SmartGcalRules[Flamingos2DynamicConfig] =
    new SmartGcalRules[Flamingos2DynamicConfig]:
      def lookupConfig(d: Flamingos2DynamicConfig): Flamingos2DynamicConfig =
        d.fpu match
          case Flamingos2FpuMask.Custom(_, w) => w.fpu.fold(d)(f => d.copy(fpu = Flamingos2FpuMask.builtin(f)))
          case _                              => d

      def calibrationConfig(d: Flamingos2DynamicConfig): Flamingos2DynamicConfig =
        val mode = Flamingos2ReadMode.forExposureTime(d.exposure)
        d.copy(readMode = mode, reads = mode.readCount)

  val ghost: SmartGcalRules[GhostDynamicConfig] =
    identityRules

  /**
   * The smart GCAL tables are keyed on builtin FPUs, so a custom mask is
   * calibrated as the builtin long slit of the same width.
   */
  val gmosNorth: SmartGcalRules[DynamicConfig.GmosNorth] =
    new SmartGcalRules[DynamicConfig.GmosNorth]:
      def lookupConfig(d: DynamicConfig.GmosNorth): DynamicConfig.GmosNorth =
        d.copy(fpu = d.fpu.map:
          case GmosFpuMask.Custom(_, w) => GmosFpuMask.Builtin(gmos.mos.Config.northFpu(w))
          case builtin                  => builtin
        )

      def calibrationConfig(d: DynamicConfig.GmosNorth): DynamicConfig.GmosNorth =
        d

  /** @see [[gmosNorth]] */
  val gmosSouth: SmartGcalRules[DynamicConfig.GmosSouth] =
    new SmartGcalRules[DynamicConfig.GmosSouth]:
      def lookupConfig(d: DynamicConfig.GmosSouth): DynamicConfig.GmosSouth =
        d.copy(fpu = d.fpu.map:
          case GmosFpuMask.Custom(_, w) => GmosFpuMask.Builtin(gmos.mos.Config.southFpu(w))
          case builtin                  => builtin
        )

      def calibrationConfig(d: DynamicConfig.GmosSouth): DynamicConfig.GmosSouth =
        d

  /**
   * A calibration step's exposure time comes from the lookup, so its read mode
   * is recomputed rather than inherited from the step being calibrated.
   */
  val gnirs: SmartGcalRules[GnirsDynamicConfig] =
    new SmartGcalRules[GnirsDynamicConfig]:
      def lookupConfig(d: GnirsDynamicConfig): GnirsDynamicConfig =
        d

      def calibrationConfig(d: GnirsDynamicConfig): GnirsDynamicConfig =
        d.copy(readMode = GnirsReadMode.forExposureTime(d.exposure))

  val igrins2: SmartGcalRules[Igrins2DynamicConfig] =
    identityRules
