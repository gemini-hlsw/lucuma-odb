// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gnirs

import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig

/**
 * Shared `SequenceState` for GNIRS observing modes (LongSlit and, once
 * implemented, IFU and Imaging). Provides the starting `GnirsDynamicConfig`
 * that mode-specific `State.modify` programs build on; the helpers from
 * `SequenceState[D]` (`scienceStep`, `flatStep`, `arcStep`, `eval`) are
 * inherited as-is.
 *
 * Mirrors the role `GmosNorthSequenceState` / `GmosSouthSequenceState`
 * play for GMOS, simplified for GNIRS (no N/S split, no `DynamicOptics`).
 */
trait GnirsSequenceState extends SequenceState[GnirsDynamicConfig]:

  override def initialDynamicConfig: GnirsDynamicConfig =
    InitialConfigs.GnirsDynamic
