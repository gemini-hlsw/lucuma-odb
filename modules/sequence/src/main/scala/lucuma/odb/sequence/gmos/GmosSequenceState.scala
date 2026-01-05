// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos

import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth

trait GmosSequenceState[D, G, L, U] extends SequenceState[D]:
  def optics: DynamicOptics[D, G, L, U]

trait GmosNorthSequenceState extends GmosSequenceState[GmosNorth, GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]:

  override def initialDynamicConfig: GmosNorth =
    InitialConfigs.GmosNorthDynamic

  override def optics: DynamicOptics[GmosNorth, GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] =
    DynamicOptics.North


trait GmosSouthSequenceState extends GmosSequenceState[GmosSouth, GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]:

  override def initialDynamicConfig: GmosSouth =
    InitialConfigs.GmosSouthDynamic

  override def optics: DynamicOptics[GmosSouth, GmosSouthGrating, GmosSouthFilter, GmosSouthFpu] =
    DynamicOptics.South