// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.testsyntax

import lucuma.core.model.sequence.ExecutionConfig
import lucuma.core.model.sequence.ExecutionSequence
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig

trait ToExecutionOps {

  extension (iec: InstrumentExecutionConfig)
    def gmosNorth: ExecutionConfig[StaticConfig.GmosNorth, DynamicConfig.GmosNorth] =
      iec match
        case InstrumentExecutionConfig.GmosNorth(ec) => ec
        case _                                       => sys.error("Expected GmosNorth")

    def gmosNorthAcquisition: ExecutionSequence[DynamicConfig.GmosNorth] =
      gmosNorth.acquisition.get

    def gmosNorthScience: ExecutionSequence[DynamicConfig.GmosNorth] =
      gmosNorth.science.get

    def gmosSouth: ExecutionConfig[StaticConfig.GmosSouth, DynamicConfig.GmosSouth] =
      iec match
        case InstrumentExecutionConfig.GmosSouth(ec) => ec
        case _                                       => sys.error("Expected GmosSouth")

    def gmosSouthAcquisition: ExecutionSequence[DynamicConfig.GmosSouth] =
      gmosSouth.acquisition.get

    def gmosSouthScience: ExecutionSequence[DynamicConfig.GmosSouth] =
      gmosSouth.science.get

}

object execution extends ToExecutionOps
