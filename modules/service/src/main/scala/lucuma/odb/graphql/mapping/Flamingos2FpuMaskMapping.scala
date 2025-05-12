// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path

import table.Flamingos2DynamicTable

trait Flamingos2FpuMaskMapping[F[_]] extends Flamingos2DynamicTable[F]:

  private def fpuMappingAtPath(
    path: Path
  ): ObjectMapping =
    ObjectMapping(path)(
      SqlField("synthetic_id", Flamingos2DynamicTable.Fpu.SyntheticId, key = true, hidden = true),
      SqlObject("customMask"),
      SqlField("builtin", Flamingos2DynamicTable.Fpu.Builtin)
    )

  lazy val Flamingos2FpuMaskMappings: List[TypeMapping] =
    List(
      fpuMappingAtPath(StepRecordType / "flamingos2" / "mask"),
      fpuMappingAtPath(Flamingos2StepType / "instrumentConfig" / "mask" )
    )