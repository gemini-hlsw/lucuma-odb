// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path

import table.Flamingos2DynamicTables

trait Flamingos2FpuMaskMapping[F[_]] extends Flamingos2DynamicTables[F]:

  private def fpuMappingAtPath(
    path:  Path,
    table: Flamingos2DynamicTable[?]
  ): ObjectMapping =
    ObjectMapping(path)(
      SqlField("synthetic_id", table.Fpu.SyntheticId, key = true, hidden = true),
      SqlObject("customMask"),
      SqlField("builtin", table.Fpu.Builtin)
    )

  lazy val Flamingos2FpuMaskMappings: List[TypeMapping] =
    List(
      fpuMappingAtPath(StepRecordType / "flamingos2" / "fpu", Flamingos2DynamicView),
      fpuMappingAtPath(CalibrationSetType / "flamingos2" / "fpu", Flamingos2CalibrationSetupTable)

      // N.B. This will be required, but ultimately unused, when flamingos2 is
      // added to "type ExecutionConfig".
//      fpuMappingAtPath(Flamingos2StepType / "instrumentConfig" / "mask" )
    )