// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path

import table.Flamingos2DynamicTables

trait Flamingos2CustomMaskMapping[F[_]] extends Flamingos2DynamicTables[F]:

  private def customMaskMappingAt(
    path:  Path,
    table: Flamingos2DynamicTable[?]
  ): ObjectMapping =
    ObjectMapping(path)(
      SqlField("synthetic_id", table.Fpu.CustomMask.SyntheticId, key = true, hidden = true),
      SqlField("attachmentId", table.Fpu.CustomMask.AttachmentId),
      SqlField("slitWidth",    table.Fpu.CustomMask.SlitWidth)
    )

  lazy val Flamingos2CustomMaskMappings: List[TypeMapping] =
    List(
      customMaskMappingAt(StepRecordType / "flamingos2" / "fpu" / "customMask", Flamingos2DynamicView),
      customMaskMappingAt(CalibrationSetType / "flamingos2" / "fpu" / "customMask", Flamingos2CalibrationSetupTable)

      // N.B. This will be required, but ultimately unused, when flamingos2 is
      // added to "type ExecutionConfig".
//      customMaskMappingAt(Flamingos2StepType / "instrumentConfig" / "fpu" / "customMask")
    )
