// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path

import table.Flamingos2DynamicTable

trait Flamingos2CustomMaskMapping[F[_]] extends Flamingos2DynamicTable[F]:

  private def customMaskMappingAt(
    path: Path
  ): ObjectMapping =
    ObjectMapping(path)(
      SqlField("synthetic_id", Flamingos2DynamicTable.Fpu.CustomMask.SyntheticId, key = true, hidden = true),
      SqlField("filename",     Flamingos2DynamicTable.Fpu.CustomMask.Filename),
      SqlField("slitWidth",    Flamingos2DynamicTable.Fpu.CustomMask.SlitWidth)
    )

  lazy val Flamingos2CustomMaskMappings: List[TypeMapping] =
    List(
      customMaskMappingAt(StepRecordType / "flamingos2" / "mask" / "customMask"),
      customMaskMappingAt(Flamingos2StepType / "instrumentConfig" / "mask" / "customMask")
    )
