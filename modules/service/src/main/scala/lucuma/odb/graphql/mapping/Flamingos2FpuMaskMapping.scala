// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path

import table.Flamingos2DynamicView

trait Flamingos2FpuMaskMapping[F[_]] extends Flamingos2DynamicView[F]:

  private def fpuMappingAtPath(
    path: Path
  ): ObjectMapping =
    ObjectMapping(path)(
      SqlField("synthetic_id", Flamingos2DynamicView.Fpu.SyntheticId, key = true, hidden = true),
      SqlObject("customMask"),
      SqlField("builtin", Flamingos2DynamicView.Fpu.Builtin)
    )

  lazy val Flamingos2FpuMaskMappings: List[TypeMapping] =
    List(
      fpuMappingAtPath(StepRecordType / "flamingos2" / "fpu")

      // N.B. This will be required, but ultimately unused, when flamingos2 is
      // added to "type ExecutionConfig".
//      fpuMappingAtPath(Flamingos2StepType / "instrumentConfig" / "mask" )
    )