// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path

import table.Flamingos2DynamicView

trait Flamingos2CustomMaskMapping[F[_]] extends Flamingos2DynamicView[F]:

  private def customMaskMappingAt(
    path: Path
  ): ObjectMapping =
    ObjectMapping(path)(
      SqlField("synthetic_id", Flamingos2DynamicView.Fpu.CustomMask.SyntheticId, key = true, hidden = true),
      SqlField("filename",     Flamingos2DynamicView.Fpu.CustomMask.Filename),
      SqlField("slitWidth",    Flamingos2DynamicView.Fpu.CustomMask.SlitWidth)
    )

  lazy val Flamingos2CustomMaskMappings: List[TypeMapping] =
    List(
      customMaskMappingAt(StepRecordType / "flamingos2" / "mask" / "customMask")

      // N.B. This will be required, but ultimately unused, when flamingos2 is
      // added to "type ExecutionConfig".
//      customMaskMappingAt(Flamingos2StepType / "instrumentConfig" / "mask" / "customMask")
    )
