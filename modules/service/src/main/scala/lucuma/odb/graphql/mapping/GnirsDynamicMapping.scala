// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path

import table.GnirsDynamicView

trait GnirsDynamicMapping[F[_]] extends GnirsDynamicView[F]:

  private def dynamicMappingAtPath(
    path:  Path,
    table: GnirsDynamicTable[?]
  ): ObjectMapping =
    ObjectMapping(path)(
      SqlField("id",              table.Id, key = true, hidden = true),
      SqlObject("exposure"),
      SqlField("coadds",          table.Coadds),
      SqlObject("centralWavelength"),
      SqlField("filter",          table.Filter),
      SqlField("decker",          table.Decker),
      SqlField("fpuSlit",         table.FpuSlit),
      SqlField("fpuOther",        table.FpuOther),
      SqlField("fpuIfu",          table.FpuIfu),
      SqlObject("acquisitionMirrorOut"),
      SqlField("camera",          table.Camera),
      SqlField("focusMotorSteps", table.FocusMotorSteps),
      SqlField("readMode",        table.ReadMode)
    )

  lazy val GnirsDynamicMappings: List[TypeMapping] =
    List(
      dynamicMappingAtPath(StepRecordType / "gnirs", GnirsDynamicView),
      dynamicMappingAtPath(CalibrationSetType / "gnirs", GnirsCalibrationSetupTable)
    )
