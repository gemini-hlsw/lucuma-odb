// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path
import io.circe.syntax.given
import lucuma.core.model.sequence.igrins2.CentralWavelength
import lucuma.odb.json.wavelength.query.given

import table.Igrins2DynamicTable

trait Igrins2DynamicMapping[F[_]] extends Igrins2DynamicTable[F]:

  private def dynamicMappingAtPath(
    path:  Path,
    table: Igrins2DynamicTable[?]
  ): ObjectMapping =
    ObjectMapping(path)(
      SqlField("id", table.Id, key = true, hidden = true),
      SqlObject("exposure"),
      CirceField("centralWavelength", CentralWavelength.asJson)
    )

  lazy val Igrins2DynamicMappings: List[TypeMapping] =
    List(
      dynamicMappingAtPath(StepRecordType / "igrins2", Igrins2DynamicTable),
      dynamicMappingAtPath(CalibrationSetType / "igrins2", Igrins2CalibrationSetupTable)
    )
