// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import io.circe.syntax.given
import lucuma.odb.json.wavelength.query.given
import lucuma.core.model.sequence.igrins2.CentralWavelength

import table.Igrins2DynamicTable

trait Igrins2DynamicMapping[F[_]] extends Igrins2DynamicTable[F]:

  lazy val Igrins2DynamicMapping: TypeMapping =
    ObjectMapping(StepRecordType / "igrins2")(
      SqlField("id", Igrins2DynamicTable.Id, key = true, hidden = true),
      SqlObject("exposure"),
      CirceField("centralWavelength", CentralWavelength.asJson)
    )