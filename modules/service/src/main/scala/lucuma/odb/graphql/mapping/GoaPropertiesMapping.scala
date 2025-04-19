// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.ProgramTable

trait GoaPropertiesMapping[F[_]] extends ProgramTable[F]:

  lazy val GoaPropertiesMapping: ObjectMapping =
    ObjectMapping(GoaPropertiesType)(
      SqlField("synthetic_id", ProgramTable.Id, key = true, hidden = true),
      SqlField("proprietaryMonths", ProgramTable.Goa.Proprietary),
      SqlField("shouldNotify", ProgramTable.Goa.ShouldNotify),
      SqlField("privateHeader", ProgramTable.Goa.PrivateHeader)
    )