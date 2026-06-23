// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.ProgramView

trait GoaPropertiesMapping[F[_]] extends ProgramView[F]:

  lazy val GoaPropertiesMapping: ObjectMapping =
    ObjectMapping(GoaPropertiesType)(
      SqlField("synthetic_id", ProgramView.Id, key = true, hidden = true),
      SqlField("proprietaryMonths", ProgramView.Goa.Proprietary),
      SqlField("shouldNotify", ProgramView.Goa.ShouldNotify),
      SqlField("privateHeader", ProgramView.Goa.PrivateHeader)
    )