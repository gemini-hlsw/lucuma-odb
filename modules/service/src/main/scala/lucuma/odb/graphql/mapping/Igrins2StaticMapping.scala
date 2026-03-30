// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.Igrins2StaticTable

trait Igrins2StaticMapping[F[_]] extends Igrins2StaticTable[F]:

  lazy val Igrins2StaticMapping: ObjectMapping =
    ObjectMapping(Igrins2StaticType)(
      SqlField("id",            Igrins2StaticTable.Id, key = true, hidden = true),
      SqlField("saveSVCImages", Igrins2StaticTable.SaveSVCImages),
      SqlField("offsetMode",    Igrins2StaticTable.OffsetMode)
    )
