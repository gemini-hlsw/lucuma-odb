// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.Flamingos2StaticTable

 trait Flamingos2StaticMapping[F[_]] extends Flamingos2StaticTable[F]:

  lazy val Flamingos2StaticMapping: ObjectMapping =
     ObjectMapping(Flamingos2StaticType)(
        SqlField("id",                      Flamingos2StaticTable.Id, key = true, hidden = true),
        SqlField("mosPreImaging",           Flamingos2StaticTable.MosPreImaging),
        SqlField("useElectronicOffsetting", Flamingos2StaticTable.UseEOffsetting)
      )