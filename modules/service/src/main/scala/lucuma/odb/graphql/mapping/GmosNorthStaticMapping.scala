// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
 package mapping

 import table.GmosStaticTables

 trait GmosNorthStaticMapping[F[_]] extends GmosStaticTables[F] {

   lazy val GmosNorthStaticMapping: ObjectMapping =
     ObjectMapping(GmosNorthStaticType)(
        SqlField("id",            GmosNorthStaticTable.Id, key = true, hidden = true),
        SqlField("detector",      GmosNorthStaticTable.Detector),
        SqlField("mosPreImaging", GmosNorthStaticTable.MosPreImaging),
        SqlField("stageMode",     GmosNorthStaticTable.StageMode)
      )

 }