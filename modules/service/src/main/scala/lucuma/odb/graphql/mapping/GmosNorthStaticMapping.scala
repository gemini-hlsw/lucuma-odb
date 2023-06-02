// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

 package lucuma.odb.graphql
 package mapping

 import table.GmosStaticTables

 trait GmosNorthStaticMapping[F[_]] extends GmosStaticTables[F] {

   private lazy val mapping: ObjectMapping =
     ObjectMapping(
       tpe = GmosNorthStaticType,
       fieldMappings = List(
         SqlField("id",            GmosNorthStaticTable.Id, key = true, hidden = true),
         SqlField("detector",      GmosNorthStaticTable.Detector),
         SqlField("mosPreImaging", GmosNorthStaticTable.MosPreImaging),
         SqlField("stageMode",     GmosNorthStaticTable.StageMode)
       )
     )

   // The GmosNorthStatic type appears in the middle of a computed JSON result.
   // In order to avoid finding this mapping in that case, we need to be
   // explicit about when to use this mapping.
   lazy val GmosNorthStaticMapping: TypeMapping =
     SwitchMapping(
       GmosNorthStaticType,
       List(
         GmosNorthVisitType / "static" -> mapping
       )
     )
 }