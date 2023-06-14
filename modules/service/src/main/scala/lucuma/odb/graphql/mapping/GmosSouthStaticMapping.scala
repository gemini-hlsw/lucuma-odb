// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

 package lucuma.odb.graphql
 package mapping

 import table.GmosStaticTables

 trait GmosSouthStaticMapping[F[_]] extends GmosStaticTables[F] {

   private lazy val mapping: ObjectMapping =
     ObjectMapping(
       tpe = GmosSouthStaticType,
       fieldMappings = List(
         SqlField("id",            GmosSouthStaticTable.Id, key = true, hidden = true),
         SqlField("detector",      GmosSouthStaticTable.Detector),
         SqlField("mosPreImaging", GmosSouthStaticTable.MosPreImaging),
         SqlField("stageMode",     GmosSouthStaticTable.StageMode)
       )
     )

   // The GmosSouthStatic type appears in the middle of a computed JSON result.
   // In order to avoid finding this mapping in that case, we need to be
   // explicit about when to use this mapping.
   lazy val GmosSouthStaticMapping: TypeMapping =
     SwitchMapping(
       GmosSouthStaticType,
       List(
         GmosSouthVisitType / "static" -> mapping
       )
     )
 }
