// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

 package lucuma.odb.graphql
 package mapping

 import table.GmosStaticTables
 import table.VisitTable

 trait GmosNorthVisitMapping[F[_]]
   extends VisitTable[F]
      with GmosStaticTables[F] {

   lazy val GmosNorthVisitMapping: ObjectMapping =
     ObjectMapping(
       tpe = GmosNorthVisitType,
       fieldMappings = List(
         SqlField("id",      VisitTable.Id, key = true),
         SqlField("created", VisitTable.Created),
         SqlObject("static", Join(VisitTable.Id, GmosNorthStaticTable.VisitId))
       )
     )
 }
