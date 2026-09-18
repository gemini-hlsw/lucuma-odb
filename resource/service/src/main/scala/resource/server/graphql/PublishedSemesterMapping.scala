// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql

import grackle.skunk.SkunkMapping
import resource.server.graphql.table.*

trait PublishedSemesterMapping[F[_]] extends PublishedSemesterTables[F]:
  this: SkunkMapping[F] =>

  lazy val PublishedSemesterMappings: List[TypeMapping] =
    List(
      ObjectMapping(PublishedSemesterType)(
        SqlField("_id", PublishedSemesterTable.Id, key = true, hidden = true),
        SqlField("site", PublishedSemesterTable.Site),
        SqlField("semester", PublishedSemesterTable.Semester),
        SqlField("title", PublishedSemesterTable.Title),
        SqlField("version", PublishedSemesterTable.Version),
        SqlField("demo", PublishedSemesterTable.Demo),
        SqlObject("nights"),
        SqlField("holidays", PublishedSemesterTable.Holidays),
        SqlObject(
          "moonEvents",
          Join(
            List(
              PublishedSemesterTable.Site     -> MoonEventTable.Site,
              PublishedSemesterTable.Semester -> MoonEventTable.Semester
            )
          )
        )
      ),
      ObjectMapping(PublishedSemesterType / "nights")(
        SqlField("_id", PublishedSemesterTable.Id, key = true, hidden = true),
        SqlField("start", PublishedSemesterTable.NightsStart),
        SqlField("end", PublishedSemesterTable.NightsEnd)
      ),
      ObjectMapping(MoonEventType)(
        SqlField("_site", MoonEventTable.Site, key = true, hidden = true),
        SqlField("_semester", MoonEventTable.Semester, key = true, hidden = true),
        SqlField("date", MoonEventTable.Date, key = true),
        SqlField("phase", MoonEventTable.Phase)
      )
    )
