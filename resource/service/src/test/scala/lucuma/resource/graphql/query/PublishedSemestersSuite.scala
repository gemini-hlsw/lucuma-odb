// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.resource.graphql.query

import cats.effect.IO
import io.circe.literal.*
import lucuma.resource.test.ResourceGraphQLSuite
import skunk.implicits.*

class PublishedSemestersSuite extends ResourceGraphQLSuite:

  private def insertSemesters: IO[Unit] =
    exec(sql"""
      insert into t_published_semester (c_site, c_semester, c_title, c_version, c_demo, c_nights_start, c_nights_end, c_holidays) values
      ('gs', '2025A', 'Gemini South Semester 2025A', null, true, '2025-02-01', '2025-08-01', '{}'),
      ('gn', '2026B', 'Gemini North Semester 2026B', 'Jun 15, 2026', false, '2026-08-01', '2027-02-01', '{2026-09-18,2026-12-25}'),
      ('gn', '2026A', 'Gemini North Semester 2026A', null, false, '2026-02-01', '2026-08-01', '{}')
    """.command) >>
      exec(sql"""
      insert into t_moon_event (c_site, c_semester, c_date, c_phase) values
      ('gn', '2026B', '2026-08-08', 'Full'),
      ('gn', '2026B', '2026-08-23', 'New')
    """.command)

  test("publishedSemesters: empty when nothing is published"):
    expectSuccess(
      query = """
        query { publishedSemesters { semester } }
      """,
      expected = json"""{ "publishedSemesters": [] }"""
    )

  test("publishedSemesters: full picker data, ordered by site then semester"):
    insertSemesters >> expectSuccess(
      query = """
        query {
          publishedSemesters {
            site semester title version demo
            nights { start end }
            holidays
            moonEvents { date phase }
          }
        }
      """,
      expected = json"""{
        "publishedSemesters": [
          { "site": "GN", "semester": "2026A", "title": "Gemini North Semester 2026A",
            "version": null, "demo": false,
            "nights": { "start": "2026-02-01", "end": "2026-08-01" },
            "holidays": [],
            "moonEvents": [] },
          { "site": "GN", "semester": "2026B", "title": "Gemini North Semester 2026B",
            "version": "Jun 15, 2026", "demo": false,
            "nights": { "start": "2026-08-01", "end": "2027-02-01" },
            "holidays": ["2026-09-18", "2026-12-25"],
            "moonEvents": [
              { "date": "2026-08-08", "phase": "FULL" },
              { "date": "2026-08-23", "phase": "NEW" }
            ] },
          { "site": "GS", "semester": "2025A", "title": "Gemini South Semester 2025A",
            "version": null, "demo": true,
            "nights": { "start": "2025-02-01", "end": "2025-08-01" },
            "holidays": [],
            "moonEvents": [] }
        ]
      }"""
    )
