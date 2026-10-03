// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.resource.graphql.query

import cats.effect.IO
import io.circe.literal.*
import lucuma.resource.test.ResourceGraphQLSuite
import skunk.implicits.*

class TooSupportSuite extends ResourceGraphQLSuite:

  private def insert(values: String): IO[Unit] =
    exec(sql"""
      insert into t_too_support_block (c_site, c_start, c_end, c_too_support, c_note) values
      #$values
    """.command)

  // GN: two abutting nights; GS: one night. Half-open intervals.
  override protected def seed: IO[Unit] =
    insert("""
      ('gn', '2026-03-01 18:00:00', '2026-03-02 06:00:00', 'Standard', null),
      ('gn', '2026-03-02 18:00:00', '2026-03-03 06:00:00', 'Rapid', 'laser night'),
      ('gs', '2026-03-01 18:00:00', '2026-03-02 06:00:00', 'None', null)
    """)

  test("tooSupport: returns whole stored intervals at the site, ordered by start"):
    expectSuccess(
      query = """
        query {
          tooSupport(site: GN, start: "2026-03-01T00:00:00Z", end: "2026-03-03T00:00:00Z") {
            site tooSupport note
            interval { start end duration { seconds } }
          }
        }
      """,
      expected = json"""{
        "tooSupport": [
          { "site": "GN", "tooSupport": "STANDARD", "note": null,
            "interval": { "start": "2026-03-01T18:00:00Z", "end": "2026-03-02T06:00:00Z", "duration": { "seconds": 43200.000000 } } },
          { "site": "GN", "tooSupport": "RAPID", "note": "laser night",
            "interval": { "start": "2026-03-02T18:00:00Z", "end": "2026-03-03T06:00:00Z", "duration": { "seconds": 43200.000000 } } }
        ]
      }"""
    )

  test("tooSupport: clip trims the interval and duration to the window"):
    // Window covers only the tail of block 1: [2026-03-02 00:00, 2026-03-02 12:00)
    expectSuccess(
      query = """
        query {
          tooSupport(site: GN, start: "2026-03-02T00:00:00Z", end: "2026-03-02T12:00:00Z", clip: true) {
            tooSupport
            interval { start end duration { seconds } }
          }
        }
      """,
      expected = json"""{
        "tooSupport": [
          { "tooSupport": "STANDARD",
            "interval": { "start": "2026-03-02T00:00:00Z", "end": "2026-03-02T06:00:00Z", "duration": { "seconds": 21600.000000 } } }
        ]
      }"""
    )

  test("tooSupport: a block starting at the window end is excluded (half-open)"):
    // Window end 18:00 equals block 2's start: no overlap under [start, end).
    expectSuccess(
      query = """
        query {
          tooSupport(site: GN, start: "2026-03-02T06:00:00Z", end: "2026-03-02T18:00:00Z") { tooSupport }
        }
      """,
      expected = json"""{ "tooSupport": [] }"""
    )

  test("tooSupport: start equal to end is an invalid argument"):
    expect(
      query = """
        query {
          tooSupport(site: GN, start: "2026-03-02T00:00:00Z", end: "2026-03-02T00:00:00Z") { tooSupport }
        }
      """,
      expected = Left(List("Argument 'start' must be before 'end'."))
    )

  test("tooSupport: a window of 400 days and one second is an invalid argument"):
    expect(
      query = """
        query {
          tooSupport(site: GN, start: "2035-01-01T00:00:00Z", end: "2036-02-05T00:00:01Z") { tooSupport }
        }
      """,
      expected = Left(List("Argument 'end' must be at most 400 days after 'start'."))
    )

  test("tooSupport: a window of exactly 400 days is accepted"):
    expectSuccess(
      query = """
        query {
          tooSupport(site: GN, start: "2035-01-01T00:00:00Z", end: "2036-02-05T00:00:00Z") { tooSupport }
        }
      """,
      expected = json"""{ "tooSupport": [] }"""
    )

  test("tooSupport: an overlapping block at the same site is rejected by the database"):
    expectDbRejection(
      insert("""('gn', '2026-03-01 20:00:00', '2026-03-01 22:00:00', 'Standard', null)""")
    )
