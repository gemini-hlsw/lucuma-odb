// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.resource.graphql.query

import cats.effect.IO
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.resource.test.ResourceGraphQLSuite
import skunk.implicits.*

import java.time.LocalDate

class TelescopeNightSuite extends ResourceGraphQLSuite:

  // GN night 2026-03-02 is [2026-03-02T00:00:00Z, 2026-03-03T00:00:00Z).
  // One too block crossing INTO the night (starts before 00:00Z), one
  // availability block inside it, one mode block after it (next night).
  private def insertBlocks: IO[Unit] =
    exec(sql"""
      insert into t_too_support_block (c_site, c_start, c_end, c_too_support, c_note) values
      ('gn', '2026-03-01 22:00:00', '2026-03-02 06:00:00', 'Standard', null);
    """.command) >>
      exec(sql"""
      insert into t_telescope_availability_block (c_site, c_start, c_end, c_availability, c_port, c_reason, c_note) values
      ('gn', '2026-03-02 02:00:00', '2026-03-02 08:00:00', 'Closed', null, 'Shutdown', null);
    """.command) >>
      exec(sql"""
      insert into t_telescope_mode_block (c_site, c_start, c_end, c_mode, c_program_references, c_partner, c_note) values
      ('gn', '2026-03-03 02:00:00', '2026-03-03 08:00:00', 'Queue', '{}', null, null);
    """.command)

  test("telescopeNight: clips records to the night and reports dataAvailable"):
    insertBlocks >> expectSuccess(
      query = """
        query {
          telescopeNight(site: GN, observingNight: "2026-03-02") {
            site observingNight dataAvailable
            interval { start end }
            telescopeAvailability { availability reason interval { start end } }
            tooSupport { tooSupport interval { start end duration { seconds } } }
            telescopeMode { mode }
          }
        }
      """,
      expected = json"""{
        "telescopeNight": {
          "site": "GN",
          "observingNight": "2026-03-02",
          "dataAvailable": true,
          "interval": { "start": "2026-03-02T00:00:00Z", "end": "2026-03-03T00:00:00Z" },
          "telescopeAvailability": [
            { "availability": "CLOSED", "reason": "Shutdown",
              "interval": { "start": "2026-03-02T02:00:00Z", "end": "2026-03-02T08:00:00Z" } }
          ],
          "tooSupport": [
            { "tooSupport": "STANDARD",
              "interval": { "start": "2026-03-02T00:00:00Z", "end": "2026-03-02T06:00:00Z", "duration": { "seconds": 21600.000000 } } }
          ],
          "telescopeMode": []
        }
      }"""
    )

  test("telescopeNights: a block crossing a night boundary appears in both nights, clipped"):
    // The too block [2026-03-01 22:00Z, 2026-03-02 06:00Z) spans GN nights
    // 2026-03-01 and 2026-03-02 (boundary 2026-03-02T00:00:00Z).
    expectSuccess(
      query = """
        query {
          telescopeNights(site: GN, start: "2026-03-01", end: "2026-03-03") {
            observingNight dataAvailable
            tooSupport { interval { start end } }
          }
        }
      """,
      expected = json"""{
        "telescopeNights": [
          { "observingNight": "2026-03-01", "dataAvailable": true,
            "tooSupport": [ { "interval": { "start": "2026-03-01T22:00:00Z", "end": "2026-03-02T00:00:00Z" } } ] },
          { "observingNight": "2026-03-02", "dataAvailable": true,
            "tooSupport": [ { "interval": { "start": "2026-03-02T00:00:00Z", "end": "2026-03-02T06:00:00Z" } } ] }
        ]
      }"""
    )

  test("telescopeNights: start >= end is an invalid argument"):
    expect(
      query = """
        query {
          telescopeNights(site: GN, start: "2026-03-02", end: "2026-03-02") { observingNight }
        }
      """,
      expected = Left(List("Argument 'start' must be before 'end'."))
    )

  test("telescopeNights: more than 400 nights is an invalid argument"):
    expect(
      query = """
        query {
          telescopeNights(site: GN, start: "2026-01-01", end: "2027-06-01") { observingNight }
        }
      """,
      expected = Left(List("Argument 'end' must be at most 400 nights after 'start'."))
    )

  test("telescopeNights: a range of exactly 400 nights is accepted"):
    expectSuccess(
      query = """
        query {
          telescopeNights(site: GS, start: "2026-01-01", end: "2027-02-05") {
            observingNight
          }
        }
      """,
      expected = Json.obj(
        "telescopeNights" -> Json.arr(
          LazyList
            .iterate(LocalDate.of(2026, 1, 1))(_.plusDays(1))
            .takeWhile(_.isBefore(LocalDate.of(2027, 2, 5)))
            .map(d => Json.obj("observingNight" -> d.toString.asJson))
            .toList*
        )
      )
    )

  test("telescopeNight: an empty night is non-null with dataAvailable false"):
    expectSuccess(
      query = """
        query {
          telescopeNight(site: GS, observingNight: "2026-03-02") {
            site observingNight dataAvailable
            telescopeAvailability { availability }
            tooSupport { tooSupport }
            telescopeMode { mode }
          }
        }
      """,
      expected = json"""{
        "telescopeNight": {
          "site": "GS", "observingNight": "2026-03-02", "dataAvailable": false,
          "telescopeAvailability": [], "tooSupport": [], "telescopeMode": []
        }
      }"""
    )
