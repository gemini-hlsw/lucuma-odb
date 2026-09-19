// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.resource.graphql.query

import cats.effect.IO
import io.circe.literal.*
import lucuma.resource.test.ResourceGraphQLSuite
import skunk.implicits.*

class TelescopeAvailabilitySuite extends ResourceGraphQLSuite:

  // Port 3 closed, whole telescope closed later, overlapping-port rows at
  // the same instant on different subjects are legal.
  private def insertBlocks: IO[Unit] =
    exec(sql"""
      insert into t_telescope_availability_block (c_site, c_start, c_end, c_availability, c_port, c_reason, c_note) values
      ('gn', '2026-04-01 18:00:00', '2026-04-02 06:00:00', 'Closed', 3, 'A&G maintenance', null),
      ('gn', '2026-04-01 18:00:00', '2026-04-02 06:00:00', 'Open', null, null, 'whole telescope'),
      ('gn', '2026-04-02 18:00:00', '2026-04-03 06:00:00', 'Closed', null, 'Shutdown', null)
    """.command)

  test("telescopeAvailability: returns port-scoped and telescope-wide blocks"):
    // window [2026-04-01T00:00Z, 2026-04-02T12:00Z) -> first two rows, ordered by start
    // (tie: both start at 18:00; order is contractual, secondary key is c_id ascending,
    // which matches insertion order here).
    insertBlocks >> expectSuccess(
      query = """
        query {
          telescopeAvailability(site: GN, start: "2026-04-01T00:00:00Z", end: "2026-04-02T12:00:00Z") {
            site availability port reason note
            interval { start end }
          }
        }
      """,
      expected = json"""{
        "telescopeAvailability": [
          { "site": "GN", "availability": "CLOSED", "port": 3, "reason": "A&G maintenance", "note": null,
            "interval": { "start": "2026-04-01T18:00:00Z", "end": "2026-04-02T06:00:00Z" } },
          { "site": "GN", "availability": "OPEN", "port": null, "reason": null, "note": "whole telescope",
            "interval": { "start": "2026-04-01T18:00:00Z", "end": "2026-04-02T06:00:00Z" } }
        ]
      }"""
    )

  test("telescopeAvailability: clip trims to the window"):
    // clip: true, window [2026-04-02T00:00Z, 2026-04-02T03:00Z) -> two rows, both with interval 00:00-03:00, duration seconds 10800
    expectSuccess(
      query = """
        query {
          telescopeAvailability(site: GN, start: "2026-04-02T00:00:00Z", end: "2026-04-02T03:00:00Z", clip: true) {
            availability
            interval { start end duration { seconds } }
          }
        }
      """,
      expected = json"""{
        "telescopeAvailability": [
          { "availability": "CLOSED",
            "interval": { "start": "2026-04-02T00:00:00Z", "end": "2026-04-02T03:00:00Z", "duration": { "seconds": 10800.000000 } } },
          { "availability": "OPEN",
            "interval": { "start": "2026-04-02T00:00:00Z", "end": "2026-04-02T03:00:00Z", "duration": { "seconds": 10800.000000 } } }
        ]
      }"""
    )

  test("telescopeAvailability: overlapping insert on the same port is rejected by the database"):
    val insertPort3: IO[Unit] =
      exec(sql"""
        insert into t_telescope_availability_block (c_site, c_start, c_end, c_availability, c_port, c_reason, c_note) values
        ('gn', '2026-04-01 20:00:00', '2026-04-01 22:00:00', 'Open', 3, null, null)
      """.command)

    val insertPort4: IO[Unit] =
      exec(sql"""
        insert into t_telescope_availability_block (c_site, c_start, c_end, c_availability, c_port, c_reason, c_note) values
        ('gn', '2026-04-01 20:00:00', '2026-04-01 22:00:00', 'Open', 4, null, null)
      """.command)

    expectDbRejection(insertPort3) >> insertPort4
