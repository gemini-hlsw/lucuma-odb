// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.resource.graphql.query

import cats.effect.IO
import io.circe.literal.*
import lucuma.resource.test.ResourceGraphQLSuite
import skunk.implicits.*

class TelescopeAvailabilitySuite extends ResourceGraphQLSuite:

  private def insert(values: String): IO[Unit] =
    exec(sql"""
      insert into t_telescope_availability_block (c_site, c_start, c_end, c_availability, c_port, c_reason, c_note) values
      #$values
    """.command)

  // Port 3 closed and the whole telescope open over the same interval (legal:
  // different ports), then the whole telescope closed the next night.
  override protected def seed: IO[Unit] =
    insert("""
      ('gn', '2026-04-01 18:00:00', '2026-04-02 06:00:00', 'Closed', 3, 'A&G maintenance', null),
      ('gn', '2026-04-01 18:00:00', '2026-04-02 06:00:00', 'Open', null, null, 'whole telescope'),
      ('gn', '2026-04-02 18:00:00', '2026-04-03 06:00:00', 'Closed', null, 'Shutdown', null)
    """)

  test("telescopeAvailability: returns port-scoped and telescope-wide blocks"):
    // Equal starts are ordered by id, which is insertion order here.
    expectSuccess(
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

  test("telescopeAvailability: an overlapping block on the same port is rejected by the database"):
    expectDbRejection(
      insert("""('gn', '2026-04-01 20:00:00', '2026-04-01 22:00:00', 'Open', 3, null, null)""")
    )
