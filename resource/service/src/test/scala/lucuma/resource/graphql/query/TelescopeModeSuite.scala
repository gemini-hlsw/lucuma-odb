// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.resource.graphql.query

import cats.effect.IO
import io.circe.literal.*
import lucuma.resource.test.ResourceGraphQLSuite
import skunk.implicits.*

class TelescopeModeSuite extends ResourceGraphQLSuite:

  private def insert(values: String): IO[Unit] =
    exec(sql"""
      insert into t_telescope_mode_block (c_site, c_start, c_end, c_mode, c_program_references, c_partner, c_note) values
      #$values
    """.command)

  // GN: a classical night naming two programs, then a block-scheduling
  // night for partner US with no programs named.
  override protected def seed: IO[Unit] =
    insert("""
      ('gn', '2026-05-01 18:00:00', '2026-05-02 06:00:00', 'Classical', array['G-2026B-1234-C','G-2026B-5678-C'], null, null),
      ('gn', '2026-05-02 18:00:00', '2026-05-03 06:00:00', 'BlockScheduling', '{}', 'us', null)
    """)

  test("telescopeMode: returns mode, program references, and partner"):
    expectSuccess(
      query = """
        query {
          telescopeMode(site: GN, start: "2026-05-01T00:00:00Z", end: "2026-05-03T00:00:00Z") {
            site mode programReferences partner note
            interval { start end duration { seconds } }
          }
        }
      """,
      expected = json"""{
        "telescopeMode": [
          { "site": "GN", "mode": "CLASSICAL", "programReferences": ["G-2026B-1234-C","G-2026B-5678-C"], "partner": null, "note": null,
            "interval": { "start": "2026-05-01T18:00:00Z", "end": "2026-05-02T06:00:00Z", "duration": { "seconds": 43200.000000 } } },
          { "site": "GN", "mode": "BLOCK_SCHEDULING", "programReferences": [], "partner": "US", "note": null,
            "interval": { "start": "2026-05-02T18:00:00Z", "end": "2026-05-03T06:00:00Z", "duration": { "seconds": 43200.000000 } } }
        ]
      }"""
    )

  test("telescopeMode: an overlapping block at the same site is rejected by the database"):
    expectDbRejection(
      insert(
        """('gn', '2026-05-01 20:00:00', '2026-05-01 22:00:00', 'Classical', '{}', null, null)"""
      )
    )

  test("telescopeMode: BlockScheduling without a partner is rejected by the database"):
    expectDbRejection(
      insert(
        """('gs', '2026-05-01 18:00:00', '2026-05-02 06:00:00', 'BlockScheduling', '{}', null, null)"""
      )
    )

  test("telescopeMode: a partner on a non-BlockScheduling mode is rejected by the database"):
    expectDbRejection(
      insert("""('gs', '2026-05-01 18:00:00', '2026-05-02 06:00:00', 'Queue', '{}', 'us', null)""")
    )
