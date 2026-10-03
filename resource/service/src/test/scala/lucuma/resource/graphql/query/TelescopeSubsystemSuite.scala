// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.resource.graphql.query

import cats.effect.IO
import io.circe.literal.*
import lucuma.resource.test.ResourceGraphQLSuite
import skunk.implicits.*

class TelescopeSubsystemSuite extends ResourceGraphQLSuite:

  private def insert(values: String): IO[Unit] =
    exec(sql"""
      insert into t_telescope_subsystem_block (c_site, c_start, c_end, c_subsystem, c_usage, c_power_source, c_note) values
      #$values
    """.command)

  // PWFS1 in science use on generator power; LGS unavailable, no power source.
  // Same interval for both: the overlap constraint is per subsystem.
  override protected def seed: IO[Unit] =
    insert("""
      ('gn', '2026-07-01 18:00:00', '2026-07-02 06:00:00', 'PWFS1', 'SCIENCE', 'GENERATOR', null),
      ('gn', '2026-07-01 18:00:00', '2026-07-02 06:00:00', 'LGS', 'UNAVAILABLE', null, 'laser fault')
    """)

  test("telescopeSubsystemAvailability: returns subsystem, usage, powerSource"):
    expectSuccess(
      query = """
        query {
          telescopeSubsystemAvailability(site: GN, start: "2026-07-01T00:00:00Z", end: "2026-07-02T00:00:00Z") {
            subsystem usage powerSource note
            interval { start end }
          }
        }
      """,
      expected = json"""{
        "telescopeSubsystemAvailability": [
          { "subsystem": "PWFS1", "usage": "SCIENCE", "powerSource": "GENERATOR", "note": null,
            "interval": { "start": "2026-07-01T18:00:00Z", "end": "2026-07-02T06:00:00Z" } },
          { "subsystem": "LGS", "usage": "UNAVAILABLE", "powerSource": null, "note": "laser fault",
            "interval": { "start": "2026-07-01T18:00:00Z", "end": "2026-07-02T06:00:00Z" } }
        ]
      }"""
    )

  test("telescopeSubsystemAvailability: subsystems keeps only the listed subsystems"):
    expectSuccess(
      query = """
        query {
          telescopeSubsystemAvailability(site: GN, start: "2026-07-01T00:00:00Z", end: "2026-07-02T00:00:00Z", subsystems: [LGS]) {
            subsystem
          }
        }
      """,
      expected = json"""{ "telescopeSubsystemAvailability": [ { "subsystem": "LGS" } ] }"""
    ) >> expectSuccess(
      query = """
        query {
          telescopeSubsystemAvailability(site: GN, start: "2026-07-01T00:00:00Z", end: "2026-07-02T00:00:00Z", subsystems: [PWFS1, LGS]) {
            subsystem
          }
        }
      """,
      expected =
        json"""{ "telescopeSubsystemAvailability": [ { "subsystem": "PWFS1" }, { "subsystem": "LGS" } ] }"""
    )

  test(
    "telescopeSubsystemAvailability: an overlapping block for the same subsystem is rejected by the database"
  ):
    expectDbRejection(
      insert(
        """('gn', '2026-07-01 20:00:00', '2026-07-01 22:00:00', 'PWFS1', 'SCIENCE', 'GENERATOR', null)"""
      )
    )

  test("telescopeNight: includes subsystems, clipped to the night"):
    // GN night 2026-07-02 = [2026-07-02T00:00:00Z, 2026-07-03T00:00:00Z);
    // both rows clip to [00:00Z, 06:00Z).
    expectSuccess(
      query = """
        query {
          telescopeNight(site: GN, observingNight: "2026-07-02") {
            dataAvailable
            subsystems {
              subsystem usage powerSource
              interval { start end }
            }
          }
        }
      """,
      expected = json"""{
        "telescopeNight": {
          "dataAvailable": true,
          "subsystems": [
            { "subsystem": "PWFS1", "usage": "SCIENCE", "powerSource": "GENERATOR",
              "interval": { "start": "2026-07-02T00:00:00Z", "end": "2026-07-02T06:00:00Z" } },
            { "subsystem": "LGS", "usage": "UNAVAILABLE", "powerSource": null,
              "interval": { "start": "2026-07-02T00:00:00Z", "end": "2026-07-02T06:00:00Z" } }
          ]
        }
      }"""
    )
