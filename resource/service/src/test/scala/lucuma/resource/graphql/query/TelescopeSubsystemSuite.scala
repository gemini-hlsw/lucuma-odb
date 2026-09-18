// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.resource.graphql.query

import cats.effect.IO
import io.circe.literal.*
import lucuma.resource.test.ResourceGraphQLSuite
import skunk.implicits.*

class TelescopeSubsystemSuite extends ResourceGraphQLSuite:

  // PWFS1 in science use on generator power; LGS unavailable, no power source.
  private def insertBlocks: IO[Unit] =
    exec(sql"""
      insert into t_telescope_subsystem_block
        (c_site, c_start, c_end, c_subsystem, c_usage, c_power_source, c_note) values
      ('gn', '2026-07-01 18:00:00', '2026-07-02 06:00:00', 'PWFS1', 'SCIENCE', 'GENERATOR', null),
      ('gn', '2026-07-01 18:00:00', '2026-07-02 06:00:00', 'LGS', 'UNAVAILABLE', null, 'laser fault')
    """.command)

  test("telescopeSubsystemAvailability: returns subsystem, usage, powerSource"):
    insertBlocks >> expectSuccess(
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

  test("telescopeSubsystemAvailability: the subsystems argument narrows the result"):
    expectSuccess(
      query = """
        query {
          telescopeSubsystemAvailability(site: GN, start: "2026-07-01T00:00:00Z", end: "2026-07-02T00:00:00Z", subsystems: [LGS]) {
            subsystem
          }
        }
      """,
      expected = json"""{
        "telescopeSubsystemAvailability": [
          { "subsystem": "LGS" }
        ]
      }"""
    )

  test("telescopeSubsystemAvailability: clip trims the interval"):
    expectSuccess(
      query = """
        query {
          telescopeSubsystemAvailability(site: GN, start: "2026-07-02T00:00:00Z", end: "2026-07-02T03:00:00Z", clip: true) {
            subsystem
            interval { start end duration { seconds } }
          }
        }
      """,
      expected = json"""{
        "telescopeSubsystemAvailability": [
          { "subsystem": "PWFS1",
            "interval": { "start": "2026-07-02T00:00:00Z", "end": "2026-07-02T03:00:00Z", "duration": { "seconds": 10800.000000 } } },
          { "subsystem": "LGS",
            "interval": { "start": "2026-07-02T00:00:00Z", "end": "2026-07-02T03:00:00Z", "duration": { "seconds": 10800.000000 } } }
        ]
      }"""
    )

  test(
    "telescopeSubsystemAvailability: overlapping insert for the same subsystem is rejected; another subsystem is fine"
  ):
    val insertPwfs1: IO[Unit] =
      exec(sql"""
        insert into t_telescope_subsystem_block
          (c_site, c_start, c_end, c_subsystem, c_usage, c_power_source, c_note) values
        ('gn', '2026-07-01 20:00:00', '2026-07-01 22:00:00', 'PWFS1', 'SCIENCE', 'GENERATOR', null)
      """.command)

    val insertPwfs2: IO[Unit] =
      exec(sql"""
        insert into t_telescope_subsystem_block
          (c_site, c_start, c_end, c_subsystem, c_usage, c_power_source, c_note) values
        ('gn', '2026-07-01 20:00:00', '2026-07-01 22:00:00', 'PWFS2', 'SCIENCE', 'GENERATOR', null)
      """.command)

    expectDbRejection(insertPwfs1) >> insertPwfs2

  test("telescopeNight: includes subsystems, clipped, with dataAvailable"):
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

  test("telescopeSubsystemAvailability: a multi-element subsystems filter includes PWFS1"):
    expectSuccess(
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
