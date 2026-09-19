// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.resource.graphql.query

import cats.effect.IO
import io.circe.literal.*
import skunk.implicits.*

class InstrumentComponentAvailabilitySuite extends ComponentFixture:

  test(
    "instrumentComponentAvailability: returns component, usage, location for blocks intersecting the window, ordered by start then id"
  ):
    insertCatalog >> expectSuccess(
      query = """
        query {
          instrumentComponentAvailability(site: GN, start: "2026-08-01T00:00:00Z", end: "2026-08-02T00:00:00Z") {
            component { id instrument componentType code existence }
            usage
            location
          }
        }
      """,
      expected = json"""{
        "instrumentComponentAvailability": [
          { "component": { "id": "c-1", "instrument": "GMOS", "componentType": "FILTER", "code": "G_PRIME", "existence": "PRESENT" },
            "usage": "SCIENCE", "location": "INSTALLED" },
          { "component": { "id": "c-2", "instrument": "GMOS", "componentType": "DISPERSER", "code": "R831_G5302", "existence": "PRESENT" },
            "usage": "UNAVAILABLE", "location": "LAB" },
          { "component": { "id": "c-3", "instrument": "GNIRS", "componentType": "DISPERSER", "code": "B1200", "existence": "DELETED" },
            "usage": "SCIENCE", "location": "INSTALLED" }
        ]
      }"""
    )

  test("instrumentComponentAvailability: componentTypes and instruments narrow the result"):
    expectSuccess(
      query = """
        query {
          instrumentComponentAvailability(site: GN, start: "2026-08-01T00:00:00Z", end: "2026-08-02T00:00:00Z", componentTypes: [DISPERSER]) {
            component { id }
          }
        }
      """,
      expected = json"""{
        "instrumentComponentAvailability": [
          { "component": { "id": "c-2" } },
          { "component": { "id": "c-3" } }
        ]
      }"""
    ) >> expectSuccess(
      query = """
        query {
          instrumentComponentAvailability(site: GN, start: "2026-08-01T00:00:00Z", end: "2026-08-02T00:00:00Z", instruments: [GMOS]) {
            component { id }
          }
        }
      """,
      expected = json"""{
        "instrumentComponentAvailability": [
          { "component": { "id": "c-1" } },
          { "component": { "id": "c-2" } }
        ]
      }"""
    )

  test("instrumentComponentAvailability: clip trims the interval"):
    expectSuccess(
      query = """
        query {
          instrumentComponentAvailability(site: GN, start: "2026-08-02T00:00:00Z", end: "2026-08-02T03:00:00Z", clip: true) {
            component { id }
            interval { start end duration { seconds } }
          }
        }
      """,
      expected = json"""{
        "instrumentComponentAvailability": [
          { "component": { "id": "c-1" },
            "interval": { "start": "2026-08-02T00:00:00Z", "end": "2026-08-02T03:00:00Z", "duration": { "seconds": 10800.000000 } } },
          { "component": { "id": "c-2" },
            "interval": { "start": "2026-08-02T00:00:00Z", "end": "2026-08-02T03:00:00Z", "duration": { "seconds": 10800.000000 } } },
          { "component": { "id": "c-3" },
            "interval": { "start": "2026-08-02T00:00:00Z", "end": "2026-08-02T03:00:00Z", "duration": { "seconds": 10800.000000 } } }
        ]
      }"""
    )

  test("telescopeNight: includes component blocks, clipped, with dataAvailable"):
    // GN night 2026-08-02 = [2026-08-02T00:00:00Z, 2026-08-03T00:00:00Z);
    // all three blocks clip to [00:00Z, 06:00Z).
    expectSuccess(
      query = """
        query {
          telescopeNight(site: GN, observingNight: "2026-08-02") {
            dataAvailable
            components {
              usage location
              component { id code existence }
              interval { start end }
            }
          }
        }
      """,
      expected = json"""{
        "telescopeNight": {
          "dataAvailable": true,
          "components": [
            { "usage": "SCIENCE", "location": "INSTALLED",
              "component": { "id": "c-1", "code": "G_PRIME", "existence": "PRESENT" },
              "interval": { "start": "2026-08-02T00:00:00Z", "end": "2026-08-02T06:00:00Z" } },
            { "usage": "UNAVAILABLE", "location": "LAB",
              "component": { "id": "c-2", "code": "R831_G5302", "existence": "PRESENT" },
              "interval": { "start": "2026-08-02T00:00:00Z", "end": "2026-08-02T06:00:00Z" } },
            { "usage": "SCIENCE", "location": "INSTALLED",
              "component": { "id": "c-3", "code": "B1200", "existence": "DELETED" },
              "interval": { "start": "2026-08-02T00:00:00Z", "end": "2026-08-02T06:00:00Z" } }
          ]
        }
      }"""
    )

  test(
    "instrumentComponentAvailability: overlapping insert for the same piece at another site is rejected; a non-overlapping insert is fine"
  ):
    val insertOverlappingAtOtherSite: IO[Unit] =
      exec(sql"""
        insert into t_instrument_component_block (c_site, c_start, c_end, c_component_id, c_usage, c_location, c_note) values
        ('gs', '2026-08-01 18:00:00', '2026-08-02 06:00:00', 'c-1', 'SCIENCE', 'INSTALLED', null)
      """.command)

    val insertNonOverlapping: IO[Unit] =
      exec(sql"""
        insert into t_instrument_component_block (c_site, c_start, c_end, c_component_id, c_usage, c_location, c_note) values
        ('gn', '2026-08-02 06:00:00', '2026-08-02 08:00:00', 'c-1', 'SCIENCE', 'FLOOR', null)
      """.command)

    expectDbRejection(insertOverlappingAtOtherSite) >> insertNonOverlapping
