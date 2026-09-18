// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.resource.graphql.query

import cats.effect.IO
import io.circe.literal.*
import lucuma.resource.test.ResourceGraphQLSuite
import skunk.implicits.*

class InstrumentAvailabilitySuite extends ResourceGraphQLSuite:

  // GMOS on port 3 for science; GHOST off-telescope in the lab, unavailable.
  private def insertBlocks: IO[Unit] =
    exec(sql"""
      insert into t_instrument_availability_block
        (c_site, c_start, c_end, c_instrument, c_published_name, c_place, c_port, c_usage, c_note) values
      ('gn', '2026-06-01 18:00:00', '2026-06-02 06:00:00', 'GMOS', 'GMOS-N', 'PORT', 3, 'SCIENCE', null),
      ('gn', '2026-06-01 18:00:00', '2026-06-02 06:00:00', 'GHOST', 'GHOST', 'LAB', null, 'UNAVAILABLE', 'awaiting repair')
    """.command)

  test("instrumentAvailability: returns instrument, publishedName, location, usage"):
    insertBlocks >> expectSuccess(
      query = """
        query {
          instrumentAvailability(site: GN, start: "2026-06-01T00:00:00Z", end: "2026-06-02T00:00:00Z") {
            instrument publishedName usage note
            location { place port }
            interval { start end }
          }
        }
      """,
      expected = json"""{
        "instrumentAvailability": [
          { "instrument": "GMOS", "publishedName": "GMOS-N", "usage": "SCIENCE", "note": null,
            "location": { "place": "PORT", "port": 3 },
            "interval": { "start": "2026-06-01T18:00:00Z", "end": "2026-06-02T06:00:00Z" } },
          { "instrument": "GHOST", "publishedName": "GHOST", "usage": "UNAVAILABLE", "note": "awaiting repair",
            "location": { "place": "LAB", "port": null },
            "interval": { "start": "2026-06-01T18:00:00Z", "end": "2026-06-02T06:00:00Z" } }
        ]
      }"""
    )

  test("instrumentAvailability: clip trims the interval"):
    expectSuccess(
      query = """
        query {
          instrumentAvailability(site: GN, start: "2026-06-02T00:00:00Z", end: "2026-06-02T03:00:00Z", clip: true) {
            instrument
            interval { start end duration { seconds } }
          }
        }
      """,
      expected = json"""{
        "instrumentAvailability": [
          { "instrument": "GMOS",
            "interval": { "start": "2026-06-02T00:00:00Z", "end": "2026-06-02T03:00:00Z", "duration": { "seconds": 10800.000000 } } },
          { "instrument": "GHOST",
            "interval": { "start": "2026-06-02T00:00:00Z", "end": "2026-06-02T03:00:00Z", "duration": { "seconds": 10800.000000 } } }
        ]
      }"""
    )

  test("instrumentAvailability: PORT without a port is rejected by the database"):
    val insert: IO[Unit] =
      exec(sql"""
        insert into t_instrument_availability_block
          (c_site, c_start, c_end, c_instrument, c_published_name, c_place, c_port, c_usage, c_note) values
        ('gs', '2026-06-01 18:00:00', '2026-06-02 06:00:00', 'GMOS', 'GMOS-S', 'PORT', null, 'SCIENCE', null)
      """.command)

    expectDbRejection(insert)

  test("instrumentAvailability: a port on a non-PORT place is rejected by the database"):
    val insert: IO[Unit] =
      exec(sql"""
        insert into t_instrument_availability_block
          (c_site, c_start, c_end, c_instrument, c_published_name, c_place, c_port, c_usage, c_note) values
        ('gs', '2026-06-01 18:00:00', '2026-06-02 06:00:00', 'GMOS', 'GMOS-S', 'LAB', 2, 'SCIENCE', null)
      """.command)

    expectDbRejection(insert)

  test(
    "instrumentAvailability: overlapping insert for the same instrument is rejected; another instrument is fine"
  ):
    val insertGmos: IO[Unit] =
      exec(sql"""
        insert into t_instrument_availability_block
          (c_site, c_start, c_end, c_instrument, c_published_name, c_place, c_port, c_usage, c_note) values
        ('gn', '2026-06-01 20:00:00', '2026-06-01 22:00:00', 'GMOS', 'GMOS-N', 'FLOOR', null, 'ENGINEERING', null)
      """.command)

    val insertGnirs: IO[Unit] =
      exec(sql"""
        insert into t_instrument_availability_block
          (c_site, c_start, c_end, c_instrument, c_published_name, c_place, c_port, c_usage, c_note) values
        ('gn', '2026-06-01 20:00:00', '2026-06-01 22:00:00', 'GNIRS', 'GNIRS', 'FLOOR', null, 'ENGINEERING', null)
      """.command)

    expectDbRejection(insertGmos) >> insertGnirs

  test("telescopeNight: includes instrument availability, clipped, with dataAvailable"):
    // GN night 2026-06-02 = [2026-06-02T00:00:00Z, 2026-06-03T00:00:00Z).
    // Both inserted rows [2026-06-01 18:00, 2026-06-02 06:00) clip to [00:00Z, 06:00Z).
    expectSuccess(
      query = """
        query {
          telescopeNight(site: GN, observingNight: "2026-06-02") {
            dataAvailable
            instrumentAvailability {
              instrument publishedName usage
              location { place port }
              interval { start end }
            }
          }
        }
      """,
      expected = json"""{
        "telescopeNight": {
          "dataAvailable": true,
          "instrumentAvailability": [
            { "instrument": "GMOS", "publishedName": "GMOS-N", "usage": "SCIENCE",
              "location": { "place": "PORT", "port": 3 },
              "interval": { "start": "2026-06-02T00:00:00Z", "end": "2026-06-02T06:00:00Z" } },
            { "instrument": "GHOST", "publishedName": "GHOST", "usage": "UNAVAILABLE",
              "location": { "place": "LAB", "port": null },
              "interval": { "start": "2026-06-02T00:00:00Z", "end": "2026-06-02T06:00:00Z" } }
          ]
        }
      }"""
    )
