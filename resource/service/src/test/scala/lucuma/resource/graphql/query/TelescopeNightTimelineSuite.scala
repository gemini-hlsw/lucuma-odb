// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.resource.graphql.query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json.*
import io.circe.JsonObject
import io.circe.syntax.*
import lucuma.core.enums.Site
import lucuma.resource.test.ResourceGraphQLSuite
import resource.model.*
import skunk.syntax.all.*

class TelescopeNightTimelineSuite extends ResourceGraphQLSuite:

  private def insertDummyData: IO[Unit] =
    session.use: s =>
      s.execute(
        sql"INSERT INTO t_telescope_night_timeline (c_site, c_observing_night, c_display_interval_start, c_display_interval_end) VALUES ('gn', '2026-08-01', '2026-08-02 04:00:00', '2026-08-02 15:30:00');".command
      ) *>
        s.execute(
          sql"INSERT INTO t_telescope_availability_status (c_site, c_observing_night, c_interval_start, c_interval_end, c_availability, c_reason, c_planned_availability) VALUES ('gn', '2026-08-01', '2026-08-02 04:00:00', '2026-08-02 15:30:00', 'Open', NULL, NULL);".command
        ) *>
        s.execute(
          sql"INSERT INTO t_telescope_too_status (c_site, c_observing_night, c_interval_start, c_interval_end, c_too_support) VALUES ('gn', '2026-08-01', '2026-08-02 04:00:00', '2026-08-02 10:00:00', 'Rapid'), ('gn', '2026-08-01', '2026-08-02 10:00:00', '2026-08-02 15:30:00', 'Interrupt');".command
        ) *>
        s.execute(
          sql"INSERT INTO t_telescope_mode_status (c_site, c_observing_night, c_interval_start, c_interval_end, c_mode_type, c_mode_program_reference) VALUES ('gn', '2026-08-01', '2026-08-02 04:00:00', '2026-08-02 15:30:00', 'Queue', 'G-2026B-1234-Q');".command
        ).void

  test("telescopeNightTimeline returns data from the database"):
    insertDummyData *>
      expectSuccess(
        query = """
          |query TelescopeNightTimeline($site: Site!) {
          |  telescopeNightTimeline(site: $site, observingNight: "2026-08-01") {
          |    site
          |    observingNight
          |    displayInterval {
          |      start
          |      end
          |      duration {
          |        seconds
          |      }
          |    }
          |    availability {
          |      interval {
          |        start
          |        end
          |        duration {
          |          seconds
          |        }
          |      }
          |      availability
          |      reason
          |      plannedAvailability
          |      site
          |    }
          |    tooStatus {
          |      interval {
          |        start
          |        end
          |        duration {
          |          seconds
          |        }
          |      }
          |      tooSupport
          |      site
          |    }
          |    modes {
          |      interval {
          |        start
          |        end
          |        duration {
          |          seconds
          |        }
          |      }
          |      site
          |      mode {
          |        type
          |        programReference
          |      }
          |    }
          |  }
          |}""".stripMargin,
        obj(
          "telescopeNightTimeline" -> obj(
            "site"            -> Site.GN.asJson,
            "observingNight"  -> fromString("2026-08-01"),
            "displayInterval" -> obj(
              "start"    -> fromString("2026-08-02T04:00:00Z"),
              "end"      -> fromString("2026-08-02T15:30:00Z"),
              "duration" -> obj(
                "seconds" -> fromBigDecimal(BigDecimal("41400.000000"))
              )
            ),
            "availability"    -> arr(
              obj(
                "interval"            -> obj(
                  "start"    -> fromString("2026-08-02T04:00:00Z"),
                  "end"      -> fromString("2026-08-02T15:30:00Z"),
                  "duration" -> obj(
                    "seconds" -> fromBigDecimal(BigDecimal("41400.000000"))
                  )
                ),
                "availability"        -> TelescopeAvailability.Open.asJson,
                "reason"              -> Null,
                "plannedAvailability" -> Null,
                "site"                -> Site.GN.asJson
              )
            ),
            "tooStatus"       -> arr(
              obj(
                "interval"   -> obj(
                  "start"    -> fromString("2026-08-02T04:00:00Z"),
                  "end"      -> fromString("2026-08-02T10:00:00Z"),
                  "duration" -> obj(
                    "seconds" -> fromBigDecimal(BigDecimal("21600.000000"))
                  )
                ),
                "tooSupport" -> TooSupport.Rapid.asJson,
                "site"       -> Site.GN.asJson
              ),
              obj(
                "interval"   -> obj(
                  "start"    -> fromString("2026-08-02T10:00:00Z"),
                  "end"      -> fromString("2026-08-02T15:30:00Z"),
                  "duration" -> obj(
                    "seconds" -> fromBigDecimal(BigDecimal("19800.000000"))
                  )
                ),
                "tooSupport" -> TooSupport.Interrupt.asJson,
                "site"       -> Site.GN.asJson
              )
            ),
            "modes"           -> arr(
              obj(
                "interval" -> obj(
                  "start"    -> fromString("2026-08-02T04:00:00Z"),
                  "end"      -> fromString("2026-08-02T15:30:00Z"),
                  "duration" -> obj(
                    "seconds" -> fromBigDecimal(BigDecimal("41400.000000"))
                  )
                ),
                "site"     -> Site.GN.asJson,
                "mode"     -> obj(
                  "type"             -> TelescopeModeType.Queue.asJson,
                  "programReference" -> fromString("G-2026B-1234-Q")
                )
              )
            )
          )
        ),
        JsonObject(
          "site" -> Site.GN.asJson
        ).some
      )
