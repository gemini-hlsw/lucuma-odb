// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.resource.graphql.query

import cats.syntax.all.*
import io.circe.Json.*
import io.circe.JsonObject
import io.circe.syntax.*
import lucuma.core.enums.Site
import lucuma.resource.test.ResourceGraphQLSuite
import resource.model.*

class TelescopeNightTimelineSuite extends ResourceGraphQLSuite:

  test("foo"):
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
      |}
      |""".stripMargin,
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
                "programReference" -> Null
              )
            )
          )
        )
      ),
      JsonObject(
        "site" -> Site.GN.asJson
      ).some
    )
