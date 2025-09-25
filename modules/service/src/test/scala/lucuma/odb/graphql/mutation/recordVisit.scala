// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.util.IdempotencyKey
import lucuma.odb.data.OdbError

class recordVisit extends OdbSuite:

  val service: User = TestUsers.service(nextId)

  override lazy val validUsers: List[User] = List(service)

  private def recordVisitTest(
    mode:     ObservingModeType,
    user:     User,
    query:    Observation.Id => String,
    expected: Either[Observation.Id => String, Json]
  ): IO[Unit] =
    for
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, mode.some)
      _   <- expectSuccessOrOdbError(user, query(oid), expected.leftMap: f =>
        case OdbError.InvalidObservation(_, Some(d)) if d === f(oid) => ()
      ) 
    yield ()

  test("recordGmosNorthVisit"):

    recordVisitTest(
      ObservingModeType.GmosNorthLongSlit,
      service,
      oid => s"""
        mutation {
          recordGmosNorthVisit(input: {
            observationId: "$oid",
            gmosNorth: {
              stageMode: NO_FOLLOW
            }
          }) {
            visit {
              gmosNorth {
                stageMode
                detector
                mosPreImaging
              }
              gmosSouth {
                stageMode
              }
            }
          }
        }
      """,
      json"""
      {
        "recordGmosNorthVisit": {
          "visit": {
            "gmosNorth": {
              "stageMode": "NO_FOLLOW",
              "detector": "HAMAMATSU",
              "mosPreImaging": "IS_NOT_MOS_PRE_IMAGING"
            },
            "gmosSouth": null
          }
        }
      }
      """.asRight
    )

  test("recordGmosNorthVisit - idempotencyKey"):
    val idm   = IdempotencyKey.FromString.getOption("7304956b-45ab-45b6-8db1-ae6f743b519c").get
    val setup =
      for
        pid <- createProgramAs(service)
        oid <- createObservationAs(service, pid, ObservingModeType.GmosNorthLongSlit.some)
      yield oid

    def recordVisit(oid: Observation.Id): IO[(Visit.Id, IdempotencyKey)] =
      query(
        user  = service,
        query = s"""
          mutation {
            recordGmosNorthVisit(input: {
              observationId: "$oid"
              gmosNorth: {
                stageMode: NO_FOLLOW
              }
              idempotencyKey: "${IdempotencyKey.FromString.reverseGet(idm)}"
            }) {
              visit {
                id
                idempotencyKey
              }
            }
          }
        """
      ).map: js =>
        val v = js.hcursor.downFields("recordGmosNorthVisit", "visit")
        (
          v.downField("id").require[Visit.Id],
          v.downField("idempotencyKey").require[IdempotencyKey]
        )

    assertIOBoolean:
      for
        o        <- setup
        (v0, k0) <- recordVisit(o)
        (v1, k1) <- recordVisit(o)
      yield (v0 === v1) && (k0 === idm) && (k1 === idm)

  test("recordGmosNorthVisit - idempotencyKey (slew)"):
    val idm0  = IdempotencyKey.FromString.getOption("100fbce7-73eb-4c82-8a1e-e987a087b89d").get
    val idm1  = IdempotencyKey.FromString.getOption("c7adcb5c-b0c4-404b-a01a-89b7658ebfac").get

    val setup =
      for
        pid <- createProgramAs(service)
        oid <- createObservationAs(service, pid, ObservingModeType.GmosNorthLongSlit.some)
      yield oid

    def recordSlew(oid: Observation.Id): IO[Visit.Id] =
      query(
        user  = service,
        query = s"""
          mutation {
            addSlewEvent(input: {
              observationId: "$oid"
              slewStage: START_SLEW
              idempotencyKey: "${IdempotencyKey.FromString.reverseGet(idm0)}"
            }) {
              event {
                visit { id }
              }
            }
          }
        """
      ).map: js =>
        js.hcursor
          .downFields("addSlewEvent", "event", "visit", "id")
          .require[Visit.Id]

    def recordVisit(oid: Observation.Id): IO[Visit.Id] =
      query(
        user  = service,
        query = s"""
          mutation {
            recordGmosNorthVisit(input: {
              observationId: "$oid"
              gmosNorth: {
                stageMode: NO_FOLLOW
              }
              idempotencyKey: "${IdempotencyKey.FromString.reverseGet(idm1)}"
            }) {
              visit { id }
            }
          }
        """
      ).map: js =>
        js.hcursor
          .downFields("recordGmosNorthVisit", "visit", "id")
          .require[Visit.Id]

    assertIOBoolean:
      for
        o  <- setup
        v0 <- recordSlew(o)
        v1 <- recordVisit(o)
      yield v0 === v1

  test("recordGmosSouthVisit"):

    recordVisitTest(
      ObservingModeType.GmosSouthLongSlit,
      service,
      oid => s"""
        mutation {
          recordGmosSouthVisit(input: {
            observationId: "$oid",
            gmosSouth: {
              stageMode: FOLLOW_XYZ
            }
          }) {
            visit {
              gmosSouth {
                stageMode
                detector
                mosPreImaging
              }
            }
          }
        }
      """,
      json"""
      {
        "recordGmosSouthVisit": {
          "visit": {
            "gmosSouth": {
              "stageMode": "FOLLOW_XYZ",
              "detector": "HAMAMATSU",
              "mosPreImaging": "IS_NOT_MOS_PRE_IMAGING"
            }
          }
        }
      }
      """.asRight
    )

  test("record visit cross site"):

    recordVisitTest(
      ObservingModeType.GmosNorthLongSlit,
      service,
      oid => s"""
        mutation {
          recordGmosSouthVisit(input: {
            observationId: "$oid",
            gmosSouth: {
              stageMode: FOLLOW_XYZ,
              detector: HAMAMATSU,
              mosPreImaging: IS_NOT_MOS_PRE_IMAGING
            }
          }) {
            visit {
              gmosSouth {
                stageMode
              }
            }
          }
        }
      """,
      ((oid: Observation.Id) => s"Observation '$oid' not found or is not a GMOS South observation").asLeft
    )

  test("recordFlamingos2Visit"):

    recordVisitTest(
      ObservingModeType.Flamingos2LongSlit,
      service,
      oid => s"""
        mutation {
          recordFlamingos2Visit(input: {
            observationId: "$oid",
            flamingos2: {
              useElectronicOffsetting: true
            }
          }) {
            visit {
              flamingos2 {
                mosPreImaging
                useElectronicOffsetting
              }
              gmosNorth {
                stageMode
              }
              gmosSouth {
                stageMode
              }
            }
          }
        }
      """,
      json"""
      {
        "recordFlamingos2Visit": {
          "visit": {
            "flamingos2": {
              "mosPreImaging": "IS_NOT_MOS_PRE_IMAGING",
              "useElectronicOffsetting": true
            },
            "gmosNorth": null,
            "gmosSouth": null
          }
        }
      }
      """.asRight
    )

  test("recordFlamingos2Visit (defaults)"):

    recordVisitTest(
      ObservingModeType.Flamingos2LongSlit,
      service,
      oid => s"""
        mutation {
          recordFlamingos2Visit(input: {
            observationId: "$oid",
            flamingos2: {}
          }) {
            visit {
              flamingos2 {
                mosPreImaging
                useElectronicOffsetting
              }
              gmosNorth {
                stageMode
              }
              gmosSouth {
                stageMode
              }
            }
          }
        }
      """,
      json"""
      {
        "recordFlamingos2Visit": {
          "visit": {
            "flamingos2": {
              "mosPreImaging": "IS_NOT_MOS_PRE_IMAGING",
              "useElectronicOffsetting": false
            },
            "gmosNorth": null,
            "gmosSouth": null
          }
        }
      }
      """.asRight
    )