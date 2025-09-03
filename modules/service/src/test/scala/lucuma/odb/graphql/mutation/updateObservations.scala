// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegShort
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.GmosBinning
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.model.Group
import lucuma.core.model.ImageQuality
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.syntax.string.*
import lucuma.core.syntax.timespan.*
import lucuma.odb.graphql.input.AllocationInput
import lucuma.odb.service.ObservationService

class updateObservations extends OdbSuite
                            with UpdateConstraintSetOps {

  val pi: User    = TestUsers.Standard.pi(nextId, nextId)
  val pi2: User   = TestUsers.Standard.pi(nextId, nextId)
  val staff: User = TestUsers.Standard.staff(nextId, nextId)

  override lazy val validUsers: List[User] = List(pi, pi2, staff)

  private def oneUpdateTest(
    user:          User,
    update:        String,
    query:         String,
    expected:      Either[String, Json],
    observingMode: Option[ObservingModeType] = None
  ): IO[Unit] =

    for
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, observingMode)
      _   <- updateObservation(user, oid, update, query, expected)
    yield ()

  private def multiUpdateTest(
    user:    User,
    updates: List[(String, String, Either[String, Json])],
    observingMode: Option[ObservingModeType] = None
  ): IO[Unit] =

    for
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, observingMode)
      _   <- updates.traverse_ { case (update, query, expected) =>
        updateObservation(user, oid, update, query, expected)
      }
    yield ()


  test("general: update that selects nothing") {
    def emptyUpdate(user: User): IO[Unit] =
      expect(
        user = user,
        query = s"""
          mutation {
            updateObservations(input: {
              SET: {
                constraintSet: {
                  cloudExtinction: ONE_POINT_ZERO
                }
              },
              WHERE: {
                id: { EQ: "o-9999" }
              }
            }) {
              observations {
                constraintSet {
                  cloudExtinction
                }
              }
            }
          }
        """,
        expected =json"""
          {
            "updateObservations": {
              "observations": []
            }
          }
        """.asRight
      )

    for
      pid <- createProgramAs(pi)
      _   <- createObservationAs(pi, pid)
      _   <- emptyUpdate(pi)
    yield ()
  }

  private def expectedSubtitleUpdate(oids: Observation.Id*): Json =
    Json.obj(
      "updateObservations" -> Json.obj(
        "observations" ->
          oids.map { oid =>
            Json.obj(
              "id" -> oid.asJson,
              "subtitle" -> "Charles Guiteau".asJson
            )
          }.asJson
      )
    )

  test("cross program update") {
    def mutation(oid0: Observation.Id, oid1: Observation.Id) =
      s"""
        mutation {
          updateObservations(input: {
            SET: {
              subtitle: "Charles Guiteau"
            },
            WHERE: {
              id: { IN: [ ${oid0.asJson}, ${oid1.asJson} ] }
            }
          }) {
            observations {
              id
              subtitle
            }
          }
        }
      """

    for
      pid0 <- createProgramAs(pi)
      oid0 <- createObservationAs(pi, pid0)
      pid1 <- createProgramAs(pi)
      oid1 <- createObservationAs(pi, pid1)
      _    <- expect(pi, mutation(oid0, oid1), expectedSubtitleUpdate(oid0, oid1).asRight)
    yield ()
  }

  test("using programId") {
    def mutation(pid: Program.Id) =
      s"""
        mutation {
          updateObservations(input: {
            SET: {
              subtitle: "Charles Guiteau"
            },
            WHERE: {
              program: {
                id: { EQ: "$pid" }
              }
            }
          }) {
            observations {
              id
              subtitle
            }
          }
        }
      """

    for
      pid <- createProgramAs(pi)
      oid0 <- createObservationAs(pi, pid)
      oid1 <- createObservationAs(pi, pid)
      _    <- expect(pi, mutation(pid), expectedSubtitleUpdate(oid0, oid1).asRight)
    yield ()
  }

  test("illegal cross program update") {
    def mutation(oid0: Observation.Id, oid1: Observation.Id) =
      s"""
        mutation {
          updateObservations(input: {
            SET: {
              subtitle: "Charles Guiteau"
            },
            WHERE: {
              id: { IN: [ ${oid0.asJson}, ${oid1.asJson} ] }
            }
          }) {
            observations {
              id
              subtitle
            }
          }
        }
      """

    for
      pid0 <- createProgramAs(pi)
      oid0 <- createObservationAs(pi, pid0)
      pid1 <- createProgramAs(pi2)
      oid1 <- createObservationAs(pi2, pid1)
      _    <- expect(pi, mutation(oid0, oid1), expectedSubtitleUpdate(oid0).asRight)
    yield ()
  }

  test("constraint set: update cloud extinction") {
    oneUpdateTest(
      user   = pi,
      update = """
        constraintSet: {
          cloudExtinction: ONE_POINT_ZERO
        }
      """,
      query = """
        observations {
          constraintSet {
            cloudExtinction
          }
        }
      """,
      expected = json"""
        {
          "updateObservations": {
            "observations": [
              {
                "constraintSet": {
                  "cloudExtinction": "ONE_POINT_ZERO"
                }
              }
            ]
          }
        }
      """.asRight
    )
  }

  test("constraint set: update air mass range") {
    oneUpdateTest(
      user   = pi,
      update = """
        constraintSet: {
          elevationRange: {
            airMass: {
              min: 1.1
            }
          }
        }
      """,
      query = """
        observations {
          constraintSet {
            elevationRange {
              airMass {
                min
                max
              }
            }
          }
        }
      """,
      expected = json"""
        {
          "updateObservations": {
            "observations": [
              {
                "constraintSet": {
                  "elevationRange": {
                    "airMass": {
                      "min": 1.10,
                      "max": 2.00
                    }
                  }
                }
              }
            ]
          }
        }
      """.asRight
      )
  }

  test("constraint set: hour angle constraint violation") {

    oneUpdateTest(
      user   = pi,
      update = """
        constraintSet: {
          elevationRange: {
            hourAngle: {
              minHours: -1.0
            }
          }
        }
      """,
      query = """
        observations {
          constraintSet {
            elevationRange {
              airMass {
                min
                max
              }
            }
          }
        }
      """,
      expected = ObservationService.MissingHourAngleConstraint.message.asLeft
    )

  }

  test("constraint set: switch elevation range constraint type") {
    oneUpdateTest(
      user   = pi,
      update = """
        constraintSet: {
          elevationRange: {
            hourAngle: {
              minHours: -1.0,
              maxHours:  1.0
            }
          }
        }
      """,
      query = """
        observations {
          constraintSet {
            elevationRange {
              airMass {
                min
                max
              }
              hourAngle {
                minHours
                maxHours
              }
            }
          }
        }
      """,
      expected = json"""
        {
          "updateObservations": {
            "observations": [
              {
                "constraintSet": {
                  "elevationRange": {
                    "airMass": null,
                    "hourAngle": {
                      "minHours": -1.00,
                      "maxHours": 1.00
                    }
                  }
                }
              }
            ]
          }
        }
      """.asRight
    )

  }

  test("constraint set: conflicting elevation range updates") {

    oneUpdateTest(
      user   = pi,
      update = """
        constraintSet: {
          elevationRange: {
            airMass: {
              min: 1.1
            },
            hourAngle: {
              minHours: -1.0
            }
          }
        }
      """,
      query = """
        observations {
          constraintSet {
            elevationRange {
              airMass {
                min
                max
              }
              hourAngle {
                minHours
                maxHours
              }
            }
          }
        }
      """,
      expected = "Argument 'input.SET.constraintSet.elevationRange' is invalid: Only one of airMass or hourAngle may be specified.".asLeft
    )

  }

  test("target environment: set explicit base in existing observation without one") {

    oneUpdateTest(
      user = pi,
      update = """
        targetEnvironment: {
          explicitBase: {
            ra: { hms: "1:00:00"},
            dec: { dms: "2:00:00"}
          }
        }
      """,
      query = """
        observations {
          targetEnvironment {
            explicitBase {
              ra { hours }
              dec { degrees }
            }
          }
        }
      """,
      expected = json"""
        {
          "updateObservations": {
            "observations": [
              {
                "targetEnvironment": {
                  "explicitBase": {
                    "ra": {
                      "hours": 1.0
                    },
                    "dec": {
                      "degrees": 2.0
                    }
                  }
                }
              }
            ]
          }
        }
      """.asRight
    )

  }


  test("target environment: set an asterism in existing observation without one") {

    def update(tid: Target.Id) = s"""
      targetEnvironment: {
        asterism: [ "${tid.show}" ]
      }
    """

    val query = """
      observations {
        targetEnvironment {
          asterism {
            id
          }
        }
      }
    """

    def expected(tid: Target.Id) = json"""
      {
        "updateObservations": {
          "observations": [
            {
              "targetEnvironment": {
                "asterism": [
                  {
                    "id": ${tid.show}
                  }
                ]
              }
            }
          ]
        }
      }
    """.asRight

    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      tid <- createTargetAs(pi, pid, "Biff")
      _   <- updateObservation(pi, oid, update(tid), query, expected(tid))
    } yield ()

  }

  test("target environment: update an asterism in existing observation") {

    def update(tids: Target.Id*) =
      s"""
        targetEnvironment: {
          asterism: [ "${tids.map(_.show).intercalate("\", \"")}" ]
        }
      """

    val query = """
      observations {
        targetEnvironment {
          asterism {
            id
          }
        }
      }
    """

    def expected(tids: Target.Id*) = json"""
      {
        "updateObservations": {
          "observations": [
            {
              "targetEnvironment": {
                "asterism": ${tids.map(tid => json"""{ "id": ${tid.asJson} }""").asJson}
              }
            }
          ]
          }
      }
    """.asRight

    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      t0  <- createTargetAs(pi, pid, "Larry")
      t1  <- createTargetAs(pi, pid, "Curly")
      t2  <- createTargetAs(pi, pid, "Moe")
      _   <- updateObservation(pi, oid, update(t0, t1), query, expected(t0, t1))
      _   <- updateObservation(pi, oid, update(t1, t2), query, expected(t1, t2))
    } yield ()

  }

  test("target environment: fail to set an asterism across programs") {

    def updateObservationsMutation(
      oid0: Observation.Id,
      oid1: Observation.Id,
      tids: Target.Id*
    ): String = s"""
      mutation {
        updateObservations(input: {
          SET: {
            targetEnvironment: {
              asterism: [ "${tids.map(_.show).intercalate("\", \"")}" ]
            }
          },
          WHERE: {
            id: { IN: [ ${oid0.asJson}, ${oid1.asJson} ] }
          }
        }) {
          observations {
            id
            targetEnvironment {
              asterism {
                id
              }
            }
          }
        }
      }
    """

    def expected(pid: Program.Id, tids: Target.Id*) =
      List(s"Target(s) ${tids.mkString(", ")} must exist and be associated with Program $pid.").asLeft

    for {
      pid0 <- createProgramAs(pi)
      oid0 <- createObservationAs(pi, pid0)
      pid1 <- createProgramAs(pi)
      oid1 <- createObservationAs(pi, pid1)

      t0  <- createTargetAs(pi, pid0, "Larry")
      t1  <- createTargetAs(pi, pid0, "Curly")
      t2  <- createTargetAs(pi, pid0, "Moe")

      _   <- expect(pi, updateObservationsMutation(oid0, oid1, t0, t1), expected(pid1, t0, t1))
      _   <- expect(pi, updateObservationsMutation(oid0, oid1, t1, t2), expected(pid1, t1, t2))
    } yield ()

  }

  test("target environment: update explicit ra in observation with existing explicit base") {

    val update1 = """
      targetEnvironment: {
        explicitBase: {
          ra: { hms: "1:00:00" },
          dec: { dms: "2:00:00"}
        }
      }
    """

    val update2 ="""
      targetEnvironment: {
        explicitBase: {
          ra: { hms: "3:00:00"}
        }
      }
    """

    val query = """
      observations {
        targetEnvironment {
          explicitBase {
            ra { hours }
            dec { degrees }
          }
        }
      }
    """

    val expected1 = json"""
      {
        "updateObservations": {
          "observations": [
            {
              "targetEnvironment": {
                "explicitBase": {
                  "ra": {
                    "hours": 1.0
                  },
                  "dec": {
                    "degrees": 2.0
                  }
                }
              }
            }
          ]
        }
      }
    """.asRight

    val expected2 = json"""
      {
        "updateObservations": {
          "observations": [
            {
              "targetEnvironment": {
                "explicitBase": {
                  "ra": {
                    "hours": 3.0
                  },
                  "dec": {
                    "degrees": 2.0
                  }
                }
              }
            }
          ]
        }
      }
    """.asRight

    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      _   <- updateObservation(pi, oid, update1, query, expected1)
      _   <- updateObservation(pi, oid, update2, query, expected2)
    } yield ()

  }

  test("target environment: delete explicit base") {

    val update1 = """
      targetEnvironment: {
        explicitBase: {
          ra: { hms: "1:00:00" },
          dec: { dms: "2:00:00"}
        }
      }
    """

    val update2 = """
      targetEnvironment: {
        explicitBase: null
      }
    """

    val query = """
      observations {
        targetEnvironment {
          explicitBase {
            ra { hours }
            dec { degrees }
          }
        }
      }
    """

    val expected1 = json"""
      {
        "updateObservations": {
          "observations": [
            {
              "targetEnvironment": {
                "explicitBase": {
                  "ra": {
                    "hours": 1.0
                  },
                  "dec": {
                    "degrees": 2.0
                  }
                }
              }
            }
          ]
          }
      }
    """.asRight

    val expected2 = json"""
      {
        "updateObservations": {
          "observations": [
            {
              "targetEnvironment": {
                "explicitBase": null
              }
            }
          ]
        }
      }
    """.asRight

    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      _   <- updateObservation(pi, oid, update1, query, expected1)
      _   <- updateObservation(pi, oid, update2, query, expected2)
    } yield ()

  }

  test("target environment: fail to set (only) explicit ra in existing observation without explicit base") {

    val update = """
      targetEnvironment: {
        explicitBase: {
          ra: { hms: "1:00:00"}
        }
      }
    """

    val query = """
      observations {
        targetEnvironment {
          explicitBase {
            ra { hours }
            dec { degrees }
          }
        }
      }
    """

    val expected = ObservationService.BothExplicitCoordinatesConstraint.message.asLeft

    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      _   <- updateObservation(pi, oid, update, query, expected)
    } yield ()

  }

  test("pos angle constraint: update") {
    oneUpdateTest(
      user = pi,
      update =
        """
        posAngleConstraint: {
          mode: ALLOW_FLIP
          angle: { degrees: 15 }
        }
      """,
      query =
        """
        observations {
          posAngleConstraint {
            mode
            angle {
              microarcseconds
              microseconds
              milliarcseconds
              milliseconds
              arcseconds
              seconds
              arcminutes
              minutes
              degrees
              hours
              dms
              hms
            }
          }
        }
      """,
      expected =
        json"""
        {
          "updateObservations": {
            "observations": [
              {
                "posAngleConstraint": {
                  "mode": "ALLOW_FLIP",
                  "angle": {
                    "microarcseconds": 54000000000,
                    "microseconds": 3600000000,
                    "milliarcseconds": 54000000,
                    "milliseconds": 3600000,
                    "arcseconds": 54000,
                    "seconds": 3600,
                    "arcminutes": 900,
                    "minutes": 60,
                    "degrees": 15,
                    "hours": 1,
                    "dms": "15:00:00.000000",
                    "hms": "01:00:00.000000"
                  }
                }
              }
            ]
          }
        }
      """.asRight
    )
  }

  private object spectroscopyScienceRequirements {
    val update: String = """
      scienceRequirements: {
        exposureTimeMode: {
          signalToNoise: {
            value: 75
            at: { nanometers: 410 }
          }
        }
        spectroscopy: {
          wavelength: { nanometers: 400 }
          resolution: 10
          wavelengthCoverage: { picometers: 10 }
          focalPlane: SINGLE_SLIT
          focalPlaneAngle: { arcseconds: 5 }
          capability: NOD_AND_SHUFFLE
        }
      }
    """

    val query: String = """
      observations {
        scienceRequirements {
          mode
          exposureTimeMode {
            signalToNoise {
              value
              at { nanometers }
            }
          }
          spectroscopy {
            wavelength { nanometers }
            resolution
            wavelengthCoverage { nanometers }
            focalPlane
            focalPlaneAngle { arcseconds }
            capability
          }
        }
      }
    """

    val expected: Json = json"""
      {
        "updateObservations": {
          "observations": [
            {
              "scienceRequirements": {
                "mode": "SPECTROSCOPY",
                "exposureTimeMode": {
                  "signalToNoise": {
                    "value": 75.000,
                    "at": {
                      "nanometers": 410.000
                    }
                  }
                },
                "spectroscopy": {
                  "wavelength": {
                    "nanometers": 400.000
                  },
                  "resolution": 10,
                  "wavelengthCoverage": {
                    "nanometers": 0.010
                  },
                  "focalPlane": "SINGLE_SLIT",
                  "focalPlaneAngle": {
                    "arcseconds": 5
                  },
                  "capability": "NOD_AND_SHUFFLE"
                }
              }
            }
          ]
        }
      }
    """

  }

  test("spectroscopy science requirements: update") {
    oneUpdateTest(
      pi,
      spectroscopyScienceRequirements.update,
      spectroscopyScienceRequirements.query,
      spectroscopyScienceRequirements.expected.asRight
    )
  }

  test("science requirements: delete spectroscopy focal plane") {
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      _   <- query(pi, updateObservationsMutation(oid, spectroscopyScienceRequirements.update, spectroscopyScienceRequirements.query))
      _   <- updateObservation(
        user   = pi,
        oid    = oid,
        update = """
          scienceRequirements: {
            spectroscopy: {
              focalPlane: null
            }
          }
        """,
        query = """
          observations {
            scienceRequirements {
              spectroscopy {
                focalPlane
              }
            }
          }
        """,
        expected = json"""
          {
            "updateObservations": {
              "observations": [
                {
                  "scienceRequirements": {
                    "spectroscopy": {
                      "focalPlane": null
                    }
                  }
                }
              ]
            }
          }
        """.asRight
      )
    } yield ()
  }

  private object imagingScienceRequirements {
    val update0: String = """
      scienceRequirements: {
        exposureTimeMode: {
          signalToNoise: {
            value: 75
            at: { nanometers: 410 }
          }
        }
        imaging: {
          minimumFov: {
            arcseconds: 150
          }
          narrowFilters: true
          broadFilters: true
          combinedFilters: true
        }
      }
    """

    val update1: String = """
      scienceRequirements: {
        exposureTimeMode: {
          signalToNoise: {
            value: 75
            at: { nanometers: 410 }
          }
        }
        imaging: {
          minimumFov: {
            arcseconds: 200
          }
          narrowFilters: true
          broadFilters: false
          combinedFilters: true

        }
      }
    """

    val update2: String = """
      scienceRequirements: {
        exposureTimeMode: {
          signalToNoise: {
            value: 75
            at: { nanometers: 410 }
          }
        }
        imaging: {
          minimumFov: {
            arcseconds: 200
          }
          narrowFilters: true
          broadFilters: false
          combinedFilters: false

        }
      }
    """

    val update3: String = """
      scienceRequirements: {
        exposureTimeMode: {
          signalToNoise: {
            value: 75
            at: { nanometers: 410 }
          }
        }
        imaging: {
          minimumFov: {
            arcseconds: 200
          }
          narrowFilters: true
          broadFilters: false
          combinedFilters: true

        }
      }
    """

    // update only broadFilters - other values should be preserved
    val update4 = """
      scienceRequirements: {
        imaging: {
          broadFilters: true
        }
      }
    """

    val query: String = """
      observations {
        scienceRequirements {
          mode
          exposureTimeMode {
            signalToNoise {
              value
              at { nanometers }
            }
          }
          imaging {
            minimumFov { arcseconds }
            narrowFilters
            broadFilters
            combinedFilters
          }
        }
      }
    """

    val expected0: Json = json"""
      {
        "updateObservations": {
          "observations": [
            {
              "scienceRequirements": {
                "mode": "IMAGING",
                "exposureTimeMode": {
                  "signalToNoise": {
                    "value": 75.000,
                    "at": {
                      "nanometers": 410.000
                    }
                  }
                },
                "imaging": {
                  "minimumFov": {
                    "arcseconds": 150
                  },
                  "narrowFilters": true,
                  "broadFilters": true,
                  "combinedFilters": true
                }
              }
            }
          ]
        }
      }
    """

    val expected1: Json = json"""
      {
        "updateObservations": {
          "observations": [
            {
              "scienceRequirements": {
                "mode": "IMAGING",
                "exposureTimeMode": {
                  "signalToNoise": {
                    "value": 75.000,
                    "at": {
                      "nanometers": 410.000
                    }
                  }
                },
                "imaging": {
                  "minimumFov": {
                    "arcseconds": 200
                  },
                  "narrowFilters": true,
                  "broadFilters": false,
                  "combinedFilters": true
                }
              }
            }
          ]
        }
      }
    """

    val expected2: Json = json"""
      {
        "updateObservations": {
          "observations": [
            {
              "scienceRequirements": {
                "mode": "IMAGING",
                "exposureTimeMode": {
                  "signalToNoise": {
                    "value": 75.000,
                    "at": {
                      "nanometers": 410.000
                    }
                  }
                },
                "imaging": {
                  "minimumFov": {
                    "arcseconds": 200
                  },
                  "narrowFilters": true,
                  "broadFilters": false,
                  "combinedFilters": false
                }
              }
            }
          ]
        }
      }
    """

    val expected3: Json = json"""
      {
        "updateObservations": {
          "observations": [
            {
              "scienceRequirements": {
                "mode": "IMAGING",
                "exposureTimeMode": {
                  "signalToNoise": {
                    "value": 75.000,
                    "at": {
                      "nanometers": 410.000
                    }
                  }
                },
                "imaging": {
                  "minimumFov": {
                    "arcseconds": 200
                  },
                  "narrowFilters": true,
                  "broadFilters": false,
                  "combinedFilters": true
                }
              }
            }
          ]
        }
      }
    """

    val expected4: Json = json"""
      {
        "updateObservations": {
          "observations": [
            {
              "scienceRequirements": {
                "mode": "IMAGING",
                "exposureTimeMode": {
                  "signalToNoise": {
                    "value": 75.000,
                    "at": {
                      "nanometers": 410.000
                    }
                  }
                },
                "imaging": {
                  "minimumFov": {
                    "arcseconds": 200
                  },
                  "narrowFilters": true,
                  "broadFilters": true,
                  "combinedFilters": true
                }
              }
            }
          ]
        }
      }
    """
  }

  test("imaging science requirements: multi updates") {
    multiUpdateTest(
      pi,
      List(
        (imagingScienceRequirements.update0, imagingScienceRequirements.query, imagingScienceRequirements.expected0.asRight),
        (imagingScienceRequirements.update1, imagingScienceRequirements.query, imagingScienceRequirements.expected1.asRight),
        (imagingScienceRequirements.update2, imagingScienceRequirements.query, imagingScienceRequirements.expected2.asRight),
        (imagingScienceRequirements.update3, imagingScienceRequirements.query, imagingScienceRequirements.expected3.asRight),
        (imagingScienceRequirements.update4, imagingScienceRequirements.query, imagingScienceRequirements.expected4.asRight)
      )
    )
  }

  test("science requirements: update from spectroscopy to imaging") {
    val spectroscopyUpdate = """
      scienceRequirements: {
        exposureTimeMode: {
          signalToNoise: {
            value: 100
            at: { nanometers: 500 }
          }
        }
        spectroscopy: {
          wavelength: { nanometers: 600 }
          resolution: 5000
          wavelengthCoverage: { nanometers: 100 }
          focalPlane: SINGLE_SLIT
          focalPlaneAngle: { arcseconds: 10 }
          capability: CORONAGRAPHY
        }
      }
    """

    val imagingUpdate = """
      scienceRequirements: {
        exposureTimeMode: {
          signalToNoise: {
            value: 75
            at: { nanometers: 410 }
          }
        }
        imaging: {
          minimumFov: { arcseconds: 120 }
          narrowFilters: true
          broadFilters: false
          combinedFilters: true
        }
      }
    """

    val query = """
      observations {
        scienceRequirements {
          mode
          exposureTimeMode {
            signalToNoise {
              value
              at { nanometers }
            }
          }
          spectroscopy {
            wavelength { nanometers }
            resolution
            wavelengthCoverage { nanometers }
            focalPlane
            focalPlaneAngle { arcseconds }
            capability
          }
          imaging {
            minimumFov { arcseconds }
            narrowFilters
            broadFilters
            combinedFilters
          }
        }
      }
    """

    val expectedSpectroscopy = json"""
      {
        "updateObservations": {
          "observations": [
            {
              "scienceRequirements": {
                "mode": "SPECTROSCOPY",
                "exposureTimeMode": {
                  "signalToNoise": {
                    "value": 100.000,
                    "at": {
                      "nanometers": 500.000
                    }
                  }
                },
                "spectroscopy": {
                  "wavelength": {
                    "nanometers": 600.000
                  },
                  "resolution": 5000,
                  "wavelengthCoverage": {
                    "nanometers": 100.000
                  },
                  "focalPlane": "SINGLE_SLIT",
                  "focalPlaneAngle": {
                    "arcseconds": 10
                  },
                  "capability": "CORONAGRAPHY"
                },
                "imaging": null
              }
            }
          ]
        }
      }
    """.asRight

    val expectedImaging = json"""
      {
        "updateObservations": {
          "observations": [
            {
              "scienceRequirements": {
                "mode": "IMAGING",
                "exposureTimeMode": {
                  "signalToNoise": {
                    "value": 75.000,
                    "at": {
                      "nanometers": 410.000
                    }
                  }
                },
                "spectroscopy": null,
                "imaging": {
                  "minimumFov": {
                    "arcseconds": 120
                  },
                  "narrowFilters": true,
                  "broadFilters": false,
                  "combinedFilters": true
                }
              }
            }
          ]
        }
      }
    """.asRight

    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      // First set spectroscopy requirements
      _   <- updateObservation(pi, oid, spectroscopyUpdate, query, expectedSpectroscopy)
      // Then update to imaging requirements - spectroscopy fields should be null
      _   <- updateObservation(pi, oid, imagingUpdate, query, expectedImaging)
    } yield ()
  }

  /*

  [ERROR] lucuma-odb-test - Error computing GraphQL respon.ignorese.
  java.lang.AssertionError: assertion failed
    at get @ skunk.util.Pool$.free$1(Pool.scala:148)
    at get @ skunk.util.Pool$.free$1(Pool.scala:148)
    at flatMap @ fs2.Compiler$Target.flatMap(Compiler.scala:163)
    at flatMap @ fs2.Compiler$Target.flatMap(Compiler.scala:163)
    at flatMap @ fs2.Pull$.fs2$Pull$$$_$interruptGuard$1(Pull.scala:938)
    at unsafeRunSync @ munit.CatsEffectFixturesPlatform$$anon$1.beforeAll(CatsEffectFixturesPlatform.scala:41)
    at *> @ skunk.net.SSLNegotiation$.negotiateSSL(SSLNegotiation.scala:49)

  test("delete science requirements spectroscopy wavelength") {
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      _ <- query(pi, updateObservationsMutation(pid, oid, scienceRequirements.update, scienceRequirements.query))
      _ <- updateObservation(
        user = pi,
        pid = pid,
        oid = oid,
        update = """
          scienceRequirements: {
            spectroscopy: {
              wavelength: null
            }
          }
        """,
        query = """
          scienceRequirements {
            spectroscopy {
              wavelength { nanometers }
            }
          }
        """,
        expected = json"""
          {
            "updateObservations": [
              {
                "scienceRequirements": {
                  "spectroscopy": {
                    "wavelength": null
                  }
                }
              }
            ]
          }
        """.asRight
      )
    } yield ()
  }
*/

  test("observing mode: create in an existing observation") {

    val update = """
      observingMode: {
        gmosNorthLongSlit: {
          grating: B1200_G5301
          filter: G_PRIME
          fpu: LONG_SLIT_0_25
          centralWavelength: {
            nanometers: 234.56
          }
        }
      }
    """

    val query = """
      observations {
        instrument
        observingMode {
          gmosNorthLongSlit {
            grating
            filter
            fpu
            centralWavelength {
              nanometers
            }
          }
          flamingos2LongSlit {
            disperser
            filter
            fpu
          }
        }
      }
    """

    val expected =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "GMOS_NORTH",
              "observingMode": {
                "gmosNorthLongSlit": {
                  "grating": "B1200_G5301",
                  "filter": "G_PRIME",
                  "fpu": "LONG_SLIT_0_25",
                  "centralWavelength": {
                    "nanometers": 234.560
                  }
                },
                "flamingos2LongSlit": null
              }
            }
          ]
        }
      }
    """.asRight

    val update1 = """
      observingMode: {
        flamingos2LongSlit: {
          disperser: R1200_JH
          filter: Y
          fpu: LONG_SLIT_2
        }
      }
    """

    val expected1 =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "FLAMINGOS2",
              "observingMode": {
                "gmosNorthLongSlit": null,
                "flamingos2LongSlit": {
                  "disperser": "R1200_JH",
                  "filter": "Y",
                  "fpu": "LONG_SLIT_2"
                }
              }
            }
          ]
        }
      }
    """.asRight

    multiUpdateTest(pi,
      List(
        (update, query, expected),
        (update1, query, expected1)
      )
    )
  }

  test("observing mode: (fail to) create in an existing observation") {

    val update = """
      observingMode: {
        gmosNorthLongSlit: {
          grating: B1200_G5301
          filter: G_PRIME
          fpu: LONG_SLIT_0_25
        }
      }
    """

    val query = """
      observations {
        observingMode {
          gmosNorthLongSlit {
            grating
            filter
            fpu
            centralWavelength {
              nanometers
            }
          }
        }
      }
    """

    val expected =  "A centralWavelength is required in order to create a GMOS North Long Slit observing mode.".asLeft
    oneUpdateTest(pi, update, query, expected)
  }

  test("observing mode: update existing") {

    val update0 = """
      observingMode: {
        gmosNorthLongSlit: {
          grating: B1200_G5301
          filter: G_PRIME
          fpu: LONG_SLIT_0_25
          centralWavelength: {
            nanometers: 234.56
          }
        }
      }
    """

    val query = """
      observations {
        instrument
        observingMode {
          gmosNorthLongSlit {
            grating
          }
          flamingos2LongSlit {
            disperser
            filter
            fpu
            initialDisperser
            initialFilter
            initialFpu
          }
        }
      }
    """

    val expected0 =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "GMOS_NORTH",
              "observingMode": {
                "gmosNorthLongSlit": {
                  "grating": "B1200_G5301"
                },
                "flamingos2LongSlit": null
              }
            }
          ]
        }
      }
    """.asRight

    val update1 = """
      observingMode: {
        gmosNorthLongSlit: {
          grating: R831_G5302
        }
      }
    """

    val expected1 =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "GMOS_NORTH",
              "observingMode": {
                "gmosNorthLongSlit": {
                  "grating": "R831_G5302"
                },
                "flamingos2LongSlit": null
              }
            }
          ]
        }
      }
    """.asRight

    val update2 = """
      observingMode: {
        flamingos2LongSlit: {
          disperser: R1200_JH
          filter: Y
          fpu: LONG_SLIT_2
        }
      }
    """

    val expected2 =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "FLAMINGOS2",
              "observingMode": {
                "gmosNorthLongSlit": null,
                "flamingos2LongSlit": {
                  "disperser": "R1200_JH",
                  "filter": "Y",
                  "fpu": "LONG_SLIT_2",
                  "initialDisperser": "R1200_JH",
                  "initialFilter": "Y",
                  "initialFpu": "LONG_SLIT_2"
                }
              }
            }
          ]
        }
      }
    """.asRight

    val update3 = """
      observingMode: {
        flamingos2LongSlit: {
          disperser: R1200_HK
          filter: Y
          fpu: LONG_SLIT_2
        }
      }
    """

    val expected3 =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "FLAMINGOS2",
              "observingMode": {
                "gmosNorthLongSlit": null,
                "flamingos2LongSlit": {
                  "disperser": "R1200_HK",
                  "filter": "Y",
                  "fpu": "LONG_SLIT_2",
                  "initialDisperser": "R1200_JH",
                  "initialFilter": "Y",
                  "initialFpu": "LONG_SLIT_2"
                }
              }
            }
          ]
        }
      }
    """.asRight

    multiUpdateTest(pi,
      List(
        (update0, query, expected0),
        (update1, query, expected1),
        (update2, query, expected2),
        (update3, query, expected3)
      )
    )
  }

  test("observing mode: update existing") {

    val update0 = """
      observingMode: {
        gmosNorthLongSlit: {
          grating: B1200_G5301
          filter: G_PRIME
          fpu: LONG_SLIT_0_25
          centralWavelength: {
            nanometers: 234.56
          }
        }
      }
    """

    val query = """
      observations {
        instrument
        observingMode {
          gmosNorthLongSlit {
            grating
          }
        }
      }
    """

    val expected0 =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "GMOS_NORTH",
              "observingMode": {
                "gmosNorthLongSlit": {
                  "grating": "B1200_G5301"
                }
              }
            }
          ]
        }
      }
    """.asRight

    val update1 = """
      observingMode: {
        gmosNorthLongSlit: {
          grating: R831_G5302
        }
      }
    """

    val expected1 =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "GMOS_NORTH",
              "observingMode": {
                "gmosNorthLongSlit": {
                  "grating": "R831_G5302"
                }
              }
            }
          ]
        }
      }
    """.asRight

    multiUpdateTest(pi, List((update0, query, expected0), (update1, query, expected1)))
  }

  test("observing mode: existing f2 update read_mode updates reads") {

    val update0 = """
      subtitle: "sub"
    """

    val expected0 =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "FLAMINGOS2",
              "observingMode": {
                "mode": "FLAMINGOS_2_LONG_SLIT",
                "gmosNorthLongSlit": null,
                "flamingos2LongSlit": {
                  "disperser": "R1200_HK",
                  "explicitReadMode": null,
                  "explicitReads": null,
                  "decker": "LONG_SLIT",
                  "defaultDecker": "LONG_SLIT",
                  "explicitDecker": null,
                  "readoutMode": "SCIENCE",
                  "defaultReadoutMode": "SCIENCE",
                  "explicitReadoutMode": null
                }
              }
            }
          ]
        }
      }
    """.asRight

    val query = """
      observations {
        instrument
        observingMode {
          mode
          gmosNorthLongSlit {
            grating
          }
          flamingos2LongSlit {
            disperser
            explicitReadMode
            explicitReads
            decker
            defaultDecker
            explicitDecker
            readoutMode
            defaultReadoutMode
            explicitReadoutMode
          }
        }
      }
    """

    val update1 = """
      observingMode: {
        flamingos2LongSlit: {
          explicitReadMode: BRIGHT
          explicitDecker: MOS
          explicitReadoutMode: ENGINEERING
        }
      }
    """

    // read mode update implicitly updates reads
    val expected1 =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "FLAMINGOS2",
              "observingMode": {
                "mode": "FLAMINGOS_2_LONG_SLIT",
                "gmosNorthLongSlit": null,
                "flamingos2LongSlit": {
                  "disperser": "R1200_HK",
                  "explicitReadMode": "BRIGHT",
                  "explicitReads": null,
                  "decker": "MOS",
                  "defaultDecker": "LONG_SLIT",
                  "explicitDecker": "MOS",
                  "readoutMode": "ENGINEERING",
                  "defaultReadoutMode": "SCIENCE",
                  "explicitReadoutMode": "ENGINEERING"
                }
              }
            }
          ]
        }
      }
    """.asRight

    multiUpdateTest(pi,
      List(
        (update0, query, expected0),
        (update1, query, expected1)
      ),
      observingMode = Some(
        ObservingModeType.Flamingos2LongSlit
      )
    )
  }

  test("observing mode: update Flamingos2 spatial offsets") {

    val update0 = """
      observingMode: {
        flamingos2LongSlit: {
          disperser: R1200_JH
          filter: Y
          fpu: LONG_SLIT_2
          explicitOffsets: [
            { p: { arcseconds: 0.0 }, q: { arcseconds: -5.0 } },
            { p: { arcseconds: 0.0 }, q: { arcseconds:  5.0 } },
            { p: { arcseconds: 0.0 }, q: { arcseconds:  3.5 } },
            { p: { arcseconds: 0.0 }, q: { arcseconds: -2.5 } }
          ]
        }
      }
    """

    val query = """
      observations {
        instrument
        observingMode {
          flamingos2LongSlit {
            disperser
            filter
            fpu
            offsets {
              p {
                arcseconds
              }
              q {
                arcseconds
              }
            }
            explicitOffsets {
              p {
                arcseconds
              }
              q {
                arcseconds
              }
            }
            defaultOffsets {
              p {
                arcseconds
              }
              q {
                arcseconds
              }
            }
          }
        }
      }
    """

    val expected0 = json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "FLAMINGOS2",
              "observingMode": {
                "flamingos2LongSlit": {
                  "disperser": "R1200_JH",
                  "filter": "Y",
                  "fpu": "LONG_SLIT_2",
                  "offsets": [
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": -5.000000 } },
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds":  5.000000 } },
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds":  3.500000 } },
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": -2.500000 } }
                  ],
                  "explicitOffsets": [
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": -5.000000 } },
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds":  5.000000 } },
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds":  3.500000 } },
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": -2.500000 } }
                  ],
                  "defaultOffsets": [
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": 15.000000 } },
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": -15.000000 } },
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": -15.000000 } },
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": 15.000000 } }
                  ]
                }
              }
            }
          ]
        }
      }
    """.asRight

    val update1 = """
      observingMode: {
        flamingos2LongSlit: {
          explicitOffsets: [
            { p: { arcseconds: 0.0 }, q: { arcseconds: -1.5 } },
            { p: { arcseconds: 0.0 }, q: { arcseconds:  1.5 } },
            { p: { arcseconds: 0.0 }, q: { arcseconds: -2.5 } },
            { p: { arcseconds: 0.0 }, q: { arcseconds:  2.5 } }
          ]
        }
      }
    """

    val expected1 = json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "FLAMINGOS2",
              "observingMode": {
                "flamingos2LongSlit": {
                  "disperser": "R1200_JH",
                  "filter": "Y",
                  "fpu": "LONG_SLIT_2",
                  "offsets": [
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": -1.500000 } },
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds":  1.500000 } },
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": -2.500000 } },
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds":  2.500000 } }
                  ],
                  "explicitOffsets": [
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": -1.500000 } },
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds":  1.500000 } },
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": -2.500000 } },
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds":  2.500000 } }
                  ],
                  "defaultOffsets": [
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": 15.000000 } },
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": -15.000000 } },
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": -15.000000 } },
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": 15.000000 } }
                  ]
                }
              }
            }
          ]
        }
      }
    """.asRight

    val update2 = """
      observingMode: {
        flamingos2LongSlit: {
          explicitOffsets: null
        }
      }
    """

    val expected2 = json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "FLAMINGOS2",
              "observingMode": {
                "flamingos2LongSlit": {
                  "disperser": "R1200_JH",
                  "filter": "Y",
                  "fpu": "LONG_SLIT_2",
                  "offsets": [
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": 15.000000 } },
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": -15.000000 } },
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": -15.000000 } },
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": 15.000000 } }
                  ],
                  "explicitOffsets": null,
                  "defaultOffsets": [
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": 15.000000 } },
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": -15.000000 } },
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": -15.000000 } },
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": 15.000000 } }
                  ]
                }
              }
            }
          ]
        }
      }
    """.asRight

    multiUpdateTest(pi,
      List(
        (update0, query, expected0),
        (update1, query, expected1),
        (update2, query, expected2)
      ),
      observingMode = Some(ObservingModeType.Flamingos2LongSlit)
    )
  }

  test("observing mode: update existing, all fields") {

    val update0 = """
      observingMode: {
        gmosNorthLongSlit: {
          grating: B1200_G5301
          filter: G_PRIME
          fpu: LONG_SLIT_0_25
          centralWavelength: {
            nanometers: 234.56
          }
          explicitXBin: FOUR
          explicitYBin: FOUR
          explicitAmpReadMode: FAST
          explicitAmpGain: HIGH
          explicitRoi: CCD2
          explicitWavelengthDithers: [
            { nanometers: -7.5 },
            { nanometers:  7.1 },
            { nanometers:  7.1 },
            { nanometers: -7.5 }
          ],
          explicitSpatialOffsets: [
            { arcseconds: -10.0 },
            { arcseconds:  10.0 },
            { arcseconds:  10.0 },
            { arcseconds: -10.0 }
          ]
        }
      }
    """

    val query = """
      observations {
        instrument
        observingMode {
          gmosNorthLongSlit {
            grating
            filter
            fpu
            centralWavelength {
              nanometers
            }
            explicitXBin
            explicitYBin
            explicitAmpReadMode
            explicitAmpGain
            explicitRoi
            explicitWavelengthDithers {
              picometers
            }
            explicitOffsets {
              arcseconds
            }
            explicitSpatialOffsets {
              arcseconds
            }
            offsets {
              arcseconds
            }
            spatialOffsets {
              arcseconds
            }
          }
        }
      }
    """

    val expected0 =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "GMOS_NORTH",
              "observingMode": {
                "gmosNorthLongSlit": {
                  "grating": "B1200_G5301",
                  "filter": "G_PRIME",
                  "fpu": "LONG_SLIT_0_25",
                  "centralWavelength": {
                    "nanometers": 234.560
                  },
                  "explicitXBin": "FOUR",
                  "explicitYBin": "FOUR",
                  "explicitAmpReadMode": "FAST",
                  "explicitAmpGain": "HIGH",
                  "explicitRoi": "CCD2",
                  "explicitWavelengthDithers": [
                    { "picometers": -7500 },
                    { "picometers":  7100 },
                    { "picometers":  7100 },
                    { "picometers": -7500 }
                  ],
                  "explicitOffsets": [
                    { "arcseconds": -10.000000 },
                    { "arcseconds":  10.000000 },
                    { "arcseconds":  10.000000 },
                    { "arcseconds": -10.000000 }
                  ],
                  "explicitSpatialOffsets": [
                    { "arcseconds": -10.000000 },
                    { "arcseconds":  10.000000 },
                    { "arcseconds":  10.000000 },
                    { "arcseconds": -10.000000 }
                  ],
                  "offsets": [
                    { "arcseconds": -10.000000 },
                    { "arcseconds":  10.000000 },
                    { "arcseconds":  10.000000 },
                    { "arcseconds": -10.000000 }
                  ],
                  "spatialOffsets": [
                    { "arcseconds": -10.000000 },
                    { "arcseconds":  10.000000 },
                    { "arcseconds":  10.000000 },
                    { "arcseconds": -10.000000 }
                  ]
                }
              }
            }
          ]
        }
      }
    """.asRight

    val update1 =
      """
      observingMode: {
        gmosNorthLongSlit: {
          grating: R831_G5302
          filter: R_PRIME
          fpu: LONG_SLIT_0_50
          centralWavelength: {
            nanometers: 654.321
          }
          explicitXBin: ONE
          explicitYBin: ONE
          explicitAmpReadMode: SLOW
          explicitAmpGain: LOW
          explicitRoi: CENTRAL_SPECTRUM
          explicitWavelengthDithers: [
            { nanometers: -10 },
            { nanometers:  10 }
          ],
          explicitSpatialOffsets: [
            { arcseconds: -2.0 },
            { arcseconds:  2.0 },
            { arcseconds:  2.0 },
            { arcseconds: -2.0 }
          ]
        }
      }
    """
    val expected1 =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "GMOS_NORTH",
              "observingMode": {
                "gmosNorthLongSlit": {
                  "grating": "R831_G5302",
                  "filter": "R_PRIME",
                  "fpu": "LONG_SLIT_0_50",
                  "centralWavelength": {
                    "nanometers": 654.321
                  },
                  "explicitXBin": "ONE",
                  "explicitYBin": "ONE",
                  "explicitAmpReadMode": "SLOW",
                  "explicitAmpGain": "LOW",
                  "explicitRoi": "CENTRAL_SPECTRUM",
                  "explicitWavelengthDithers": [
                    { "picometers": -10000 },
                    { "picometers":  10000 }
                  ],
                  "explicitOffsets": [
                    { "arcseconds": -2.000000 },
                    { "arcseconds":  2.000000 },
                    { "arcseconds":  2.000000 },
                    { "arcseconds": -2.000000 }
                  ],
                  "explicitSpatialOffsets": [
                    { "arcseconds": -2.000000 },
                    { "arcseconds":  2.000000 },
                    { "arcseconds":  2.000000 },
                    { "arcseconds": -2.000000 }
                  ],
                  "offsets": [
                    { "arcseconds": -2.000000 },
                    { "arcseconds":  2.000000 },
                    { "arcseconds":  2.000000 },
                    { "arcseconds": -2.000000 }
                  ],
                  "spatialOffsets": [
                    { "arcseconds": -2.000000 },
                    { "arcseconds":  2.000000 },
                    { "arcseconds":  2.000000 },
                    { "arcseconds": -2.000000 }
                  ]
                }
              }
            }
          ]
        }
      }
    """.asRight

    multiUpdateTest(pi, List((update0, query, expected0), (update1, query, expected1)))
  }

  test("observing mode: switch mode types") {

    val update0 =
      """
      observingMode: {
        gmosNorthLongSlit: {
          grating: B1200_G5301
          filter: G_PRIME
          fpu: LONG_SLIT_0_25
          centralWavelength: {
            nanometers: 234.56
          }
        }
      }
    """

    val query0 =
      """
      observations {
        instrument
        observingMode {
          gmosNorthLongSlit {
            grating
          }
        }
      }
    """

    val expected0 =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "GMOS_NORTH",
              "observingMode": {
                "gmosNorthLongSlit": {
                  "grating": "B1200_G5301"
                }
              }
            }
          ]
        }
      }
    """.asRight

    val update1 =
      """
      observingMode: {
        gmosSouthLongSlit: {
          grating: R831_G5322
          filter: G_PRIME
          fpu: LONG_SLIT_0_25
          centralWavelength: {
            nanometers: 234.56
          }
        }
      }
    """

    val query1 =
      """
      observations {
        instrument
        observingMode {
          gmosSouthLongSlit {
            grating
          }
        }
      }
    """

    val expected1 =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "GMOS_SOUTH",
              "observingMode": {
                "gmosSouthLongSlit": {
                  "grating": "R831_G5322"
                }
              }
            }
          ]
        }
      }
    """.asRight

    multiUpdateTest(pi, List((update0, query0, expected0), (update1, query1, expected1)))
  }

  test("observing mode: (fail to) switch mode types") {

    val update0 =
      """
      observingMode: {
        gmosNorthLongSlit: {
          grating: B1200_G5301
          filter: G_PRIME
          fpu: LONG_SLIT_0_25
          centralWavelength: {
            nanometers: 234.56
          }
        }
      }
    """

    val query0 =
      """
      observations {
        observingMode {
          gmosNorthLongSlit {
            grating
          }
        }
      }
    """

    val expected0 =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "observingMode": {
                "gmosNorthLongSlit": {
                  "grating": "B1200_G5301"
                }
              }
            }
          ]
        }
      }
    """.asRight

    val update1 =
      """
      observingMode: {
        gmosSouthLongSlit: {
          grating: R831_G5322
          filter: G_PRIME
          fpu: LONG_SLIT_0_25
        }
      }
    """

    val query1 =
      """
      observations {
        observingMode {
          gmosSouthLongSlit {
            grating
          }
        }
      }
    """

    val expected1 = "A centralWavelength is required in order to create a GMOS South Long Slit observing mode.".asLeft

    multiUpdateTest(pi, List((update0, query0, expected0), (update1, query1, expected1)))
  }

  test("observing mode: delete f2") {

    val update0 = """
      subtitle: "sub"
    """

    val expected0 =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "FLAMINGOS2",
              "observingMode": {
                "mode": "FLAMINGOS_2_LONG_SLIT",
                "gmosNorthLongSlit": null,
                "flamingos2LongSlit": {
                  "disperser": "R1200_HK"
                }
              }
            }
          ]
        }
      }
    """.asRight

    val query = """
      observations {
        instrument
        observingMode {
          mode
          gmosNorthLongSlit {
            grating
          }
          flamingos2LongSlit {
            disperser
          }
        }
      }
    """

    val update1 = """
      observingMode: null
    """

    val expected1 =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": null,
              "observingMode": null
            }
          ]
        }
      }
    """.asRight

    multiUpdateTest(pi,
      List(
        (update0, query, expected0),
        (update1, query, expected1)
      ),
      observingMode = Some(
        ObservingModeType.Flamingos2LongSlit
      )
    )
  }

  test("observing mode: delete existing") {

    val update0 = """
      observingMode: {
        gmosNorthLongSlit: {
          grating: B1200_G5301
          filter: G_PRIME
          fpu: LONG_SLIT_0_25
          centralWavelength: {
            nanometers: 234.56
          }
        }
      }
    """

    val query = """
      observations {
        instrument
        observingMode {
          mode
          gmosNorthLongSlit {
            grating
          }
          flamingos2LongSlit {
            disperser
          }
        }
      }
    """

    val expected0 =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "GMOS_NORTH",
              "observingMode": {
                "mode": "GMOS_NORTH_LONG_SLIT",
                "gmosNorthLongSlit": {
                  "grating": "B1200_G5301"
                },
                "flamingos2LongSlit": null
              }
            }
          ]
        }
      }
    """.asRight

    val update1 = """
      observingMode: null
    """

    val expected1 =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": null,
              "observingMode": null
            }
          ]
        }
      }
    """.asRight

    val update2 = """
      observingMode: {
        flamingos2LongSlit: {
          disperser: R1200_JH
          filter: Y
          fpu: LONG_SLIT_2
        }
      }
    """

    val expected2 =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "FLAMINGOS2",
              "observingMode": {
                "mode": "FLAMINGOS_2_LONG_SLIT",
                "gmosNorthLongSlit": null,
                "flamingos2LongSlit": {
                  "disperser": "R1200_JH"
                }
              }
            }
          ]
        }
      }
    """.asRight

    multiUpdateTest(pi,
      List(
        (update0, query, expected0),
        (update1, query, expected1),
        (update2, query, expected2)
        )
      )
  }

  def moveObservationsAs(user: User, oids: List[Observation.Id], gid: Option[Group.Id], index: Option[NonNegShort]): IO[Unit] =
    query(
      user = user,
      query = s"""
        mutation {
          updateObservations(input: {
            SET: {
              groupId: ${gid.asJson}
              ${index.map(_.value.asJson).foldMap(j => s"groupIndex: $j")}
            },
            WHERE: {
              id: { IN: ${oids.asJson} }
            }
          }) {
            observations {
              id
            }
          }
        }
      """
    ).void

  def oidElementSet(os: Observation.Id*): Set[Either[Group.Id, Observation.Id]] =
    os.map(_.asRight[Group.Id]).toSet

  test("grouping: move observations into a group (at end)") {
    for {
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid)
      o1  <- createObservationInGroupAs(pi, pid, None, None)
      o2  <- createObservationInGroupAs(pi, pid, None, None)
      o3  <- createObservationInGroupAs(pi, pid, Some(gid), None)
      _   <- moveObservationsAs(pi, List(o1, o2), Some(gid), None)
      es  <- groupElementsAs(pi, pid, Some(gid))
    } yield {
      assertEquals(es.toSet, oidElementSet(o2, o1, o3))
      assertEquals(es.drop(1).toSet, oidElementSet(o2, o1))
    }
  }

  test("grouping: move observations into a group (at beginning)") {
    for {
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid)
      o1  <- createObservationInGroupAs(pi, pid, None, None)
      o2  <- createObservationInGroupAs(pi, pid, None, None)
      o3  <- createObservationInGroupAs(pi, pid, Some(gid), None)
      _   <- moveObservationsAs(pi, List(o1, o2), Some(gid), Some(NonNegShort.unsafeFrom(0)))
      es  <- groupElementsAs(pi, pid, Some(gid))
    } yield {
      assertEquals(es.toSet, oidElementSet(o2, o1, o3))
      assertEquals(es.dropRight(1).toSet, oidElementSet(o2, o1))
    }
  }

  test("grouping: move observations into a group (in the middle)") {
    for {
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid)
      o1  <- createObservationInGroupAs(pi, pid, None, None)
      o2  <- createObservationInGroupAs(pi, pid, None, None)
      o3  <- createObservationInGroupAs(pi, pid, Some(gid), None)
      o4  <- createObservationInGroupAs(pi, pid, Some(gid), None)
      _   <- moveObservationsAs(pi, List(o1, o2), Some(gid), Some(NonNegShort.unsafeFrom(1)))
      es  <- groupElementsAs(pi, pid, Some(gid))
    } yield {
      assertEquals(es.toSet, oidElementSet(o2, o1, o3, o4))
      assertEquals(es.drop(1).dropRight(1).toSet, oidElementSet(o2, o1))
    }
  }

  test("grouping: move observations out of a group (at end of program)") {
    for {
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid)
      o1  <- createObservationInGroupAs(pi, pid, None, None)
      o2  <- createObservationInGroupAs(pi, pid, Some(gid), None)
      o3  <- createObservationInGroupAs(pi, pid, Some(gid), None)
      _   <- moveObservationsAs(pi, List(o2, o3), None, None)
      es  <- groupElementsAs(pi, pid, None)
    } yield {
      assertEquals(es.take(2), List(Left(gid), Right(o1)))
      assertEquals(es.drop(2).toSet, oidElementSet(o2, o3))
    }
  }

  test("grouping: move observations out of a group (at beginning of program)") {
    for {
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid)
      o1  <- createObservationInGroupAs(pi, pid, None, None)
      o2  <- createObservationInGroupAs(pi, pid, Some(gid), None)
      o3  <- createObservationInGroupAs(pi, pid, Some(gid), None)
      _   <- moveObservationsAs(pi, List(o2, o3), None, Some(NonNegShort.unsafeFrom(0)))
      es  <- groupElementsAs(pi, pid, None)
    } yield {
      assertEquals(es.take(2).toSet, oidElementSet(o2, o3))
      assertEquals(es.drop(2), List(Left(gid), Right(o1)))
    }
  }

  test("grouping: move observations out of a group (in the middle of program)") {
    for {
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid)
      o1  <- createObservationInGroupAs(pi, pid, None, None)
      o2  <- createObservationInGroupAs(pi, pid, Some(gid), None)
      o3  <- createObservationInGroupAs(pi, pid, Some(gid), None)
      _   <- moveObservationsAs(pi, List(o2, o3), None, Some(NonNegShort.unsafeFrom(1)))
      es  <- groupElementsAs(pi, pid, None)
    } yield {
      assertEquals(es(0), Left(gid))
      assertEquals(es.drop(1).take(2).toSet, oidElementSet(o2, o3))
      assertEquals(es(3), Right(o1))
    }
  }

  test("grouping: move observations between groups") {
    for {
      pid <- createProgramAs(pi)
      g1  <- createGroupAs(pi, pid)
      g2  <- createGroupAs(pi, pid)
      o1  <- createObservationInGroupAs(pi, pid, Some(g1), None)
      o2  <- createObservationInGroupAs(pi, pid, Some(g1), None)
      o3  <- createObservationInGroupAs(pi, pid, Some(g1), None)
      _   <- moveObservationsAs(pi, List(o2, o3), Some(g2), None)
      e1  <- groupElementsAs(pi, pid, Some(g1))
      e2  <- groupElementsAs(pi, pid, Some(g2))
    } yield {
      assertEquals(e1, List(Right(o1)))
      assertEquals(e2.toSet, oidElementSet(o2, o3))
    }
  }

  test("grouping: Hugo's example") {
    for {
      pid <- createProgramAs(pi)
      o1  <- createObservationAs(pi, pid)
      o2  <- createObservationAs(pi, pid)
      g   <- createGroupAs(pi, pid)
      _   <- moveObservationsAs(pi, List(o1), Some(g), None)
    } yield ()
  }

  test("update observer notes") {
    oneUpdateTest(
      user   = pi,
      update = """
        observerNotes: "My new note"
      """,
      query = """
        observations {
          observerNotes
        }
      """,
      expected = json"""
        {
          "updateObservations": {
            "observations": [
              {
                "observerNotes": "My new note"
              }
            ]
          }
        }
      """.asRight
    )
  }

  test("update scienceBand") {
    for {
      pid <- createProgramAs(pi)
      _   <- setAllocationsAs(staff, pid, List(AllocationInput(TimeAccountingCategory.US, ScienceBand.Band2, 1.hourTimeSpan)))
      oid <- createObservationAs(pi, pid)
      _   <- setScienceBandAs(pi, oid, ScienceBand.Band2.some)
      b1  <- observationsWhere(pi, "scienceBand: { EQ: BAND2 }")
    } yield assertEquals(b1, List(oid))
  }

  test("null scienceBand") {
    for {
      pid <- createProgramAs(pi)
      _   <- setAllocationsAs(staff, pid, List(AllocationInput(TimeAccountingCategory.US, ScienceBand.Band2, 1.hourTimeSpan)))
      oid <- createObservationAs(pi, pid)
      _   <- setScienceBandAs(pi, oid, ScienceBand.Band2.some)
      _   <- setScienceBandAs(pi, oid, none[ScienceBand])
      bn  <- observationsWhere(pi, s"""program: { id: { EQ: "$pid" } }, scienceBand: { IS_NULL: true }""")
    } yield assertEquals(bn, List(oid))
  }

  test("attempt to assign an invalid scienceBand") {
    for {
      pid <- createProgramAs(pi)
      _   <- setAllocationsAs(staff, pid, List(AllocationInput(TimeAccountingCategory.US, ScienceBand.Band2, 1.hourTimeSpan)))
      oid <- createObservationAs(pi, pid)
      _   <- expect(pi,
        s"""
          mutation {
            updateObservations(
              input: {
                SET: {
                  scienceBand: BAND1
                }
                WHERE: {
                  id: { EQ: "$oid" }
                }
              }
            ) {
              observations {
                id
              }
            }
          }
        """,
        List(s"One or more programs have not been allocated time in BAND1: $pid").asLeft
      )
    } yield ()
  }

  test("science mode auto-set: spectroscopy fields trigger spectroscopy mode") {
    createProgramAs(pi).flatMap { pid =>
      query(pi, s"""
        mutation {
          createObservation(input: {
            programId: ${pid.asJson}
            SET: {
              scienceRequirements: {
                spectroscopy: {
                  wavelength: { micrometers: 1.5 }
                  resolution: 1000
                }
              }
            }
          }) {
            observation {
              id
              scienceRequirements {
                mode
                spectroscopy {
                  wavelength { micrometers }
                  resolution
                }
                imaging {
                  minimumFov { microarcseconds }
                  narrowFilters
                }
              }
            }
          }
        }
      """).map { result =>
        assertEquals(
          result.hcursor.downFields("createObservation", "observation", "scienceRequirements").focus,
          Some(json"""{
            "mode": "SPECTROSCOPY",
            "spectroscopy": {
              "wavelength": {
                "micrometers": 1.5
              },
              "resolution": 1000
            },
            "imaging": null
          }""")
        )
      }
    }
  }

  test("science mode auto-set: imaging fields trigger imaging mode") {
    createProgramAs(pi).flatMap { pid =>
      query(pi, s"""
        mutation {
          createObservation(input: {
            programId: ${pid.asJson}
            SET: {
              scienceRequirements: {
                imaging: {
                  minimumFov: { microarcseconds: 1000000 }
                  narrowFilters: true
                }
              }
            }
          }) {
            observation {
              id
              scienceRequirements {
                mode
                spectroscopy {
                  wavelength { micrometers }
                  resolution
                }
                imaging {
                  minimumFov { microarcseconds }
                  narrowFilters
                }
              }
            }
          }
        }
      """).map { result =>
        assertEquals(
          result.hcursor.downFields("createObservation", "observation", "scienceRequirements").focus,
          Some(json"""{
            "mode": "IMAGING",
            "spectroscopy": null,
            "imaging": {
              "minimumFov": {
                "microarcseconds": 1000000
              },
              "narrowFilters": true
            }
          }""")
        )
      }
    }
  }

  test("science mode auto-set: clearing all fields sets mode to null"):
    createProgramAs(pi).flatMap { pid =>
      // First create observation with only one spectroscopy field
      query(pi, s"""
        mutation {
          createObservation(input: {
            programId: ${pid.asJson}
            SET: {
              scienceRequirements: {
                spectroscopy: {
                  wavelength: { micrometers: 1.5 }
                }
              }
            }
          }) {
            observation { id }
          }
        }
      """).flatMap { js =>
        js.hcursor.downFields("createObservation", "observation", "id").as[Observation.Id]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
      }.flatMap { oid =>
        // Then update to clear the only field
        query(pi, s"""
          mutation {
            updateObservations(input: {
              SET: {
                scienceRequirements: {
                  spectroscopy: {
                    wavelength: null
                  }
                }
              }
              WHERE: {
                id: { EQ: "$oid" }
              }
            }) {
              observations { id }
            }
          }
        """).flatMap { _ =>
          query(
            pi,
            s"""
              query {
                observation(observationId: "$oid") {
                  scienceRequirements {
                    mode
                    spectroscopy {
                      wavelength { micrometers }
                      resolution
                      wavelengthCoverage { micrometers }
                      focalPlane
                      focalPlaneAngle { microarcseconds }
                      capability
                    }
                    imaging {
                      minimumFov { microarcseconds }
                      narrowFilters
                    }
                  }
                }
              }
            """
          ).map { result =>
            // When all spectroscopy fields are null, the mode should be set to null
            assertEquals(
              result,
              json"""
                {
                  "observation": {
                    "scienceRequirements": {
                      "mode": null,
                      "spectroscopy": null,
                      "imaging": null
                    }
                  }
                }
              """
            )
          }
        }
      }
    }

  test("science mode auto-set: spectroscopy fields always trigger spectroscopy mode"):
    createProgramAs(pi).flatMap { pid =>
      query(pi, s"""
        mutation {
          createObservation(input: {
            programId: ${pid.asJson}
            SET: {
              scienceRequirements: {
                spectroscopy: {
                  wavelength: { micrometers: 1.5 }
                }
              }
            }
          }) {
            observation {
              id
              scienceRequirements {
                mode
                spectroscopy {
                  wavelength { micrometers }
                }
                imaging {
                  minimumFov { microarcseconds }
                  narrowFilters
                }
              }
            }
          }
        }
      """).map { result =>
        assertEquals(
          result.hcursor.downFields("createObservation", "observation", "scienceRequirements").focus,
          Some(json"""{
            "mode": "SPECTROSCOPY",
            "spectroscopy": {
              "wavelength": {
                "micrometers": 1.5
              }
            },
            "imaging": null
          }""")
        )
      }
    }

  test("observing mode: create GMOS imaging in an existing observation"):

    val update = """
      observingMode: {
        gmosNorthImaging: {
          filters: [G_PRIME, R_PRIME]
          explicitMultipleFiltersMode: INTERLEAVED
          explicitBin: TWO
          explicitAmpReadMode: FAST
          explicitAmpGain: HIGH
          explicitRoi: CCD2
        }
      }
    """

    val query = """
      observations {
        instrument
        observingMode {
          gmosNorthImaging {
            filters
            initialFilters
            multipleFiltersMode
            bin
            ampReadMode
            ampGain
            roi
          }
          gmosSouthImaging {
            filters
            initialFilters
            multipleFiltersMode
            bin
            ampReadMode
            ampGain
            roi
          }
        }
      }
    """

    val expected =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "GMOS_NORTH",
              "observingMode": {
                "gmosNorthImaging": {
                  "filters": ["G_PRIME", "R_PRIME"],
                  "initialFilters": ["G_PRIME", "R_PRIME"],
                  "multipleFiltersMode": "INTERLEAVED",
                  "bin": "TWO",
                  "ampReadMode": "FAST",
                  "ampGain": "HIGH",
                  "roi": "CCD2"
                },
                "gmosSouthImaging": null
              }
            }
          ]
        }
      }
    """.asRight

    oneUpdateTest(pi, update, query, expected)

  test("observing mode: update existing GMOS imaging"):

    val update0 = """
      observingMode: {
        gmosNorthImaging: {
          filters: [G_PRIME, R_PRIME]
          explicitBin: ONE
          explicitAmpReadMode: SLOW
          explicitAmpGain: LOW
          explicitRoi: FULL_FRAME
        }
      }
    """

    val query = """
      observations {
        instrument
        observingMode {
          gmosNorthImaging {
            filters
            multipleFiltersMode
            bin
            ampReadMode
            ampGain
            roi
          }
        }
      }
    """

    val expected0 =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "GMOS_NORTH",
              "observingMode": {
                "gmosNorthImaging": {
                  "filters": ["G_PRIME", "R_PRIME"],
                  "multipleFiltersMode": "GROUPED",
                  "bin": "ONE",
                  "ampReadMode": "SLOW",
                  "ampGain": "LOW",
                  "roi": "FULL_FRAME"
                }
              }
            }
          ]
        }
      }
    """.asRight

    val update1 = """
      observingMode: {
        gmosNorthImaging: {
          filters: [I_PRIME, Z_PRIME]
          explicitMultipleFiltersMode: INTERLEAVED
          explicitBin: FOUR
          explicitAmpReadMode: FAST
          explicitAmpGain: HIGH
          explicitRoi: CCD2
        }
      }
    """

    val expected1 =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "GMOS_NORTH",
              "observingMode": {
                "gmosNorthImaging": {
                  "filters": ["I_PRIME", "Z_PRIME"],
                  "multipleFiltersMode": "INTERLEAVED",
                  "bin": "FOUR",
                  "ampReadMode": "FAST",
                  "ampGain": "HIGH",
                  "roi": "CCD2"
                }
              }
            }
          ]
        }
      }
    """.asRight

    multiUpdateTest(pi,
      List(
        (update0, query, expected0),
        (update1, query, expected1)
      )
    )

  test("observing mode: switch from GMOS imaging to long slit"):

    val update0 = """
      observingMode: {
        gmosNorthImaging: {
          filters: [G_PRIME, R_PRIME]
        }
      }
    """

    val query0 = """
      observations {
        instrument
        observingMode {
          gmosNorthImaging {
            filters
          }
        }
      }
    """

    val expected0 =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "GMOS_NORTH",
              "observingMode": {
                "gmosNorthImaging": {
                  "filters": ["G_PRIME", "R_PRIME"]
                }
              }
            }
          ]
        }
      }
    """.asRight

    val update1 = """
      observingMode: {
        gmosNorthLongSlit: {
          grating: B1200_G5301
          filter: G_PRIME
          fpu: LONG_SLIT_0_25
          centralWavelength: {
            nanometers: 500
          }
        }
      }
    """

    val query1 = """
      observations {
        instrument
        observingMode {
          gmosNorthImaging {
            filters
          }
          gmosNorthLongSlit {
            grating
          }
        }
      }
    """

    val expected1 =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "GMOS_NORTH",
              "observingMode": {
                "gmosNorthImaging": null,
                "gmosNorthLongSlit": {
                  "grating": "B1200_G5301"
                }
              }
            }
          ]
        }
      }
    """.asRight

    multiUpdateTest(pi, List((update0, query0, expected0), (update1, query1, expected1)))

  test("observing mode: delete GMOS imaging"):

    val update0 = """
      observingMode: {
        gmosNorthImaging: {
          filters: [G_PRIME, R_PRIME]
        }
      }
    """

    val query = """
      observations {
        instrument
        observingMode {
          mode
          gmosNorthImaging {
            filters
          }
        }
      }
    """

    val expected0 =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "GMOS_NORTH",
              "observingMode": {
                "mode": "GMOS_NORTH_IMAGING",
                "gmosNorthImaging": {
                  "filters": ["G_PRIME", "R_PRIME"]
                }
              }
            }
          ]
        }
      }
    """.asRight

    val update1 = """
      observingMode: null
    """

    val expected1 =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": null,
              "observingMode": null
            }
          ]
        }
      }
    """.asRight

    multiUpdateTest(pi,
      List(
        (update0, query, expected0),
        (update1, query, expected1)
      )
    )

  test("observing mode: update GMOS imaging binning without filters"):

    val update = """
      observingMode: {
        gmosSouthImaging: {
          explicitBin: FOUR
        }
      }
    """

    val query = """
      observations {
        observingMode {
          gmosSouthImaging {
            filters
            explicitBin
          }
        }
      }
    """

    val expected =
      json"""
        {
          "updateObservations": {
            "observations": [
              {
                "observingMode": {
                  "gmosSouthImaging": {
                    "filters": ["G_PRIME", "R_PRIME"],
                    "explicitBin": "FOUR"
                  }
                }
              }
            ]
          }
        }
      """.asRight
    oneUpdateTest(pi, update, query, expected, ObservingModeType.GmosSouthImaging.some)


  test("observing mode: update default GMOS imaging bin by changing IQ"):
    val gaussian: String = """
      sourceProfile: {
        gaussian: {
          fwhm: { arcseconds: 0.1 }
          spectralDefinition: {
            bandNormalized: {
              sed: {
                stellarLibrary: B5_III
              }
              brightnesses: [
                {
                  band: R
                  value: 15.0
                  units: VEGA_MAGNITUDE
                }
              ]
            }
          }
        }
      }
    """

    def expectBinning(o: Observation.Id, b: GmosBinning): IO[Unit] =
      expect(
        user  = pi,
        query = s"""
            query {
              observation(observationId: "$o") {
                observingMode {
                  gmosNorthImaging {
                    bin
                  }
                }
              }
            }
        """,
        json"""
          {
            "observation": {
              "observingMode": {
                "gmosNorthImaging": {
                  "bin": ${b.tag.toScreamingSnakeCase}
                }
              }
            }
          }
        """.asRight
      )

    val update: String = """
      constraintSet: {
        imageQuality: POINT_ONE
      }
    """

    val throwawayQuery = """
      observations {
        observingMode {
          gmosSouthImaging {
            defaultBin
          }
        }
      }
    """

    for
      p <- createProgramAs(pi)
      t <- createTargetAs(pi, p, sourceProfile = gaussian)
      o <- createGmosNorthImagingObservationAs(pi, p, iq = ImageQuality.Preset.OnePointZero, offsets = None, t)
      _ <- expectBinning(o, GmosBinning.Two)
      _ <- query(pi, updateObservationsMutation(o, update, throwawayQuery))
      _ <- expectBinning(o, GmosBinning.One)
    yield ()


  test("observing mode: (fail to) update GMOS imaging with empty filters"):

    val update = """
      observingMode: {
        gmosNorthImaging: {
          filters: []
        }
      }
    """

    val query = """
      observations {
        observingMode {
          gmosNorthImaging {
            filters
          }
        }
      }
    """

    val expected = "Argument 'input.SET.observingMode.gmosNorthImaging' is invalid: At least one filter must be specified for GMOS imaging observations.".asLeft
    oneUpdateTest(pi, update, query, expected)

  test("observing mode: (fail to) update existing GMOS imaging with empty filters - rollback other changes"):
    createProgramAs(pi).flatMap { pid =>
      createGmosNorthImagingObservationAs(pi, pid).flatMap { oid =>
        val initialUpdate = """
          observingMode: {
            gmosNorthImaging: {
              filters: [G_PRIME, R_PRIME],
              explicitBin: TWO,
              explicitAmpGain: LOW
            }
          }
        """

        val failingUpdate = """
          observingMode: {
            gmosNorthImaging: {
              filters: [],
              explicitBin: FOUR,
              explicitAmpGain: HIGH
            }
          }
        """

        val query = """
          observations {
            observingMode {
              gmosNorthImaging {
                filters
                bin
                ampGain
              }
            }
          }
        """

        for {
          _ <- expect(
            user = pi,
            query = updateObservationsMutation(oid, initialUpdate, query),
            expected = json"""
              {
                "updateObservations": {
                  "observations": [
                    {
                      "observingMode": {
                        "gmosNorthImaging": {
                          "filters": ["G_PRIME", "R_PRIME"],
                          "bin": "TWO",
                          "ampGain": "LOW"
                        }
                      }
                    }
                  ]
                }
              }
            """.asRight
          )
          _ <- expect(
            user = pi,
            query = updateObservationsMutation(oid, failingUpdate, query),
            expected = List("Argument 'input.SET.observingMode.gmosNorthImaging' is invalid: At least one filter must be specified for GMOS imaging observations.").asLeft
          )
          // Verify that ALL values remain unchanged (transaction rollback)
          _ <- expect(
            user = pi,
            query = s"""
              query {
                observation(observationId: "$oid") {
                  observingMode {
                    gmosNorthImaging {
                      filters
                      initialFilters
                      bin
                      ampGain
                    }
                  }
                }
              }
            """,
            expected = json"""
              {
                "observation": {
                  "observingMode": {
                    "gmosNorthImaging": {
                      "filters": ["G_PRIME", "R_PRIME"],
                      "initialFilters": ["G_PRIME", "R_PRIME"],
                      "bin": "TWO",
                      "ampGain": "LOW"
                    }
                  }
                }
              }
            """.asRight
          )
        } yield ()
      }
    }

  test("observing mode: set GMOS North imaging in existing observation") {
    val update = """
      observingMode: {
        gmosNorthImaging: {
          filters: [G_PRIME, R_PRIME, I_PRIME]
          explicitMultipleFiltersMode: INTERLEAVED
          explicitBin: TWO
          explicitAmpReadMode: SLOW
          explicitAmpGain: LOW
          explicitRoi: FULL_FRAME
        }
      }
    """

    val query = """
      observations {
        instrument
        observingMode {
          gmosNorthImaging {
            filters
            multipleFiltersMode
            bin
            ampReadMode
            ampGain
            roi
          }
        }
      }
    """

    val expected = json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "GMOS_NORTH",
              "observingMode": {
                "gmosNorthImaging": {
                  "filters": ["G_PRIME", "I_PRIME", "R_PRIME"],
                  "multipleFiltersMode": "INTERLEAVED",
                  "bin": "TWO",
                  "ampReadMode": "SLOW",
                  "ampGain": "LOW",
                  "roi": "FULL_FRAME"
                }
              }
            }
          ]
        }
      }
    """.asRight

    oneUpdateTest(pi, update, query, expected)
  }

  test("observing mode: set GMOS South imaging in existing observation") {
    val update = """
      observingMode: {
        gmosSouthImaging: {
          filters: [G_PRIME, R_PRIME]
          explicitMultipleFiltersMode: INTERLEAVED
          explicitBin: FOUR
          explicitAmpReadMode: FAST
          explicitAmpGain: HIGH
          explicitRoi: CCD2
        }
      }
    """

    val query = """
      observations {
        instrument
        observingMode {
          gmosSouthImaging {
            filters
            multipleFiltersMode
            bin
            ampReadMode
            ampGain
            roi
          }
        }
      }
    """

    val expected = json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "GMOS_SOUTH",
              "observingMode": {
                "gmosSouthImaging": {
                  "filters": ["G_PRIME", "R_PRIME"],
                  "multipleFiltersMode": "INTERLEAVED",
                  "bin": "FOUR",
                  "ampReadMode": "FAST",
                  "ampGain": "HIGH",
                  "roi": "CCD2"
                }
              }
            }
          ]
        }
      }
    """.asRight

    oneUpdateTest(pi, update, query, expected)
  }

  test("updateObservations: flamingos2 rejects 2 spatial offsets"):
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      _   <- interceptGraphQL("Argument 'input.SET.observingMode.flamingos2LongSlit' is invalid: Flamingos2 must have exactly 0 or 4 offsets, but 2 were provided.") {
        query(pi, s"""
          mutation {
            updateObservations(input: {
              SET: {
                observingMode: {
                  flamingos2LongSlit: {
                    disperser: R1200_HK
                    filter: Y
                    fpu: LONG_SLIT_2
                    explicitOffsets: [
                      { p: { arcseconds: 0.0 }, q: { arcseconds: -5.0 } },
                      { p: { arcseconds: 0.0 }, q: { arcseconds:  5.0 } }
                    ]
                  }
                }
              }
              WHERE: {
                id: { EQ: ${oid.asJson} }
              }
            }) {
              observations {
                observingMode {
                  flamingos2LongSlit {
                    offsets {
                      p { arcseconds }
                      q { arcseconds }
                    }
                  }
                }
              }
            }
          }
        """)
      }
    } yield ()

  test("updateObservations: flamingos2 accepts clearing spatial offsets"):
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      // First, set some offsets
      _   <- query(pi, s"""
        mutation {
          updateObservations(input: {
            SET: {
              observingMode: {
                flamingos2LongSlit: {
                  disperser: R1200_HK
                  filter: Y
                  fpu: LONG_SLIT_2
                  explicitOffsets: [
                    { p: { arcseconds: 0.0 }, q: { arcseconds: -10.0 } },
                    { p: { arcseconds: 0.0 }, q: { arcseconds:  10.0 } },
                    { p: { arcseconds: 0.0 }, q: { arcseconds:   5.0 } },
                    { p: { arcseconds: 0.0 }, q: { arcseconds:  -5.0 } }
                  ]
                }
              }
            }
            WHERE: {
              id: { EQ: ${oid.asJson} }
            }
          }) {
            observations { id }
          }
        }
      """)
      _   <- query(pi, s"""
        mutation {
          updateObservations(input: {
            SET: {
              observingMode: {
                flamingos2LongSlit: {
                  explicitOffsets: null
                }
              }
            }
            WHERE: {
              id: { EQ: ${oid.asJson} }
            }
          }) {
            observations {
              observingMode {
                flamingos2LongSlit {
                  offsets {
                    p { arcseconds }
                    q { arcseconds }
                  }
                }
              }
            }
          }
        }
      """)
    } yield ()

  test("[flamingos2] updateObservations with custom explicitOffsets"):
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      _   <- expect(pi, s"""
        mutation {
          updateObservations(input: {
            SET: {
              observingMode: {
                flamingos2LongSlit: {
                  disperser: R1200_HK
                  filter: Y
                  fpu: LONG_SLIT_2
                  explicitOffsets: [
                    { p: { arcseconds: 1.0 }, q: { arcseconds: -3.0 } },
                    { p: { arcseconds: 2.0 }, q: { arcseconds:  4.0 } },
                    { p: { arcseconds: -1.5 }, q: { arcseconds: 2.5 } },
                    { p: { arcseconds: 0.5 }, q: { arcseconds: -1.0 } }
                  ]
                }
              }
            }
            WHERE: {
              id: { EQ: ${oid.asJson} }
            }
          }) {
            observations {
              observingMode {
                flamingos2LongSlit {
                  disperser
                  offsets {
                    p { arcseconds }
                    q { arcseconds }
                  }
                  explicitOffsets {
                    p { arcseconds }
                    q { arcseconds }
                  }
                  defaultOffsets {
                    p { arcseconds }
                    q { arcseconds }
                  }
                }
              }
            }
          }
        }
      """, json"""
        {
          "updateObservations": {
            "observations": [
              {
                "observingMode": {
                  "flamingos2LongSlit": {
                    "disperser": "R1200_HK",
                    "offsets": [
                      { "p": { "arcseconds": 1.000000 }, "q": { "arcseconds": -3.000000 } },
                      { "p": { "arcseconds": 2.000000 }, "q": { "arcseconds": 4.000000 } },
                      { "p": { "arcseconds": -1.500000 }, "q": { "arcseconds": 2.500000 } },
                      { "p": { "arcseconds": 0.500000 }, "q": { "arcseconds": -1.000000 } }
                    ],
                    "explicitOffsets": [
                      { "p": { "arcseconds": 1.000000 }, "q": { "arcseconds": -3.000000 } },
                      { "p": { "arcseconds": 2.000000 }, "q": { "arcseconds": 4.000000 } },
                      { "p": { "arcseconds": -1.500000 }, "q": { "arcseconds": 2.500000 } },
                      { "p": { "arcseconds": 0.500000 }, "q": { "arcseconds": -1.000000 } }
                    ],
                    "defaultOffsets": [
                      { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": 15.000000 } },
                      { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": -15.000000 } },
                      { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": -15.000000 } },
                      { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": 15.000000 } }
                    ]
                  }
                }
              }
            ]
          }
        }
      """.asRight)
    } yield ()

  test("field precedence: new offset fields take priority over deprecated ones"):

    val update = """
      observingMode: {
        gmosNorthLongSlit: {
          grating: B1200_G5301
          filter: G_PRIME
          fpu: LONG_SLIT_0_25
          centralWavelength: {
            nanometers: 500
          }
          explicitSpatialOffsets: [
            { arcseconds: 100.0 },
            { arcseconds: 200.0 }
          ]
          explicitOffsets: [
            { arcseconds: 1.0 },
            { arcseconds: 2.0 },
            { arcseconds: 3.0 }
          ]
        }
      }
    """

    val query = """
      observations {
        observingMode {
          gmosNorthLongSlit {
            explicitOffsets { arcseconds }
            explicitSpatialOffsets { arcseconds }
            offsets { arcseconds }
            spatialOffsets { arcseconds }
          }
        }
      }
    """

    val expected =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "observingMode": {
                "gmosNorthLongSlit": {
                  "explicitOffsets": [
                    { "arcseconds": 1.000000 },
                    { "arcseconds": 2.000000 },
                    { "arcseconds": 3.000000 }
                  ],
                  "explicitSpatialOffsets": [
                    { "arcseconds": 1.000000 },
                    { "arcseconds": 2.000000 },
                    { "arcseconds": 3.000000 }
                  ],
                  "offsets": [
                    { "arcseconds": 1.000000 },
                    { "arcseconds": 2.000000 },
                    { "arcseconds": 3.000000 }
                  ],
                  "spatialOffsets": [
                    { "arcseconds": 1.000000 },
                    { "arcseconds": 2.000000 },
                    { "arcseconds": 3.000000 }
                  ]
                }
              }
            }
          ]
        }
      }
      """.asRight

    oneUpdateTest(pi, update, query, expected, Some(ObservingModeType.GmosNorthLongSlit))
}

trait UpdateConstraintSetOps { this: OdbSuite =>

  def updateObservationsMutation(
    oid:    Observation.Id,
    update: String,
    query:  String
  ): String = s"""
    mutation {
      updateObservations(input: {
        SET: {
          $update
        },
        WHERE: {
          id: { EQ: ${oid.asJson} }
        }
      }) {
        $query
      }
    }
  """

  def updateObservation(
    user:     User,
    oid:      Observation.Id,
    update:   String,
    query:    String,
    expected: Either[String, Json]
  ): IO[Unit] =
    expect(
      user     = user,
      query    = updateObservationsMutation(oid, update, query),
      expected = expected.leftMap(msg => List(msg))
    )

  def updateObservationsTimesMutation(
    oid:    Observation.Id,
    update: String,
    query:  String
  ): String = s"""
    mutation {
      updateObservationsTimes(input: {
        SET: {
          $update
        },
        WHERE: {
          id: { EQ: ${oid.asJson} }
        }
      }) {
        $query
      }
    }
  """

  def updateObservationTimes(
    user:     User,
    oid:      Observation.Id,
    update:   String,
    query:    String,
    expected: Either[String, Json]
  ): IO[Unit] =
    expect(
      user     = user,
      query    = updateObservationsTimesMutation(oid, update, query),
      expected = expected.leftMap(msg => List(msg))
    )

}
