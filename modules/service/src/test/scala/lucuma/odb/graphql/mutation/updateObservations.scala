// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegShort
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.graphql.input.CoordinatesInput
import lucuma.odb.service.ObservationService

class updateObservations extends OdbSuite                          
                            with UpdateConstraintSetOps {

  val pi: User = TestUsers.Standard.pi(nextId, nextId)

  override lazy val validUsers: List[User] = List(pi)

  private def oneUpdateTest(
    user:     User,
    update:   String,
    query:    String,
    expected: Either[String, Json]
  ): IO[Unit] =

    for
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid)
      _   <- updateObservation(user, pid, oid, update, query, expected)
    yield ()

  private def multiUpdateTest(
    user:    User,
    updates: List[(String, String, Either[String, Json])]
  ): IO[Unit] =

    for
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid)
      _   <- updates.traverse_ { case (update, query, expected) =>
        updateObservation(user, pid, oid, update, query, expected)
      }
    yield ()


  test("general: update that selects nothing") {
    def emptyUpdate(user: User, pid: Program.Id): IO[Unit] =
      expect(
        user = user,
        query = s"""
          mutation {
            updateObservations(input: {
              programId: ${pid.asJson}
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
      _   <- emptyUpdate(pi, pid)
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
      _   <- updateObservation(pi, pid, oid, update(tid), query, expected(tid))
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
      _   <- updateObservation(pi, pid, oid, update(t0, t1), query, expected(t0, t1))
      _   <- updateObservation(pi, pid, oid, update(t1, t2), query, expected(t1, t2))
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
      _   <- updateObservation(pi, pid, oid, update1, query, expected1)
      _   <- updateObservation(pi, pid, oid, update2, query, expected2)
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
      _   <- updateObservation(pi, pid, oid, update1, query, expected1)
      _   <- updateObservation(pi, pid, oid, update2, query, expected2)
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
      _   <- updateObservation(pi, pid, oid, update, query, expected)
    } yield ()

  }

  test("visualization time: set") {

    val update   = """
      visualizationTime: "2022-08-30 17:18:00"
    """

    val query    = "observations { visualizationTime }"

    val expected = json"""
      {
        "updateObservations": {
          "observations": [
            {
              "visualizationTime": "2022-08-30 17:18:00"
            }
          ]
        }
      }
    """.asRight

    oneUpdateTest(pi, update, query, expected)
  }

  test("visualization time: set ISO-8601") {

    val update = """
      visualizationTime: "2022-08-30 17:18:00"
    """

    val query = "observations { visualizationTime }"

    val expected = json"""
      {
        "updateObservations": {
          "observations": [
            {
              "visualizationTime": "2022-08-30 17:18:00"
            }
          ]
        }
      }
    """.asRight

    oneUpdateTest(pi, update, query, expected)
  }

  test("visualization time: delete") {

    val update0  = """
      visualizationTime: "2022-08-30 17:18:00"
    """

    val update1 = """
      visualizationTime: null
    """

    val query    = "observations { visualizationTime }"

    val expected0 = json"""
      {
        "updateObservations": {
          "observations": [
            {
              "visualizationTime": "2022-08-30 17:18:00"
            }
          ]
        }
      }
    """.asRight

    val expected1 = json"""
      {
        "updateObservations": {
          "observations": [
            {
              "visualizationTime": null
            }
          ]
        }
      }
    """.asRight

    multiUpdateTest(pi, List((update0, query, expected0), (update1, query, expected1)))

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

  private object scienceRequirements {
    val update: String = """
      scienceRequirements: {
        spectroscopy: {
          wavelength: { nanometers: 400 }
          resolution: 10
          signalToNoise: 75
          signalToNoiseAt: { nanometers: 410 }
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
          spectroscopy {
            wavelength { nanometers }
            resolution
            signalToNoise
            signalToNoiseAt { nanometers }
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
                "spectroscopy": {
                  "wavelength": {
                    "nanometers": 400.000
                  },
                  "resolution": 10,
                  "signalToNoise": 75.000,
                  "signalToNoiseAt": {
                    "nanometers": 410.000
                  },
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

  test("science requirements: update") {
    oneUpdateTest(
      pi,
      scienceRequirements.update,
      scienceRequirements.query,
      scienceRequirements.expected.asRight
    )
  }

  test("science requirements: delete spectroscopy focal plane") {
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      _   <- query(pi, updateObservationsMutation(pid, oid, scienceRequirements.update, scienceRequirements.query))
      _   <- updateObservation(
        user   = pi,
        pid    = pid,
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
                }
              }
            }
          ]
        }
      }
    """.asRight

    oneUpdateTest(pi, update, query, expected)
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
            explicitSpatialOffsets {
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
                  "explicitSpatialOffsets": [
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
          explicitRoi: TOP_SPECTRUM
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
                  "explicitRoi": "TOP_SPECTRUM",
                  "explicitWavelengthDithers": [
                    { "picometers": -10000 },
                    { "picometers":  10000 }
                  ],
                  "explicitSpatialOffsets": [
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

    multiUpdateTest(pi, List((update0, query, expected0), (update1, query, expected1)))
  }

  def moveObservationsAs(user: User, pid: Program.Id, oids: List[Observation.Id], gid: Option[Group.Id], index: Option[NonNegShort]): IO[Unit] =
    query(
      user = user,
      query = s"""
        mutation {
          updateObservations(input: {
            programId: ${pid.asJson}
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

  test("grouping: move observations into a group (at end)") {
    for {
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid)
      o1  <- createObservationInGroupAs(pi, pid, None, None)
      o2  <- createObservationInGroupAs(pi, pid, None, None)
      o3  <- createObservationInGroupAs(pi, pid, Some(gid), None)
      _   <- moveObservationsAs(pi, pid, List(o1, o2), Some(gid), None)
      es  <- groupElementsAs(pi, pid, Some(gid))
    } yield {
      assertEquals(es.toSet, Set(Right(o2), Right(o1), Right(o3)))
      assertEquals(es.drop(1).toSet, Set(Right(o2), Right(o1)))
    }  
  }

  test("grouping: move observations into a group (at beginning)") {
    for {
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid)
      o1  <- createObservationInGroupAs(pi, pid, None, None)
      o2  <- createObservationInGroupAs(pi, pid, None, None)
      o3  <- createObservationInGroupAs(pi, pid, Some(gid), None)
      _   <- moveObservationsAs(pi, pid, List(o1, o2), Some(gid), Some(NonNegShort.unsafeFrom(0)))
      es  <- groupElementsAs(pi, pid, Some(gid))
    } yield {
      assertEquals(es.toSet, Set(Right(o2), Right(o1), Right(o3)))
      assertEquals(es.dropRight(1).toSet, Set(Right(o2), Right(o1)))
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
      _   <- moveObservationsAs(pi, pid, List(o1, o2), Some(gid), Some(NonNegShort.unsafeFrom(1)))
      es  <- groupElementsAs(pi, pid, Some(gid))
    } yield {
      assertEquals(es.toSet, Set(Right(o2), Right(o1), Right(o3), Right(o4)))
      assertEquals(es.drop(1).dropRight(1).toSet, Set(Right(o2), Right(o1)))
    }
  }

  test("grouping: move observations out of a group (at end of program)") {
    for {
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid)
      o1  <- createObservationInGroupAs(pi, pid, None, None)
      o2  <- createObservationInGroupAs(pi, pid, Some(gid), None)
      o3  <- createObservationInGroupAs(pi, pid, Some(gid), None)
      _   <- moveObservationsAs(pi, pid, List(o2, o3), None, None)
      es  <- groupElementsAs(pi, pid, None)
    } yield {
      assertEquals(es.take(2), List(Left(gid), Right(o1)))
      assertEquals(es.drop(2).toSet, Set(Right(o2), Right(o3)))
    }  
  }

  test("grouping: move observations out of a group (at beginning of program)") {
    for {
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid)
      o1  <- createObservationInGroupAs(pi, pid, None, None)
      o2  <- createObservationInGroupAs(pi, pid, Some(gid), None)
      o3  <- createObservationInGroupAs(pi, pid, Some(gid), None)
      _   <- moveObservationsAs(pi, pid, List(o2, o3), None, Some(NonNegShort.unsafeFrom(0)))
      es  <- groupElementsAs(pi, pid, None)
    } yield {
      assertEquals(es.take(2).toSet, Set(Right(o2), Right(o3)))
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
      _   <- moveObservationsAs(pi, pid, List(o2, o3), None, Some(NonNegShort.unsafeFrom(1)))
      es  <- groupElementsAs(pi, pid, None)
    } yield {
      assertEquals(es(0), Left(gid))
      assertEquals(es.drop(1).take(2).toSet, Set(Right(o2), Right(o3)))
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
      _   <- moveObservationsAs(pi, pid, List(o2, o3), Some(g2), None)
      e1  <- groupElementsAs(pi, pid, Some(g1))
      e2  <- groupElementsAs(pi, pid, Some(g2))
    } yield {
      assertEquals(e1, List(Right(o1)))
      assertEquals(e2.toSet, Set(Right(o2), Right(o3)))
    }  
  }

  test("grouping: Hugo's example") {
    for {
      pid <- createProgramAs(pi)
      o1  <- createObservationAs(pi, pid)
      o2  <- createObservationAs(pi, pid)
      g   <- createGroupAs(pi, pid)
      _   <- moveObservationsAs(pi, pid, List(o1), Some(g), None)
    } yield ()
  }

}

trait UpdateConstraintSetOps { this: OdbSuite =>

  def updateObservationsMutation(
    pid:    Program.Id,
    oid:    Observation.Id,
    update: String,
    query:  String
  ): String = s"""
    mutation {
      updateObservations(input: {
        programId: ${pid.asJson}
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
    pid:      Program.Id,
    oid:      Observation.Id,
    update:   String,
    query:    String,
    expected: Either[String, Json]
  ): IO[Unit] =
    expect(
      user     = user,
      query    = updateObservationsMutation(pid, oid, update, query),
      expected = expected.leftMap(msg => List(msg))
    )

}
