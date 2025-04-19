// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.all.*
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.ProgramReference.Description
import lucuma.core.model.User
import lucuma.odb.graphql.input.ProgramPropertiesInput
import lucuma.odb.service.Services

class updateTargets extends OdbSuite {

  val pi: User = TestUsers.Standard.pi(nextId, nextId)
  val staff = TestUsers.Standard.staff(3, 103)
  val service  = TestUsers.service(4)

  override lazy val validUsers: List[User] = List(pi, staff, service)

  test("no updates") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid, "target-1").flatMap { tid =>
       expect(
        user = pi,
        query = s"""
          mutation {
            updateTargets(input: {
              SET: {}
              WHERE: {
                id: { EQ: "$tid"}
              }
            }) {
              targets {
                id
              }
            }
          }
        """,
        expected = Right(
          json"""
            {
              "updateTargets" : {
                "targets" : [
                  {
                    "id" : $tid
                  }
                ]
              }
            }
          """
        )
       )
      }
    }
  }

  test("update name") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid, "target-1").flatMap { tid =>
       expect(
        user = pi,
        query = s"""
          mutation {
            updateTargets(input: {
              SET: {
                name: "new name"
              }
              WHERE: {
                id: { EQ: "$tid"}
              }
            }) {
              targets {
                id
                name
              }
            }
          }
        """,
        expected = Right(
          json"""
            {
              "updateTargets" : {
                "targets" : [
                  {
                    "id" : $tid,
                    "name" : "new name"
                  }
                ]
              }
            }
          """
        )
       )
      }
    }
  }

  test("update name to null (fails)") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid, "target-1").flatMap { tid =>
       expect(
        user = pi,
        query = s"""
          mutation {
            updateTargets(input: {
              SET: {
                name: null
              }
              WHERE: {
                id: { EQ: "$tid"}
              }
            }) {
              targets {
                id
                name
              }
            }
          }
        """,
        expected = Left(List("Argument 'input.SET.name' is invalid: cannot be null"))
       )
      }
    }
  }

  test("update calibration targets is allowed directly with the id by staff") {
    for {
      pid  <- withServices(service) { s =>
                Services.asSuperUser:
                  s.session.transaction.use { xa =>
                    s.programService
                      .insertCalibrationProgram(
                        ProgramPropertiesInput.Create.Default.some,
                        CalibrationRole.Photometric,
                        Description.unsafeFrom("PHOTO"))(using xa)
                }
              }
      tid  <- createTargetAs(staff, pid)
      _    <- expect(
                user = staff,
                query = s"""
                  mutation {
                    updateTargets(input: {
                      SET: {
                        name: "New Guía"
                      }
                      WHERE: {
                        id: { EQ: "$tid"}
                      }
                    }) {
                      targets {
                        id
                        name
                        calibrationRole
                      }
                    }
                  }
                """,
                expected = Right(
                  json"""
                    {
                      "updateTargets" : {
                        "targets" : [ {
                          "id" : $tid,
                          "name" : "New Guía",
                          "calibrationRole": "PHOTOMETRIC"
                        } ]
                      }
                    }
                  """
                )
              )
    } yield ()
  }

  test("delete orphan calibration targets") {
    for {
      pid  <-  createProgramAs(pi)
      tid  <- createTargetAs(pi, pid)
      _    <- setTargetCalibrationRole(tid, CalibrationRole.Photometric)
      _    <- withServices(service) { s =>
                s.session.transaction.use { xa =>
                  s.targetService.deleteOrphanCalibrationTargets(pid)(using xa)
                }
              }
      _    <- expect(
                user = staff,
                query = s"""
                  query {
                      target(targetId: ${tid.asJson}) {
                        id
                      }
                    }
                  """,
                expected = Right(
                    json"""
                    {
                      "target": null
                    }
                  """
                )
              )
    } yield ()
  }

  test("update tracking (sidereal -> sidereal)") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid, "target-1").flatMap { tid =>
       expect(
        user = pi,
        query = s"""
          mutation {
            updateTargets(input: {
              SET: {
                sidereal: {
                  ra: { degrees: 42 }
                }
              }
              WHERE: {
                id: { EQ: "$tid"}
              }
            }) {
              targets {
                id
                sidereal {
                  ra { degrees }
                }
              }
            }
          }
        """,
        expected = Right(
          json"""
            {
              "updateTargets" : {
                "targets" : [
                  {
                    "id" : $tid,
                    "sidereal" : {
                      "ra" : {
                        "degrees" : 42.0
                      }
                    }
                  }
                ]
              }
            }
          """
        )
       )
      }
    }
  }

  test("update tracking (sidereal -> nonsidereal)") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid, "target-1").flatMap { tid =>
       expect(
        user = pi,
        query = s"""
          mutation {
            updateTargets(input: {
              SET: {
                nonsidereal: {
                  keyType: COMET
                  des: "foo"
                }
              }
              WHERE: {
                id: { EQ: "$tid"}
              }
            }) {
              targets {
                id
                sidereal {
                  ra { degrees }
                }
                nonsidereal {
                  keyType
                  des
                  key
                }
              }
            }
          }
        """,
        expected = Right(
          json"""
            {
              "updateTargets" : {
                "targets" : [
                  {
                    "id" : $tid,
                    "sidereal" : null,
                    "nonsidereal" : {
                      "keyType" : "COMET",
                      "des" : "foo",
                      "key" : "Comet_foo"
                    }
                  }
                ]
              }
            }
          """
        )
       )
      }
    }
  }

  // This one works correctly but someone somewhere (I suspect cats-effect Resource) is dumping a
  // stacktrace for the constraint failure that we handle. So I want to track that down to avoid
  // a bunch of worrying text in the test output.
  test("update tracking (nonsidereal -> sidereal, incomplete)".ignore) {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid, "target-1").flatMap { tid =>
        // first change to nonsidereal
        expect(
          user = pi,
          query = s"""
            mutation {
              updateTargets(input: {
                SET: {
                  nonsidereal: {
                    keyType: COMET
                    des: "foo"
                  }
                }
                WHERE: {
                  id: { EQ: "$tid"}
                }
              }) {
                targets {
                  id
                }
              }
            }
          """,
          expected = Right(
            json"""
              {
                "updateTargets" : {
                  "targets" : [
                    {
                      "id" : $tid
                    }
                  ]
                }
              }
            """
          )
        ) *>
        // and now change back, but don't define everything
        expect(
          user = pi,
          query = s"""
            mutation {
              updateTargets(input: {
                SET: {
                  sidereal: {
                    ra: { degrees: 0 }
                  }
                }
                WHERE: {
                  id: { EQ: "$tid"}
                }
              }) {
                targets {
                  sidereal {
                    ra { degrees }
                    dec { degrees }
                    epoch
                  }
                  nonsidereal {
                    keyType
                    des
                  }
                }
              }
            }
          """,
          expected = Left(List("Sidereal targets require RA, Dec, and Epoch to be defined."))
        )
      }
    }
  }

  test("update tracking (nonsidereal -> sidereal)") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid, "target-1").flatMap { tid =>
        // first change to nonsidereal
        expect(
          user = pi,
          query = s"""
            mutation {
              updateTargets(input: {
                SET: {
                  nonsidereal: {
                    keyType: COMET
                    des: "foo"
                  }
                }
                WHERE: {
                  id: { EQ: "$tid"}
                }
              }) {
                targets {
                  id
                }
              }
            }
          """,
          expected = Right(
            json"""
              {
                "updateTargets" : {
                  "targets" : [
                    {
                      "id" : $tid
                    }
                  ]
                }
              }
            """
          )
        ) *>
        // and now change back
        expect(
          user = pi,
          query = s"""
            mutation {
              updateTargets(input: {
                SET: {
                  sidereal: {
                    ra: { degrees: 12 }
                    dec: { degrees: 67 }
                    epoch: "J1997.234"
                  }
                }
                WHERE: {
                  id: { EQ: "$tid"}
                }
              }) {
                targets {
                  sidereal {
                    ra { degrees }
                    dec { degrees }
                    epoch
                  }
                  nonsidereal {
                    keyType
                    des
                  }
                }
              }
            }
          """,
          expected = Right(
            json"""
              {
                "updateTargets" : {
                  "targets" : [
                    {
                      "sidereal" : {
                        "ra" : {
                          "degrees" : 12.0
                        },
                        "dec" : {
                          "degrees" : 67.0
                        },
                        "epoch" : "J1997.234"
                      },
                      "nonsidereal" : null
                    }
                  ]
                }
              }
            """
          )
        )
      }
    }
  }

  test("update source profile (point/bandNormalized/sed)") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid, "target-1").flatMap { tid =>
        expect(
          user = pi,
          query = s"""
            mutation {
              updateTargets(input: {
                SET: {
                  sourceProfile: {
                    point: {
                      bandNormalized: {
                        sed: {
                          planetaryNebula: NGC7009
                        }
                      }
                    }
                  }
                }
                WHERE: {
                  id: { EQ: "$tid"}
                }
              }) {
                targets {
                  sourceProfile {
                    point {
                      bandNormalized {
                        sed {
                          planetaryNebula
                        }
                      }
                    }
                  }
                }
              }
            }
          """,
          expected = Right(
            json"""
              {
                "updateTargets" : {
                  "targets" : [
                    {
                      "sourceProfile" : {
                        "point" : {
                          "bandNormalized" : {
                            "sed" : {
                              "planetaryNebula" : "NGC7009"
                            }
                          }
                        }
                      }
                    }
                  ]
                }
              }
            """
          )
        )
      }
    }
  }

  test("update source profile (point/bandNormalized/brightnesses)") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid, "target-1").flatMap { tid =>
        expect(
          user = pi,
          query = s"""
            mutation {
              updateTargets(input: {
                SET: {
                  sourceProfile: {
                    point: {
                      bandNormalized: {
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
                WHERE: {
                  id: { EQ: "$tid"}
                }
              }) {
                targets {
                  sourceProfile {
                    point {
                      bandNormalized {
                        sed {
                          stellarLibrary
                        }
                        brightnesses {
                          band
                          value
                          units
                        }
                      }
                    }
                  }
                }
              }
            }
          """,
          expected = Right(
            json"""
              {
                "updateTargets" : {
                  "targets" : [
                    {
                      "sourceProfile" : {
                        "point" : {
                          "bandNormalized" : {
                            "sed" : {
                              "stellarLibrary" : "B5_III"
                            },
                            "brightnesses" : [
                              {
                                "band" : "R",
                                 "value" : "15.0",
                                 "units" : "VEGA_MAGNITUDE"
                              }
                            ]
                          }
                        }
                      }
                    }
                  ]
                }
              }
            """
          )
        )
      }
    }
  }

  test("update source profile (uniform/bandNormalized/brightnesses)") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid, "target-1", """
        sourceProfile: {
          uniform: {
            bandNormalized: {
              sed: {
                stellarLibrary: B5_III
              }
              brightnesses: []
            }
          }
        }
      """).flatMap { tid =>
        expect(
          user = pi,
          query = s"""
            mutation {
              updateTargets(input: {
                SET: {
                  sourceProfile: {
                    uniform: {
                      bandNormalized: {
                        brightnesses: [
                          {
                             band: R
                             value: 15.0
                             units: VEGA_MAG_PER_ARCSEC_SQUARED
                          }
                        ]
                      }
                    }
                  }
                }
                WHERE: {
                  id: { EQ: "$tid"}
                }
              }) {
                targets {
                  sourceProfile {
                    uniform {
                      bandNormalized {
                        sed {
                          stellarLibrary
                        }
                        brightnesses {
                          band
                          value
                          units
                        }
                      }
                    }
                  }
                }
              }
            }
          """,
          expected = Right(
            json"""
              {
                "updateTargets" : {
                  "targets" : [
                    {
                      "sourceProfile" : {
                        "uniform" : {
                          "bandNormalized" : {
                            "sed" : {
                              "stellarLibrary" : "B5_III"
                            },
                            "brightnesses" : [
                              {
                                "band" : "R",
                                 "value" : "15.0",
                                 "units" : "VEGA_MAG_PER_ARCSEC_SQUARED"
                              }
                            ]
                          }
                        }
                      }
                    }
                  ]
                }
              }
            """
          )
        )
      }
    }
  }

  test("update source profile (gaussian/bandNormalized/brightnesses)") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid, "target-1", """
        sourceProfile: {
          gaussian: {
            spectralDefinition: {
              bandNormalized: {
                sed: {
                  stellarLibrary: B5_III
                }
                brightnesses: []
              }
            }
            fwhm: {
              microarcseconds: 42
            }
          }
        }
      """).flatMap { tid =>
        expect(
          user = pi,
          query = s"""
            mutation {
              updateTargets(input: {
                SET: {
                  sourceProfile: {
                    gaussian: {
                      spectralDefinition: {
                        bandNormalized: {
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
                }
                WHERE: {
                  id: { EQ: "$tid"}
                }
              }) {
                targets {
                  sourceProfile {
                    gaussian {
                      bandNormalized {
                        brightnesses {
                          band
                          value
                          units
                        }
                      }
                      fwhm {
                        microarcseconds
                      }
                    }
                  }
                }
              }
            }
          """,
          expected = Right(
            json"""
              {
                "updateTargets" : {
                  "targets" : [
                    {
                      "sourceProfile" : {
                        "gaussian" : {
                          "bandNormalized" : {
                            "brightnesses" : [
                              {
                                "band" : "R",
                                 "value" : "15.0",
                                 "units" : "VEGA_MAGNITUDE"
                              }
                            ]
                          },
                          "fwhm" : {
                            "microarcseconds" : 42
                          }
                        }
                      }
                    }
                  ]
                }
              }
            """
          )
        )
      }
    }
  }

  test("update source profile (point -> gaussian, incomplete)") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid, "target-1").flatMap { tid =>
        expect(
          user = pi,
          query = s"""
            mutation {
              updateTargets(input: {
                SET: {
                  sourceProfile: {
                    gaussian: {
                    }
                  }
                }
                WHERE: {
                  id: { EQ: "$tid"}
                }
              }) {
                targets {
                  sourceProfile {
                    gaussian {
                      fwhm { degrees }
                    }
                    point {
                      bandNormalized {
                        sed {
                          planetaryNebula
                        }
                      }
                    }
                  }
                }
              }
            }
          """,
          expected = Left(List("Not a gaussian source.  To change profile type, please provide a full definition."))
        )
      }
    }
  }

  test("update source profile (point/bandNormalized -> gaussian, complete)") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid, "target-1").flatMap { tid =>
        expect(
          user = pi,
          query = s"""
            mutation {
              updateTargets(input: {
                SET: {
                  sourceProfile: {
                    gaussian: {
                      fwhm: { degrees: 42 }
                      spectralDefinition: {
                        emissionLines: {
                          lines: []
                          fluxDensityContinuum: {
                            value: 0.8
                            error: 0.0001
                            units: W_PER_M_SQUARED_PER_UM
                          }
                        }
                      }
                    }
                  }
                }
                WHERE: {
                  id: { EQ: "$tid"}
                }
              }) {
                targets {
                  sourceProfile {
                    gaussian {
                      fwhm { degrees }
                      emissionLines {
                        fluxDensityContinuum {
                          value
                          units
                          error
                        }
                      }
                    }
                    point {
                      bandNormalized {
                        sed {
                          planetaryNebula
                        }
                      }
                    }
                  }
                }
              }
            }
          """,
          expected = Right(
            json"""
              {
                "updateTargets" : {
                  "targets" : [
                    {
                      "sourceProfile" : {
                        "gaussian" : {
                          "fwhm" : {
                            "degrees" : 42.000000
                          },
                          "emissionLines" : {
                            "fluxDensityContinuum" : {
                              "value" : "0.8",
                              "units" : "W_PER_M_SQUARED_PER_UM",
                              "error" : "0.00010"
                            }
                          }
                        },
                        "point" : null
                      }
                    }
                  ]
                }
              }
            """
          )
        )
      }
    }
  }

  test("update source profile (point/emissionLines -> gaussian, complete)") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid, "target-1", """
        sourceProfile: {
          point: {
            emissionLines: {
              lines: []
              fluxDensityContinuum: {
                value: 1
                units: ERG_PER_S_PER_CM_SQUARED_PER_A
                error: 0.01
              }
            }
          }
        }
      """).flatMap { tid =>
        expect(
          user = pi,
          query = s"""
            mutation {
              updateTargets(input: {
                SET: {
                  sourceProfile: {
                    gaussian: {
                      fwhm: { degrees: 42 }
                      spectralDefinition: {
                        emissionLines: {
                          lines: []
                          fluxDensityContinuum: {
                            value: 0.8
                            error: 0.0001
                            units: W_PER_M_SQUARED_PER_UM
                          }
                        }
                      }
                    }
                  }
                }
                WHERE: {
                  id: { EQ: "$tid"}
                }
              }) {
                targets {
                  sourceProfile {
                    gaussian {
                      fwhm { degrees }
                      emissionLines {
                        fluxDensityContinuum {
                          value
                          units
                          error
                        }
                      }
                    }
                    point {
                      bandNormalized {
                        sed {
                          planetaryNebula
                        }
                      }
                    }
                  }
                }
              }
            }
          """,
          expected = Right(
            json"""
              {
                "updateTargets" : {
                  "targets" : [
                    {
                      "sourceProfile" : {
                        "gaussian" : {
                          "fwhm" : {
                            "degrees" : 42.000000
                          },
                          "emissionLines" : {
                            "fluxDensityContinuum" : {
                              "value" : "0.8",
                              "units" : "W_PER_M_SQUARED_PER_UM",
                              "error" : "0.00010"
                            }
                          }
                        },
                        "point" : null
                      }
                    }
                  ]
                }
              }
            """
          )
        )
      }
    }
  }

  test("update source profile (gaussian -> point/bandNormalized, complete)") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid, "target-1", """
        sourceProfile: {
          gaussian: {
            spectralDefinition: {
              bandNormalized: {
                sed: {
                  stellarLibrary: B5_III
                }
                brightnesses: []
              }
            }
            fwhm: {
              microarcseconds: 42
            }
          }
        }
      """).flatMap { tid =>
        expect(
          user = pi,
          query = s"""
            mutation {
              updateTargets(input: {
                SET: {
                  sourceProfile: {
                    point: {
                      bandNormalized: {
                        sed: {
                          stellarLibrary: B5_III
                        }
                        brightnesses: []
                      }
                    }
                  }
                }
                WHERE: {
                  id: { EQ: "$tid"}
                }
              }) {
                targets {
                  sourceProfile {
                    gaussian {
                      bandNormalized {
                        brightnesses {
                          band
                          value
                          units
                        }
                      }
                      fwhm {
                        microarcseconds
                      }
                    }
                    point {
                      bandNormalized {
                        sed {
                          stellarLibrary
                        }
                        brightnesses {
                          value
                        }
                      }
                    }
                  }
                }
              }
            }
          """,
          expected = Right(
            json"""
              {
                "updateTargets" : {
                  "targets" : [
                    {
                      "sourceProfile" : {
                        "gaussian" : null,
                        "point": {
                          "bandNormalized": {
                            "sed": {
                              "stellarLibrary": "B5_III"
                            },
                            "brightnesses": []
                          }
                        }
                      }
                    }
                  ]
                }
              }
            """
          )
        )
      }
    }
  }

  // ShortCut 2260
  test("update source profile (point/emissionLines -> point/bandNormalized, complete)") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid, "target-1", """
        sourceProfile: {
          point: {
            emissionLines: {
              lines: []
              fluxDensityContinuum: {
                value: 1
                units: ERG_PER_S_PER_CM_SQUARED_PER_A
                error: 0.01
              }
            }
          }
        }
      """).flatMap { tid =>
        expect(
          user = pi,
          query = s"""
            mutation {
              updateTargets(input: {
                SET: {
                  sourceProfile: {
                    point: {
                      bandNormalized: {
                        sed: {
                          stellarLibrary: B5_III
                        }
                        brightnesses: []
                      }
                    }
                  }
                }
                WHERE: {
                  id: { EQ: "$tid"}
                }
              }) {
                targets {
                  sourceProfile {
                    gaussian {
                      bandNormalized {
                        brightnesses {
                          band
                          value
                          units
                        }
                      }
                      fwhm {
                        microarcseconds
                      }
                    }
                    point {
                      bandNormalized {
                        sed {
                          stellarLibrary
                        }
                        brightnesses {
                          value
                        }
                      }
                    }
                  }
                }
              }
            }
          """,
          expected = Right(
            json"""
              {
                "updateTargets" : {
                  "targets" : [
                    {
                      "sourceProfile" : {
                        "gaussian" : null,
                        "point": {
                          "bandNormalized": {
                            "sed": {
                              "stellarLibrary": "B5_III"
                            },
                            "brightnesses": []
                          }
                        }
                      }
                    }
                  ]
                }
              }
            """
          )
        )
      }
    }
  }

  test("update source profile (gaussian -> point/emissionLines, complete)") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid, "target-1", """
        sourceProfile: {
          gaussian: {
            spectralDefinition: {
              bandNormalized: {
                sed: {
                  stellarLibrary: B5_III
                }
                brightnesses: []
              }
            }
            fwhm: {
              microarcseconds: 42
            }
          }
        }
      """).flatMap { tid =>
        expect(
          user = pi,
          query = s"""
            mutation {
              updateTargets(input: {
                SET: {
                  sourceProfile: {
                  point: {
                    emissionLines: {
                      lines: []
                      fluxDensityContinuum: {
                        value: 1
                        units: ERG_PER_S_PER_CM_SQUARED_PER_A
                      }
                    }
                  }
                  }
                }
                WHERE: {
                  id: { EQ: "$tid"}
                }
              }) {
                targets {
                  sourceProfile {
                    gaussian {
                      fwhm {
                        microarcseconds
                      }
                    }
                    point {
                      emissionLines {
                        lines {
                          lineWidth
                        }
                        fluxDensityContinuum {
                          value
                          units
                          error
                        }
                      }
                      bandNormalized {
                        sed {
                          stellarLibrary
                        }
                        brightnesses {
                          value
                        }
                      }
                    }
                  }
                }
              }
            }
          """,
          expected = Right(
            json"""
              {
                "updateTargets" : {
                  "targets" : [
                    {
                      "sourceProfile" : {
                        "gaussian" : null,
                        "point":{
                          "emissionLines": {
                            "lines": [],
                            "fluxDensityContinuum": {
                              "value": "1",
                              "units": "ERG_PER_S_PER_CM_SQUARED_PER_A",
                              "error": null
                            }
                          },
                          "bandNormalized": null
                        }
                      }
                    }
                  ]
                }
              }
            """
          )
        )
      }
    }
  }

  // ShortCut 2260
  test("update source profile (point/bandNormalized -> point/emissionLines, complete)") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid, "target-1").flatMap { tid =>
        expect(
          user = pi,
          query = s"""
            mutation {
              updateTargets(input: {
                SET: {
                  sourceProfile: {
                  point: {
                    emissionLines: {
                      lines: []
                      fluxDensityContinuum: {
                        value: 1
                        units: ERG_PER_S_PER_CM_SQUARED_PER_A
                      }
                    }
                  }
                  }
                }
                WHERE: {
                  id: { EQ: "$tid"}
                }
              }) {
                targets {
                  sourceProfile {
                    gaussian {
                      fwhm {
                        microarcseconds
                      }
                    }
                    point {
                      emissionLines {
                        lines {
                          lineWidth
                        }
                        fluxDensityContinuum {
                          value
                          units
                          error
                        }
                      }
                      bandNormalized {
                        sed {
                          stellarLibrary
                        }
                        brightnesses {
                          value
                        }
                      }
                    }
                  }
                }
              }
            }
          """,
          expected = Right(
            json"""
              {
                "updateTargets" : {
                  "targets" : [
                    {
                      "sourceProfile" : {
                        "gaussian" : null,
                        "point": {
                          "emissionLines": {
                            "lines": [],
                            "fluxDensityContinuum": {
                              "value": "1",
                              "units": "ERG_PER_S_PER_CM_SQUARED_PER_A",
                              "error": null
                            }
                          },
                          "bandNormalized": null
                        }
                      }
                    }
                  ]
                }
              }
            """
          )
        )
      }
    }
  }

  test("update source profile (point/bandNormalized -> uniform/bandNormalized, complete)") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid, "target-1").flatMap { tid =>
        expect(
          user = pi,
          query = s"""
            mutation {
              updateTargets(input: {
                SET: {
                  sourceProfile: {
                    uniform: {
                      bandNormalized: {
                        sed: {
                          stellarLibrary: B5_III
                        }
                        brightnesses: [
                          {
                             band: R
                             value: 15.0
                             units: VEGA_MAG_PER_ARCSEC_SQUARED
                          }
                        ]
                      }
                    }
                  }
                }
                WHERE: {
                  id: { EQ: "$tid"}
                }
              }) {
                targets {
                  sourceProfile {
                    uniform {
                      bandNormalized {
                        sed {
                          stellarLibrary
                        }
                        brightnesses {
                          band
                          value
                          units
                        }
                      }
                    }
                  }
                }
              }
            }
          """,
          expected = Right(
            json"""
              {
                "updateTargets" : {
                  "targets" : [
                    {
                      "sourceProfile" : {
                        "uniform" : {
                          "bandNormalized" : {
                            "sed" : {
                              "stellarLibrary" : "B5_III"
                            },
                            "brightnesses" : [
                              {
                                "band" : "R",
                                 "value" : "15.0",
                                 "units" : "VEGA_MAG_PER_ARCSEC_SQUARED"
                              }
                            ]
                          }
                        }
                      }
                    }
                  ]
                }
              }
            """
          )
        )
      }
    }
  }

  // ShortCut 2260
  test("update source profile (uniform/emissionLines -> uniform/bandNormalized, complete)") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid, "target-1", """
        sourceProfile: {
          uniform: {
            emissionLines: {
              lines: []
              fluxDensityContinuum: {
                value: 1
                units: ERG_PER_S_PER_CM_SQUARED_PER_A_PER_ARCSEC_SQUARED
              }
            }
          }
        }
      """).flatMap { tid =>
        expect(
          user = pi,
          query = s"""
            mutation {
              updateTargets(input: {
                SET: {
                  sourceProfile: {
                    uniform: {
                      bandNormalized: {
                        sed: {
                          stellarLibrary: B5_III
                        }
                        brightnesses: [
                          {
                             band: R
                             value: 15.0
                             units: VEGA_MAG_PER_ARCSEC_SQUARED
                          }
                        ]
                      }
                    }
                  }
                }
                WHERE: {
                  id: { EQ: "$tid"}
                }
              }) {
                targets {
                  sourceProfile {
                    uniform {
                      bandNormalized {
                        sed {
                          stellarLibrary
                        }
                        brightnesses {
                          band
                          value
                          units
                        }
                      }
                    }
                  }
                }
              }
            }
          """,
          expected = Right(
            json"""
              {
                "updateTargets" : {
                  "targets" : [
                    {
                      "sourceProfile" : {
                        "uniform" : {
                          "bandNormalized" : {
                            "sed" : {
                              "stellarLibrary" : "B5_III"
                            },
                            "brightnesses" : [
                              {
                                "band" : "R",
                                 "value" : "15.0",
                                 "units" : "VEGA_MAG_PER_ARCSEC_SQUARED"
                              }
                            ]
                          }
                        }
                      }
                    }
                  ]
                }
              }
            """
          )
        )
      }
    }
  }

}
