// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.all.*
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.TargetDisposition
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
      createAllTargetTypesAs(pi, pid).flatMap: tids =>
        tids.traverse { tid =>
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
      createAllTargetTypesAs(pi, pid).flatMap: tids =>
        tids.traverse { tid =>
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
      createAllTargetTypesAs(pi, pid).flatMap { tids =>
        tids.traverse { tid =>
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
      tid  <- createTargetViaServiceAs(pi, pid, TargetDisposition.Calibration, CalibrationRole.Photometric.some)
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

  test("update tracking (nonsidereal -> sidereal, incomplete)") {
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

  test("update tracking (sidereal, nonsidereal -> opportunity)") {
    createProgramAs(pi).flatMap { pid =>
      createSiderealTargetAs(pi, pid).flatMap { tid =>
        expect(
          user = pi,
          query = s"""
            mutation {
              updateTargets(input: {
                SET: {
                  opportunity: {
                    region: {
                      rightAscensionArc: {
                        type: FULL
                      }
                      declinationArc: {
                        type: PARTIAL
                        start: {
                          degrees: 10
                        }
                        end: {
                          degrees: 50
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

  test("update tracking (nonsidereal -> opportunity)") {
    createProgramAs(pi).flatMap { pid =>
      createNonsiderealTargetAs(pi, pid).flatMap { tid =>
        expect(
          user = pi,
          query = s"""
            mutation {
              updateTargets(input: {
                SET: {
                  opportunity: {
                    region: {
                      rightAscensionArc: {
                        type: FULL
                      }
                      declinationArc: {
                        type: PARTIAL
                        start: {
                          degrees: 10
                        }
                        end: {
                          degrees: 50
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

  test("update tracking (opportunity -> sidereal)") {
    createProgramAs(pi).flatMap { pid =>
      createOpportunityTargetAs(pi, pid).flatMap { tid =>
        expect(
          user = pi,
          query = s"""
            mutation {
              updateTargets(input: {
                SET: {
                  sidereal: {
                    ra: { hours: "0.0" }
                    dec: { degrees: "0.0" }
                    epoch: "J2000.000"
                    radialVelocity: {
                      kilometersPerSecond: 0.0
                    }
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
                    dec { degrees }
                    epoch
                    radialVelocity { kilometersPerSecond }
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
                          "degrees" : 0.0
                        },
                        "dec" : {
                          "degrees" : 0.0
                        },
                        "epoch" : "J2000.000",
                        "radialVelocity" : {
                          "kilometersPerSecond" : 0.0000
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

  test("update tracking (opportunity -> nonsidereal)") {
    createProgramAs(pi).flatMap { pid =>
      createOpportunityTargetAs(pi, pid).flatMap { tid =>
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
                      "id" : $tid,
                      "nonsidereal" : {
                        "keyType" : "COMET",
                        "des" : "foo"
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

  test("update opportunity region (empty arcs)") {
    createProgramAs(pi).flatMap { pid =>
      createOpportunityTargetAs(pi, pid).flatMap { tid =>
        expect(
          user = pi,
          query = s"""
            mutation {
              updateTargets(input: {
                SET: {
                  opportunity: {
                    region: {
                      rightAscensionArc: {
                        type: EMPTY
                      }
                      declinationArc: {
                        type: EMPTY
                      }
                    }
                  }
                }
                WHERE: {
                  id: { EQ: "$tid"}
                }
              }) {
                targets {
                  id
                  opportunity {
                    region {
                      rightAscensionArc {
                        type
                        start { degrees }
                        end { degrees }
                      }
                      declinationArc {
                        type
                        start { degrees }
                        end { degrees }
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
                      "id" : $tid,
                      "opportunity" : {
                        "region" : {
                          "rightAscensionArc" : {
                            "type" : "EMPTY",
                            "start" : null,
                            "end" : null
                          },
                          "declinationArc" : {
                            "type" : "EMPTY",
                            "start" : null,
                            "end" : null
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

  test("update opportunity region (full arcs)") {
    createProgramAs(pi).flatMap { pid =>
      createOpportunityTargetAs(pi, pid).flatMap { tid =>
        expect(
          user = pi,
          query = s"""
            mutation {
              updateTargets(input: {
                SET: {
                  opportunity: {
                    region: {
                      rightAscensionArc: {
                        type: FULL
                      }
                      declinationArc: {
                        type: FULL
                      }
                    }
                  }
                }
                WHERE: {
                  id: { EQ: "$tid"}
                }
              }) {
                targets {
                  id
                  opportunity {
                    region {
                      rightAscensionArc {
                        type
                        start { degrees }
                        end { degrees }
                      }
                      declinationArc {
                        type
                        start { degrees }
                        end { degrees }
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
                      "id" : $tid,
                      "opportunity" : {
                        "region" : {
                          "rightAscensionArc" : {
                            "type" : "FULL",
                            "start" : null,
                            "end" : null
                          },
                          "declinationArc" : {
                            "type" : "FULL",
                            "start" : null,
                            "end" : null
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

  test("update opportunity region (partial arcs)") {
    createProgramAs(pi).flatMap { pid =>
      createOpportunityTargetAs(pi, pid).flatMap { tid =>
        expect(
          user = pi,
          query = s"""
            mutation {
              updateTargets(input: {
                SET: {
                  opportunity: {
                    region: {
                      rightAscensionArc: {
                        type: PARTIAL
                        start: { degrees: 110 }
                        end: { degrees: 120 }
                      }
                      declinationArc: {
                        type: PARTIAL
                        start: { degrees: 10 }
                        end: { degrees: 20 }
                      }
                    }
                  }
                }
                WHERE: {
                  id: { EQ: "$tid"}
                }
              }) {
                targets {
                  id
                  opportunity {
                    region {
                      rightAscensionArc {
                        type
                        start { degrees }
                        end { degrees }
                      }
                      declinationArc {
                        type
                        start { degrees }
                        end { degrees }
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
                      "id" : $tid,
                      "opportunity" : {
                        "region" : {
                          "rightAscensionArc" : {
                            "type" : "PARTIAL",
                            "start" : { "degrees": 110.0 },
                            "end" : { "degrees": 120.0 }
                          },
                          "declinationArc" : {
                            "type" : "PARTIAL",
                            "start" : { "degrees": 10.0 },
                            "end" : { "degrees": 20.0 }
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
    }  }

  test("update source profile (point/bandNormalized/sed)") {
    createProgramAs(pi).flatMap { pid =>
      createAllTargetTypesAs(pi, pid).flatMap { tids =>
        tids.traverse { tid =>
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
  }

  test("update source profile (point/bandNormalized/brightnesses)") {
    createProgramAs(pi).flatMap { pid =>
      createAllTargetTypesAs(pi, pid).flatMap { tids =>
        tids.traverse { tid =>
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
  }

  test("update source profile (uniform/bandNormalized/brightnesses)") {
    createProgramAs(pi).flatMap { pid =>
      createAllTargetTypesAs(pi, pid, """
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
      """).flatMap { tids =>
        tids.traverse { tid =>
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
  }

  test("update source profile (gaussian/bandNormalized/brightnesses)") {
    createProgramAs(pi).flatMap { pid =>
      createAllTargetTypesAs(pi, pid, """
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
      """).flatMap { tids =>
        tids.traverse { tid =>
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
  }

  test("update source profile (point -> gaussian, incomplete)") {
    createProgramAs(pi).flatMap { pid =>
      createAllTargetTypesAs(pi, pid).flatMap { tids =>
        tids.traverse { tid =>
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
  }

  test("update source profile (point/bandNormalized -> gaussian, complete)") {
    createProgramAs(pi).flatMap { pid =>
      createAllTargetTypesAs(pi, pid).flatMap { tids =>
        tids.traverse { tid =>
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
  }

  test("update source profile (point/emissionLines -> gaussian, complete)") {
    createProgramAs(pi).flatMap { pid =>
      createAllTargetTypesAs(pi, pid, """
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
      """).flatMap { tids =>
        tids.traverse { tid =>
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
  }

  test("update source profile (gaussian -> point/bandNormalized, complete)") {
    createProgramAs(pi).flatMap { pid =>
      createAllTargetTypesAs(pi, pid, """
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
      """).flatMap { tids =>
        tids.traverse { tid =>
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
  }

  // ShortCut 2260
  test("update source profile (point/emissionLines -> point/bandNormalized, complete)") {
    createProgramAs(pi).flatMap { pid =>
      createAllTargetTypesAs(pi, pid, """
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
      """).flatMap { tids =>
        tids.traverse { tid =>
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
  }

  test("update source profile (gaussian -> point/emissionLines, complete)") {
    createProgramAs(pi).flatMap { pid =>
      createAllTargetTypesAs(pi, pid, """
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
      """).flatMap { tids =>
        tids.traverse { tid =>
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
  }

  // ShortCut 2260
  test("update source profile (point/bandNormalized -> point/emissionLines, complete)") {
    createProgramAs(pi).flatMap { pid =>
      createAllTargetTypesAs(pi, pid).flatMap { tids =>
        tids.traverse { tid =>
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
  }

  test("update source profile (point/bandNormalized -> uniform/bandNormalized, complete)") {
    createProgramAs(pi).flatMap { pid =>
      createAllTargetTypesAs(pi, pid).flatMap { tids =>
        tids.traverse { tid =>
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

  // ShortCut 2260
  test("update source profile (uniform/emissionLines -> uniform/bandNormalized, complete)") {
    createProgramAs(pi).flatMap { pid =>
      createAllTargetTypesAs(pi, pid, """
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
      """).flatMap { tids =>
        tids.traverse { tid =>
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

}
