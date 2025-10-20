// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.Site
import lucuma.core.model.ImageQuality
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.syntax.string.toScreamingSnakeCase

class observingModeGroup extends OdbSuite:

  val pi         = TestUsers.Standard.pi(nextId, nextId)
  val staff      = TestUsers.Standard.staff(nextId, nextId)
  val validUsers = List(pi, staff)

  private def siteName(site: Site): String =
    site match
      case Site.GN => "North"
      case Site.GS => "South"

  private def createObservation(
    user:     User,
    pid:      Program.Id,
    site:     Site,
    grating:  String,
    fpu:      String = "LONG_SLIT_0_25",
    xbin:     Option[GmosXBinning] = None,
    ybin:     Option[GmosYBinning] = None,
    iq:       ImageQuality.Preset,
    asterism: List[Target.Id]
  ): IO[Observation.Id] =
    query(
      user  = user,
      query = s"""
        mutation {
          createObservation(input: {
            programId: ${pid.asJson}
            SET: {
              constraintSet: {
                imageQuality: ${iq.tag.toUpperCase}
              }
              scienceRequirements: {
                exposureTimeMode: {
                  signalToNoise: {
                    value: 10.0
                    at: { nanometers: 234.56 }
                  }
                }
              }
              observingMode: {
                gmos${siteName(site)}LongSlit: {
                  grating: $grating
                  filter: G_PRIME
                  fpu: $fpu
                  centralWavelength: {
                    nanometers: 234.56
                  }
                  explicitXBin: ${xbin.map(_.tag.toScreamingSnakeCase).getOrElse("null")}
                  explicitYBin: ${ybin.map(_.tag.toScreamingSnakeCase).getOrElse("null")}
                }
              }
              targetEnvironment: {
                asterism: [ ${asterism.map(_.asJson).mkString(", ")} ]
              }
            }
          }) {
            observation {
              id
            }
          }
        }
      """
    ).map(_.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id])

  test("modes should be correctly grouped"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid, "Biff",
         """
            sourceProfile: {
              gaussian: {
                fwhm: {
                  microarcseconds: 647200
                }
                spectralDefinition: {
                  bandNormalized: {
                    sed: {
                      stellarLibrary: B5_III
                    }
                    brightnesses: []
                  }
                }
              }
            }
          """
      ).flatMap: tid =>
        def create2(iq: ImageQuality.Preset, grating: GmosNorthGrating) =
          createObservation(
            pi,
            pid,
            Site.GN,
            grating = grating.tag.toScreamingSnakeCase,
            iq = iq,
            asterism =List(tid)
          ).replicateA(2)

        (create2(ImageQuality.Preset.OnePointFive, GmosNorthGrating.B1200_G5301),
         create2(ImageQuality.Preset.PointOne, GmosNorthGrating.R400_G5310),
         create2(ImageQuality.Preset.PointOne, GmosNorthGrating.R831_G5302)
        ).parTupled.flatMap: (g1, g2, g3) =>
          expect(
            user = pi,
            query = s"""
              query {
                observingModeGroup(programId: ${pid.asJson}) {
                  matches {
                    observingMode {
                      gmosNorthLongSlit {
                        grating
                      }
                    }
                    observations {
                      matches {
                        id
                      }
                    }
                  }
                }
              }
            """,
            expected =
              // N.B. the ordering of groups is based on the concatenation of all the components so it's deterministic
              json"""
                {
                  "observingModeGroup" : {
                    "matches" : [
                      {
                        "observingMode" : {
                          "gmosNorthLongSlit": {
                            "grating" : "B1200_G5301"
                          }
                        },
                        "observations" : {
                          "matches" : ${g1.map { id => Json.obj("id" -> id.asJson) }.asJson }
                        }
                      },
                      {
                        "observingMode" : {
                          "gmosNorthLongSlit": {
                            "grating" : "R400_G5310"
                          }
                        },
                        "observations" : {
                          "matches" : ${g2.map { id => Json.obj("id" -> id.asJson) }.asJson }
                        }
                      },
                      {
                        "observingMode" : {
                          "gmosNorthLongSlit": {
                            "grating" : "R831_G5302"
                          }
                        },
                        "observations" : {
                          "matches" : ${g3.map { id => Json.obj("id" -> id.asJson) }.asJson }
                        }
                      }
                    ]
                  }
                }
              """.asRight
            )

  test("default bin value matches explicit bin value"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid, "Biff",
         """
            sourceProfile: {
              gaussian: {
                fwhm: {
                  microarcseconds: 647200
                }
                spectralDefinition: {
                  bandNormalized: {
                    sed: {
                      stellarLibrary: B5_III
                    }
                    brightnesses: []
                  }
                }
              }
            }
          """
      ).flatMap: tid =>

        def createObs(bin: Option[GmosXBinning]): IO[Observation.Id] =
          createObservation(
              pi,
              pid,
              Site.GN,
              grating  = GmosNorthGrating.B1200_G5301.tag.toScreamingSnakeCase,
              fpu      = GmosNorthFpu.LongSlit_5_00.tag.toScreamingSnakeCase,
              xbin     = bin,
              iq       = ImageQuality.Preset.PointOne,
              asterism = List(tid)
          )

        // Default xbin in first obs, explicit in second obs
        List(createObs(none), createObs(GmosXBinning.Two.some)).sequence.flatMap: g =>
          expect(
            user = pi,
            query = s"""
              query {
                observingModeGroup(programId: ${pid.asJson}) {
                  matches {
                    observingMode {
                      gmosNorthLongSlit {
                        grating
                        xBin
                      }
                    }
                    observations {
                      matches {
                        id
                      }
                    }
                  }
                }
              }
            """,
            expected =
              json"""
                {
                  "observingModeGroup" : {
                    "matches" : [
                      {
                        "observingMode" : {
                          "gmosNorthLongSlit": {
                            "grating": "B1200_G5301",
                            "xBin": "TWO"
                          }
                        },
                        "observations" : {
                          "matches" : ${g.map { id => Json.obj("id" -> id.asJson) }.asJson }
                        }
                      }
                    ]
                  }
                }
              """.asRight
            )

  test("distinct observing modes edited to become same"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid, "Biff",
         """
            sourceProfile: {
              gaussian: {
                fwhm: {
                  microarcseconds: 647200
                }
                spectralDefinition: {
                  bandNormalized: {
                    sed: {
                      stellarLibrary: B5_III
                    }
                    brightnesses: []
                  }
                }
              }
            }
          """
      ).flatMap: tid =>

        def createObs(grating: GmosNorthGrating): IO[Observation.Id] =
          createObservation(
              pi,
              pid,
              Site.GN,
              grating  = grating.tag.toScreamingSnakeCase,
              fpu      = GmosNorthFpu.LongSlit_5_00.tag.toScreamingSnakeCase,
              iq       = ImageQuality.Preset.PointOne,
              asterism = List(tid)
          )

        for {
          o1 <- createObs(GmosNorthGrating.B1200_G5301)
          o2 <- createObs(GmosNorthGrating.R400_G5310)
          _  <- query(
            user  = pi,
            query = s"""
              mutation {
                updateObservations(input: {
                  SET: {
                    observingMode: {
                      gmosNorthLongSlit: {
                        grating: ${GmosNorthGrating.B1200_G5301.tag.toScreamingSnakeCase}
                      }
                    }
                  }
                  WHERE: {
                    id: { EQ: "${o2.toString}" }
                  }
                }) {
                  observations {
                    id
                  }
                }
              }
            """
          )
          _ <-  expect(
            user = pi,
            query = s"""
              query {
                observingModeGroup(programId: ${pid.asJson}) {
                  matches {
                    observingMode {
                      gmosNorthLongSlit {
                        grating
                      }
                    }
                    observations {
                      matches {
                        id
                      }
                    }
                  }
                }
              }
            """,
            expected =
              json"""
                {
                  "observingModeGroup" : {
                    "matches" : [
                      {
                        "observingMode" : {
                          "gmosNorthLongSlit": {
                            "grating": "B1200_G5301"
                          }
                        },
                        "observations" : {
                          "matches" : ${List(o1, o2).map { id => Json.obj("id" -> id.asJson) }.asJson }
                        }
                      }
                    ]
                  }
                }
              """.asRight
            )
        } yield ()
