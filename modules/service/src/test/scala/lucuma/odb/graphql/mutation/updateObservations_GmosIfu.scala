// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.StandardUser
import lucuma.core.model.Target
import lucuma.core.model.User

/**
 * Update and clone for the GMOS IFU observing mode.
 *
 * The analysis is the interesting part: it is stored as two nullable columns with an at-most-one
 * constraint, so switching between a summation radius and a single element offset has to clear
 * the other column, and clearing back to the default has to clear both.  A stale value left
 * behind would trip the constraint or, worse, be read back as the wrong sampling.
 */
class updateObservations_GmosIfu extends OdbSuite:

  val pi: StandardUser = TestUsers.Standard.pi(nextId, nextId)

  lazy val validUsers: List[User] = List(pi)

  private val scienceRequirements: String =
    """
      exposureTimeMode: {
        signalToNoise: { value: 100.0, at: { nanometers: 510 } }
      }
      spectroscopy: {
        wavelength: { nanometers: 500 }
        resolution: 100
        wavelengthCoverage: { nanometers: 20 }
        focalPlane: SINGLE_SLIT
        focalPlaneAngle: { microarcseconds: 0 }
      }
    """

  private val northMode: String =
    """
      gmosNorthIfu: {
        grating: R831_G5302
        filter: R_PRIME
        fpu: TWO_SLITS
        centralWavelength: { nanometers: 500 }
      }
    """

  private def create(pid: Program.Id, tid: Target.Id, mode: String): IO[Observation.Id] =
    query(
      user  = pi,
      query = s"""
        mutation {
          createObservation(input: {
            programId: ${pid.asJson}
            SET: {
              targetEnvironment: { asterism: ${List(tid).asJson} }
              scienceRequirements: { $scienceRequirements }
              observingMode: { $mode }
            }
          }) {
            observation { id }
          }
        }
      """
    ).map(_.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id])

  private def setup: IO[(Program.Id, Observation.Id)] =
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      oid <- create(pid, tid, northMode)
    yield (pid, oid)

  private def updateMutation(oid: Observation.Id, mode: String, selection: String): String =
    s"""
      mutation {
        updateObservations(input: {
          WHERE: { id: { EQ: "$oid" } }
          SET: { observingMode: { $mode } }
        }) {
          observations {
            observingMode { $selection }
          }
        }
      }
    """

  private val analysisSelection: String =
    "gmosNorthIfu { ifuAnalysis { sumRadius { arcseconds } singleOffset { arcseconds } } explicitIfuAnalysis { sumRadius { arcseconds } singleOffset { arcseconds } } }"

  private def analysisResult(ifu: String, explicit: String) =
    json"""
      {
        "updateObservations": {
          "observations": [
            {
              "observingMode": {
                "gmosNorthIfu": {
                  "ifuAnalysis": ${io.circe.parser.parse(ifu).toOption.get},
                  "explicitIfuAnalysis": ${io.circe.parser.parse(explicit).toOption.get}
                }
              }
            }
          ]
        }
      }
    """

  private val defaultAnalysis = """{ "sumRadius": { "arcseconds": 0.200000 }, "singleOffset": null }"""
  private val radiusHalf      = """{ "sumRadius": { "arcseconds": 0.500000 }, "singleOffset": null }"""
  private val offsetOne       = """{ "sumRadius": null, "singleOffset": { "arcseconds": 1.000000 } }"""

  test("set an explicit summation radius"):
    setup.flatMap: (_, oid) =>
      expect(pi, updateMutation(
        oid,
        "gmosNorthIfu: { explicitIfuAnalysis: { sumRadius: { arcseconds: 0.5 } } }",
        analysisSelection
      ), analysisResult(radiusHalf, radiusHalf).asRight)

  // Switching shape must clear the column the other shape used, or the at-most-one constraint
  // rejects the row.
  test("switch from a summation radius to a single element offset"):
    for
      (_, oid) <- setup
      _        <- query(pi, updateMutation(
                    oid,
                    "gmosNorthIfu: { explicitIfuAnalysis: { sumRadius: { arcseconds: 0.5 } } }",
                    analysisSelection
                  ))
      _        <- expect(pi, updateMutation(
                    oid,
                    "gmosNorthIfu: { explicitIfuAnalysis: { singleOffset: { arcseconds: 1.0 } } }",
                    analysisSelection
                  ), analysisResult(offsetOne, offsetOne).asRight)
    yield ()

  // Null clears the override, so the effective value falls back to the default sampling.
  test("clearing the analysis reverts to the default"):
    for
      (_, oid) <- setup
      _        <- query(pi, updateMutation(
                    oid,
                    "gmosNorthIfu: { explicitIfuAnalysis: { singleOffset: { arcseconds: 1.0 } } }",
                    analysisSelection
                  ))
      _        <- expect(pi, updateMutation(
                    oid,
                    "gmosNorthIfu: { explicitIfuAnalysis: null }",
                    analysisSelection
                  ), analysisResult(defaultAnalysis, "null").asRight)
    yield ()

  test("update the aperture, grating and filter"):
    setup.flatMap: (_, oid) =>
      expect(pi, updateMutation(
        oid,
        "gmosNorthIfu: { fpu: ONE_SLIT_BLUE, grating: B1200_G5301, filter: null }",
        "gmosNorthIfu { fpu grating filter initialFpu initialGrating initialFilter }"
      ), json"""
        {
          "updateObservations": {
            "observations": [
              {
                "observingMode": {
                  "gmosNorthIfu": {
                    "fpu": "ONE_SLIT_BLUE",
                    "grating": "B1200_G5301",
                    "filter": null,
                    "initialFpu": "TWO_SLITS",
                    "initialGrating": "R831_G5302",
                    "initialFilter": "R_PRIME"
                  }
                }
              }
            ]
          }
        }
      """.asRight)

  test("explicit telescope configs round-trip through an update"):
    setup.flatMap: (_, oid) =>
      expect(pi, updateMutation(
        oid,
        """gmosNorthIfu: {
             explicitTelescopeConfigs: [
               { offset: { p: { arcseconds: 1.5 }, q: { arcseconds: 0.9 } }, guiding: ENABLED },
               { offset: { p: { arcseconds: -1.5 }, q: { arcseconds: -0.9 } }, guiding: ENABLED }
             ]
           }""",
        "gmosNorthIfu { telescopeConfigs { offset { p { arcseconds } q { arcseconds } } guiding } }"
      ), json"""
        {
          "updateObservations": {
            "observations": [
              {
                "observingMode": {
                  "gmosNorthIfu": {
                    "telescopeConfigs": [
                      { "offset": { "p": { "arcseconds": 1.500000 }, "q": { "arcseconds": 0.900000 } }, "guiding": "ENABLED" },
                      { "offset": { "p": { "arcseconds": -1.500000 }, "q": { "arcseconds": -0.900000 } }, "guiding": "ENABLED" }
                    ]
                  }
                }
              }
            ]
          }
        }
      """.asRight)

  // The clone copies the mode row wholesale, so anything the caller set has to come across --
  // including the analysis, which lives in its own pair of columns.
  test("clone carries the mode across, analysis included"):
    for
      (_, oid) <- setup
      _        <- query(pi, updateMutation(
                    oid,
                    """gmosNorthIfu: {
                         fpu: ONE_SLIT_RED
                         explicitIfuAnalysis: { sumRadius: { arcseconds: 0.5 } }
                       }""",
                    analysisSelection
                  ))
      cid      <- query(pi, s"""
                    mutation {
                      cloneObservation(input: { observationId: "$oid" }) {
                        newObservation { id }
                      }
                    }
                  """).map(_.hcursor.downFields("cloneObservation", "newObservation", "id").require[Observation.Id])
      _        <- expect(pi, s"""
                    query {
                      observation(observationId: "$cid") {
                        observingMode {
                          mode
                          gmosNorthIfu {
                            fpu
                            grating
                            initialFpu
                            explicitIfuAnalysis { sumRadius { arcseconds } singleOffset { arcseconds } }
                          }
                        }
                      }
                    }
                  """, json"""
                    {
                      "observation": {
                        "observingMode": {
                          "mode": "GMOS_NORTH_IFU",
                          "gmosNorthIfu": {
                            "fpu": "ONE_SLIT_RED",
                            "grating": "R831_G5302",
                            "initialFpu": "TWO_SLITS",
                            "explicitIfuAnalysis": { "sumRadius": { "arcseconds": 0.500000 }, "singleOffset": null }
                          }
                        }
                      }
                    }
                  """.asRight)
    yield ()
