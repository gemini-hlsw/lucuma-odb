// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
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

class createObservation_GmosIfu extends OdbSuite:

  val pi: StandardUser = TestUsers.Standard.pi(nextId, nextId)

  lazy val validUsers: List[User] = List(pi)

  private def scienceRequirements: String =
    """
      exposureTimeMode: {
        signalToNoise: {
          value: 100.0
          at: { nanometers: 510 }
        }
      }
      spectroscopy: {
        wavelength: { nanometers: 500 }
        resolution: 100
        wavelengthCoverage: { nanometers: 20 }
        focalPlane: SINGLE_SLIT
        focalPlaneAngle: { microarcseconds: 0 }
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

  private def setup(mode: String): IO[Observation.Id] =
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      oid <- create(pid, tid, mode)
    yield oid

  private val northMode: String =
    """
      gmosNorthIfu: {
        grating: R831_G5302
        filter: R_PRIME
        fpu: TWO_SLITS
        centralWavelength: { nanometers: 500 }
      }
    """

  private val southMode: String =
    """
      gmosSouthIfu: {
        grating: B1200_G5321
        fpu: ONE_SLIT_BLUE
        centralWavelength: { nanometers: 500 }
      }
    """

  private def modeQuery(oid: Observation.Id, field: String): String =
    s"""
      query {
        observation(observationId: "$oid") {
          observingMode {
            mode
            $field {
              grating
              filter
              fpu
              centralWavelength { nanometers }
              acquisition { filter defaultFilter explicitFilter roi defaultRoi explicitRoi }
              ifuAnalysis { sumRadius { arcseconds } singleOffset { arcseconds } }
              defaultIfuAnalysis { sumRadius { arcseconds } singleOffset { arcseconds } }
              explicitIfuAnalysis { sumRadius { arcseconds } singleOffset { arcseconds } }
              xBin
              defaultXBin
              yBin
              defaultYBin
              roi
              ampGain
              ampReadMode
              telescopeConfigs { offset { p { arcseconds } q { arcseconds } } guiding }
              defaultTelescopeConfigs { offset { p { arcseconds } q { arcseconds } } guiding }
              explicitTelescopeConfigs { offset { p { arcseconds } q { arcseconds } } guiding }
              initialGrating
              initialFilter
              initialFpu
              initialCentralWavelength { nanometers }
            }
          }
        }
      }
    """

  // A single guided position on target: the IFU has a dedicated sky field 60" away, so it does
  // not nod for background.
  private val defaultTelescopeConfigs =
    json"""[{ "offset": { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": 0.000000 } }, "guiding": "ENABLED" }]"""

  // One lenslet pitch, which encloses only the element on the field centre.
  private val defaultAnalysis =
    json"""{ "sumRadius": { "arcseconds": 0.200000 }, "singleOffset": null }"""

  test("create GMOS North IFU"):
    setup(northMode).flatMap: oid =>
      expect(pi, modeQuery(oid, "gmosNorthIfu"), json"""
        {
          "observation": {
            "observingMode": {
              "mode": "GMOS_NORTH_IFU",
              "gmosNorthIfu": {
                "grating": "R831_G5302",
                "filter": "R_PRIME",
                "fpu": "TWO_SLITS",
                "centralWavelength": { "nanometers": 500.000 },
                "acquisition": {
                  "filter": "G_PRIME",
                  "defaultFilter": "G_PRIME",
                  "explicitFilter": null,
                  "roi": "CCD2_FULL_FRAME",
                  "defaultRoi": "CCD2_FULL_FRAME",
                  "explicitRoi": null
                },
                "ifuAnalysis": $defaultAnalysis,
                "defaultIfuAnalysis": $defaultAnalysis,
                "explicitIfuAnalysis": null,
                "xBin": "ONE",
                "defaultXBin": "ONE",
                "yBin": "ONE",
                "defaultYBin": "ONE",
                "roi": "FULL_FRAME",
                "ampGain": "LOW",
                "ampReadMode": "SLOW",
                "telescopeConfigs": $defaultTelescopeConfigs,
                "defaultTelescopeConfigs": $defaultTelescopeConfigs,
                "explicitTelescopeConfigs": null,
                "initialGrating": "R831_G5302",
                "initialFilter": "R_PRIME",
                "initialFpu": "TWO_SLITS",
                "initialCentralWavelength": { "nanometers": 500.000 }
              }
            }
          }
        }
      """.asRight)

  test("create GMOS South IFU, no filter"):
    setup(southMode).flatMap: oid =>
      expect(pi, modeQuery(oid, "gmosSouthIfu"), json"""
        {
          "observation": {
            "observingMode": {
              "mode": "GMOS_SOUTH_IFU",
              "gmosSouthIfu": {
                "grating": "B1200_G5321",
                "filter": null,
                "fpu": "ONE_SLIT_BLUE",
                "centralWavelength": { "nanometers": 500.000 },
                "acquisition": {
                  "filter": "G_PRIME",
                  "defaultFilter": "G_PRIME",
                  "explicitFilter": null,
                  "roi": "CCD2_FULL_FRAME",
                  "defaultRoi": "CCD2_FULL_FRAME",
                  "explicitRoi": null
                },
                "ifuAnalysis": $defaultAnalysis,
                "defaultIfuAnalysis": $defaultAnalysis,
                "explicitIfuAnalysis": null,
                "xBin": "ONE",
                "defaultXBin": "ONE",
                "yBin": "ONE",
                "defaultYBin": "ONE",
                "roi": "FULL_FRAME",
                "ampGain": "LOW",
                "ampReadMode": "SLOW",
                "telescopeConfigs": $defaultTelescopeConfigs,
                "defaultTelescopeConfigs": $defaultTelescopeConfigs,
                "explicitTelescopeConfigs": null,
                "initialGrating": "B1200_G5321",
                "initialFilter": null,
                "initialFpu": "ONE_SLIT_BLUE",
                "initialCentralWavelength": { "nanometers": 500.000 }
              }
            }
          }
        }
      """.asRight)

  // The sampling geometry is what the ITC integrates over, so it has to survive a round trip.
  test("an explicit sum radius round-trips"):
    setup("""
      gmosNorthIfu: {
        grating: R831_G5302
        fpu: TWO_SLITS
        centralWavelength: { nanometers: 500 }
        explicitIfuAnalysis: { sumRadius: { arcseconds: 0.5 } }
      }
    """).flatMap: oid =>
      expect(pi, s"""
        query {
          observation(observationId: "$oid") {
            observingMode {
              gmosNorthIfu {
              ifuAnalysis { sumRadius { arcseconds } singleOffset { arcseconds } }
                explicitIfuAnalysis { sumRadius { arcseconds } singleOffset { arcseconds } }
              }
            }
          }
        }
      """, json"""
        {
          "observation": {
            "observingMode": {
              "gmosNorthIfu": {
                "ifuAnalysis": { "sumRadius": { "arcseconds": 0.500000 }, "singleOffset": null },
                "explicitIfuAnalysis": { "sumRadius": { "arcseconds": 0.500000 }, "singleOffset": null }
              }
            }
          }
        }
      """.asRight)

  test("a single element offset round-trips"):
    setup("""
      gmosNorthIfu: {
        grating: R831_G5302
        fpu: ONE_SLIT_RED
        centralWavelength: { nanometers: 500 }
        explicitIfuAnalysis: { singleOffset: { arcseconds: 1.5 } }
      }
    """).flatMap: oid =>
      expect(pi, s"""
        query {
          observation(observationId: "$oid") {
            observingMode {
              gmosNorthIfu {
              ifuAnalysis { sumRadius { arcseconds } singleOffset { arcseconds } }
                explicitIfuAnalysis { sumRadius { arcseconds } singleOffset { arcseconds } }
              }
            }
          }
        }
      """, json"""
        {
          "observation": {
            "observingMode": {
              "gmosNorthIfu": {
                "ifuAnalysis": { "sumRadius": null, "singleOffset": { "arcseconds": 1.500000 } },
                "explicitIfuAnalysis": { "sumRadius": null, "singleOffset": { "arcseconds": 1.500000 } }
              }
            }
          }
        }
      """.asRight)
