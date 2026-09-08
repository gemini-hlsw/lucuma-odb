// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.service.GuideProbeRules

class updateObservations_explicitGuideProbe extends OdbSuite with UpdateObservationsOps:

  val pi: User = TestUsers.Standard.pi(nextId, nextId)

  override lazy val validUsers: List[User] = List(pi)

  val GuideProbeGraph: String =
    """
      observations {
        targetEnvironment {
          guideProbe
          defaultGuideProbe
          explicitGuideProbe
        }
      }
    """

  def probes(guide: Option[GuideProbe], default: Option[GuideProbe], explicit: Option[GuideProbe]): Json =
    json"""
      {
        "updateObservations": {
          "observations": [
            {
              "targetEnvironment": {
                "guideProbe": $guide,
                "defaultGuideProbe": $default,
                "explicitGuideProbe": $explicit
              }
            }
          ]
        }
      }
    """

  def setProbe(probe: String): String =
    s"""
      targetEnvironment: {
        explicitGuideProbe: $probe
      }
    """

  test("clear explicit guide probe with null"):
    for
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some)
      _   <- updateObservation(pi, oid, setProbe("PWFS1"), GuideProbeGraph, probes(GuideProbe.PWFS1.some, none, GuideProbe.PWFS1.some).asRight)
      _   <- updateObservation(pi, oid, setProbe("null"), GuideProbeGraph, probes(none, none, none).asRight)
    yield ()

  test("default guide probe is OIWFS for a sidereal GMOS target"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      oid <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid)
      _   <- updateObservation(pi, oid, "existence: PRESENT", GuideProbeGraph, probes(GuideProbe.GmosOIWFS.some, GuideProbe.GmosOIWFS.some, none).asRight)
    yield ()

  test("default guide probe is PWFS2 for a nonsidereal GMOS target"):
    for
      pid <- createProgramAs(pi)
      tid <- createNonsiderealTargetAs(pi, pid)
      oid <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid)
      _   <- updateObservation(pi, oid, "existence: PRESENT", GuideProbeGraph, probes(GuideProbe.PWFS2.some, GuideProbe.PWFS2.some, none).asRight)
    yield ()

  test("explicit OIWFS overrides the nonsidereal default"):
    for
      pid <- createProgramAs(pi)
      tid <- createNonsiderealTargetAs(pi, pid)
      oid <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid)
      _   <- updateObservation(pi, oid, setProbe("GMOS_OIWFS"), GuideProbeGraph, probes(GuideProbe.GmosOIWFS.some, GuideProbe.PWFS2.some, GuideProbe.GmosOIWFS.some).asRight)
    yield ()

  test("no default guide probe without an observing mode"):
    oneUpdateTest(
      user     = pi,
      update   = "existence: PRESENT",
      query    = GuideProbeGraph,
      expected = probes(none, none, none).asRight
    )

  test("reject a guide probe the observing mode cannot use"):
    for
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid, ObservingModeType.Flamingos2LongSlit.some)
      _   <- updateObservation(
               pi, oid, setProbe("GMOS_OIWFS"), GuideProbeGraph,
               s"Observation $oid: ${GuideProbeRules.notAllowedMessage(ObservingModeType.Flamingos2LongSlit, GuideProbe.GmosOIWFS)}".asLeft
             )
    yield ()

  test("a probe rejected for the old mode is accepted when the same update changes the mode"):
    val update =
      s"""
        observingMode: {
          flamingos2LongSlit: {
            disperser: R1200_JH
            filter: Y
            fpu: LONG_SLIT_2
            exposureTimeMode: {
              signalToNoise: {
                value: 20.0
                at: { nanometers: 1234.56 }
              }
            }
          }
        }
        ${setProbe("FLAMINGOS2_OIWFS")}
      """
    for
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some)
      _   <- updateObservation(pi, oid, update, GuideProbeGraph, probes(GuideProbe.Flamingos2OIWFS.some, none, GuideProbe.Flamingos2OIWFS.some).asRight)
    yield ()

  test("create observation with an explicit guide probe"):
    createProgramAs(pi).flatMap: pid =>
      expect(
        user  = pi,
        query = s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                targetEnvironment: { explicitGuideProbe: PWFS1 }
              }
            }) {
              observation {
                targetEnvironment { explicitGuideProbe }
              }
            }
          }
        """,
        expected = json"""
          {
            "createObservation": {
              "observation": {
                "targetEnvironment": { "explicitGuideProbe": "PWFS1" }
              }
            }
          }
        """.asRight
      )

  test("create observation rejects a probe the mode cannot use"):
    createProgramAs(pi).flatMap: pid =>
      expect(
        user  = pi,
        query = s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                observingMode: {
                  ghostIfu: {
                    resolutionMode: STANDARD
                  }
                }
                targetEnvironment: { explicitGuideProbe: PWFS1 }
              }
            }) {
              observation { id }
            }
          }
        """,
        expected = List(GuideProbeRules.notAllowedMessage(ObservingModeType.GhostIfu, GuideProbe.PWFS1)).asLeft
      )

  test("clone copies the explicit guide probe"):
    for
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some)
      _   <- updateObservation(pi, oid, setProbe("PWFS2"), GuideProbeGraph, probes(GuideProbe.PWFS2.some, none, GuideProbe.PWFS2.some).asRight)
      _   <- expect(
               user  = pi,
               query = s"""
                 mutation {
                   cloneObservation(input: { observationId: "$oid" }) {
                     newObservation { targetEnvironment { explicitGuideProbe } }
                   }
                 }
               """,
               expected = json"""
                 {
                   "cloneObservation": {
                     "newObservation": {
                       "targetEnvironment": { "explicitGuideProbe": "PWFS2" }
                     }
                   }
                 }
               """.asRight
             )
    yield ()
