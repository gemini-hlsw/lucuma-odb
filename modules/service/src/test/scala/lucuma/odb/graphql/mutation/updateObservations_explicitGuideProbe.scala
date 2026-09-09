// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.syntax.string.*
import lucuma.odb.service.GuideProbeRules

class updateObservations_explicitGuideProbe extends query.ExecutionTestSupportForGmos with UpdateObservationsOps:

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

  def setProbe(probe: Option[GuideProbe]): String =
    s"""
      targetEnvironment: {
        explicitGuideProbe: ${probe.fold("null")(_.tag.toScreamingSnakeCase)}
      }
    """

  test("clear explicit guide probe with null"):
    for
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some)
      _   <- updateObservation(pi, oid, setProbe(GuideProbe.PWFS1.some), GuideProbeGraph, probes(GuideProbe.PWFS1.some, none, GuideProbe.PWFS1.some).asRight)
      _   <- updateObservation(pi, oid, setProbe(none), GuideProbeGraph, probes(none, none, none).asRight)
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
      _   <- updateObservation(pi, oid, setProbe(GuideProbe.GmosOIWFS.some), GuideProbeGraph, probes(GuideProbe.GmosOIWFS.some, GuideProbe.PWFS2.some, GuideProbe.GmosOIWFS.some).asRight)
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
               pi, oid, setProbe(GuideProbe.GmosOIWFS.some), GuideProbeGraph,
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
        ${setProbe(GuideProbe.Flamingos2OIWFS.some)}
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
                targetEnvironment: { explicitGuideProbe: GMOS_OIWFS }
              }
            }) {
              observation { id }
            }
          }
        """,
        expected = List(GuideProbeRules.notAllowedMessage(ObservingModeType.GhostIfu, GuideProbe.GmosOIWFS)).asLeft
      )

  test("clone copies the explicit guide probe"):
    for
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some)
      _   <- updateObservation(pi, oid, setProbe(GuideProbe.PWFS2.some), GuideProbeGraph, probes(GuideProbe.PWFS2.some, none, GuideProbe.PWFS2.some).asRight)
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

  // Same rule as blind offsets: PIs only before execution, staff while ongoing.
  def ongoingObservation: IO[Observation.Id] =
    for
      p <- createProgramAs(pi)
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      v <- recordVisitAs(serviceUser, o)
      s <- firstAcquisitionStepId(serviceUser, o)
      _ <- addEndStepEvent(s, v)
    yield o

  test("pi cannot set the explicit guide probe on an ongoing observation"):
    for
      oid <- ongoingObservation
      _   <- updateObservation(
               pi, oid, setProbe(GuideProbe.PWFS1.some), GuideProbeGraph,
               s"Observation $oid is ineligible for this operation due to its workflow state (Ongoing with allowed transition to Completed).".asLeft
             )
    yield ()

  test("staff can set and clear the explicit guide probe on an ongoing observation"):
    for
      oid <- ongoingObservation
      _   <- updateObservation(staff, oid, setProbe(GuideProbe.PWFS1.some), GuideProbeGraph, probes(GuideProbe.PWFS1.some, GuideProbe.GmosOIWFS.some, GuideProbe.PWFS1.some).asRight)
      _   <- updateObservation(staff, oid, setProbe(none), GuideProbeGraph, probes(GuideProbe.GmosOIWFS.some, GuideProbe.GmosOIWFS.some, none).asRight)
    yield ()
