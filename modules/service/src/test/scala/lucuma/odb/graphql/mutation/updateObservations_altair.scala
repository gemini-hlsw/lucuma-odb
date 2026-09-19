// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.service.AltairRules
import lucuma.odb.service.GuideProbeRules

class updateObservations_altair extends OdbSuite with UpdateObservationsOps:

  val pi: User = TestUsers.Standard.pi(nextId, nextId)

  override lazy val validUsers: List[User] = List(pi)

  val AltairGraph: String =
    """
      observations {
        targetEnvironment {
          altair { mode explicitFieldLens cassRotator ndFilter }
          guideProbe
          defaultGuideProbe
          cassRotator
        }
      }
    """

  private def targetEnvironment(
    altair:      Json,
    guide:       Option[GuideProbe],
    default:     Option[GuideProbe],
    cassRotator: String
  ): Json =
    json"""
      {
        "targetEnvironment": {
          "altair": $altair,
          "guideProbe": $guide,
          "defaultGuideProbe": $default,
          "cassRotator": $cassRotator
        }
      }
    """

  private def updated(
    altair:      Json,
    guide:       Option[GuideProbe],
    default:     Option[GuideProbe],
    cassRotator: String
  ): Json =
    json"""
      {
        "updateObservations": {
          "observations": [ ${targetEnvironment(altair, guide, default, cassRotator)} ]
        }
      }
    """

  private def altairJson(mode: String, fieldLens: Option[String], cassRotator: String, ndFilter: String): Json =
    json"""
      {
        "mode": $mode,
        "explicitFieldLens": $fieldLens,
        "cassRotator": $cassRotator,
        "ndFilter": $ndFilter
      }
    """

  // A GNIRS long slit observing mode, minimal but complete.
  val GnirsMode: String =
    """
      observingMode: {
        gnirsSpectroscopy: {
          grating: D111
          prism: MIRROR
          camera: SHORT_BLUE
          slit: { fpu: LONG_SLIT_0_30 }
          filter: ORDER3
          centralWavelengths: [
            {
              centralWavelength: { nanometers: 2200 }
              exposureTimeMode: {
                timeAndCount: {
                  time: { seconds: 30.0 }
                  count: 3
                  at: { nanometers: 2200 }
                }
              }
            }
          ]
        }
      }
    """

  val GmosMode: String =
    """
      observingMode: {
        gmosNorthLongSlit: {
          grating: R831_G5302
          filter: R_PRIME
          fpu: LONG_SLIT_0_50
          centralWavelength: { nanometers: 500 }
          exposureTimeMode: {
            signalToNoise: {
              value: 20.0
              at: { nanometers: 500 }
            }
          }
        }
      }
    """

  private def createWithAltair(pid: Program.Id, tid: Target.Id, mode: String, altair: String): String =
    s"""
      mutation {
        createObservation(input: {
          programId: "$pid"
          SET: {
            targetEnvironment: {
              asterism: [ "$tid" ]
              altair: $altair
            }
            $mode
          }
        }) {
          observation {
            targetEnvironment {
              altair { mode explicitFieldLens cassRotator ndFilter }
              guideProbe
              defaultGuideProbe
              cassRotator
            }
          }
        }
      }
    """

  private def gnirsObservationAs(altair: Option[String]): IO[Observation.Id] =
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      oid <- createObservationAs(pi, pid, ObservingModeType.GnirsLongSlit.some, tid)
      _   <- altair.traverse_ : a =>
               updateObservation(
                 pi, oid, s"targetEnvironment: { altair: $a }", "observations { id }",
                 json"""{ "updateObservations": { "observations": [ { "id": $oid } ] } }""".asRight
               )
    yield oid

  test("create a GNIRS observation with Altair NGS"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      _   <- expect(
               user     = pi,
               query    = createWithAltair(pid, tid, GnirsMode, "{ mode: NGS }"),
               expected = json"""
                 {
                   "createObservation": {
                     "observation": ${targetEnvironment(
                       altairJson("NGS", none, "FOLLOWING", "OUT"),
                       GuideProbe.AltairAOWFS.some,
                       GuideProbe.AltairAOWFS.some,
                       "FOLLOWING"
                     )}
                   }
                 }
               """.asRight
             )
    yield ()

  test("Altair LGS+P1 guides on PWFS1"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      _   <- expect(
               user     = pi,
               query    = createWithAltair(pid, tid, GnirsMode, "{ mode: LGS_P1 }"),
               expected = json"""
                 {
                   "createObservation": {
                     "observation": ${targetEnvironment(
                       altairJson("LGS_P1", none, "FOLLOWING", "OUT"),
                       GuideProbe.PWFS1.some,
                       GuideProbe.PWFS1.some,
                       "FOLLOWING"
                     )}
                   }
                 }
               """.asRight
             )
    yield ()

  test("an LGS mode cannot take the field lens out"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      _   <- expect(
               user     = pi,
               query    = createWithAltair(pid, tid, GnirsMode, "{ mode: LGS, fieldLens: OUT }"),
               expected = List(AltairRules.LgsFieldLensMessage).asLeft
             )
    yield ()

  test("Altair cannot be used with a GMOS observing mode"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      _   <- expect(
               user     = pi,
               query    = createWithAltair(pid, tid, GmosMode, "{ mode: NGS }"),
               expected = List(AltairRules.NotGnirsMessage).asLeft
             )
    yield ()

  test("an update replaces the whole Altair configuration"):
    for
      oid <- gnirsObservationAs("{ mode: NGS, fieldLens: IN, cassRotator: FIXED, ndFilter: IN }".some)
      _   <- updateObservation(
               pi, oid, "targetEnvironment: { altair: { mode: LGS } }", AltairGraph,
               updated(
                 altairJson("LGS", none, "FOLLOWING", "OUT"),
                 GuideProbe.AltairAOWFS.some,
                 GuideProbe.AltairAOWFS.some,
                 "FOLLOWING"
               ).asRight
             )
    yield ()

  test("a stored cass rotator overrides the instrument default"):
    for
      oid <- gnirsObservationAs("{ mode: NGS, cassRotator: FIXED }".some)
      _   <- updateObservation(
               pi, oid, "existence: PRESENT", AltairGraph,
               updated(
                 altairJson("NGS", none, "FIXED", "OUT"),
                 GuideProbe.AltairAOWFS.some,
                 GuideProbe.AltairAOWFS.some,
                 "FIXED"
               ).asRight
             )
    yield ()

  test("null clears the Altair configuration"):
    for
      oid <- gnirsObservationAs("{ mode: NGS }".some)
      _   <- updateObservation(
               pi, oid, "targetEnvironment: { altair: null }", AltairGraph,
               updated(Json.Null, GuideProbe.PWFS2.some, GuideProbe.PWFS2.some, "FOLLOWING").asRight
             )
    yield ()

  test("clearing Altair is rejected while the explicit probe is the Altair sensor"):
    for
      oid <- gnirsObservationAs("{ mode: NGS }".some)
      _   <- updateObservation(
               pi, oid, "targetEnvironment: { explicitGuideProbe: ALTAIR_AOWFS }", AltairGraph,
               updated(altairJson("NGS", none, "FOLLOWING", "OUT"), GuideProbe.AltairAOWFS.some, GuideProbe.AltairAOWFS.some, "FOLLOWING").asRight
             )
      _   <- updateObservation(
               pi, oid, "targetEnvironment: { altair: null }", AltairGraph,
               s"Observation $oid: ${GuideProbeRules.notAllowedMessage(ObservingModeType.GnirsLongSlit, GuideProbe.AltairAOWFS)}".asLeft
             )
    yield ()

  test("moving an Altair observation off GNIRS is rejected"):
    for
      oid <- gnirsObservationAs("{ mode: NGS }".some)
      _   <- updateObservation(
               pi, oid, GmosMode, AltairGraph,
               s"Observation $oid: ${AltairRules.NotGnirsMessage}".asLeft
             )
    yield ()

  test("Altair fixes the guide probe, so an explicit PWFS2 is rejected"):
    for
      oid <- gnirsObservationAs("{ mode: NGS }".some)
      _   <- updateObservation(
               pi, oid, "targetEnvironment: { explicitGuideProbe: PWFS2 }", AltairGraph,
               s"Observation $oid: ${GuideProbeRules.notAllowedMessage(ObservingModeType.GnirsLongSlit, GuideProbe.PWFS2)}".asLeft
             )
    yield ()
