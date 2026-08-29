// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import io.circe.syntax.*
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.graphql.query.ObservingModeSetupOperations.ConstraintSet

/**
 * The configuration request uniqueness key must carry exactly the columns that `SelectRequest`
 * matches exactly.  Where it does not, two genuinely distinct requests collide, the insert's
 * `ON CONFLICT DO NOTHING` yields no row, and the fallback lookup cannot find the colliding row
 * either -- so the caller gets "likely due to an incorrect unique index" rather than a request.
 *
 * The GMOS imaging filters are deliberately not covered here: they are matched by containment,
 * not equality, so they must stay out of the key.
 */
class configurationRequests_UniquenessKey
  extends OdbSuite
     with ObservingModeSetupOperations:

  val admin: User = TestUsers.Standard.admin(3, 32)
  val pi: User    = TestUsers.Standard.pi(1, 30)

  val validUsers: List[User] = List(pi, admin)

  private def gnirsLongSlit(grating: String, camera: String): String =
    s"""
      gnirsSpectroscopy: {
        grating: $grating
        prism: MIRROR
        camera: $camera
        slit: { fpu: LONG_SLIT_0_30 }
        filter: ORDER3
        centralWavelengths: [
          {
            centralWavelength: { nanometers: 2200 }
            exposureTimeMode: {
              timeAndCount: { time: { seconds: 30.0 } count: 3 at: { nanometers: 2200 } }
            }
          }
        ]
      }
    """

  private def gnirsIfu(grating: String): String =
    s"""
      gnirsSpectroscopy: {
        grating: $grating
        prism: MIRROR
        camera: SHORT_BLUE
        ifu: { fpu: LOW_RESOLUTION }
        filter: ORDER3
        centralWavelengths: [
          {
            centralWavelength: { nanometers: 2200 }
            exposureTimeMode: {
              timeAndCount: { time: { seconds: 30.0 } count: 3 at: { nanometers: 2200 } }
            }
          }
        ]
      }
    """

  // The configuration's visitor radius is the AGS diameter (AngleMapping), not the science FoV.
  private def visitor(agsArcsec: Int): String =
    s"""
      visitor: {
        mode: ALOPEKE_SPECKLE
        centralWavelength: { nanometers: 700 }
        agsDiameter: { arcseconds: $agsArcsec }
        scienceFovDiameter: { arcseconds: 1 }
      }
    """

  private val visitorRequirements: String =
    """
      exposureTimeMode: {
        timeAndCount: { time: { seconds: 30.0 } count: 4 at: { nanometers: 700 } }
      }
      imaging: {
        minimumFov: { arcseconds: 1 }
        narrowFilters: false
        broadFilters: false
        combinedFilters: true
      }
    """

  private def programWithProposal: IO[Program.Id] =
    for
      cfpid <- createGeminiCallForProposalsAs(admin)
      pid   <- createProgramAs(pi, "Uniqueness key")
      _     <- addProposal(pi, pid, Some(cfpid), None)
    yield pid

  // One program, one target, one set of conditions: the observing mode is all that can tell the
  // two requests apart.
  private def setup: IO[(Program.Id, Target.Id)] =
    for
      pid <- programWithProposal
      tid <- createTargetWithProfileAs(pi, pid)
    yield (pid, tid)

  private def requestFor(pid: Program.Id, tid: Target.Id, mode: String): IO[ConfigurationRequest.Id] =
    createObservationWithModeAs(pi, pid, List(tid), mode).flatMap(createConfigurationRequestAs(pi, _))

  private def assertDistinct(a: String, b: String): IO[Unit] =
    setup.flatMap: (pid, tid) =>
      for
        r1 <- requestFor(pid, tid, a)
        r2 <- requestFor(pid, tid, b)
        _  <- IO(assertNotEquals(r1, r2, s"Expected distinct configuration requests, got $r1 twice."))
      yield ()

  test("GNIRS long slit - differing only in grating"):
    assertDistinct(gnirsLongSlit("D111", "SHORT_BLUE"), gnirsLongSlit("D32", "SHORT_BLUE"))

  test("GNIRS long slit - differing only in camera"):
    assertDistinct(gnirsLongSlit("D111", "SHORT_BLUE"), gnirsLongSlit("D111", "LONG_BLUE"))

  test("GNIRS IFU - differing only in grating"):
    assertDistinct(gnirsIfu("D111"), gnirsIfu("D32"))

  // `createObservationWithModeAs` hardwires spectroscopy requirements, which a visitor imaging
  // mode will not accept.
  private def createVisitorObservation(pid: Program.Id, tid: Target.Id, mode: String): IO[Observation.Id] =
    query(
      pi,
      s"""
        mutation {
          createObservation(input: {
            programId: ${pid.asJson}
            SET: {
              $ConstraintSet,
              targetEnvironment: { asterism: ${List(tid).asJson} }
              scienceRequirements: { $visitorRequirements }
              observingMode: { $mode }
            }
          }) {
            observation { id }
          }
        }
      """
    ).map(_.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id])

  test("visitor - differing only in AGS diameter"):
    setup.flatMap: (pid, tid) =>
      for
        o1 <- createVisitorObservation(pid, tid, visitor(1))
        o2 <- createVisitorObservation(pid, tid, visitor(4))
        r1 <- createConfigurationRequestAs(pi, o1)
        r2 <- createConfigurationRequestAs(pi, o2)
        _  <- IO(assertNotEquals(r1, r2, s"Expected distinct configuration requests, got $r1 twice."))
      yield ()

  // Opportunity targets carry null reference coordinates, so the region columns are the whole
  // discriminant.
  private def createOpportunityTargetWithRegion(pid: Program.Id, decStart: Int, decEnd: Int): IO[Target.Id] =
    query(
      pi,
      s"""
        mutation {
          createTarget(input: {
            programId: ${pid.asJson}
            SET: {
              name: "Opportunity $decStart-$decEnd"
              opportunity: {
                region: {
                  rightAscensionArc: { type: FULL }
                  declinationArc: { type: PARTIAL, start: { degrees: $decStart }, end: { degrees: $decEnd } }
                }
              }
              $DefaultSourceProfile
            }
          }) {
            target { id }
          }
        }
      """
    ).map(_.hcursor.downFields("createTarget", "target", "id").require[Target.Id])

  test("opportunity targets - differing only in region"):
    for
      pid <- programWithProposal
      t1  <- createOpportunityTargetWithRegion(pid, 10, 70)
      t2  <- createOpportunityTargetWithRegion(pid, 20, 60)
      r1  <- requestFor(pid, t1, gnirsLongSlit("D111", "SHORT_BLUE"))
      r2  <- requestFor(pid, t2, gnirsLongSlit("D111", "SHORT_BLUE"))
      _   <- IO(assertNotEquals(r1, r2, s"Expected distinct configuration requests, got $r1 twice."))
    yield ()

  // Widening the key must not stop genuinely identical requests collapsing onto one.
  test("identical requests still collapse onto one"):
    setup.flatMap: (pid, tid) =>
      for
        r1 <- requestFor(pid, tid, gnirsLongSlit("D111", "SHORT_BLUE"))
        r2 <- requestFor(pid, tid, gnirsLongSlit("D111", "SHORT_BLUE"))
        _  <- IO(assertEquals(r1, r2))
      yield ()
