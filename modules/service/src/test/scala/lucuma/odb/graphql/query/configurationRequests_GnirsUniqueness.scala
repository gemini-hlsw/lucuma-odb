// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User

/**
 * The configuration request uniqueness key must include the GNIRS columns.  Without them two GNIRS
 * requests that differ only in a GNIRS parameter leave every mode column in the key NULL, collide
 * under NULLS NOT DISTINCT, and the second `ON CONFLICT DO NOTHING` insert silently yields the
 * first request instead of a new one.
 */
class configurationRequests_GnirsUniqueness
  extends OdbSuite
     with ObservingModeSetupOperations:

  val admin: User = TestUsers.Standard.admin(3, 32)
  val pi: User    = TestUsers.Standard.pi(1, 30)

  val validUsers: List[User] = List(pi, admin)

  private def longSlitMode(grating: String, camera: String, prism: String): String =
    s"""
      gnirsSpectroscopy: {
        grating: $grating
        prism: $prism
        camera: $camera
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
    """

  private def ifuMode(grating: String): String =
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
              timeAndCount: {
                time: { seconds: 30.0 }
                count: 3
                at: { nanometers: 2200 }
              }
            }
          }
        ]
      }
    """

  // One program, one target, one set of conditions: the observing mode is the only thing that can
  // tell the two requests apart.
  private def setup: IO[(Program.Id, Target.Id)] =
    for
      cfpid <- createGeminiCallForProposalsAs(admin)
      pid   <- createProgramAs(pi, "GNIRS uniqueness")
      _     <- addProposal(pi, pid, Some(cfpid), None)
      tid   <- createTargetWithProfileAs(pi, pid)
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

  test("long slit requests differing only in grating are distinct"):
    assertDistinct(
      longSlitMode("D111", "SHORT_BLUE", "MIRROR"),
      longSlitMode("D32",  "SHORT_BLUE", "MIRROR")
    )

  test("long slit requests differing only in camera are distinct"):
    assertDistinct(
      longSlitMode("D111", "SHORT_BLUE", "MIRROR"),
      longSlitMode("D111", "LONG_BLUE",  "MIRROR")
    )

  test("IFU requests differing only in grating are distinct"):
    assertDistinct(
      ifuMode("D111"),
      ifuMode("D32")
    )

  // The same configuration really should collapse onto one request; widening the key must not
  // break that.
  test("identical requests still collapse onto one"):
    setup.flatMap: (pid, tid) =>
      for
        r1 <- requestFor(pid, tid, longSlitMode("D111", "SHORT_BLUE", "MIRROR"))
        r2 <- requestFor(pid, tid, longSlitMode("D111", "SHORT_BLUE", "MIRROR"))
        _  <- IO(assertEquals(r1, r2))
      yield ()
