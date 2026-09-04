// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Ephemeris
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import skunk.codec.all.*
import skunk.syntax.all.*

/**
 * A Target of Opportunity keeps its identity when the alert arrives: it gains a resolution and
 * keeps its region, rather than being replaced by a sidereal target. These tests pin the
 * consequences that are easy to regress -- that the region survives, that a resolved ToO still
 * reports as an `opportunity` and not as a top-level `sidereal`, and that resolving is reversible.
 */
class targetResolution extends OdbSuite {

  val pi: User = TestUsers.Standard.pi(nextId, nextId)

  override lazy val validUsers: List[User] = List(pi)

  private val SourceProfile =
    """
      sourceProfile: {
        point: {
          bandNormalized: {
            sed: { stellarLibrary: B5_III }
            brightnesses: []
          }
        }
      }
    """

  private val Region =
    """
      region: {
        rightAscensionArc: { type: PARTIAL, start: { degrees: "10.000" }, end: { degrees: "20.000" } }
        declinationArc:    { type: PARTIAL, start: { degrees: "40.000" }, end: { degrees: "50.000" } }
      }
    """

  // Enough of the target to answer "which subtype is this, and what did it resolve to".
  private val TargetGraph =
    """
      {
        sidereal { ra { degrees } }
        nonsidereal { des }
        opportunity {
          region { rightAscensionArc { type start { degrees } } }
          resolution {
            sidereal { ra { degrees } dec { degrees } epoch }
            nonsidereal { des keyType }
          }
        }
      }
    """

  private def createToo(pid: Program.Id, resolution: String = ""): IO[Target.Id] =
    query(pi,
      s"""
        mutation {
          createTarget(input: {
            programId: ${pid.asJson}
            SET: {
              name: "Burst"
              $SourceProfile
              opportunity: { $Region $resolution }
            }
          }) { target { id } }
        }
      """
    ).map(_.hcursor.downFields("createTarget", "target", "id").require[Target.Id])

  /** Creates a ToO with the given region -- possibly none at all -- and reports the region it got. */
  private def createTooWithRegion(pid: Program.Id, region: String): IO[Json] =
    query(pi,
      s"""
        mutation {
          createTarget(input: {
            programId: ${pid.asJson}
            SET: {
              name: "Burst"
              $SourceProfile
              opportunity: { $region }
            }
          }) {
            target { opportunity { region { rightAscensionArc { type } declinationArc { type } } } }
          }
        }
      """
    ).map(_.hcursor.downFields("createTarget", "target", "opportunity", "region").require[Json])

  private def selectTarget(tid: Target.Id): IO[Json] =
    query(pi, s"""query { target(targetId: ${tid.asJson}) $TargetGraph }""")
      .map(_.hcursor.downField("target").require[Json])

  private def updateToo(tid: Target.Id, opportunity: String): IO[Json] =
    query(pi,
      s"""
        mutation {
          updateTargets(input: {
            SET: { opportunity: { $Region $opportunity } }
            WHERE: { id: { EQ: ${tid.asJson} } }
          }) { targets $TargetGraph }
        }
      """
    ).map(_.hcursor.downFields("updateTargets", "targets").downN(0).require[Json])

  private val UnresolvedRegion =
    json"""{ "rightAscensionArc": { "type": "PARTIAL", "start": { "degrees": 10.0 } } }"""

  private val SiderealResolution =
    json"""
      {
        "sidereal": { "ra": { "degrees": 12.0 }, "dec": { "degrees": 42.0 }, "epoch": "J2000.000" },
        "nonsidereal": null
      }
    """

  private val ResolveSidereal =
    """resolution: { sidereal: { ra: { degrees: "12.000" }, dec: { degrees: "42.000" }, epoch: "J2000.000" } }"""

  test("a Target of Opportunity is created unresolved") {
    for
      pid <- createProgramAs(pi)
      tid <- createToo(pid)
      js  <- selectTarget(tid)
    yield assertEquals(
      js,
      json"""{
        "sidereal": null,
        "nonsidereal": null,
        "opportunity": { "region": $UnresolvedRegion, "resolution": null }
      }"""
    )
  }

  test("resolving keeps the region, and the target is still an opportunity target") {
    for
      pid <- createProgramAs(pi)
      tid <- createToo(pid)
      js  <- updateToo(tid, ResolveSidereal)
    yield assertEquals(
      js,
      json"""{
        "sidereal": null,
        "nonsidereal": null,
        "opportunity": { "region": $UnresolvedRegion, "resolution": $SiderealResolution }
      }"""
    )
  }

  test("a resolved Target of Opportunity can be created outright") {
    for
      pid <- createProgramAs(pi)
      tid <- createToo(pid, ResolveSidereal)
      js  <- selectTarget(tid)
    yield assertEquals(
      js.hcursor.downFields("opportunity", "resolution").require[Json],
      SiderealResolution
    )
  }

  test("resolving to a nonsidereal target works too") {
    for
      pid <- createProgramAs(pi)
      tid <- createToo(pid)
      js  <- updateToo(tid, """resolution: { nonsidereal: { keyType: COMET, des: "1P" } }""")
    yield assertEquals(
      js.hcursor.downFields("opportunity", "resolution").require[Json],
      json"""{ "sidereal": null, "nonsidereal": { "des": "1P", "keyType": "COMET" } }"""
    )
  }

  test("editing the region of a resolved target leaves the resolution alone") {
    for
      pid <- createProgramAs(pi)
      tid <- createToo(pid)
      _   <- updateToo(tid, ResolveSidereal)
      js  <- updateToo(tid, "")   // region only, resolution omitted
    yield assertEquals(
      js.hcursor.downFields("opportunity", "resolution").require[Json],
      SiderealResolution
    )
  }

  test("assigning a null resolution un-resolves the target and keeps the region") {
    for
      pid <- createProgramAs(pi)
      tid <- createToo(pid)
      _   <- updateToo(tid, ResolveSidereal)
      js  <- updateToo(tid, "resolution: null")
    yield assertEquals(
      js,
      json"""{
        "sidereal": null,
        "nonsidereal": null,
        "opportunity": { "region": $UnresolvedRegion, "resolution": null }
      }"""
    )
  }

  test("supplying a top-level subtype stops the target being an opportunity target") {
    for
      pid <- createProgramAs(pi)
      tid <- createToo(pid, ResolveSidereal)
      _   <- query(pi,
               s"""
                 mutation {
                   updateTargets(input: {
                     SET: { sidereal: { ra: { degrees: "1.000" }, dec: { degrees: "2.000" }, epoch: "J2000.000" } }
                     WHERE: { id: { EQ: ${tid.asJson} } }
                   }) { targets { id } }
                 }
               """)
      js  <- selectTarget(tid)
    yield assertEquals(
      js,
      json"""{
        "sidereal": { "ra": { "degrees": 1.0 } },
        "nonsidereal": null,
        "opportunity": null
      }"""
    )
  }

  test("resolving without restating the region leaves the region alone") {
    for
      pid <- createProgramAs(pi)
      tid <- createToo(pid)
      // No region here at all: it is optional on edit precisely so that resolving
      // cannot silently redraw the patch of sky the TAC approved.
      js  <- query(pi,
               s"""
                 mutation {
                   updateTargets(input: {
                     SET: { opportunity: { $ResolveSidereal } }
                     WHERE: { id: { EQ: ${tid.asJson} } }
                   }) { targets $TargetGraph }
                 }
               """
             ).map(_.hcursor.downFields("updateTargets", "targets").downN(0).require[Json])
    yield assertEquals(
      js,
      json"""{
        "sidereal": null,
        "nonsidereal": null,
        "opportunity": { "region": $UnresolvedRegion, "resolution": $SiderealResolution }
      }"""
    )
  }

  test("a null region is rejected") {
    for
      pid <- createProgramAs(pi)
      tid <- createToo(pid)
      _   <- expect(
               user = pi,
               query = s"""
                 mutation {
                   updateTargets(input: {
                     SET: { opportunity: { region: null } }
                     WHERE: { id: { EQ: ${tid.asJson} } }
                   }) { targets { id } }
                 }
               """,
               expected = List("Argument 'input.SET.opportunity.region' is invalid: cannot be null").asLeft
             )
    yield ()
  }

  test("creating without a region approves the whole sky") {
    for
      pid <- createProgramAs(pi)
      js  <- createTooWithRegion(pid, "")
    yield assertEquals(
      js,
      json"""{ "rightAscensionArc": { "type": "FULL" }, "declinationArc": { "type": "FULL" } }"""
    )
  }

  test("an omitted arc leaves that axis unconstrained") {
    for
      pid <- createProgramAs(pi)
      js  <- createTooWithRegion(
               pid,
               """region: { declinationArc: { type: PARTIAL, start: { degrees: "40.000" }, end: { degrees: "50.000" } } }"""
             )
    yield assertEquals(
      js,
      json"""{ "rightAscensionArc": { "type": "FULL" }, "declinationArc": { "type": "PARTIAL" } }"""
    )
  }

  test("a null arc is rejected") {
    for
      pid <- createProgramAs(pi)
      _   <- expect(
               user = pi,
               query = s"""
                 mutation {
                   createTarget(input: {
                     programId: ${pid.asJson}
                     SET: { name: "Burst" $SourceProfile opportunity: { region: { declinationArc: null } } }
                   }) { target { id } }
                 }
               """,
               expected = List("Argument 'input.SET.opportunity.region.declinationArc' is invalid: cannot be null").asLeft
             )
    yield ()
  }

  /** How many ephemeris elements are stored under `des`, which is the whole point below. */
  private def storedElements(des: String): IO[Long] =
    session.use: s =>
      s.prepareR(sql"select count(*) from t_ephemeris where c_des = $text".query(int8)).use: pq =>
        pq.unique(des)

  // A user-supplied ephemeris can arrive as an opportunity target's resolution rather than as the
  // target's own tracking.  Both write the same key columns, so both have to replace the stored
  // elements; the nested path used to write the columns and leave the old ephemeris in place,
  // pointing the target at data nobody supplied.
  test("resolving to a user-supplied ephemeris replaces the stored elements") {
    for
      pid    <- createProgramAs(pi)
      full   <- createUserDefinedEphemerisFor(Ephemeris.Key.Comet("1P"))
      nsid   <- createNonsiderealTargetWithUserSuppliedEphemerisAs(pi, pid, full)
      js     <- query(pi, s"""query { target(targetId: ${nsid.asJson}) { nonsidereal { key des } } }""")
      key     = js.hcursor.downFields("target", "nonsidereal", "key").require[String]
      des     = js.hcursor.downFields("target", "nonsidereal", "des").require[String]
      before <- storedElements(des)
      half    = full.map(es => es.take(es.length / 2))
      tid    <- createToo(pid)
      _      <- query(pi,
                  s"""
                    mutation {
                      updateTargets(input: {
                        SET: { opportunity: { resolution: { nonsidereal: { key: "$key" ephemeris: ${half.asGraphQL} } } } }
                        WHERE: { id: { EQ: ${tid.asJson} } }
                      }) { targets { id } }
                    }
                  """
                )
      after  <- storedElements(des)
    yield
      assertEquals(before, full.toList.map(_.length.toLong).sum)
      assertEquals(after,  half.toList.map(_.length.toLong).sum)
  }

  test("a plain sidereal target can be converted into a Target of Opportunity") {
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid, "Ordinary")
      js  <- updateToo(tid, "")
    yield assertEquals(
      js,
      json"""{
        "sidereal": null,
        "nonsidereal": null,
        "opportunity": { "region": $UnresolvedRegion, "resolution": null }
      }"""
    )
  }

}
