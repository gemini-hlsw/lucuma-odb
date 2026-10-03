// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User

/**
 * A Target of Opportunity is a placeholder: it carries a region and no coordinates, and when the
 * alert arrives a real target takes its place in the asterism rather than the placeholder gaining
 * one. These tests pin what the region itself does -- that it defaults to the whole sky, that an
 * omitted arc leaves that axis alone, that neither it nor an arc may be null -- and that the
 * subtype is a one-of: supplying a top-level sidereal target stops this being an opportunity
 * target, and the reverse conversion works too.
 */
class opportunityTarget extends OdbSuite {

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

  // Enough of the target to answer "which subtype is this".
  private val TargetGraph =
    """
      {
        sidereal { ra { degrees } }
        nonsidereal { des }
        opportunity {
          region { rightAscensionArc { type start { degrees } } }
        }
      }
    """

  private def createToo(pid: Program.Id): IO[Target.Id] =
    query(pi,
      s"""
        mutation {
          createTarget(input: {
            programId: ${pid.asJson}
            SET: {
              name: "Burst"
              $SourceProfile
              opportunity: { $Region }
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

  private def makeToo(tid: Target.Id): IO[Json] =
    query(pi,
      s"""
        mutation {
          updateTargets(input: {
            SET: { opportunity: { $Region } }
            WHERE: { id: { EQ: ${tid.asJson} } }
          }) { targets $TargetGraph }
        }
      """
    ).map(_.hcursor.downFields("updateTargets", "targets").downN(0).require[Json])

  private val ExpectedRegion =
    json"""{ "rightAscensionArc": { "type": "PARTIAL", "start": { "degrees": 10.0 } } }"""

  test("supplying a top-level subtype stops the target being an opportunity target") {
    for
      pid <- createProgramAs(pi)
      tid <- createToo(pid)
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

  test("a plain sidereal target can be converted into a Target of Opportunity") {
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid, "Ordinary")
      js  <- makeToo(tid)
    yield assertEquals(
      js,
      json"""{
        "sidereal": null,
        "nonsidereal": null,
        "opportunity": { "region": $ExpectedRegion }
      }"""
    )
  }

}
