// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User

/**
 * The Altair field lens as the API reports it: the automatic choice, which in NGS follows the
 * separation of the selected guide star, and the effective value the explicit override wins. The
 * AOWFS patrol field is only a few tens of arcseconds across, so these share
 * `guideEnvironmentGnirsAltair`'s candidate table.
 */
class altairFieldLens extends ExecutionTestSupportForGnirs
                            with GuideEnvironmentSuite:

  override val gaiaResponseString: String = GaiaVoTables.altairCandidates

  override def createObservationAs(user: User, pid: Program.Id, tids: List[Target.Id]): IO[Observation.Id] =
    createGnirsLongSlitObservationAs(user, pid, tids*)

  private def setAltair(oid: Observation.Id, altair: String): IO[Unit] =
    query(
      user  = pi,
      query = s"""
        mutation {
          updateObservations(input: {
            WHERE: { id: { EQ: "$oid" } }
            SET: { targetEnvironment: { altair: $altair } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  private def observationWithAltair(altair: String, guideStar: Boolean): IO[Observation.Id] =
    for
      p <- createProgramAs(pi)
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationAs(pi, p, List(t))
      _ <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
      _ <- setAltair(o, altair)
      _ <- IO.whenA(guideStar)(setGuideTargetName(pi, o, defaultTargetName.some))
    yield o

  private def fieldLensQuery(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: "$oid") {
          targetEnvironment {
            altair { defaultFieldLens fieldLens }
          }
        }
      }
    """

  private def fieldLensResult(defaultFieldLens: Json, fieldLens: Json): Either[List[String], Json] =
    json"""
    {
      "observation": {
        "targetEnvironment": {
          "altair": {
            "defaultFieldLens": $defaultFieldLens,
            "fieldLens":        $fieldLens
          }
        }
      }
    }
    """.asRight

  private def expectFieldLenses(
    altair:           String,
    guideStar:        Boolean,
    defaultFieldLens: Json,
    fieldLens:        Json
  ): IO[Unit] =
    observationWithAltair(altair, guideStar).flatMap: oid =>
      expect(pi, fieldLensQuery(oid), expected = fieldLensResult(defaultFieldLens, fieldLens))

  test("LGS always uses the field lens"):
    expectFieldLenses("{ mode: LGS }", guideStar = false, json""""IN"""", json""""IN"""")

  test("NGS more than 1 arcsecond off axis uses the field lens"):
    expectFieldLenses("{ mode: NGS }", guideStar = true, json""""IN"""", json""""IN"""")

  test("an explicit NGS field lens overrides the automatic choice"):
    expectFieldLenses("{ mode: NGS, fieldLens: OUT }", guideStar = true, json""""IN"""", json""""OUT"""")

  test("NGS has no field lens until a guide star is selected"):
    expectFieldLenses("{ mode: NGS }", guideStar = false, Json.Null, Json.Null)
