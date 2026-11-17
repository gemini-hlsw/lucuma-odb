// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.ags.GuideStarName
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.util.Timestamp

class setGuideTarget extends query.ExecutionTestSupportForGmos:

  val GaiaSourceId: Long = 1L
  val GaiaName: String   = GuideStarName.gaiaSourceId.reverseGet(GaiaSourceId).value.value

  val Now: Timestamp = Timestamp.FromString.getOption("2024-08-25T00:00:00Z").get

  private val OneOfError: String =
    "Exactly one key must be specified for oneOf input object GuideTargetSelectionInput in field 'setGuideTarget' of type 'Mutation'"

  private val TargetEnvironmentSelection: String =
    """
      targetEnvironment {
        guideTargetName
        guideTargetSelection {
          catalogName
          observationTarget { targetId }
        }
      }
    """

  private def setMutation(oid: Observation.Id, guideTarget: String): String =
    s"""
      mutation {
        setGuideTarget(input: { observationId: ${oid.asJson}, guideTarget: $guideTarget }) {
          observation {
            $TargetEnvironmentSelection
          }
        }
      }
    """

  private def selectionQuery(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: ${oid.asJson}) {
          $TargetEnvironmentSelection
        }
      }
    """

  private def targetEnvironmentJson(name: Option[String], selection: Json): Json =
    json"""
      {
        "targetEnvironment": {
          "guideTargetName": ${name.asJson},
          "guideTargetSelection": $selection
        }
      }
    """

  private val CatalogSelection: Json =
    json"""{ "catalogName": $GaiaName, "observationTarget": null }"""

  private def observationTargetSelection(targetId: Option[Target.Id]): Json =
    json"""{ "catalogName": null, "observationTarget": { "targetId": ${targetId.asJson} } }"""

  // Sets through the new mutation, checks its result, then reads the selection back.
  private def setAndCheck(
    oid:         Observation.Id,
    guideTarget: String,
    name:        Option[String],
    selection:   Json
  ): IO[Unit] =
    val targetEnvironment: Json = targetEnvironmentJson(name, selection)
    for
      _ <- expect(pi, setMutation(oid, guideTarget), json"""{ "setGuideTarget": { "observation": $targetEnvironment } }""".asRight)
      _ <- expectSelection(oid, name, selection)
    yield ()

  private def expectSelection(oid: Observation.Id, name: Option[String], selection: Json): IO[Unit] =
    expect(pi, selectionQuery(oid), json"""{ "observation": ${targetEnvironmentJson(name, selection)} }""".asRight)

  private def gmosObservation: IO[(Target.Id, Observation.Id)] =
    for
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- setObservationTimeAndDuration(pi, oid, Now.some, none)
    yield (tid, oid)

  test("starts as automatic selection"):
    gmosObservation.flatMap: (_, oid) =>
      expectSelection(oid, none, Json.Null)

  test("select a catalog star by name"):
    gmosObservation.flatMap: (_, oid) =>
      setAndCheck(oid, s"""{ catalogName: "$GaiaName" }""", GaiaName.some, CatalogSelection)

  test("select the observation target without an id"):
    gmosObservation.flatMap: (_, oid) =>
      setAndCheck(oid, "{ observationTarget: {} }", none, observationTargetSelection(none))

  test("select an observation target by id"):
    gmosObservation.flatMap: (tid, oid) =>
      setAndCheck(oid, s"""{ observationTarget: { targetId: "$tid" } }""", none, observationTargetSelection(tid.some))

  test("null returns to automatic selection"):
    for
      (_, oid) <- gmosObservation
      _        <- setAndCheck(oid, "{ observationTarget: {} }", none, observationTargetSelection(none))
      _        <- setAndCheck(oid, "null", none, Json.Null)
      _        <- setAndCheck(oid, s"""{ catalogName: "$GaiaName" }""", GaiaName.some, CatalogSelection)
      _        <- setAndCheck(oid, "null", none, Json.Null)
    yield ()

  test("a catalog star replaces an observation target, and the other way round"):
    for
      (tid, oid) <- gmosObservation
      _          <- setAndCheck(oid, s"""{ observationTarget: { targetId: "$tid" } }""", none, observationTargetSelection(tid.some))
      _          <- setAndCheck(oid, s"""{ catalogName: "$GaiaName" }""", GaiaName.some, CatalogSelection)
      _          <- setAndCheck(oid, "{ observationTarget: {} }", none, observationTargetSelection(none))
    yield ()

  test("both fields set is an error"):
    gmosObservation.flatMap: (_, oid) =>
      expect(
        pi,
        setMutation(oid, s"""{ catalogName: "$GaiaName", observationTarget: {} }"""),
        List(s"$OneOfError, but found 'catalogName', 'observationTarget'").asLeft
      )

  test("neither field set is an error"):
    gmosObservation.flatMap: (_, oid) =>
      expect(
        pi,
        setMutation(oid, "{}"),
        List(OneOfError).asLeft
      )

  private def invalidCatalogName(name: String): IO[Unit] =
    gmosObservation.flatMap: (_, oid) =>
      expect(
        pi,
        setMutation(oid, s"""{ catalogName: "$name" }"""),
        List(s"Argument 'input.guideTarget.catalogName' is invalid: Invalid guide target name '$name'").asLeft
      )

  test("a name that is not a Gaia DR3 name is an error"):
    List("abc", "Gaia DR4 1", "Gaia DR3 abc").traverse_(invalidCatalogName)

  test("a Gaia DR3 name whose source id overflows is an error"):
    invalidCatalogName("Gaia DR3 99999999999999999999")

  private val MissingTargetId: Target.Id = Target.Id.parse("t-ffffff").get

  private def invalidTarget(targetId: Target.Id): String =
    s"Target $targetId does not exist, is not visible, or is ineligible for the requested operation."

  test("an observation target that does not exist is rejected by the foreign key"):
    gmosObservation.flatMap: (_, oid) =>
      for
        _ <- expect(
               pi,
               setMutation(oid, s"""{ observationTarget: { targetId: "$MissingTargetId" } }"""),
               List(invalidTarget(MissingTargetId)).asLeft
             )
        _ <- expectSelection(oid, none, Json.Null)
      yield ()

  test("an observation target of another program is rejected"):
    for
      (_, oid)  <- gmosObservation
      otherPid  <- createProgramAs(pi)
      otherTid  <- createTargetWithProfileAs(pi, otherPid)
      _         <- expect(
                     pi,
                     setMutation(oid, s"""{ observationTarget: { targetId: "$otherTid" } }"""),
                     List(invalidTarget(otherTid)).asLeft
                   )
      _         <- expectSelection(oid, none, Json.Null)
    yield ()

  // Selected, it would block its own cleanup through the foreign key.
  test("a blind offset target is rejected, and the blind offset can still be disabled"):
    for
      pid               <- createProgramAs(pi)
      tid               <- createTargetWithProfileAs(pi, pid)
      (oid, blindTid)   <- createObservationWithBlindOffsetAs(pi, pid, "Blind Offset", tid)
      _                 <- expect(
                             pi,
                             setMutation(oid, s"""{ observationTarget: { targetId: "$blindTid" } }"""),
                             List(invalidTarget(blindTid)).asLeft
                           )
      _                 <- expectSelection(oid, none, Json.Null)
      _                 <- query(
                             user  = pi,
                             query = s"""
                               mutation {
                                 updateObservations(input: {
                                   WHERE: { id: { EQ: "$oid" } }
                                   SET: { targetEnvironment: { useBlindOffset: false } }
                                 }) {
                                   observations { id }
                                 }
                               }
                             """
                           )
    yield ()

  test("a target of the program that is not in the observation is accepted"):
    for
      pid       <- createProgramAs(pi)
      tid       <- createTargetWithProfileAs(pi, pid)
      spareTid  <- createTargetWithProfileAs(pi, pid)
      oid       <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _         <- setAndCheck(oid, s"""{ observationTarget: { targetId: "$spareTid" } }""", none, observationTargetSelection(spareTid.some))
    yield ()

  test("a name set with the deprecated mutation shows as a catalog selection"):
    for
      (_, oid) <- gmosObservation
      _        <- setGuideTargetName(pi, oid, GaiaName.some)
      _        <- expectSelection(oid, GaiaName.some, CatalogSelection)
    yield ()

  test("a name set with the deprecated mutation replaces an observation target"):
    for
      (tid, oid) <- gmosObservation
      _          <- setAndCheck(oid, s"""{ observationTarget: { targetId: "$tid" } }""", none, observationTargetSelection(tid.some))
      _          <- setGuideTargetName(pi, oid, GaiaName.some)
      _          <- expectSelection(oid, GaiaName.some, CatalogSelection)
      _          <- setGuideTargetName(pi, oid, none)
      _          <- expectSelection(oid, none, Json.Null)
    yield ()

  test("the deprecated mutation clears an observation target with null"):
    for
      (_, oid) <- gmosObservation
      _        <- setAndCheck(oid, "{ observationTarget: {} }", none, observationTargetSelection(none))
      _        <- setGuideTargetName(pi, oid, none)
      _        <- expectSelection(oid, none, Json.Null)
    yield ()
