// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mutation

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.ags.GuideStarName
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.TargetDisposition
import lucuma.core.model.Observation
import lucuma.core.model.ObservationReference
import lucuma.core.model.Target
import lucuma.core.util.Timestamp
import lucuma.odb.data.OdbError

class setGuideTarget extends query.ExecutionTestSupportForGmos:

  val GaiaSourceId: Long = 1L
  val GaiaName: String   = GuideStarName.gaiaSourceId.reverseGet(GaiaSourceId).value.value

  val Now: Timestamp   = Timestamp.FromString.getOption("2024-08-25T00:00:00Z").get
  val Later: Timestamp = Now.plusSecondsOption(120L).get

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

  test("a calibration target is accepted, as automatic selection considers it"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetViaServiceAs(pi, pid, TargetDisposition.Calibration, CalibrationRole.Telluric.some)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- setAndCheck(oid, s"""{ observationTarget: { targetId: "$tid" } }""", none, observationTargetSelection(tid.some))
    yield ()

  test("a deleted science target is rejected"):
    for
      (tid, oid) <- gmosObservation
      _          <- deleteTargetAs(pi, tid)
      _          <- expect(
                      pi,
                      setMutation(oid, s"""{ observationTarget: { targetId: "$tid" } }"""),
                      List(invalidTarget(tid)).asLeft
                    )
      _          <- expectSelection(oid, none, Json.Null)
    yield ()

  // `guideTarget` is a GraphQL literal; none leaves the field out.
  private def setMutationFor(
    oid:         Option[Observation.Id],
    oref:        Option[ObservationReference],
    guideTarget: Option[String]
  ): String =
    s"""
      mutation {
        setGuideTarget(
          input: {
            observationId: ${oid.asJson}
            observationReference: ${oref.asJson}
            ${guideTarget.foldMap(gt => s"guideTarget: $gt")}
          }
        ) {
          observation {
            $TargetEnvironmentSelection
          }
        }
      }
    """

  private val CatalogInput: String = s"""{ catalogName: "$GaiaName" }"""

  private def setResult(name: Option[String], selection: Json): Either[List[String], Json] =
    json"""{ "setGuideTarget": { "observation": ${targetEnvironmentJson(name, selection)} } }""".asRight

  private val CatalogResult: Either[List[String], Json] =
    setResult(GaiaName.some, CatalogSelection)

  private def observationByReference: IO[(Observation.Id, ObservationReference)] =
    for
      pid <- createProgramAs(pi)
      ref <- setProgramReference(staff, pid, """calibration: { semester: "2025B", instrument: GMOS_NORTH }""")
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- setObservationTimeAndDuration(pi, oid, Now.some, none)
    yield (oid, ObservationReference(ref.get, PosInt.unsafeFrom(1)))

  test("missing target"):
    for
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      _   <- expect(
               staff,
               setMutationFor(oid.some, none, CatalogInput.some),
               List(s"Could not generate a sequence for $oid: observation is missing target").asLeft
             )
    yield ()

  test("missing observing mode"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createObservationAs(pi, pid, tid)
      _   <- expect(
               staff,
               setMutationFor(oid.some, none, CatalogInput.some),
               List(s"Could not generate a sequence for $oid: observation is missing observing mode").asLeft
             )
    yield ()

  test("can set without an observation time"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- expect(staff, setMutationFor(oid.some, none, CatalogInput.some), CatalogResult)
    yield ()

  test("valid set as staff"):
    gmosObservation.flatMap: (_, oid) =>
      expect(staff, setMutationFor(oid.some, none, CatalogInput.some), CatalogResult)

  test("user must have access to the program"):
    gmosObservation.flatMap: (_, oid) =>
      expect(
        pi2,
        setMutationFor(oid.some, none, CatalogInput.some),
        List(OdbError.InvalidArgument().message).asLeft
      )

  test("can specify observation by reference"):
    observationByReference.flatMap: (_, oref) =>
      expect(pi, setMutationFor(none, oref.some, CatalogInput.some), CatalogResult)

  test("can specify observation by both id and reference"):
    observationByReference.flatMap: (oid, oref) =>
      expect(pi, setMutationFor(oid.some, oref.some, CatalogInput.some), CatalogResult)

  test("observation id and reference must correspond"):
    for
      (oid1, oref) <- observationByReference
      pid          <- createProgramAs(pi)
      oid2         <- createObservationAs(pi, pid)
      _            <- expect(
                        pi,
                        setMutationFor(oid2.some, oref.some, CatalogInput.some),
                        List(s"Observation '${oref.label}' (id $oid1) does not correspond to observation id $oid2.").asLeft
                      )
    yield ()

  test("must specify either observation id or reference"):
    expect(
      pi,
      setMutationFor(none, none, none),
      List("One of observationId or observationReference must be provided.").asLeft
    )

  test("omission unsets"):
    gmosObservation.flatMap: (_, oid) =>
      for
        _ <- expect(staff, setMutationFor(oid.some, none, CatalogInput.some), CatalogResult)
        _ <- expect(pi, setMutationFor(oid.some, none, none), setResult(none, Json.Null))
      yield ()

  test("changing observation time keeps the selection"):
    gmosObservation.flatMap: (_, oid) =>
      for
        _ <- expect(staff, setMutationFor(oid.some, none, CatalogInput.some), CatalogResult)
        _ <- setObservationTimeAndDuration(pi, oid, Later.some, none)
        _ <- expectSelection(oid, GaiaName.some, CatalogSelection)
      yield ()

  test("can always unset"):
    gmosObservation.flatMap: (tid, oid) =>
      for
        _ <- expect(staff, setMutationFor(oid.some, none, CatalogInput.some), CatalogResult)
        _ <- updateAsterisms(pi, List(oid), List.empty, List(tid), List(oid -> List.empty))
        _ <- expect(staff, setMutationFor(oid.some, none, none), setResult(none, Json.Null))
      yield ()
