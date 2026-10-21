// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.either.*
import cats.effect.IO
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.util.CalculationState
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.odb.data.OdbError

class updateObservations_DaytimeCalibrationSets extends OdbSuite with DatabaseOperations with DaytimeCalibrationOps:

  val pi    = TestUsers.Standard.pi(1, 101)
  val staff = TestUsers.Standard.staff(3, 103)

  override val validUsers = List(pi, staff)

  private def gmosNorthSetup(exposureSeconds: Int, roi: String = "FULL_FRAME"): String =
    s"""
      gmosNorth: {
        exposure: { seconds: $exposureSeconds }
        readout: { xBin: TWO, yBin: TWO, ampCount: TWELVE, ampGain: LOW, ampReadMode: SLOW }
        dtax: ZERO
        roi: $roi
      }
    """

  private val gmosSouthSetup: String =
    """
      gmosSouth: {
        exposure: { seconds: 1 }
        readout: { xBin: ONE, yBin: ONE, ampCount: TWELVE, ampGain: LOW, ampReadMode: SLOW }
        dtax: ZERO
        roi: FULL_FRAME
      }
    """

  private val flatGcal: String =
    "gcal: { continuum: QUARTZ_HALOGEN5, filter: ND10, shutter: OPEN }"

  private val arcGcal: String =
    "gcal: { arcs: [AR_ARC, CU_AR_ARC], filter: ND10, diffuser: IR, shutter: CLOSED }"

  private val bias: String =
    s"{ kind: BIAS, count: 5, ${gmosNorthSetup(10, "CCD2")} }"

  private val dark: String =
    s"{ kind: DARK, count: 2, ${gmosNorthSetup(60)} }"

  private val flat: String =
    s"{ kind: FLAT, count: 3, $flatGcal, ${gmosNorthSetup(15)} }"

  private val arc: String =
    s"{ kind: ARC, count: 1, $arcGcal, ${gmosNorthSetup(10)} }"

  private val setsSelection: String =
    """
      calibrationSets {
        kind
        count
        gcal { continuum arcs filter diffuser shutter }
        gmosNorth { exposure { microseconds } roi }
        gmosSouth { exposure { microseconds } roi }
      }
    """

  private def createQuery(pid: Program.Id, instrument: String, sets: String): String =
    s"""
      mutation {
        createObservation(input: {
          programId: ${pid.asJson}
          SET: {
            observingMode: {
              daytimeCalibration: { instrument: $instrument, calibrationSets: [$sets] }
            }
          }
        }) {
          observation { observingMode { daytimeCalibration { $setsSelection } } }
        }
      }
    """

  private def createWithSets(pid: Program.Id, sets: String): IO[Observation.Id] =
    query(
      user  = staff,
      query = s"""
        mutation {
          createObservation(input: {
            programId: ${pid.asJson}
            SET: {
              observingMode: {
                daytimeCalibration: { instrument: GMOS_NORTH, calibrationSets: [$sets] }
              }
            }
          }) {
            observation { id }
          }
        }
      """
    ).map(_.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id])

  private def updateQuery(oid: Observation.Id, daytimeCalibration: String): String =
    s"""
      mutation {
        updateObservations(input: {
          SET: { observingMode: { daytimeCalibration: $daytimeCalibration } }
          WHERE: { id: { EQ: ${oid.asJson} } }
        }) {
          observations { observingMode { daytimeCalibration { instrument $setsSelection } } }
        }
      }
    """

  private def setsQuery(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: ${oid.asJson}) {
          observingMode { daytimeCalibration { $setsSelection } }
        }
      }
    """

  private def north(seconds: Int, roi: String = "FULL_FRAME"): Json =
    json"""{ "exposure": { "microseconds": ${seconds * 1000000L} }, "roi": $roi }"""

  private def set(kind: String, count: Int, gcal: Json, north: Json): Json =
    json"""{ "kind": $kind, "count": $count, "gcal": $gcal, "gmosNorth": $north, "gmosSouth": null }"""

  private val flatGcalJson: Json =
    json"""{ "continuum": "QUARTZ_HALOGEN5", "arcs": [], "filter": "ND10", "diffuser": "VISIBLE", "shutter": "OPEN" }"""

  private val arcGcalJson: Json =
    json"""{ "continuum": null, "arcs": ["AR_ARC", "CU_AR_ARC"], "filter": "ND10", "diffuser": "IR", "shutter": "CLOSED" }"""

  test("sets are stored in order, bias exposure zeroed, diffuser defaulted"):
    for
      pid <- createProgramAs(staff)
      _   <- expect(
        user     = staff,
        query    = createQuery(pid, "GMOS_NORTH", List(flat, arc, dark, bias).mkString(", ")),
        expected = json"""
          {
            "createObservation": {
              "observation": {
                "observingMode": {
                  "daytimeCalibration": {
                    "calibrationSets": [
                      ${set("FLAT", 3, flatGcalJson, north(15))},
                      ${set("ARC", 1, arcGcalJson, north(10))},
                      ${set("DARK", 2, Json.Null, north(60))},
                      ${set("BIAS", 5, Json.Null, north(0, "CCD2"))}
                    ]
                  }
                }
              }
            }
          }
        """.asRight
      ).void
    yield ()

  test("an edit replaces the whole list"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, s"$flat, $dark")
      _   <- expect(
        user     = staff,
        query    = updateQuery(oid, s"{ calibrationSets: [$bias, $flat] }"),
        expected = json"""
          {
            "updateObservations": {
              "observations": [
                {
                  "observingMode": {
                    "daytimeCalibration": {
                      "instrument": "GMOS_NORTH",
                      "calibrationSets": [
                        ${set("BIAS", 5, Json.Null, north(0, "CCD2"))},
                        ${set("FLAT", 3, flatGcalJson, north(15))}
                      ]
                    }
                  }
                }
              ]
            }
          }
        """.asRight
      )
    yield ()

  test("omitting the list on an edit keeps the rows"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, dark)
      _   <- query(staff, updateQuery(oid, "{ instrument: GMOS_NORTH }"))
      _   <- expect(
        user     = staff,
        query    = setsQuery(oid),
        expected = json"""
          {
            "observation": {
              "observingMode": {
                "daytimeCalibration": {
                  "calibrationSets": [ ${set("DARK", 2, Json.Null, north(60))} ]
                }
              }
            }
          }
        """.asRight
      )
    yield ()

  private def expectRejected(pid: Program.Id, sets: String, fragment: String) =
    expectOdbError(
      user     = staff,
      query    = createQuery(pid, "GMOS_NORTH", sets),
      expected = { case OdbError.InvalidArgument(Some(m)) if m.contains(fragment) => () }
    )

  test("a flat needs a gcal with a continuum lamp"):
    for
      pid <- createProgramAs(staff)
      _   <- expectRejected(pid, s"{ kind: FLAT, count: 1, ${gmosNorthSetup(1)} }", "needs a `gcal`")
      _   <- expectRejected(pid, s"{ kind: FLAT, count: 1, $arcGcal, ${gmosNorthSetup(1)} }", "continuum lamp and no arc")
    yield ()

  test("an arc needs a gcal with arc lamps"):
    for
      pid <- createProgramAs(staff)
      _   <- expectRejected(pid, s"{ kind: ARC, count: 1, $flatGcal, ${gmosNorthSetup(1)} }", "at least one arc lamp")
    yield ()

  test("a bias, dark or daytime observe takes no gcal"):
    for
      pid <- createProgramAs(staff)
      _   <- expectRejected(pid, s"{ kind: BIAS, count: 1, $flatGcal, ${gmosNorthSetup(1)} }", "takes no `gcal`")
      _   <- expectRejected(pid, s"{ kind: DAYTIME_OBSERVE, count: 1, $flatGcal, ${gmosNorthSetup(1)} }", "takes no `gcal`")
    yield ()

  test("exactly one setup, for the mode's instrument"):
    for
      pid <- createProgramAs(staff)
      _   <- expectRejected(pid, "{ kind: DARK, count: 1 }", "Exactly one instrument setup")
      _   <- expectRejected(pid, s"{ kind: DARK, count: 1, ${gmosNorthSetup(1)}, $gmosSouthSetup }", "Exactly one instrument setup")
      _   <- expectRejected(pid, s"{ kind: DARK, count: 1, $gmosSouthSetup }", "is for GMOS South but")
    yield ()

  test("the instrument may not change under rows for another instrument"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, dark)
      _   <- expectOdbError(
        user     = staff,
        query    = updateQuery(oid, "{ instrument: GMOS_SOUTH }"),
        expected = { case OdbError.InvalidArgument(Some(m)) if m.contains("is for GMOS North but") => () }
      )
      _   <- expect(
        user     = staff,
        query    = updateQuery(oid, s"{ instrument: GMOS_SOUTH, calibrationSets: [{ kind: DARK, count: 1, $gmosSouthSetup }] }"),
        expected = json"""
          {
            "updateObservations": {
              "observations": [
                {
                  "observingMode": {
                    "daytimeCalibration": {
                      "instrument": "GMOS_SOUTH",
                      "calibrationSets": [
                        { "kind": "DARK", "count": 1, "gcal": null, "gmosNorth": null, "gmosSouth": { "exposure": { "microseconds": 1000000 }, "roi": "FULL_FRAME" } }
                      ]
                    }
                  }
                }
              ]
            }
          }
        """.asRight
      )
    yield ()

  test("rows are frozen once a sequence is stored"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, dark)
      _   <- query(staff, s"""
               mutation {
                 replaceGmosNorthSequence(input: ${input(oid, SequenceType.Science, atomInput("Darks", gmosNorthDarkStep()))}) {
                   sequence { id }
                 }
               }
             """)
      _   <- expectOdbError(
        user     = staff,
        query    = updateQuery(oid, s"{ calibrationSets: [$bias] }"),
        expected = { case OdbError.InvalidArgument(Some(m)) if m.contains("Delete the sequence first") => () }
      )
    yield ()

  test("a PI may not set calibration sets"):
    for
      pid <- createProgramAs(pi)
      _   <- expectOdbError(
        user     = pi,
        query    = createQuery(pid, "GMOS_NORTH", dark),
        expected = { case OdbError.NotAuthorized(_, _) => () }
      )
    yield ()

  test("rows are cloned with the observation"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, s"$flat, $bias")
      oid2 <- cloneObservationAs(staff, oid)
      _   <- expect(
        user     = staff,
        query    = setsQuery(oid2),
        expected = json"""
          {
            "observation": {
              "observingMode": {
                "daytimeCalibration": {
                  "calibrationSets": [
                    ${set("FLAT", 3, flatGcalJson, north(15))},
                    ${set("BIAS", 5, Json.Null, north(0, "CCD2"))}
                  ]
                }
              }
            }
          }
        """.asRight
      )
    yield ()

  test("an explicit null instrument is rejected on an edit"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, dark)
      _   <- expectOdbError(
        user     = staff,
        query    = updateQuery(oid, "{ instrument: null }"),
        expected = { case OdbError.InvalidArgument(_) => () }
      )
    yield ()

  test("a GMOS South calibration with a daytime observe row"):
    for
      pid <- createProgramAs(staff)
      _   <- expect(
        user     = staff,
        query    = createQuery(pid, "GMOS_SOUTH", s"{ kind: DAYTIME_OBSERVE, count: 2, $gmosSouthSetup }"),
        expected = json"""
          {
            "createObservation": {
              "observation": {
                "observingMode": {
                  "daytimeCalibration": {
                    "calibrationSets": [
                      { "kind": "DAYTIME_OBSERVE", "count": 2, "gcal": null, "gmosNorth": null, "gmosSouth": { "exposure": { "microseconds": 1000000 }, "roi": "FULL_FRAME" } }
                    ]
                  }
                }
              }
            }
          }
        """.asRight
      )
    yield ()

  test("rows are editable again after deleteSequence"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, dark)
      _   <- query(staff, s"""
               mutation {
                 replaceGmosNorthSequence(input: ${input(oid, SequenceType.Science, atomInput("Darks", gmosNorthDarkStep()))}) {
                   sequence { id }
                 }
               }
             """)
      _   <- query(staff, s"mutation { deleteSequence(input: { observationId: ${oid.asJson} }) { observation { id } } }")
      _   <- expect(
        user     = staff,
        query    = updateQuery(oid, s"{ calibrationSets: [$bias] }"),
        expected = json"""
          {
            "updateObservations": {
              "observations": [
                {
                  "observingMode": {
                    "daytimeCalibration": {
                      "instrument": "GMOS_NORTH",
                      "calibrationSets": [ ${set("BIAS", 5, Json.Null, north(0, "CCD2"))} ]
                    }
                  }
                }
              ]
            }
          }
        """.asRight
      )
    yield ()

  test("changing the rows invalidates obscalc"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, dark)
      _   <- setObscalcState(oid, CalculationState.Ready)
      _   <- query(staff, updateQuery(oid, s"{ calibrationSets: [$bias] }"))
      row <- obscalcRow(oid)
    yield assertEquals(row._1, CalculationState.Pending)
