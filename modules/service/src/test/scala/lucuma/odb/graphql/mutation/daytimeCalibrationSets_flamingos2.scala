// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.foldable.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.odb.data.OdbError

class daytimeCalibrationSets_flamingos2 extends OdbSuite with DatabaseOperations with DaytimeCalibrationOps:

  val staff       = TestUsers.Standard.staff(3, 103)
  val serviceUser = TestUsers.service(4)

  override val validUsers = List(staff, serviceUser)

  private def setup(
    exposureSeconds: Int,
    filter:          String         = "J",
    windowCover:     Option[String] = None,
    slit:            Boolean        = false
  ): String =
    s"""
      flamingos2: {
        exposure: { seconds: $exposureSeconds }
        filter: $filter
        readMode: BRIGHT
        lyotWheel: F16
        decker: LONG_SLIT
        readoutMode: SCIENCE
        reads: READS_1
        ${windowCover.foldMap(w => s"windowCover: $w")}
        ${if slit then "disperser: R1200_JH, fpu: { builtin: LONG_SLIT_2 }" else ""}
      }
    """

  private val flatGcal: String =
    "gcal: { continuum: QUARTZ_HALOGEN5, filter: ND10, shutter: OPEN }"

  private val arcGcal: String =
    "gcal: { arcs: [AR_ARC], filter: ND10, shutter: CLOSED }"

  private val mockupSets: String =
    List(
      s"{ kind: DARK, count: 10, ${setup(60, windowCover = "CLOSE".some)} }",
      s"{ kind: FLAT, count: 3, $flatGcal, ${setup(15, slit = true)} }",
      s"{ kind: ARC, count: 2, $arcGcal, ${setup(10, slit = true)} }"
    ).mkString(", ")

  private def createQuery(pid: Program.Id, sets: String): String =
    s"""
      mutation {
        createObservation(input: {
          programId: ${pid.asJson}
          SET: {
            observingMode: {
              daytimeCalibration: { instrument: FLAMINGOS2, calibrationSets: [$sets] }
            }
          }
        }) {
          observation { id }
        }
      }
    """

  private def createWithSets(pid: Program.Id, sets: String): IO[Observation.Id] =
    query(staff, createQuery(pid, sets))
      .map(_.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id])

  private def expectRejected(pid: Program.Id, sets: String, fragment: String): IO[Unit] =
    expectOdbError(
      user     = staff,
      query    = createQuery(pid, sets),
      expected = { case OdbError.InvalidArgument(Some(m)) if m.contains(fragment) => () }
    )

  private val atomSelection: String =
    """
      description
      steps {
        observeClass
        stepConfig { stepType }
        telescopeConfig { guiding }
        instrumentConfig { exposure { seconds } filter windowCover fpu { builtin } }
      }
    """

  private def executionQuery(oid: Observation.Id): String =
    s"""
      query {
        executionConfig(observationId: ${oid.asJson}) {
          flamingos2 {
            science {
              nextAtom { $atomSelection }
              possibleFuture { $atomSelection }
              hasMore
            }
          }
        }
      }
    """

  private def atom(kind: String, count: Int, stepType: String, seconds: Int, windowCover: Json, fpu: Json): Json =
    val step = json"""
      {
        "observeClass": "DAY_CAL",
        "stepConfig": { "stepType": $stepType },
        "telescopeConfig": { "guiding": "DISABLED" },
        "instrumentConfig": {
          "exposure": { "seconds": ${BigDecimal(seconds).setScale(6)} },
          "filter": "J",
          "windowCover": $windowCover,
          "fpu": $fpu
        }
      }
    """
    json"""{ "description": ${s"$kind x$count"}, "steps": ${List.fill(count)(step)} }"""

  private val longSlit2: Json =
    json"""{ "builtin": "LONG_SLIT_2" }"""

  private val mockupAtoms: List[Json] =
    List(
      atom("Dark", 10, "DARK", 60, "CLOSE".asJson, Json.Null),
      atom("Flat", 3,  "GCAL", 15, Json.Null, longSlit2),
      atom("Arc",  2,  "GCAL", 10, Json.Null, longSlit2)
    )

  private def expectedExecution(atoms: List[Json]): Json =
    json"""
      {
        "executionConfig": {
          "flamingos2": {
            "science": {
              "nextAtom": ${atoms.head},
              "possibleFuture": ${atoms.tail},
              "hasMore": false
            }
          }
        }
      }
    """

  test("one atom per set, count steps each, in row order"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, mockupSets)
      _   <- expect(staff, executionQuery(oid), expectedExecution(mockupAtoms).asRight)
    yield ()

  test("sets round trip through the observing mode, window cover included"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, mockupSets)
      _   <- expect(
        user     = staff,
        query    = s"""
          query {
            observation(observationId: ${oid.asJson}) {
              observingMode {
                daytimeCalibration {
                  instrument
                  calibrationSets {
                    kind
                    count
                    gmosNorth { roi }
                    flamingos2 { exposure { seconds } filter windowCover disperser fpu { builtin } centralWavelength { nanometers } }
                  }
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "observation": {
              "observingMode": {
                "daytimeCalibration": {
                  "instrument": "FLAMINGOS2",
                  "calibrationSets": [
                    { "kind": "DARK", "count": 10, "gmosNorth": null, "flamingos2": { "exposure": { "seconds": 60.000000 }, "filter": "J", "windowCover": "CLOSE", "disperser": null, "fpu": null, "centralWavelength": { "nanometers": 1250.000 } } },
                    { "kind": "FLAT", "count": 3,  "gmosNorth": null, "flamingos2": { "exposure": { "seconds": 15.000000 }, "filter": "J", "windowCover": null, "disperser": "R1200_JH", "fpu": { "builtin": "LONG_SLIT_2" }, "centralWavelength": { "nanometers": 1250.000 } } },
                    { "kind": "ARC",  "count": 2,  "gmosNorth": null, "flamingos2": { "exposure": { "seconds": 10.000000 }, "filter": "J", "windowCover": null, "disperser": "R1200_JH", "fpu": { "builtin": "LONG_SLIT_2" }, "centralWavelength": { "nanometers": 1250.000 } } }
                  ]
                }
              }
            }
          }
        """.asRight
      )
    yield ()

  test("a dark without a window cover stays unset"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, s"{ kind: DARK, count: 1, ${setup(5)} }")
      _   <- expect(
        staff,
        executionQuery(oid),
        expectedExecution(List(atom("Dark", 1, "DARK", 5, Json.Null, Json.Null))).asRight
      )
    yield ()

  test("a bias or daytime observe is rejected"):
    for
      pid <- createProgramAs(staff)
      _   <- expectRejected(pid, s"{ kind: BIAS, count: 1, ${setup(0)} }", "must be one of DARK, FLAT, ARC, not BIAS")
      _   <- expectRejected(pid, s"{ kind: DAYTIME_OBSERVE, count: 1, ${setup(1)} }", "must be one of DARK, FLAT, ARC, not DAYTIME_OBSERVE")
    yield ()

  test("the setup must be for the mode's instrument"):
    for
      pid <- createProgramAs(staff)
      _   <- expectRejected(
        pid,
        "{ kind: DARK, count: 1, gmosNorth: { exposure: { seconds: 1 }, readout: { xBin: ONE, yBin: ONE, ampCount: TWELVE, ampGain: LOW, ampReadMode: SLOW }, dtax: ZERO, roi: FULL_FRAME } }",
        "is for GMOS North but"
      )
    yield ()

  test("defined with sets, three atoms in the digest"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, mockupSets)
      _   <- runObscalcUpdateAs(serviceUser, pid, oid)
      _   <- expect(
        user     = staff,
        query    = s"""
          query {
            observation(observationId: ${oid.asJson}) {
              workflow { value { state validationErrors { messages } } }
              execution { digest { value { science { atomCount observeClass } } } }
            }
          }
        """,
        expected = json"""
          {
            "observation": {
              "workflow": { "value": { "state": "DEFINED", "validationErrors": [] } },
              "execution": { "digest": { "value": { "science": { "atomCount": 3, "observeClass": "DAY_CAL" } } } }
            }
          }
        """.asRight
      )
    yield ()

  test("no sets: empty sequence and Undefined"):
    for
      pid <- createProgramAs(staff)
      oid <- createDaytimeCalibrationAs(staff, pid, Instrument.Flamingos2)
      _   <- expect(
        user     = staff,
        query    = executionQuery(oid),
        expected = json"""{ "executionConfig": { "flamingos2": { "science": null } } }""".asRight
      )
      _   <- runObscalcUpdateAs(serviceUser, pid, oid)
      _   <- expect(
        user     = staff,
        query    = workflowQuery(oid),
        expected = json"""
          {
            "observation": {
              "workflow": {
                "value": {
                  "state": "UNDEFINED",
                  "validationErrors": [ { "messages": [ "No sequence has been defined." ] } ]
                }
              }
            }
          }
        """.asRight
      )
    yield ()

  test("a hand-entered sequence takes precedence over the sets"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, mockupSets)
      _   <- query(staff, s"""
               mutation {
                 replaceFlamingos2Sequence(input: ${input(oid, SequenceType.Science, atomInput("Darks", flamingos2DarkStep))}) {
                   sequence { id }
                 }
               }
             """)
      _   <- expect(
        staff,
        executionQuery(oid),
        expectedExecution(List(json"""
          {
            "description": "Darks",
            "steps": [
              {
                "observeClass": "DAY_CAL",
                "stepConfig": { "stepType": "DARK" },
                "telescopeConfig": { "guiding": "DISABLED" },
                "instrumentConfig": { "exposure": { "seconds": 20.000000 }, "filter": "J", "windowCover": null, "fpu": null }
              }
            ]
          }
        """)).asRight
      )
    yield ()

  test("the first visit materializes the sets and freezes them"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, mockupSets)
      _   <- recordVisitAs(serviceUser, oid)
      _   <- expect(staff, executionQuery(oid), expectedExecution(mockupAtoms).asRight)
      _   <- expectOdbError(
        user     = staff,
        query    = s"""
          mutation {
            updateObservations(input: {
              SET: { observingMode: { daytimeCalibration: { calibrationSets: [] } } }
              WHERE: { id: { EQ: ${oid.asJson} } }
            }) {
              observations { id }
            }
          }
        """,
        expected = { case OdbError.InvalidArgument(Some(m)) if m.contains("Delete the sequence first") => () }
      )
    yield ()

  test("materializing keeps a dark's window cover unset"):
    val atoms = List(atom("Dark", 1, "DARK", 5, Json.Null, Json.Null))
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, s"{ kind: DARK, count: 1, ${setup(5)} }")
      _   <- recordVisitAs(serviceUser, oid)
      _   <- expect(staff, executionQuery(oid), expectedExecution(atoms).asRight)
    yield ()

  test("rows are cloned with the observation"):
    for
      pid <- createProgramAs(staff)
      oid <- createWithSets(pid, mockupSets)
      cid <- cloneObservationAs(staff, oid)
      _   <- expect(staff, executionQuery(cid), expectedExecution(mockupAtoms).asRight)
    yield ()
