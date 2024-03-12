// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.odb.data.DatasetReference
import lucuma.odb.data.ObservationReference
import lucuma.odb.data.ObservingModeType

class recordDataset extends OdbSuite {

  val service: User = TestUsers.service(nextId)

  override lazy val validUsers: List[User] = List(service)

  private def setup(
    mode: ObservingModeType,
    user: User
  ): IO[(Program.Id, Observation.Id, Visit.Id, Atom.Id, Step.Id)] =
    for {
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, mode.some)
      vid <- recordVisitAs(user, mode.instrument, oid)
      aid <- recordAtomAs(user, mode.instrument, vid)
      sid <- recordStepAs(user, mode.instrument, aid)
    } yield (pid, oid, vid, aid, sid)

  private def recordDatasetTest(
    mode:     ObservingModeType,
    user:     User,
    query:    Step.Id => String,
    expected: (Observation.Id, Step.Id) => Either[String, Json]
  ): IO[Unit] =
    for {
      ids <- setup(mode, user)
      (_, oid, _, _, sid) = ids
      _   <- expect(user, query(sid), expected(oid, sid).leftMap(msg => List(msg)))
    } yield ()

  test("recordDataset") {
    recordDatasetTest(
      ObservingModeType.GmosNorthLongSlit,
      service,
      sid => s"""
        mutation {
          recordDataset(input: {
            stepId: ${sid.asJson},
            filename: "N18630101S0001.fits"
          }) {
            dataset {
              step {
                id
              }
              index
              observation {
                id
              }
              filename
              qaState
              interval { start }
            }
          }
        }
      """,
      (oid, sid) => json"""
        {
          "recordDataset": {
            "dataset": {
              "step": {
                "id": $sid
              },
              "index": 1,
              "observation": {
                "id": $oid
              },
              "filename": "N18630101S0001.fits",
              "qaState": null,
              "interval": null
            }
          }
        }
      """.asRight
    )
  }

  test("recordDataset - init QA state") {
    recordDatasetTest(
      ObservingModeType.GmosNorthLongSlit,
      service,
      sid => s"""
        mutation {
          recordDataset(input: {
            stepId: ${sid.asJson},
            filename: "N18630101S0002.fits",
            qaState: USABLE
          }) {
            dataset {
              step {
                id
              }
              index
              observation {
                id
              }
              filename
              qaState
              interval { start }
            }
          }
        }
      """,
      (oid, sid) => json"""
        {
          "recordDataset": {
            "dataset": {
              "step": {
                "id": $sid
              },
              "index": 1,
              "observation": {
                "id": $oid
              },
              "filename": "N18630101S0002.fits",
              "qaState": "USABLE",
              "interval": null
            }
          }
        }
      """.asRight
    )
  }

  test("recordDataset - reused filename") {
    recordDatasetTest(
      ObservingModeType.GmosNorthLongSlit,
      service,
      sid => s"""
        mutation {
          recordDataset(input: {
            stepId: ${sid.asJson},
            filename: "N18630101S0002.fits",
            qaState: USABLE
          }) {
            dataset {
              filename
            }
          }
        }
      """,
      (oid, sid) => "The filename 'N18630101S0002.fits' is already assigned".asLeft
    )
  }

  test("recordDataset - unkown step id") {
    recordDatasetTest(
      ObservingModeType.GmosNorthLongSlit,
      service,
      sid => s"""
        mutation {
          recordDataset(input: {
            stepId: "s-d506e5d9-e5d1-4fcc-964c-90afedabc9e8",
            filename: "N18630101S0003.fits"
          }) {
            dataset {
              filename
            }
          }
        }
      """,
      (oid, sid) => "Step id 's-d506e5d9-e5d1-4fcc-964c-90afedabc9e8' not found".asLeft
    )
  }

  private def recordDataset(sid: Step.Id, filename: String): IO[Option[DatasetReference]] =
    query(
      user  = service,
      query = s"""
        mutation {
          recordDataset(input: {
            stepId: "$sid"
            filename: "$filename"
          }) {
            dataset {
              reference { label }
            }
          }
        }
      """.stripMargin
    ).flatMap { js =>
      js.hcursor
        .downFields("recordDataset", "dataset", "reference", "label")
        .as[Option[DatasetReference]]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
    }

  test("recordDataset - reference") {
    for {
      ids   <- setup(ObservingModeType.GmosNorthLongSlit, service)
      (pid, oid, _, _, sid) = ids
      pRef  <- setProgramReference(service, pid, """calibration: { semester: "2025B", instrument: GMOS_NORTH }""")
      oRef   = ObservationReference(pRef.get, PosInt.unsafeFrom(1))
      dRef0 <- recordDataset(sid, "N18630101S0004.fits")
      dRef1 <- recordDataset(sid, "N18630101S0005.fits")
    } yield {
      assertEquals(dRef0, DatasetReference(oRef, PosInt.unsafeFrom(1), PosInt.unsafeFrom(1)).some)
      assertEquals(dRef1, DatasetReference(oRef, PosInt.unsafeFrom(1), PosInt.unsafeFrom(2)).some)
    }

  }
}
