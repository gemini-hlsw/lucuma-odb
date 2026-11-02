// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.eq.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.data.OdbError

class staffOnlyFpusAndFilters extends OdbSuite:

  val pi:    User = TestUsers.Standard.pi(nextId, nextId)
  val staff: User = TestUsers.Standard.staff(nextId, nextId)
  override lazy val validUsers: List[User] = List(pi, staff)

  private def setup(mode: ObservingModeType): IO[(Program.Id, Target.Id, Observation.Id)] =
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      oid <- createObservationAs(pi, pid, mode.some, tid)
    yield (pid, tid, oid)

  private def updateMode(user: User, oid: Observation.Id, mode: String): IO[Json] =
    query(
      user  = user,
      query =
        s"""
          mutation {
            updateObservations(input: {
              SET: { observingMode: $mode }
              WHERE: { id: { EQ: "$oid" } }
            }) { observations { id } }
          }
        """
    )

  private def createWithMode(user: User, pid: Program.Id, tid: Target.Id, mode: String): IO[Json] =
    query(
      user  = user,
      query =
        s"""
          mutation {
            createObservation(input: {
              programId: "$pid"
              SET: {
                targetEnvironment: { asterism: [ "$tid" ] }
                observingMode: $mode
              }
            }) { observation { id } }
          }
        """
    )

  private val F2Pinhole: String =
    """{
      flamingos2LongSlit: {
        disperser: R1200_HK
        filter: Y
        fpu: PINHOLE
        exposureTimeMode: { signalToNoise: { value: 100.0 at: { nanometers: 2100 } } }
      }
    }"""

  private val GmosNorthEngineering: String =
    """{
      gmosNorthLongSlit: {
        grating: R831_G5302
        filter: HARTMANN_A_R_PRIME
        fpu: LONG_SLIT_0_50
        centralWavelength: { nanometers: 500 }
      }
    }"""

  private def expectNotAuthorized(io: IO[?]): IO[Unit] =
    interceptOdbError(io) {
      case OdbError.NotAuthorized(uid, _) if uid === pi.id => // expected
    }

  test("PI cannot create a Flamingos 2 long slit with a pinhole"):
    expectNotAuthorized:
      for
        pid <- createProgramAs(pi)
        tid <- createTargetAs(pi, pid)
        _   <- createWithMode(pi, pid, tid, F2Pinhole)
      yield ()

  test("Staff can create a Flamingos 2 long slit with a pinhole"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      _   <- createWithMode(staff, pid, tid, F2Pinhole)
    yield ()

  test("PI cannot select a sub-pixel pinhole on update"):
    expectNotAuthorized:
      setup(ObservingModeType.Flamingos2LongSlit).flatMap: (_, _, oid) =>
        updateMode(pi, oid, "{ flamingos2LongSlit: { fpu: SUB_PIX_PINHOLE } }")

  test("PI can edit other fields once staff selected a pinhole"):
    for
      (_, _, oid) <- setup(ObservingModeType.Flamingos2LongSlit)
      _           <- updateMode(staff, oid, "{ flamingos2LongSlit: { fpu: PINHOLE } }")
      _           <- expect(
        user  = pi,
        query =
          s"""
            mutation {
              updateObservations(input: {
                SET: { observingMode: { flamingos2LongSlit: { filter: J } } }
                WHERE: { id: { EQ: "$oid" } }
              }) {
                observations { observingMode { flamingos2LongSlit { fpu filter } } }
              }
            }
          """,
        expected = Right(json"""
          {
            "updateObservations": {
              "observations": [
                { "observingMode": { "flamingos2LongSlit": { "fpu": "PINHOLE", "filter": "J" } } }
              ]
            }
          }
        """)
      )
    yield ()

  test("PI cannot create a GMOS long slit with an engineering filter"):
    expectNotAuthorized:
      for
        pid <- createProgramAs(pi)
        tid <- createTargetAs(pi, pid)
        _   <- createWithMode(pi, pid, tid, GmosNorthEngineering)
      yield ()

  test("PI cannot select an engineering filter on GMOS MOS update"):
    expectNotAuthorized:
      setup(ObservingModeType.GmosSouthMos).flatMap: (_, _, oid) =>
        updateMode(pi, oid, "{ gmosSouthMos: { filter: HARTMANN_B_R_PRIME } }")

  test("PI cannot clone with an engineering filter"):
    expectNotAuthorized:
      setup(ObservingModeType.GmosNorthLongSlit).flatMap: (_, _, oid) =>
        query(
          user  = pi,
          query =
            s"""
              mutation {
                cloneObservation(input: {
                  observationId: "$oid"
                  SET: { observingMode: { gmosNorthLongSlit: { filter: HARTMANN_A_R_PRIME } } }
                }) { newObservation { id } }
              }
            """
        )

  test("Staff can select an engineering filter, PI can clear it"):
    for
      (_, _, oid) <- setup(ObservingModeType.GmosNorthLongSlit)
      _           <- updateMode(staff, oid, "{ gmosNorthLongSlit: { filter: HARTMANN_A_R_PRIME } }")
      _           <- expect(
        user  = pi,
        query =
          s"""
            mutation {
              updateObservations(input: {
                SET: { observingMode: { gmosNorthLongSlit: { filter: null } } }
                WHERE: { id: { EQ: "$oid" } }
              }) {
                observations { observingMode { gmosNorthLongSlit { filter } } }
              }
            }
          """,
        expected = Right(json"""
          {
            "updateObservations": {
              "observations": [
                { "observingMode": { "gmosNorthLongSlit": { "filter": null } } }
              ]
            }
          }
        """)
      )
    yield ()
