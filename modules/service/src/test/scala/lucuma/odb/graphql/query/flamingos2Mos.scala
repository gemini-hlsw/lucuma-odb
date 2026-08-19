// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.data.NonEmptySet
import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import fs2.Stream
import fs2.text.utf8
import io.circe.literal.*
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.GcalArc
import lucuma.core.enums.GcalBaselineType
import lucuma.core.enums.GcalContinuum
import lucuma.core.enums.GcalDiffuser
import lucuma.core.enums.GcalFilter
import lucuma.core.enums.GcalShutter
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.odb.service.Services
import lucuma.odb.smartgcal.data.Flamingos2
import lucuma.odb.smartgcal.data.SmartGcalValue
import lucuma.odb.smartgcal.data.SmartGcalValue.LegacyInstrumentConfig
import org.http4s.Request
import org.http4s.Response
import skunk.Session

/**
 * End-to-end checks that a Flamingos 2 MOS observation can be planned: the ITC
 * returns a result, the science sequence generates, and AGS finds a guide star.
 * Detailed guide star expectations live in `guideEnvironmentF2`.
 */
class flamingos2Mos extends OdbSuite with ObservingModeSetupOperations:

  val user: User = TestUsers.service(3)

  override val validUsers: List[User] = List(user)

  // AGS must not reach the real Gaia catalog, which resets connections often enough to
  // fail this suite. Serve the same canned candidates the guide environment suites use.
  override protected def httpRequestHandler: Request[IO] => Resource[IO, Response[IO]] =
    _ =>
      Resource.eval:
        IO.pure(Response(body = Stream(GaiaVoTables.multipleCandidates).through(utf8.encode)))

  private val ObsTime: Timestamp =
    Timestamp.FromString.getOption("2025-02-01T00:00:00Z").get

  private val ObsDuration: TimeSpan =
    1.hourTimeSpan

  // The MOS observation is calibrated as its equivalent long slit, so the smart gcal
  // rows are keyed on the builtin FPU that matches the custom mask's slit width.
  override def dbInitialization: Option[Session[IO] => IO[Unit]] = Some: s =>
    val key =
      Flamingos2.TableKey(
        Flamingos2Disperser.R1200JH.some,
        Flamingos2Filter.JH,
        Flamingos2Fpu.LongSlit2.some
      )

    def value(lamp: Gcal.Lamp, filter: GcalFilter, shutter: GcalShutter): SmartGcalValue.Legacy =
      SmartGcalValue(
        Gcal(lamp, filter, GcalDiffuser.Ir, shutter),
        GcalBaselineType.Night,
        PosInt.unsafeFrom(1),
        LegacyInstrumentConfig(TimeSpan.unsafeFromMicroseconds(15_000_000L))
      )

    val rows: List[Flamingos2.TableRow] =
      List(
        Flamingos2.TableRow(PosLong.unsafeFrom(1), key, value(Gcal.Lamp.fromContinuum(GcalContinuum.IrGreyBodyHigh), GcalFilter.Nd20, GcalShutter.Open)),
        Flamingos2.TableRow(PosLong.unsafeFrom(1), key, value(Gcal.Lamp.fromArcs(NonEmptySet.one(GcalArc.ArArc)), GcalFilter.Nir, GcalShutter.Closed))
      )

    servicesFor(user).map(_(s)).use: services =>
      services.transactionally:
        rows.zipWithIndex.traverse_ : (r, i) =>
          Services.asSuperUser:
            services.smartGcalService.insertFlamingos2(i, r)

  private def setup: IO[(Program.Id, Observation.Id, Target.Id)] =
    for
      p <- createProgramAs(user, "Flamingos 2 MOS Testing")
      t <- createTargetWithProfileAs(user, p)
      o <- createFlamingos2MosObservationAs(user, p, List(t))
    yield (p, o, t)

  test("the ITC returns a science and an acquisition result"):
    setup.flatMap: (_, oid, tid) =>
      expect(
        user  = user,
        query = s"""
          query {
            observation(observationId: "$oid") {
              itc {
                ... on ItcSpectroscopy {
                  itcType
                  spectroscopyScience {
                    selected {
                      targetId
                      exposureTime { seconds }
                      exposureCount
                    }
                  }
                  acquisition {
                    selected {
                      targetId
                      exposureTime { seconds }
                      exposureCount
                    }
                  }
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "observation": {
              "itc": {
                "itcType": "SPECTROSCOPY",
                "spectroscopyScience": {
                  "selected": {
                    "targetId": $tid,
                    "exposureTime": { "seconds": 10.000000 },
                    "exposureCount": 6
                  }
                },
                "acquisition": {
                  "selected": {
                    "targetId": $tid,
                    "exposureTime": { "seconds": 10.000000 },
                    "exposureCount": 6
                  }
                }
              }
            }
          }
        """.asRight
      )

  test("the mode type is accepted as a query filter"):
    for
      (pid, oid, _) <- setup
      oids          <- observationsWhere(user, s"""program: { id: { EQ: "$pid" } }, observingModeType: { EQ: FLAMINGOS_2_MOS }""")
    yield assertEquals(oids, List(oid))

  test("the sequence is generated"):
    setup.flatMap: (_, oid, _) =>
      expect(
        user     = user,
        query    = s"""
          query {
            executionConfig(observationId: "$oid") {
              instrument
            }
          }
        """,
        expected = json"""
          {
            "executionConfig": {
              "instrument": "FLAMINGOS2"
            }
          }
        """.asRight
      )

  test("the guide environment resolves"):
    setup.flatMap: (_, oid, _) =>
      setObservationTimeAndDuration(user, oid, ObsTime.some, ObsDuration.some) *>
      expect(
        user     = user,
        query    = s"""
          query {
            observation(observationId: "$oid") {
              targetEnvironment {
                guideEnvironment {
                  posAngle { degrees }
                  guideTargets { probe }
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "observation": {
              "targetEnvironment": {
                "guideEnvironment": {
                  "posAngle": { "degrees": 270.000000 },
                  "guideTargets": [
                    { "probe": "FLAMINGOS2_OIWFS" }
                  ]
                }
              }
            }
          }
        """.asRight
      )
