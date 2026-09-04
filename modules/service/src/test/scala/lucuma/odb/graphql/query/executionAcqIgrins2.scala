// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.StepGuideState.Disabled
import lucuma.core.enums.StepGuideState.Enabled
import lucuma.core.math.Angle
import lucuma.core.model.Observation
import lucuma.core.model.sequence.igrins2.SvcDefaultExposure
import lucuma.core.model.sequence.igrins2.SvcDefaultTelescopeConfigs
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.sequence.igrins2.longslit.Acquisition as Igrins2Acquisition

/**
 * The IGRINS-2 SVC (Slit-Viewing Camera) acquisition sequence.
 */
class executionAcqIgrins2 extends ExecutionTestSupportForIgrins2:
  val ExposureTime: TimeSpan = 20.secondTimeSpan

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(ExposureTime, PosInt.unsafeFrom(4))

  val qA = -1.25
  val qB =  1.25

  private def setup: IO[Observation.Id] =
    for {
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createIgrins2LongSlitObservationAs(pi, p, t)
    } yield o

  test("[igrins2] no SVC configuration implies empty acquisition"):
    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = igrins2AcquisitionQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "igrins2" -> Json.obj(
                "acquisition" -> Json.Null
              )
            )
          ).asRight
      ) *> expect(
        user     = pi,
        query    = igrins2ScienceQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "igrins2" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom" -> igrins2ExpectedScienceAtom(ExposureTime,
                    (0, qA, Enabled), (0, qB, Enabled), (0, qB, Enabled), (0, qA, Enabled)
                  ),
                  "possibleFuture" -> List.empty[Json].asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      )

  test("[igrins2] SVC at defaults produces an uninterrupted atom at the default offsets, plus repetitions"):
    (for
      oid <- setup
      _   <- enableIgrins2Svc(oid)
    yield oid).flatMap: oid =>
      val defaultOffsets = SvcDefaultTelescopeConfigs.toList.map: tc =>
        (Angle.signedDecimalArcseconds.get(tc.offset.p.toAngle), Angle.signedDecimalArcseconds.get(tc.offset.q.toAngle), tc.guiding)

      expect(
        user     = pi,
        query    = igrins2AcquisitionQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "igrins2" -> Json.obj(
                "acquisition" -> Json.obj(
                  "nextAtom" -> igrins2ExpectedAcquisitionAtom(SvcDefaultExposure, defaultOffsets*),
                  "possibleFuture" -> igrins2ExpectedAcquisitionRepeats(SvcDefaultExposure, defaultOffsets.last).asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      ) *> expect(
        user     = pi,
        query    = igrins2ScienceQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "igrins2" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom" -> igrins2ExpectedScienceAtom(ExposureTime,
                    (0, qA, Enabled), (0, qB, Enabled), (0, qB, Enabled), (0, qA, Enabled)
                  ),
                  "possibleFuture" -> List.empty[Json].asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      )

  test("[igrins2] SVC with an explicit exposure"):
    (for
      oid <- setup
      _   <- enableIgrins2Svc(oid, explicitExposureSeconds = Some(BigDecimal(10)))
    yield oid).flatMap: oid =>
      val defaultOffsets = SvcDefaultTelescopeConfigs.toList.map: tc =>
        (Angle.signedDecimalArcseconds.get(tc.offset.p.toAngle), Angle.signedDecimalArcseconds.get(tc.offset.q.toAngle), tc.guiding)

      expect(
        user     = pi,
        query    = igrins2AcquisitionQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "igrins2" -> Json.obj(
                "acquisition" -> Json.obj(
                  "nextAtom" -> igrins2ExpectedAcquisitionAtom(10.secondTimeSpan, defaultOffsets*),
                  "possibleFuture" -> igrins2ExpectedAcquisitionRepeats(10.secondTimeSpan, defaultOffsets.last).asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      )

  test("[igrins2] every SVC acquisition atom has a distinct id"):
    (for
      oid <- setup
      _   <- enableIgrins2Svc(oid)
    yield oid).flatMap: oid =>
      query(
        pi,
        s"""
          query {
            executionConfig(observationId: "$oid") {
              igrins2 {
                acquisition {
                  nextAtom { id }
                  possibleFuture { id }
                }
              }
            }
          }
        """
      ).map: c =>
        val acq    = c.hcursor.downFields("executionConfig", "igrins2", "acquisition")
        val next   = acq.downFields("nextAtom", "id").as[String].toOption.toList
        val future = acq.downField("possibleFuture").values.toList.flatten.flatMap: j =>
          j.hcursor.downField("id").as[String].toOption

        assertEquals(future.size, Igrins2Acquisition.RepeatingAtomCount)
        val ids = next ++ future
        assertEquals(ids.distinct.size, ids.size)

  test("[igrins2] SVC with an explicit set of offsets"):
    val explicitConfigs =
      """[
        { offset: { p: { arcseconds: 0 }, q: { arcseconds: 0 } }, guiding: ENABLED  },
        { offset: { p: { arcseconds: 3 }, q: { arcseconds: 0 } }, guiding: DISABLED },
        { offset: { p: { arcseconds: 6 }, q: { arcseconds: 0 } }, guiding: ENABLED  }
      ]"""

    (for
      oid <- setup
      _   <- enableIgrins2Svc(oid, explicitTelescopeConfigs = Some(explicitConfigs))
    yield oid).flatMap: oid =>
      expect(
        user     = pi,
        query    = igrins2AcquisitionQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "igrins2" -> Json.obj(
                "acquisition" -> Json.obj(
                  "nextAtom" -> igrins2ExpectedAcquisitionAtom(SvcDefaultExposure,
                    (0, 0, Enabled), (3, 0, Disabled), (6, 0, Enabled)
                  ),
                  "possibleFuture" -> igrins2ExpectedAcquisitionRepeats(SvcDefaultExposure,
                    (6, 0, Enabled)
                  ).asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      ) *> expect(
        user     = pi,
        query    = igrins2ScienceQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "igrins2" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom" -> igrins2ExpectedScienceAtom(ExposureTime,
                    (0, qA, Enabled), (0, qB, Enabled), (0, qB, Enabled), (0, qA, Enabled)
                  ),
                  "possibleFuture" -> List.empty[Json].asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      )
