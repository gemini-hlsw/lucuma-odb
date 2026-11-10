// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.itc.client.ImagingInput
import lucuma.itc.client.InstrumentMode
import lucuma.odb.sequence.flamingos2.AcquisitionClassificationSignalToNoise

/**
 * The automatic long slit acquisition type comes from an ITC call at the
 * classification signal-to-noise, whatever exposure time mode the acquisition
 * itself uses. Here that call answers with a Bright exposure while the
 * acquisition's own request takes longer. MOS is never classified.
 */
class executionAcqFlamingos2_Classification extends ExecutionTestSupportForFlamingos2:

  val ClassificationExposure: TimeSpan = 2.secTimeSpan
  val RequestedExposure: TimeSpan      = 8.secTimeSpan

  override def fakeItcImagingResultFor(input: ImagingInput): Option[IntegrationTime] =
    input.mode.exposureTimeMode match
      case ExposureTimeMode.TimeAndCountMode(t, c, _)                                                  =>
        IntegrationTime(t, c).some
      case ExposureTimeMode.SignalToNoiseMode(sn, _) if sn === AcquisitionClassificationSignalToNoise =>
        IntegrationTime(ClassificationExposure, classificationCount(input)).some
      case _                                                                                           =>
        IntegrationTime(RequestedExposure, PosInt.unsafeFrom(1)).some

  // A K-short acquisition needs two exposures to reach the classification S/N.
  private def classificationCount(input: ImagingInput): PosInt =
    input.mode match
      case InstrumentMode.Flamingos2Imaging(filter = Flamingos2Filter.KShort) => PosInt.unsafeFrom(2)
      case _                                                                  => PosInt.unsafeFrom(1)

  private def setup(mode: String): IO[Observation.Id] =
    for
      p <- createProgramWithNonPartnerPi(pi)
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), mode)
    yield o

  private def stepsQuery(oid: Observation.Id): String =
    s"""
      query {
        executionConfig(observationId: "$oid") {
          flamingos2 {
            acquisition {
              nextAtom {
                steps {
                  instrumentConfig { exposure { seconds } }
                  telescopeConfig { offset { q { arcseconds } } }
                }
              }
            }
          }
        }
      }
    """

  // The API reports both to six decimal places.
  private def step(exposureSeconds: Int, qArcseconds: Int): Json =
    Json.obj(
      "instrumentConfig" -> Json.obj("exposure" -> Json.obj("seconds" -> BigDecimal(exposureSeconds).setScale(6).asJson)),
      "telescopeConfig"  -> Json.obj("offset" -> Json.obj("q" -> Json.obj("arcseconds" -> BigDecimal(qArcseconds).setScale(6).asJson)))
    )

  private def expectSteps(oid: Observation.Id, steps: Json*): IO[Unit] =
    expect(
      user     = pi,
      query    = stepsQuery(oid),
      expected =
        Json.obj(
          "executionConfig" -> Json.obj(
            "flamingos2" -> Json.obj(
              "acquisition" -> Json.obj("nextAtom" -> Json.obj("steps" -> steps.toList.asJson))
            )
          )
        ).asRight
    )

  private def longSlit(acquisition: String): String =
    s"flamingos2LongSlit: { disperser: R1200_JH, filter: JH, fpu: LONG_SLIT_1, acquisition: { $acquisition } }"

  private def mos(acquisition: String): String =
    s"flamingos2Mos: { disperser: R1200_JH, filter: JH, customMask: { slitWidth: CUSTOM_WIDTH_1_PIX }, acquisition: { $acquisition } }"

  test("long slit: a higher requested signal-to-noise lengthens the exposure, not the sequence"):
    val acquisition: String =
      "exposureTimeMode: { signalToNoise: { value: 50, at: { nanometers: 1250 } } }"
    setup(longSlit(acquisition)).flatMap: oid =>
      expectSteps(oid, step(8, 0), step(10, 0), step(8, 0))

  test("long slit: a long time and count exposure does not make a bright target faint"):
    val acquisition: String =
      "exposureTimeMode: { timeAndCount: { time: { seconds: 6 }, count: 1, at: { nanometers: 1250 } } }"
    setup(longSlit(acquisition)).flatMap: oid =>
      expectSteps(oid, step(6, 0), step(10, 0), step(6, 0))

  test("long slit: a short exposure repeated past the limit is faint"):
    setup(longSlit("explicitFilter: K_SHORT")).flatMap: oid =>
      expectSteps(oid, step(2, 10), step(2, 0), step(10, 0), step(2, 10), step(2, 0))

  test("long slit: an explicit FAINT overrides the classification"):
    val acquisition: String =
      "explicitAcquisitionType: FAINT, skyOffset: { p: { arcseconds: 0 }, q: { arcseconds: 10 } }"
    setup(longSlit(acquisition)).flatMap: oid =>
      expectSteps(oid, step(2, 10), step(2, 0), step(10, 0), step(2, 10), step(2, 0))

  test("MOS: a target the ITC finds bright is still acquired as Faint"):
    setup(mos("explicitFilter: H")).flatMap: oid =>
      expectSteps(oid, step(5, 0), step(5, 10), step(5, 0), step(5, 10))

  test("MOS: an explicit BRIGHT takes no sky steps"):
    setup(mos("explicitAcquisitionType: BRIGHT")).flatMap: oid =>
      expectSteps(oid, step(5, 0), step(5, 0))
