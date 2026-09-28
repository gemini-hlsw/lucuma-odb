// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import lucuma.core.model.Observation

// The calibration set interval drops from 90 to 60 minutes at 2.6 µm, judged by the
// longest central wavelength in the configuration.
class executionDigest_calibrationCountGnirs
  extends OdbSuite
  with ExecutionTestSupportForGnirs
  with CalibrationCountTestSupport:

  // Sixty minutes of exposure per wavelength: one 90-minute set, two 60-minute ones.
  private def setWavelengths(oid: Observation.Id, nanometers: List[Int]): IO[Unit] =
    val configs = nanometers.map: nm =>
      s"""{
        centralWavelength: { nanometers: $nm }
        exposureTimeMode: {
          timeAndCount: {
            time: { minutes: 10 }
            count: 6
            at: { nanometers: $nm }
          }
        }
      }"""
    query(
      pi,
      s"""mutation {
        updateObservations(input: {
          WHERE: { id: { EQ: "$oid" } }
          SET: {
            observingMode: {
              gnirsLongSlit: { centralWavelengths: [ ${configs.mkString(", ")} ] }
            }
          }
        }) {
          observations { id }
        }
      }"""
    ).void

  test("90-minute sets below 2.6 µm, 60-minute sets from 2.6 µm"):
    assertIO(
      for
        p  <- createProgramAs(pi)
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createGnirsLongSlitObservationAs(pi, p, t)
        _  <- setWavelengths(o, List(2200))
        c1 <- calibrationCount(p, o)
        _  <- setWavelengths(o, List(2599))
        c2 <- calibrationCount(p, o)
        _  <- setWavelengths(o, List(2600))
        c3 <- calibrationCount(p, o)
        _  <- setWavelengths(o, List(3400))
        c4 <- calibrationCount(p, o)
      yield (c1, c2, c3, c4),
      (1, 1, 2, 2)
    )

  // Two wavelengths make two hours of science: three 60-minute sets, where
  // the 90-minute interval of the shorter wavelength alone would give two.
  test("the longest central wavelength decides the interval"):
    assertIO(
      for
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createGnirsLongSlitObservationAs(pi, p, t)
        _ <- setWavelengths(o, List(2200, 3400))
        c <- calibrationCount(p, o)
      yield c,
      3
    )
