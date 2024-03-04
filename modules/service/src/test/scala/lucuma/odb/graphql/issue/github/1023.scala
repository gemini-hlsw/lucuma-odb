// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.github

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import io.circe.literal.*
import lucuma.core.enums.GcalBaselineType
import lucuma.core.enums.GcalContinuum
import lucuma.core.enums.GcalDiffuser
import lucuma.core.enums.GcalFilter
import lucuma.core.enums.GcalShutter
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.graphql.query.ObservingModeSetupOperations
import lucuma.odb.service.Services
import lucuma.odb.smartgcal.data.Gmos.GratingConfigKey
import lucuma.odb.smartgcal.data.Gmos.TableKey
import lucuma.odb.smartgcal.data.Gmos.TableRow
import lucuma.odb.smartgcal.data.SmartGcalValue
import lucuma.odb.smartgcal.data.SmartGcalValue.LegacyInstrumentConfig
import natchez.Trace.Implicits.noop
import skunk.Session


class GitHub_1023 extends OdbSuite with ObservingModeSetupOperations {

  val pi: User   = TestUsers.Standard.pi(1, 30)

  override val validUsers: List[User] =
    List(pi)

  override def dbInitialization: Option[Session[IO] => IO[Unit]] = Some { s =>
    val tableRow1: TableRow.North =
      TableRow(
        PosLong.unsafeFrom(1),
        TableKey(
          GratingConfigKey(
            GmosNorthGrating.R831_G5302,
            GmosGratingOrder.One,
            BoundedInterval.unsafeOpenUpper(Wavelength.Min, Wavelength.Max)
          ).some,
          GmosNorthFilter.RPrime.some,
          GmosNorthFpu.LongSlit_0_50.some,
          GmosXBinning.One,
          GmosYBinning.Two,
          GmosAmpGain.Low
        ),
        SmartGcalValue(
          Gcal(
            Gcal.Lamp.fromContinuum(GcalContinuum.QuartzHalogen5W),
            GcalFilter.Gmos,
            GcalDiffuser.Ir,
            GcalShutter.Open
          ),
          GcalBaselineType.Night,
          PosInt.unsafeFrom(1),
          LegacyInstrumentConfig(
            TimeSpan.unsafeFromMicroseconds(1_000_000L)
          )
        )
      )
    Enums.load(s).flatMap { e =>
      val services = Services.forUser(pi /* doesn't matter*/, e)(s)
      services.transactionally {
        services.smartGcalService.insertGmosNorth(1, tableRow1)
      }
    }
  }


  test("one inline fragment") {

    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi, "Interface Issue")
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   config(futureLimit: 100) {
                     ... on GmosNorthExecutionConfig {
                       gmosNorthStatic {
                         stageMode
                         detector
                       }
                     }
                   }
                 }
               }
             }
          """,
        expected =
          json"""
            {
              "observation" : {
                "execution" : {
                  "config" : {
                    "gmosNorthStatic" : {
                      "stageMode" : "FOLLOW_XY",
                      "detector" : "HAMAMATSU"
                    }
                  }
                }
              }
            }
          """.asRight
        )
    }
  }

  test("two inline fragments") {

    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi, "Interface Issue")
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   config(futureLimit: 100) {
                     ... on GmosNorthExecutionConfig {
                       gmosNorthStatic {
                         stageMode
                         detector
                       }
                     }
                     ... on GmosSouthExecutionConfig {
                       gmosSouthStatic {
                         stageMode
                         detector
                       }
                     }
                   }
                 }
               }
             }
          """,
        expected =
          json"""
            {
              "observation" : {
                "execution" : {
                  "config" : {
                    "gmosNorthStatic" : {
                      "stageMode" : "FOLLOW_XY",
                      "detector" : "HAMAMATSU"
                    }
                  }
                }
              }
            }
          """.asRight
        )
    }
  }
}