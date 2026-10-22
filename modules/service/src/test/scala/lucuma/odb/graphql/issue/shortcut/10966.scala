// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.GnirsReadMode
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.syntax.string.*
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.itc.client.SpectroscopyInput
import lucuma.odb.graphql.feature.TelluricCalibrationsTestSupport
import lucuma.odb.graphql.query.ExecutionTestSupportForGnirs
import lucuma.odb.graphql.subscription.SubscriptionUtils

import java.time.Instant

// sc-10966: a telluric exposure time is calculated by the ITC for a bright standard star, so its
// read mode must follow the exposure time the ITC picked, not the science observation's explicit one.
class ShortCut_10966
  extends OdbSuite
  with ExecutionTestSupportForGnirs
  with TelluricCalibrationsTestSupport
  with SubscriptionUtils:

  val when: Instant = Instant.parse("2024-01-01T12:00:00Z")

  // The telluric's derived S/N mode gets a short exposure, read out Bright.
  val TelluricExposure: TimeSpan = 2.secondTimeSpan

  override def fakeItcSpectroscopyResultFor(input: SpectroscopyInput): Option[IntegrationTime] =
    input.mode.exposureTimeMode match
      case ExposureTimeMode.SignalToNoiseMode(_, _) => IntegrationTime(TelluricExposure, PosInt.unsafeFrom(4)).some
      case _                                        => none

  private def setExplicitReadMode(oid: Observation.Id, readMode: GnirsReadMode): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: {
              observingMode: {
                gnirsSpectroscopy: {
                  explicitReadMode: ${readMode.tag.toScreamingSnakeCase}
                }
              }
            }
            WHERE: { id: { EQ: "$oid" } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  // (exposure seconds, read mode) of every step in the first science atom.
  private def scienceSteps(oid: Observation.Id): IO[List[(BigDecimal, String)]] =
    query(
      pi,
      executionConfigQuery(oid, "gnirs", "science", "steps { instrumentConfig { exposure { seconds } readMode } }", None)
    ).map: json =>
      json.hcursor
        .downFields("executionConfig", "gnirs", "science", "nextAtom", "steps")
        .values.toList.flatten
        .map: s =>
          val c = s.hcursor.downField("instrumentConfig")
          (c.downFields("exposure", "seconds").require[BigDecimal], c.downField("readMode").require[String])

  private def setIfuTelescopeConfigs(oid: Observation.Id, qs: List[Int]): IO[Unit] =
    val configs = qs.map(q => s"{ offset: { p: { arcseconds: 0 } q: { arcseconds: $q } } }").mkString(" ")
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: {
              observingMode: {
                gnirsSpectroscopy: {
                  ifu: { telescopeConfigs: [ $configs ] }
                }
              }
            }
            WHERE: { id: { EQ: "$oid" } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  private def explicitReadMode(oid: Observation.Id): IO[Option[String]] =
    query(
      pi,
      s"""
        query {
          observation(observationId: "$oid") {
            observingMode { gnirsSpectroscopy { explicitReadMode } }
          }
        }
      """
    ).map(_.hcursor.downFields("observation", "observingMode", "gnirsSpectroscopy", "explicitReadMode").require[Option[String]])

  // The q offsets, in arcseconds, of an IFU observation's telescope configs.
  private def ifuOffsetsQ(oid: Observation.Id): IO[List[BigDecimal]] =
    query(
      pi,
      s"""
        query {
          observation(observationId: "$oid") {
            observingMode { gnirsSpectroscopy { ifu { telescopeConfigs { offset { q { arcseconds } } } } } }
          }
        }
      """
    ).map:
      _.hcursor
        .downFields("observation", "observingMode", "gnirsSpectroscopy", "ifu", "telescopeConfigs")
        .values.toList.flatten
        .map(_.hcursor.downFields("offset", "q", "arcseconds").require[BigDecimal])

  private def assertTelluricSteps(steps: List[(BigDecimal, String)]): Unit =
    assert(steps.nonEmpty, "expected telluric science steps")
    steps.foreach: (exposure, readMode) =>
      assertEquals(exposure, TelluricExposure.toSeconds)
      assertEquals(readMode, GnirsReadMode.forExposureTime(TelluricExposure).tag.toScreamingSnakeCase)

  test("a telluric's read mode follows its own exposure time, not the science's explicit read mode"):
    for
      pid   <- createProgramAs(pi)
      tid   <- createTargetWithProfileAs(pi, pid)
      oid   <- createGnirsLongSlitObservationAs(pi, pid, tid)
      _     <- setExplicitReadMode(oid, GnirsReadMode.VeryFaint)
      _     <- runObscalcUpdate(pid, oid)
      _     <- recalculateCalibrations(pid, when, oid)
      tel   <- calibrationObservationFor(oid, CalibrationRole.Telluric).map(_.get)
      _     <- sleep >> resolveTelluricTargets
      steps <- scienceSteps(tel)
    yield assertTelluricSteps(steps)

  test("a resync clears a read mode set on the science after its telluric was created"):
    for
      pid    <- createProgramAs(pi)
      tid    <- createTargetWithProfileAs(pi, pid)
      oid    <- createGnirsLongSlitObservationAs(pi, pid, tid)
      _      <- runObscalcUpdate(pid, oid)
      _      <- recalculateCalibrations(pid, when, oid)
      before <- calibrationObservationFor(oid, CalibrationRole.Telluric)
      _      <- setExplicitReadMode(oid, GnirsReadMode.VeryFaint)
      _      <- recalculateCalibrations(pid, when, oid)
      tel    <- calibrationObservationFor(oid, CalibrationRole.Telluric).map(_.get)
      sciRm  <- explicitReadMode(oid)
      telRm  <- explicitReadMode(tel)
      _      <- sleep >> resolveTelluricTargets
      steps  <- scienceSteps(tel)
    yield
      // The same telluric, resynced rather than recreated.
      assertEquals(before, tel.some)
      assertEquals(sciRm, "VERY_FAINT".some)
      assertEquals(telRm, none)
      assertTelluricSteps(steps)

  test("an IFU telluric drops the science's explicit read mode but keeps its offsets"):
    for
      pid    <- createProgramAs(pi)
      tid    <- createTargetWithProfileAs(pi, pid)
      oid    <- createGnirsIfuObservationAs(pi, pid, tid)
      _      <- setExplicitReadMode(oid, GnirsReadMode.VeryFaint)
      _      <- setIfuTelescopeConfigs(oid, List(1, -1))
      _      <- runObscalcUpdate(pid, oid)
      _      <- recalculateCalibrations(pid, when, oid)
      tel    <- calibrationObservationFor(oid, CalibrationRole.Telluric).map(_.get)
      telRm  <- explicitReadMode(tel)
      telQs  <- ifuOffsetsQ(tel)
    yield
      assertEquals(telRm, none)
      assertEquals(telQs, List(BigDecimal(1), BigDecimal(-1)))
