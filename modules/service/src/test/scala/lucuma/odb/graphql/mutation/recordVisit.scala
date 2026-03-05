// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.data.NonEmptySet
import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.GcalArc
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
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.StepStage
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.core.util.IdempotencyKey
import lucuma.core.util.TimeSpan
import lucuma.odb.data.OdbError
import lucuma.odb.data.StepExecutionState
import lucuma.odb.service.Services
import lucuma.odb.smartgcal.data.Flamingos2
import lucuma.odb.smartgcal.data.Gmos
import lucuma.odb.smartgcal.data.SmartGcalValue
import lucuma.odb.smartgcal.data.SmartGcalValue.LegacyInstrumentConfig
import skunk.Session


class recordVisit extends OdbSuite with query.GenerationTestSupport with ExecutionState:

  val service: User = TestUsers.service(nextId)

  override lazy val validUsers: List[User] = List(service)

  val gn_key_0_50: Gmos.TableKey[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] =
    Gmos.TableKey(
      Gmos.GratingConfigKey(
        GmosNorthGrating.R831_G5302,
        GmosGratingOrder.One,
        BoundedInterval.unsafeOpenUpper(Wavelength.Min, Wavelength.Max)
      ).some,
      GmosNorthFilter.RPrime.some,
      GmosNorthFpu.LongSlit_0_50.some,
      GmosXBinning.One,
      GmosYBinning.Two,
      GmosAmpGain.Low
    )

  val gs_key_0_50: Gmos.TableKey[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu] =
    Gmos.TableKey(
      Gmos.GratingConfigKey(
        GmosSouthGrating.B1200_G5321,
        GmosGratingOrder.One,
        BoundedInterval.unsafeOpenUpper(Wavelength.Min, Wavelength.Max)
      ).some,
      GmosSouthFilter.RPrime.some,
      GmosSouthFpu.LongSlit_0_50.some,
      GmosXBinning.One,
      GmosYBinning.Two,
      GmosAmpGain.Low
    )

  val f2_key_HK: Flamingos2.TableKey =
    Flamingos2.TableKey(
      Flamingos2Disperser.R1200HK.some,
      Flamingos2Filter.Y,
      Flamingos2Fpu.LongSlit2.some
    )

  val flat =
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

  val arc =
    SmartGcalValue(
      Gcal(
        Gcal.Lamp.fromArcs(NonEmptySet.one(GcalArc.CuArArc)),
        GcalFilter.None,
        GcalDiffuser.Visible,
        GcalShutter.Closed
      ),
      GcalBaselineType.Day,
      PosInt.unsafeFrom(1),
      LegacyInstrumentConfig(
        TimeSpan.unsafeFromMicroseconds(1_000_000L)
      )
    )

  private val Line1: PosLong =
    PosLong.unsafeFrom(1)

  override def dbInitialization: Option[Session[IO] => IO[Unit]] = Some: s =>
    val gn_rows: List[Gmos.TableRow.North] =
      List(
        Gmos.TableRow(Line1, gn_key_0_50, flat),
        Gmos.TableRow(Line1, gn_key_0_50, arc),
      )

    val gs_rows: List[Gmos.TableRow.South] =
      List(
        Gmos.TableRow(Line1, gs_key_0_50, flat),
        Gmos.TableRow(Line1, gs_key_0_50, arc)
      )

    val f2_rows: List[Flamingos2.TableRow] =
      List(
        Flamingos2.TableRow(Line1, f2_key_HK, flat),
        Flamingos2.TableRow(Line1, f2_key_HK, arc)
      )

    servicesFor(service /* doesn't matter*/).map(_(s)).use: services =>
      services.transactionally:

        val f2 = f2_rows.zipWithIndex.traverse_ : (r, i) =>
          Services.asSuperUser:
            services.smartGcalService.insertFlamingos2(i, r)

        val north = gn_rows.zipWithIndex.traverse_ : (r, i) =>
          Services.asSuperUser:
            services.smartGcalService.insertGmosNorth(i, r)

        val south = gs_rows.zipWithIndex.traverse_ : (r, i) =>
          Services.asSuperUser:
            services.smartGcalService.insertGmosSouth(i, r)

        f2 *> north *> south

  private def recordVisitTest(
    mode:     ObservingModeType,
    user:     User,
    query:    Observation.Id => String,
    expected: Either[Observation.Id => String, Json]
  ): IO[Unit] =
    for
      pid <- createProgramAs(user)
      tid <- createTargetWithProfileAs(user, pid)
      oid <- createObservationAs(user, pid, mode.some, tid)
      _   <- expectSuccessOrOdbError(user, query(oid), expected.leftMap: f =>
        case OdbError.InvalidObservation(_, Some(d)) if d === f(oid) => ()
      ) 
    yield ()

  test("recordGmosNorthVisit"):

    recordVisitTest(
      ObservingModeType.GmosNorthLongSlit,
      service,
      oid => s"""
        mutation {
          recordGmosNorthVisit(input: {
            observationId: "$oid",
            gmosNorth: {
              stageMode: NO_FOLLOW
            }
          }) {
            visit {
              gmosNorth {
                stageMode
                detector
                mosPreImaging
              }
              gmosSouth {
                stageMode
              }
            }
          }
        }
      """,
      json"""
      {
        "recordGmosNorthVisit": {
          "visit": {
            "gmosNorth": {
              "stageMode": "NO_FOLLOW",
              "detector": "HAMAMATSU",
              "mosPreImaging": "IS_NOT_MOS_PRE_IMAGING"
            },
            "gmosSouth": null
          }
        }
      }
      """.asRight
    )

  test("recordGmosNorthVisit - idempotencyKey"):
    val idm   = IdempotencyKey.FromString.getOption("7304956b-45ab-45b6-8db1-ae6f743b519c").get
    val setup =
      for
        pid <- createProgramAs(service)
        tid <- createTargetWithProfileAs(service, pid)
        oid <- createObservationAs(service, pid, ObservingModeType.GmosNorthLongSlit.some, tid)
      yield oid

    def recordVisit(oid: Observation.Id): IO[(Visit.Id, IdempotencyKey)] =
      query(
        user  = service,
        query = s"""
          mutation {
            recordGmosNorthVisit(input: {
              observationId: "$oid"
              gmosNorth: {
                stageMode: NO_FOLLOW
              }
              idempotencyKey: "${IdempotencyKey.FromString.reverseGet(idm)}"
            }) {
              visit {
                id
                idempotencyKey
              }
            }
          }
        """
      ).map: js =>
        val v = js.hcursor.downFields("recordGmosNorthVisit", "visit")
        (
          v.downField("id").require[Visit.Id],
          v.downField("idempotencyKey").require[IdempotencyKey]
        )

    assertIOBoolean:
      for
        o        <- setup
        (v0, k0) <- recordVisit(o)
        (v1, k1) <- recordVisit(o)
      yield (v0 === v1) && (k0 === idm) && (k1 === idm)

  test("recordGmosNorthVisit - idempotencyKey (slew)"):
    val idm0  = IdempotencyKey.FromString.getOption("100fbce7-73eb-4c82-8a1e-e987a087b89d").get
    val idm1  = IdempotencyKey.FromString.getOption("c7adcb5c-b0c4-404b-a01a-89b7658ebfac").get

    val setup =
      for
        pid <- createProgramAs(service)
        tid <- createTargetWithProfileAs(service, pid)
        oid <- createObservationAs(service, pid, ObservingModeType.GmosNorthLongSlit.some, tid)
      yield oid

    def recordSlew(oid: Observation.Id): IO[Visit.Id] =
      query(
        user  = service,
        query = s"""
          mutation {
            addSlewEvent(input: {
              observationId: "$oid"
              slewStage: START_SLEW
              idempotencyKey: "${IdempotencyKey.FromString.reverseGet(idm0)}"
            }) {
              event {
                visit { id }
              }
            }
          }
        """
      ).map: js =>
        js.hcursor
          .downFields("addSlewEvent", "event", "visit", "id")
          .require[Visit.Id]

    def recordVisit(oid: Observation.Id): IO[Visit.Id] =
      query(
        user  = service,
        query = s"""
          mutation {
            recordGmosNorthVisit(input: {
              observationId: "$oid"
              gmosNorth: {
                stageMode: NO_FOLLOW
              }
              idempotencyKey: "${IdempotencyKey.FromString.reverseGet(idm1)}"
            }) {
              visit { id }
            }
          }
        """
      ).map: js =>
        js.hcursor
          .downFields("recordGmosNorthVisit", "visit", "id")
          .require[Visit.Id]

    assertIOBoolean:
      for
        o  <- setup
        v0 <- recordSlew(o)
        v1 <- recordVisit(o)
      yield v0 === v1

  test("recordGmosSouthVisit"):

    recordVisitTest(
      ObservingModeType.GmosSouthLongSlit,
      service,
      oid => s"""
        mutation {
          recordGmosSouthVisit(input: {
            observationId: "$oid",
            gmosSouth: {
              stageMode: FOLLOW_XYZ
            }
          }) {
            visit {
              gmosSouth {
                stageMode
                detector
                mosPreImaging
              }
            }
          }
        }
      """,
      json"""
      {
        "recordGmosSouthVisit": {
          "visit": {
            "gmosSouth": {
              "stageMode": "FOLLOW_XYZ",
              "detector": "HAMAMATSU",
              "mosPreImaging": "IS_NOT_MOS_PRE_IMAGING"
            }
          }
        }
      }
      """.asRight
    )

  test("record visit cross site"):

    recordVisitTest(
      ObservingModeType.GmosNorthLongSlit,
      service,
      oid => s"""
        mutation {
          recordGmosSouthVisit(input: {
            observationId: "$oid",
            gmosSouth: {
              stageMode: FOLLOW_XYZ,
              detector: HAMAMATSU,
              mosPreImaging: IS_NOT_MOS_PRE_IMAGING
            }
          }) {
            visit {
              gmosSouth {
                stageMode
              }
            }
          }
        }
      """,
      ((oid: Observation.Id) => s"Observation '$oid' not found or is not a GMOS South observation").asLeft
    )

  test("recordFlamingos2Visit"):

    recordVisitTest(
      ObservingModeType.Flamingos2LongSlit,
      service,
      oid => s"""
        mutation {
          recordFlamingos2Visit(input: {
            observationId: "$oid",
            flamingos2: {
              useElectronicOffsetting: true
            }
          }) {
            visit {
              flamingos2 {
                mosPreImaging
                useElectronicOffsetting
              }
              gmosNorth {
                stageMode
              }
              gmosSouth {
                stageMode
              }
            }
          }
        }
      """,
      json"""
      {
        "recordFlamingos2Visit": {
          "visit": {
            "flamingos2": {
              "mosPreImaging": "IS_NOT_MOS_PRE_IMAGING",
              "useElectronicOffsetting": true
            },
            "gmosNorth": null,
            "gmosSouth": null
          }
        }
      }
      """.asRight
    )

  test("recordFlamingos2Visit (defaults)"):

    recordVisitTest(
      ObservingModeType.Flamingos2LongSlit,
      service,
      oid => s"""
        mutation {
          recordFlamingos2Visit(input: {
            observationId: "$oid",
            flamingos2: {}
          }) {
            visit {
              flamingos2 {
                mosPreImaging
                useElectronicOffsetting
              }
              gmosNorth {
                stageMode
              }
              gmosSouth {
                stageMode
              }
            }
          }
        }
      """,
      json"""
      {
        "recordFlamingos2Visit": {
          "visit": {
            "flamingos2": {
              "mosPreImaging": "IS_NOT_MOS_PRE_IMAGING",
              "useElectronicOffsetting": false
            },
            "gmosNorth": null,
            "gmosSouth": null
          }
        }
      }
      """.asRight
    )

  test("recordGmosNorthVisit - abandon steps"):
    assertIOBoolean:
      for
        pid <- createProgramAs(service)
        tid <- createTargetWithProfileAs(service, pid)
        oid <- createObservationAs(service, pid, ObservingModeType.GmosNorthLongSlit.some, tid)
        sa  <- firstAcquisitionAtomStepIds(service, oid)
        ss  <- firstScienceAtomStepIds(service, oid)
        v0  <- recordVisitAs(service, Instrument.GmosNorth, oid)
        _   <- addStepEventAs(service, sa(0), v0, StepStage.StartStep)
        _   <- addStepEventAs(service, ss(0), v0, StepStage.StartStep)
        e0  <- stepExecutionState(service, oid)
        _   <- recordVisitAs(service, Instrument.GmosNorth, oid)
        e1  <- stepExecutionState(service, oid)
      yield e0.forall(_ === StepExecutionState.Ongoing) &&
            e1.forall(_ === StepExecutionState.Abandoned)