// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.IO
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.GcalBaselineType
import lucuma.core.enums.GcalContinuum
import lucuma.core.enums.GcalDiffuser
import lucuma.core.enums.GcalFilter
import lucuma.core.enums.GcalShutter
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.SmartGcalType
import lucuma.core.model.User
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.OdbSuite
import lucuma.odb.graphql.TestUsers
import lucuma.odb.smartgcal.data.Flamingos2
import lucuma.odb.smartgcal.data.Gmos
import lucuma.odb.smartgcal.data.SmartGcalValue
import lucuma.odb.smartgcal.data.SmartGcalValue.LegacyInstrumentConfig

/**
 * Imaging lookups leave the grating (or disperser), order and FPU empty, so
 * they exercise the `IS NULL` side of every optional search key column.  Each
 * instrument gets an imaging row plus a long slit decoy that differs only in
 * having an FPU, so a lookup must tell `IS NULL` apart from `= value`.
 */
class SmartGcalServiceSuite_NullKeys extends OdbSuite:

  val serviceUser = TestUsers.service(3)

  override val validUsers: List[User] = List(serviceUser)

  private def flat(exposure: TimeSpan): SmartGcalValue.Legacy =
    SmartGcalValue(
      Gcal(
        Gcal.Lamp.fromContinuum(GcalContinuum.QuartzHalogen5W),
        GcalFilter.Gmos,
        GcalDiffuser.Ir,
        GcalShutter.Open
      ),
      GcalBaselineType.Night,
      PosInt.unsafeFrom(1),
      LegacyInstrumentConfig(exposure)
    )

  private val imagingFlat  = flat(7.secondTimeSpan)
  private val longSlitFlat = flat(9.secondTimeSpan)

  private def gmosNorthKey(fpu: Option[GmosNorthFpu]): Gmos.TableKey.North =
    Gmos.TableKey(none, GmosNorthFilter.GPrime.some, fpu, GmosXBinning.One, GmosYBinning.One, GmosAmpGain.Low)

  private def gmosSouthKey(fpu: Option[GmosSouthFpu]): Gmos.TableKey.South =
    Gmos.TableKey(none, GmosSouthFilter.GPrime.some, fpu, GmosXBinning.One, GmosYBinning.One, GmosAmpGain.Low)

  private def f2Key(fpu: Option[Flamingos2Fpu]): Flamingos2.TableKey =
    Flamingos2.TableKey(none, Flamingos2Filter.Y, fpu)

  private val line = PosLong.unsafeFrom(1)

  test("GMOS North: imaging lookup matches only the row with no grating, order or FPU"):
    val insert = withServices(serviceUser): s =>
      Services.asSuperUser:
        s.smartGcalService.insertGmosNorth(1, Gmos.TableRow(line, gmosNorthKey(none), imagingFlat)) *>
        s.smartGcalService.insertGmosNorth(2, Gmos.TableRow(line, gmosNorthKey(GmosNorthFpu.LongSlit_1_00.some), longSlitFlat))

    def select(fpu: Option[GmosNorthFpu]): IO[List[Gcal]] =
      withServices(serviceUser): s =>
        Services.asSuperUser:
          val k = gmosNorthKey(fpu)
          s.smartGcalService
           .selectGmosNorth(Gmos.SearchKey.North(none, k.filter, k.fpu, k.xBin, k.yBin, k.gain), SmartGcalType.Flat)
           .map(_.map(_._2))

    for
      _        <- insert
      imaging  <- select(none)
      longSlit <- select(GmosNorthFpu.LongSlit_1_00.some)
      missing  <- select(GmosNorthFpu.LongSlit_0_50.some)
    yield
      assertEquals(imaging, List(imagingFlat.gcalConfig))
      assertEquals(longSlit, List(longSlitFlat.gcalConfig))
      assertEquals(missing, Nil)

  test("GMOS South: imaging lookup matches only the row with no grating, order or FPU"):
    val insert = withServices(serviceUser): s =>
      Services.asSuperUser:
        s.smartGcalService.insertGmosSouth(1, Gmos.TableRow(line, gmosSouthKey(none), imagingFlat)) *>
        s.smartGcalService.insertGmosSouth(2, Gmos.TableRow(line, gmosSouthKey(GmosSouthFpu.LongSlit_1_00.some), longSlitFlat))

    def select(fpu: Option[GmosSouthFpu]): IO[List[Gcal]] =
      withServices(serviceUser): s =>
        Services.asSuperUser:
          val k = gmosSouthKey(fpu)
          s.smartGcalService
           .selectGmosSouth(Gmos.SearchKey.South(none, k.filter, k.fpu, k.xBin, k.yBin, k.gain), SmartGcalType.Flat)
           .map(_.map(_._2))

    for
      _        <- insert
      imaging  <- select(none)
      longSlit <- select(GmosSouthFpu.LongSlit_1_00.some)
      missing  <- select(GmosSouthFpu.LongSlit_0_50.some)
    yield
      assertEquals(imaging, List(imagingFlat.gcalConfig))
      assertEquals(longSlit, List(longSlitFlat.gcalConfig))
      assertEquals(missing, Nil)

  test("Flamingos-2: imaging lookup matches only the row with no disperser or FPU"):
    val insert = withServices(serviceUser): s =>
      Services.asSuperUser:
        s.smartGcalService.insertFlamingos2(1, Flamingos2.TableRow(line, f2Key(none), imagingFlat)) *>
        s.smartGcalService.insertFlamingos2(2, Flamingos2.TableRow(line, f2Key(Flamingos2Fpu.LongSlit2.some), longSlitFlat))

    def select(fpu: Option[Flamingos2Fpu]): IO[List[Gcal]] =
      withServices(serviceUser): s =>
        Services.asSuperUser:
          s.smartGcalService
           .selectFlamingos2(f2Key(fpu), SmartGcalType.Flat)
           .map(_.map(_._2))

    for
      _        <- insert
      imaging  <- select(none)
      longSlit <- select(Flamingos2Fpu.LongSlit2.some)
      missing  <- select(Flamingos2Fpu.LongSlit4.some)
    yield
      assertEquals(imaging, List(imagingFlat.gcalConfig))
      assertEquals(longSlit, List(longSlitFlat.gcalConfig))
      assertEquals(missing, Nil)
