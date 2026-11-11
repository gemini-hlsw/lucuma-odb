// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.Resource
import com.dimafeng.testcontainers.PostgreSQLContainer
import db.migration.SmartFlamingos2Loader
import db.migration.SmartGcalLoader
import db.migration.SmartGmosLoader
import lucuma.core.model.User
import lucuma.odb.graphql.OdbSuite
import org.postgresql.core.BaseConnection

import java.io.ByteArrayInputStream
import java.io.InputStream
import java.sql.DriverManager

class SmartGcalLoaderSuite extends OdbSuite:

  override val validUsers: List[User] = Nil

  private val F2Header: String =
    "Disperser,Filter,Focal Plane Unit,,/=>/,,Calibration Observe,Calibration Filter,Calibration Diffuser,Calibration Lamps,Calibration Shutter,Calibration Exposure Time,Calibration Coadds,Calibration Basecal"

  private val Flat: String      = "None,Y (1.02 um),Imaging (none),,,,6,ND4.0,IR,IR grey body - high,Open,20,1,Day"
  private val NoFlat: String    = "None,Y (1.02 um),Imaging (none),,,,0,ND4.0,IR,IR grey body - high,Open,20,1,Day"
  private val Arc: String       = "None,Y (1.02 um),Imaging (none),,,,1,ND2.0,IR,Ar arc,Closed,30,1,Day"
  private val OtherFlat: String = "None,H (1.65 um),Imaging (none),,,,6,ND4-5,IR,IR grey body - high,Open,3,1,Day"

  private val GmosHeader: String =
    "Disperser,Filter,Focal Plane Unit,Xbin,Ybin,Central Wavelength,Order,Gain ,,,/=>/,Calibration Observe,Calibration Filter,Calibration Diffuser,Calibration Lamps,Calibration Shutter,Calibration Exposure Time,Calibration Coadds,Calibration Basecal"

  private def gmosFlat(wavelengths: String, count: Int): String =
    s"$$B1200.*,$$.*,$$.* 0.25 arcsec,1,1,$wavelengths,1,Low,,,,$count,GMOS balance,Visible,Quartz Halogen,Closed,6,1,Night"

  // Rolled back, so the definitions shared with other suites are left alone.
  private val connection: Resource[IO, BaseConnection] =
    Resource
      .make(
        IO.blocking:
          val c = DriverManager.getConnection(
            s"jdbc:postgresql://${databaseConfig.host}:${databaseConfig.port}/${databaseConfig.database}",
            PostgreSQLContainer.defaultUsername,
            PostgreSQLContainer.defaultPassword
          ).unwrap(classOf[BaseConnection])
          c.execSQLUpdate("BEGIN")
          c
      )(c => IO.blocking { c.execSQLUpdate("ROLLBACK"); c.close() })

  private def load[A](loader: SmartGcalLoader[A], header: String)(lines: String*): IO[Unit] =
    val content = ("1,2026/11/10 00:00:00 HST" :: header :: lines.toList).mkString("\n")
    val file: IO[InputStream] = IO(new ByteArrayInputStream(content.getBytes("UTF-8")))
    connection.use(loader.load(_, NonEmptyList.one("test.csv" -> file)))

  private def loadF2(lines: String*): IO[Unit] =
    load(SmartFlamingos2Loader.F2, F2Header)(lines*)

  private def loadGmos(lines: String*): IO[Unit] =
    load(SmartGmosLoader.North, GmosHeader)(lines*)

  private def rejected(instrument: String, zero: Int, other: Int): String =
    s"Smart GCAL $instrument: flat line $zero has a Calibration Observe of 0, but flat line $other matches the same configuration. A configuration with no calibration must have no other line."

  test("a No-Calibration Row loads when it is the only line for its configuration"):
    loadF2(NoFlat, OtherFlat)

  test("a No-Calibration Row loads beside a line of another calibration type"):
    loadF2(NoFlat, Arc)

  test("a No-Calibration Row beside another line for the same configuration is rejected"):
    interceptMessageIO[RuntimeException](rejected("Flamingos 2", 4, 3))(loadF2(Flat, NoFlat))

  test("a No-Calibration Row loads beside a line for another wavelength range"):
    loadGmos(gmosFlat("450 - 550", 1), gmosFlat("600 - 700", 0))

  test("a No-Calibration Row inside the wavelength range of another line is rejected"):
    interceptMessageIO[RuntimeException](rejected("GMOS North", 4, 3)):
      loadGmos(gmosFlat("450 - 550", 1), gmosFlat("500 - 520", 0))
