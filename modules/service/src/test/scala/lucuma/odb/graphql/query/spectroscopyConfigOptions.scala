// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.Order
import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.order.*
import cats.syntax.traverse.*
import io.circe.Decoder
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.FocalPlane
import lucuma.core.enums.Instrument
import lucuma.core.enums.Site
import lucuma.core.enums.SpectroscopyCapabilities
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import skunk.Session
import skunk.syntax.all.*

class spectroscopyConfigOptions extends OdbSuite {

  val pi = TestUsers.Standard.pi(1, 30)
  val validUsers = List(pi)


  override def dbInitialization: Option[Session[IO] => IO[Unit]] = Some { s =>
    // Add a F2 option
    (s.execute(sql"insert into t_spectroscopy_config_option values('Flamingos2', 1, 'R3K + H + 0.36\"', 'single_slit', 'Long Slit 8px', 144000, 263000000, 'R3000', 'H', 1486000, 1775000, 1630500, 289000, 700, false, NULL, 'gs')".command) *>
      s.execute(sql"insert into t_spectroscopy_config_option_f2 values('Flamingos2', 1, 'LongSlit_8', 'R3000', 'H')".command)).void
  }

  case class ConfigOption(
    name:               String,
    instrument:         Instrument,
    focalPlane:         FocalPlane,
    fpuLabel:           String,
    slitWidth:          Angle,
    slitLength:         Angle,
    disperserLabel:     String,
    filterLabel:        Option[String],
    wavelengthMin:      Wavelength,
    wavelengthMax:      Wavelength,
    wavelengthOptimal:  Wavelength,
    wavelengthCoverage: Wavelength,
    resolution:         Int,
    ao:                 Boolean,
    capability:         Option[SpectroscopyCapabilities],
    site:               Site
  )

  object ConfigOption {

    import lucuma.odb.json.angle.decoder.given
    import lucuma.odb.json.wavelength.decoder.given

    given Decoder[ConfigOption] =
      Decoder.instance { c =>
        for {
          name     <- c.downField("name").as[String]
          inst     <- c.downField("instrument").as[Instrument]
          fplane   <- c.downField("focalPlane").as[FocalPlane]
          fpuLabel <- c.downField("fpuLabel").as[String]
          sWidth   <- c.downField("slitWidth").as[Angle]
          sLength  <- c.downField("slitLength").as[Angle]
          disLabel <- c.downField("disperserLabel").as[String]
          filLabel <- c.downField("filterLabel").as[Option[String]]
          waveMin  <- c.downField("wavelengthMin").as[Wavelength]
          waveMax  <- c.downField("wavelengthMax").as[Wavelength]
          waveOpt  <- c.downField("wavelengthOptimal").as[Wavelength]
          waveCov  <- c.downField("wavelengthCoverage").as[Wavelength]
          res      <- c.downField("resolution").as[Int]
          ao       <- c.downField("adaptiveOptics").as[Boolean]
          cap      <- c.downField("capability").as[Option[SpectroscopyCapabilities]]
          site     <- c.downField("site").as[Site]
        } yield ConfigOption(
          name,
          inst,
          fplane,
          fpuLabel,
          sWidth,
          sLength,
          disLabel,
          filLabel,
          waveMin,
          waveMax,
          waveOpt,
          waveCov,
          res,
          ao,
          cap,
          site
        )
      }

    def toNameSet(opts: Iterable[ConfigOption]): Set[(Instrument, String)] =
      opts.map(o => (o.instrument, o.name)).toSet
  }

  def optionsWhere(where: String): IO[List[ConfigOption]] = {

    query(
      user = pi,
      query = s"""
        query {
          spectroscopyConfigOptions(
            WHERE: {
              $where
            }
          ) {
            name
            instrument

            focalPlane

            fpuLabel
            slitWidth { arcseconds }
            slitLength { arcseconds }

            disperserLabel
            filterLabel

            wavelengthMin { micrometers }
            wavelengthMax { micrometers }
            wavelengthOptimal { micrometers }
            wavelengthCoverage { micrometers }

            resolution
            adaptiveOptics
            capability
            site
          }
        }
      """
    ).flatMap {
      _.hcursor
       .downField("spectroscopyConfigOptions")
       .values
       .toList
       .flatMap(_.toList)
       .traverse(_.as[ConfigOption])
       .leftMap(f => new RuntimeException(f.message))
       .liftTo[IO]
    }
  }

  val allOptions: IO[List[ConfigOption]] =
    optionsWhere("")

  test("simple query") {
    expect(
      user = pi,
      query = s"""
        query {
          spectroscopyConfigOptions(
            WHERE: {
              instrument: { EQ: GMOS_NORTH }
              focalPlane: { EQ: SINGLE_SLIT }
              resolution: { GTE: 7000 }
            }
          ) {
            name
          }
        }
      """,
      expected = json"""
        {
          "spectroscopyConfigOptions": [
            {
              "name": "B1200 0.25\""
            },
            {
              "name": "R831 0.25\""
            }
          ]
        }
      """.asRight
    )
  }

  test("wavelengthOptimal") {
    expect(
      user = pi,
      query = s"""
        query {
          spectroscopyConfigOptions(
            WHERE: {
              wavelengthOptimal: {
                AND: [
                  { nanometers: { GT: 760.0 } }
                  { nanometers: { LT: 770.0 } }
                ]
              }
              instrument: { EQ: GMOS_NORTH }
              slitWidth: { arcseconds: { EQ: 1.0 } }
            }
          ) {
            name
          }
        }
      """,
      expected = json"""
        {
          "spectroscopyConfigOptions": [
            {
              "name": "R400 1.0\""
            }
          ]
        }
      """.asRight
    )
  }

  test("wavelengthCoverage") {
    expect(
      user = pi,
      query = s"""
        query {
          spectroscopyConfigOptions(
            WHERE: {
              wavelengthCoverage: {
                AND: [
                  { nanometers: { GT: 350.0 } }
                  { nanometers: { LT: 400.0 } }
                ]
              }
              instrument: { EQ: GMOS_NORTH }
              slitWidth: { arcseconds: { EQ: 1.0 } }
            }
          ) {
            name
          }
        }
      """,
      expected = json"""
        {
          "spectroscopyConfigOptions": [
            {
              "name": "B480 1.0\""
            }
          ]
        }
      """.asRight
    )
  }

  test("""331" < SlitWidth """) {
    expect(
      user = pi,
      query = s"""
        query {
          spectroscopyConfigOptions(
            WHERE: {
              instrument: { EQ: GMOS_NORTH }
              slitLength: { microarcseconds: { GTE: 331000000 } }
            }
          ) {
            name
          }
        }
      """,
      expected = json"""
        {
          "spectroscopyConfigOptions": [
          ]
        }
      """.asRight
    )
  }

  test("""1" < SlitWidth < 2"""") {
    given Order[Angle] = Angle.AngleOrder
    val min    = Angle.fromMicroarcseconds(1_000_000L)
    val max    = Angle.fromMicroarcseconds(2_000_000L)
    val expect = allOptions.map(_.filter(o => o.slitWidth > min && o.slitWidth < max))
    val actual = optionsWhere {
      """
              slitWidth: {
                AND: [
                  { arcseconds: { GT: 1.0 } },
                  { arcseconds: { LT: 2.0 } }
                ]
              }
      """
    }

    for {
      es <- expect.map(ConfigOption.toNameSet)
      as <- actual.map(ConfigOption.toNameSet)
    } yield assertEquals(es, as)
  }

  test("rangeIncludes (too small)") {
    expect(
      user = pi,
      query = s"""
        query {
          spectroscopyConfigOptions(
            WHERE: {
              rangeIncludes: { micrometers: 0.35 }
            }
          ) {
            name
          }
        }
      """,
      expected = json"""
        {
          "spectroscopyConfigOptions": [
          ]
        }
      """.asRight
    )
  }

  test("rangeIncludes (in range)") {
    val w      = Wavelength.unsafeFromIntPicometers(463_000)
    val expect = allOptions.map(_.filter(o => o.wavelengthMin < w && w < o.wavelengthMax))
    val actual = optionsWhere(s"""rangeIncludes: { micrometers: 0.463 }""")
    for {
      es <- expect.map(ConfigOption.toNameSet)
      as <- actual.map(ConfigOption.toNameSet)
    } yield assertEquals(es, as)
  }

  test("AND") {
    val expect = allOptions.map(_.filter(o => o.resolution > 3000 && o.resolution < 4000))
    val actual = optionsWhere(s"""AND: [ { resolution: { GT: 3000 } }, { resolution: { LT: 4000 } }]""")
    for {
      es <- expect.map(ConfigOption.toNameSet)
      as <- actual.map(ConfigOption.toNameSet)
    } yield assertEquals(es, as)
  }

  test("OR") {
    val expect = allOptions.map(_.filter(o => o.resolution < 500 || o.resolution > 5000))
    val actual = optionsWhere(s"""OR: [ { resolution: { LT: 500 } }, { resolution: { GT: 5000 } }]""")
    for {
      es <- expect.map(ConfigOption.toNameSet)
      as <- actual.map(ConfigOption.toNameSet)
    } yield assertEquals(es, as)
  }

  test("NOT") {
    val expect = allOptions.map(_.filter(_.resolution >= 500))
    val actual = optionsWhere(s"""NOT: { resolution: { LT: 500 } }""")
    for {
      es <- expect.map(ConfigOption.toNameSet)
      as <- actual.map(ConfigOption.toNameSet)
    } yield assertEquals(es, as)
  }

  test("adaptiveOptics false") {
    val expect = allOptions  // for now, all are AO-free
    val actual = optionsWhere(s"""adaptiveOptics: { EQ: false }""")
    for {
      es <- expect.map(ConfigOption.toNameSet)
      as <- actual.map(ConfigOption.toNameSet)
    } yield assertEquals(es, as)
  }

  test("adaptiveOptics true") {
    optionsWhere(s"""adaptiveOptics: { EQ: true }""").map { a =>
      assertEquals(List.empty[ConfigOption], a) // for now, there are none
    }
  }

  test("Combined") {
    expect(
      user = pi,
      query = s"""
        query {
          spectroscopyConfigOptions(
            WHERE: {
              capability: { IS_NULL: true }
              focalPlane: { EQ: SINGLE_SLIT }
              instrument: { EQ: GMOS_NORTH }
              resolution: { LT: 500 }
              site: { EQ: GN }
              slitWidth: {
                AND: [
                  { arcseconds: { GT: 1.0 } },
                  { arcseconds: { LT: 2.0 } }
                ]
              }
            }
          ) {
            name
            instrument

            focalPlane

            fpuLabel
            slitWidth { arcseconds }
            slitLength { arcseconds }

            disperserLabel
            filterLabel

            wavelengthMin { micrometers }
            wavelengthMax { micrometers }
            wavelengthOptimal { micrometers }
            wavelengthCoverage { micrometers }

            resolution
            adaptiveOptics
            capability
            site

          }
        }
      """,
      expected = json"""
        {
          "spectroscopyConfigOptions": [
            {
              "name" : "R150 1.5\"",
              "instrument": "GMOS_NORTH",
              "focalPlane" : "SINGLE_SLIT",
              "fpuLabel" : "1.5\"",
              "slitWidth" : {
                "arcseconds" : 1.5
              },
              "slitLength" : {
                "arcseconds" : 330
              },
              "disperserLabel" : "R150",
              "filterLabel": null,
              "wavelengthMin" : {
                "micrometers" : 0.360000
              },
              "wavelengthMax" : {
                "micrometers" : 1.030000
              },
              "wavelengthOptimal" : {
                "micrometers" : 0.717000
              },
              "wavelengthCoverage" : {
                "micrometers" : 1.219000
              },
              "resolution" : 210,
              "adaptiveOptics" : false,
              "capability" : null,
              "site" : "GN"
            }
          ]
        }
      """.asRight
    )
  }

  test("GmosNorth") {
    expect(
      user = pi,
      query = s"""
        query {
          spectroscopyConfigOptions(
            WHERE: {
              focalPlane: { EQ: SINGLE_SLIT }
              instrument: { EQ: GMOS_NORTH }
              resolution: { LT: 500 }
              slitWidth: {
                AND: [
                  { arcseconds: { GT: 1.0 } },
                  { arcseconds: { LT: 2.0 } }
                ]
              }
            }
          ) {
            name
            gmosNorth {
              fpu
              grating
            }
          }
        }
      """,
      expected = json"""
        {
          "spectroscopyConfigOptions": [
            {
              "name" : "R150 1.5\"",
              "gmosNorth": {
                "fpu": "LONG_SLIT_1_50",
                "grating": "R150_G5308"
              }
            }
          ]
        }
      """.asRight
    )
  }

  test("GmosSouth") {
    expect(
      user = pi,
      query = s"""
        query {
          spectroscopyConfigOptions(
            WHERE: {
              focalPlane: { EQ: SINGLE_SLIT }
              instrument: { EQ: GMOS_SOUTH }
              resolution: { LT: 500 }
              slitWidth: {
                AND: [
                  { arcseconds: { GT: 1.0 } },
                  { arcseconds: { LT: 2.0 } }
                ]
              }
            }
          ) {
            name
            gmosSouth {
              fpu
              grating
            }
          }
        }
      """,
      expected = json"""
        {
          "spectroscopyConfigOptions": [
            {
              "name" : "R150 1.5\"",
              "gmosSouth": {
                "fpu": "LONG_SLIT_1_50",
                "grating": "R150_G5326"
              }
            }
          ]
        }
      """.asRight
    )
  }

  test("Flamingos2") {
    expect(
      user = pi,
      query = s"""
        query {
          spectroscopyConfigOptions(
            WHERE: {
              instrument: { EQ: FLAMINGOS2 }
            }
          ) {
            name
            flamingos2 {
              fpu
              disperser
              filter
            }
          }
        }
      """,
      expected = json"""
        {
          "spectroscopyConfigOptions": [
            {
              "name" : "R3K + H + 0.36\\\"",
              "flamingos2": {
                "fpu": "LONG_SLIT_8",
                "disperser": "R3000",
                "filter": "H"
              }
            }
          ]
        }
      """.asRight
    )
  }
}
