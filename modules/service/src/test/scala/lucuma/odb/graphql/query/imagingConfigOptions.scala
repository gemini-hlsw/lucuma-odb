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
import lucuma.core.enums.ImagingCapabilities
import lucuma.core.enums.Instrument
import lucuma.core.enums.Site
import lucuma.core.math.Angle
import skunk.Session
import skunk.syntax.all.*

class imagingConfigOptions extends OdbSuite {

  val pi = TestUsers.Standard.pi(1, 30)
  val validUsers = List(pi)


  override def dbInitialization: Option[Session[IO] => IO[Unit]] = Some { s =>
    (s.execute(sql"insert into t_imaging_config_option (c_instrument, c_index, c_fov, c_filter_label, c_ao, c_site) values('GmosNorth', 1, 330000000, 'z+CaT', false, 'gn')".command) *>
      s.execute(sql"insert into t_imaging_config_option (c_instrument, c_index, c_fov, c_filter_label, c_ao, c_site) values('GmosNorth', 2, 350000000, 'OVI', false, 'gn')".command) *>
      s.execute(sql"insert into t_imaging_config_option (c_instrument, c_index, c_fov, c_filter_label, c_ao, c_site) values('GmosSouth', 3, 150000000, 'OVIC', false, 'gs')".command) *>
      s.execute(sql"insert into t_imaging_config_option (c_instrument, c_index, c_fov, c_filter_label, c_ao, c_capability, c_site) values('Alopeke', 4, 3000000, 'EO466', false, 'speckle', 'gn')".command) *>
      s.execute(sql"insert into t_imaging_config_option_gmos_north values('GmosNorth', 1, 'ZPrime_CaT')".command) *>
      s.execute(sql"insert into t_imaging_config_option_gmos_north values('GmosNorth', 2, 'OVI')".command) *>
      s.execute(sql"insert into t_imaging_config_option_gmos_south values('GmosSouth', 3, 'OVIC')".command)).void
  }

  case class ConfigOption(
    instrument:         Instrument,
    filterLabel:        Option[String],
    ao:                 Boolean,
    site:               Site,
    fov:                Angle,
    capability:         Option[ImagingCapabilities]
  )

  object ConfigOption {

    import lucuma.odb.json.angle.decoder.given

    given Decoder[ConfigOption] =
      Decoder.instance { c =>
        for {
          inst     <- c.downField("instrument").as[Instrument]
          filLabel <- c.downField("filterLabel").as[Option[String]]
          ao       <- c.downField("adaptiveOptics").as[Boolean]
          site     <- c.downField("site").as[Site]
          fov      <- c.downField("fov").as[Angle]
          cap      <- c.downField("capability").as[Option[ImagingCapabilities]]
        } yield ConfigOption(
          inst,
          filLabel,
          ao,
          site,
          fov,
          cap
        )
      }
  }

  def optionsWhere(where: String): IO[List[ConfigOption]] = {

    query(
      user = pi,
      query = s"""
        query {
          imagingConfigOptions(
            WHERE: {
              $where
            }
          ) {
            instrument
            fov { arcseconds }
            filterLabel
            adaptiveOptics
            site
            capability
          }
        }
      """
    ).flatMap {
      _.hcursor
       .downField("imagingConfigOptions")
       .values
       .toList
       .flatMap(_.toList)
       .traverse(_.as[ConfigOption])
       .leftMap(f => {f.printStackTrace();new RuntimeException(f.message)})
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
          imagingConfigOptions(
            WHERE: {
              instrument: { EQ: GMOS_NORTH }
            }
          ) {
            instrument
            fov {
              arcseconds
            }
          }
        }
      """,
      expected = json"""
        {
          "imagingConfigOptions": [
            {
              "instrument": "GMOS_NORTH",
              "fov": {
                "arcseconds": 350
              }
            },
            {
              "instrument": "GMOS_NORTH",
              "fov": {
                "arcseconds": 330
              }
            }
          ]
        }
      """.asRight
    )
  }

  test("AND") {
    val expect = allOptions.map(_.filter(o => (o.fov.toMicroarcseconds / 1e6) > 3000 && o.site === Site.GS))
    val actual = optionsWhere(s"""AND: [ { site: { EQ: GS } }, { fov: { arcseconds: { GT: 300 } } }]""")
    for {
      es <- expect
      as <- actual
    } yield assertEquals(es, as)
  }

  test("OR") {
    val expect = allOptions.map(_.filter(o => (o.fov.toMicroarcseconds / 1e6) < 500 || o.site > Site.GN))
    val actual = optionsWhere(s"""OR: [ { fov: { arcseconds: { LT: 500 } } }, { site: { EQ: GN } }]""")
    for {
      es <- expect
      as <- actual
    } yield assertEquals(es, as)
  }

  test("NOT") {
    val expect = allOptions.map(_.filter(_.fov.toMicroarcseconds / 1e6 <= 300))
    val actual = optionsWhere(s"""NOT: { fov: { arcseconds: { GT: 300 } } }""")
    for {
      es <- expect
      as <- actual
    } yield assertEquals(es, as)
  }

  test("adaptiveOptics false") {
    val expect = allOptions  // for now, all are AO-free
    val actual = optionsWhere(s"""adaptiveOptics: { EQ: false }""")
    for {
      es <- expect
      as <- actual
    } yield assertEquals(es, as)
  }

  test("adaptiveOptics true") {
    optionsWhere(s"""adaptiveOptics: { EQ: true }""").map { a =>
      assertEquals(List.empty[ConfigOption], a) // for now, there are none
    }
  }

  test("""100" < FoV < 400"""") {
    given Order[Angle] = Angle.AngleOrder
    val min    = Angle.fromDoubleArcseconds(100)
    val max    = Angle.fromDoubleArcseconds(400)
    val expect = allOptions.map(_.filter(o => o.fov > min && o.fov < max))
    val actual = optionsWhere {
      """
              fov: {
                AND: [
                  { arcseconds: { GT: 100 } },
                  { arcseconds: { LT: 400 } }
                ]
              }
      """
    }

    for {
      es <- expect
      as <- actual
    } yield assertEquals(es, as)
  }

  test("GmosNorth") {
    expect(
      user = pi,
      query = s"""
        query {
          imagingConfigOptions(
            WHERE: {
              instrument: { EQ: GMOS_NORTH }
            }
          ) {
            gmosNorth {
              filter
            }
          }
        }
      """,
      expected = json"""
        {
          "imagingConfigOptions": [
            {
              "gmosNorth": {
                "filter": "OVI"
              }
            },
            {
              "gmosNorth": {
                "filter": "Z_PRIME_CA_T"
              }
            }
          ]
        }
      """.asRight
    )
  }

  test("capability EQ SPECKLE") {
    val expect = allOptions.map(_.filter(_.capability.contains(ImagingCapabilities.Speckle)))
    val actual = optionsWhere(s"""capability: { EQ: SPECKLE }""")
    for {
      es <- expect
      as <- actual
    } yield assertEquals(es, as)
  }

  test("capability field selection") {
    expect(
      user = pi,
      query = s"""
        query {
          imagingConfigOptions(
            WHERE: {
              instrument: { EQ: ALOPEKE }
            }
          ) {
            instrument
            capability
          }
        }
      """,
      expected = json"""
        {
          "imagingConfigOptions": [
            {
              "instrument": "ALOPEKE",
              "capability": "SPECKLE"
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
          imagingConfigOptions(
            WHERE: {
              instrument: { EQ: GMOS_SOUTH }
            }
          ) {
            gmosSouth {
              filter
            }
          }
        }
      """,
      expected = json"""
        {
          "imagingConfigOptions": [
            {
              "gmosSouth": {
                "filter": "OVIC"
              }
            }
          ]
        }
      """.asRight
    )
  }
}
