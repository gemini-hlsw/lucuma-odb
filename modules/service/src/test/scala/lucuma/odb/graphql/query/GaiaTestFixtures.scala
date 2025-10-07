// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.Async
import cats.syntax.all.*
import org.http4s.*
import org.http4s.client.Client

object GaiaTestFixtures {

  /**
   * Create a mock HTTP client for Gaia queries that returns VOTable XML responses.
   * This is based on GaiaClientMock but exposes the underlying HTTP client directly.
   */
  def mockGaiaHttpClient[F[_]: Async](voTableXml: String): Client[F] =
    Client.fromHttpApp[F](HttpApp[F]: request =>
      val query           = request.uri.query.params.getOrElse("QUERY", "")
      val sourceIdPattern = """WHERE source_id = (\d+)""".r

      val responseXml = sourceIdPattern.findFirstMatchIn(query) match {
        case Some(m) =>
          val sourceId = m.group(1).toLong
          filterVoTableById(voTableXml, sourceId)
        case None    =>
          voTableXml
      }

      Response[F](Status.Ok).withEntity(responseXml).pure[F])

  /**
   * Filter VOTable XML to only include rows matching a specific source ID.
   */
  def filterVoTableById(voTableXml: String, sourceId: Long): String = {
    import scala.xml._

    val xml      = XML.loadString(voTableXml)
    val targetId = sourceId.toString

    val filteredRows = (xml \\ "TR").filter: tr =>
      (tr \ "TD").headOption.exists: td =>
        td.text === targetId || td.text.endsWith(targetId)

    val table = xml \\ "TABLE"
    if (table.isEmpty) {
      voTableXml
    } else {
      val fields        = table.head \ "FIELD"
      val filteredTable =
        <TABLE>
          {fields}
          <DATA>
            <TABLEDATA>
              {filteredRows}
            </TABLEDATA>
          </DATA>
        </TABLE>

      Utility.trim(filteredTable).toString
    }
  }

  val gaiaResponseWithStars: String =
    """<?xml version="1.0" encoding="UTF-8"?>
    |<VOTABLE version="1.4" xmlns="http://www.ivoa.net/xml/VOTable/v1.3" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.ivoa.net/xml/VOTable/v1.3 http://www.ivoa.net/xml/VOTable/v1.3">
    |    <RESOURCE type="results">
    |        <INFO name="QUERY_STATUS" value="OK" />
    |        <COOSYS ID="GAIADR3" epoch="J2016.0" system="ICRS" />
    |        <RESOURCE>
    |            <COOSYS ID="t14806478-coosys-1" epoch="J2016.0" system="ICRS"/>
    |        </RESOURCE>
    |        <TABLE>
    |            <FIELD datatype="long" name="source_id" ucd="meta.id">
    |                <DESCRIPTION>Unique source identifier (unique within a particular Data Release)</DESCRIPTION>
    |            </FIELD>
    |            <FIELD datatype="double" name="ra" ref="t14806478-coosys-1" ucd="pos.eq.ra;meta.main" unit="deg" utype="Char.SpatialAxis.Coverage.Location.Coord.Position2D.Value2.C1">
    |                <DESCRIPTION>Right ascension</DESCRIPTION>
    |            </FIELD>
    |            <FIELD datatype="double" name="pmra" ucd="pos.pm;pos.eq.ra" unit="mas.yr**-1">
    |                <DESCRIPTION>Proper motion in right ascension direction</DESCRIPTION>
    |            </FIELD>
    |            <FIELD datatype="double" name="dec" ref="t14806478-coosys-1" ucd="pos.eq.dec;meta.main" unit="deg" utype="Char.SpatialAxis.Coverage.Location.Coord.Position2D.Value2.C2">
    |                <DESCRIPTION>Declination</DESCRIPTION>
    |            </FIELD>
    |            <FIELD datatype="double" name="pmdec" ucd="pos.pm;pos.eq.dec" unit="mas.yr**-1">
    |                <DESCRIPTION>Proper motion in declination direction</DESCRIPTION>
    |            </FIELD>
    |            <FIELD datatype="double" name="parallax" ucd="pos.parallax.trig" unit="mas">
    |                <DESCRIPTION>Parallax</DESCRIPTION>
    |            </FIELD>
    |            <FIELD datatype="float" name="radial_velocity" ucd="spect.dopplerVeloc.opt;em.opt.I" unit="km.s**-1">
    |                <DESCRIPTION>Radial velocity</DESCRIPTION>
    |            </FIELD>
    |            <FIELD datatype="float" name="phot_g_mean_mag" ucd="phot.mag;em.opt" unit="mag">
    |                <DESCRIPTION>G-band mean magnitude</DESCRIPTION>
    |            </FIELD>
    |            <FIELD datatype="float" name="phot_rp_mean_mag" ucd="phot.mag;em.opt.R" unit="mag">
    |                <DESCRIPTION>Integrated RP mean magnitude</DESCRIPTION>
    |            </FIELD>
    |            <DATA>
    |                <TABLEDATA>
    |                    <TR>
    |                        <TD>3219118090462918016</TD>
    |                        <TD>86.5934741222927</TD>
    |                        <TD>0.43862335651876644</TD>
    |                        <TD>-0.14795707230703778</TD>
    |                        <TD>-0.7414519014405084</TD>
    |                        <TD>2.4315367202517466</TD>
    |                        <TD>10.090042</TD>
    |                        <TD>14.204175</TD>
    |                        <TD>13.172072</TD>
    |                    </TR>
    |                    <TR>
    |                        <TD>3219142829474535424</TD>
    |                        <TD>86.59328782338685</TD>
    |                        <TD>-10.331736040138617</TD>
    |                        <TD>-0.06075629321549123</TD>
    |                        <TD>-29.666695525022078</TD>
    |                        <TD>2.496796996582742</TD>
    |                        <TD></TD>
    |                        <TD>15.189251</TD>
    |                        <TD>14.364465</TD>
    |                    </TR>
    |                    <TR>
    |                        <TD>3219118640218737920</TD>
    |                        <TD>86.50103315602114</TD>
    |                        <TD>0.8061180282403659</TD>
    |                        <TD>-0.1406363314163743</TD>
    |                        <TD>-1.0936840305376552</TD>
    |                        <TD>2.3714995175435116</TD>
    |                        <TD></TD>
    |                        <TD>15.209204</TD>
    |                        <TD>13.883842</TD>
    |                    </TR>
    |                </TABLEDATA>
    |            </DATA>
    |        </TABLE>
    |    </RESOURCE>
    |</VOTABLE>""".stripMargin

  val gaiaEmptyResponse: String =
    """<?xml version="1.0" encoding="UTF-8"?>
    |<VOTABLE version="1.4" xmlns="http://www.ivoa.net/xml/VOTable/v1.3" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.ivoa.net/xml/VOTable/v1.3 http://www.ivoa.net/xml/VOTable/v1.3">
    |    <RESOURCE type="results">
    |        <INFO name="QUERY_STATUS" value="OK" />
    |        <COOSYS ID="GAIADR3" epoch="J2016.0" system="ICRS" />
    |        <RESOURCE>
    |            <COOSYS ID="t14806478-coosys-1" epoch="J2016.0" system="ICRS"/>
    |        </RESOURCE>
    |        <TABLE>
    |            <FIELD datatype="long" name="source_id" ucd="meta.id">
    |                <DESCRIPTION>Unique source identifier (unique within a particular Data Release)</DESCRIPTION>
    |            </FIELD>
    |            <FIELD datatype="double" name="ra" ref="t14806478-coosys-1" ucd="pos.eq.ra;meta.main" unit="deg" utype="Char.SpatialAxis.Coverage.Location.Coord.Position2D.Value2.C1">
    |                <DESCRIPTION>Right ascension</DESCRIPTION>
    |            </FIELD>
    |            <FIELD datatype="double" name="pmra" ucd="pos.pm;pos.eq.ra" unit="mas.yr**-1">
    |                <DESCRIPTION>Proper motion in right ascension direction</DESCRIPTION>
    |            </FIELD>
    |            <FIELD datatype="double" name="dec" ref="t14806478-coosys-1" ucd="pos.eq.dec;meta.main" unit="deg" utype="Char.SpatialAxis.Coverage.Location.Coord.Position2D.Value2.C2">
    |                <DESCRIPTION>Declination</DESCRIPTION>
    |            </FIELD>
    |            <FIELD datatype="double" name="pmdec" ucd="pos.pm;pos.eq.dec" unit="mas.yr**-1">
    |                <DESCRIPTION>Proper motion in declination direction</DESCRIPTION>
    |            </FIELD>
    |            <FIELD datatype="double" name="parallax" ucd="pos.parallax.trig" unit="mas">
    |                <DESCRIPTION>Parallax</DESCRIPTION>
    |            </FIELD>
    |            <FIELD datatype="float" name="radial_velocity" ucd="spect.dopplerVeloc.opt;em.opt.I" unit="km.s**-1">
    |                <DESCRIPTION>Radial velocity</DESCRIPTION>
    |            </FIELD>
    |            <FIELD datatype="float" name="phot_g_mean_mag" ucd="phot.mag;em.opt" unit="mag">
    |                <DESCRIPTION>G-band mean magnitude</DESCRIPTION>
    |            </FIELD>
    |            <FIELD datatype="float" name="phot_rp_mean_mag" ucd="phot.mag;em.opt.R" unit="mag">
    |                <DESCRIPTION>Integrated RP mean magnitude</DESCRIPTION>
    |            </FIELD>
    |            <DATA>
    |                <TABLEDATA>
    |                </TABLEDATA>
    |            </DATA>
    |        </TABLE>
    |    </RESOURCE>
    |</VOTABLE>""".stripMargin
}
