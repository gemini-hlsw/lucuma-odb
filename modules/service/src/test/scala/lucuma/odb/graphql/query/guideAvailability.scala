// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import fs2.Stream
import fs2.text.utf8
import io.circe.Json
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
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.core.util.TimeSpan
import lucuma.odb.service.Services
import lucuma.odb.smartgcal.data.Gmos.GratingConfigKey
import lucuma.odb.smartgcal.data.Gmos.TableKey
import lucuma.odb.smartgcal.data.Gmos.TableRow
import lucuma.odb.smartgcal.data.SmartGcalValue
import lucuma.odb.smartgcal.data.SmartGcalValue.LegacyInstrumentConfig
import natchez.Trace.Implicits.noop
import org.http4s.Request
import org.http4s.Response
import skunk.Session

class guideAvailability extends OdbSuite with ObservingModeSetupOperations {

  val pi         = TestUsers.Standard.pi(1, 30)
  val validUsers = List(pi)

  val successStart = "2023-10-31T00:00:00Z"
  val successEnd   = "2024-02-28T00:00:00Z"

  val emptyStart   = "3025-10-31T00:00:00Z"
  val emptyEnd     = "3025-12-31T01:00:00Z"

  override def dbInitialization: Option[Session[IO] => IO[Unit]] = Some { s =>
    val tableRow: TableRow.North =
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
    val services                 = Services.forUser(pi /* doesn't matter*/ )(s)
    services.transactionally {
      services.smartGcalService.insertGmosNorth(1, tableRow)
    }
  }

  // increased the proper motion of the second candidate so we could get multiple availabilities.
  val gaiaReponseString = 
  """<?xml version="1.0" encoding="UTF-8"?>
  |<VOTABLE version="1.4" xmlns="http://www.ivoa.net/xml/VOTable/v1.3" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.ivoa.net/xml/VOTable/v1.3 http://www.ivoa.net/xml/VOTable/v1.3">
  |    <RESOURCE type="results">
  |        <INFO name="QUERY_STATUS" value="OK" />
  |        <INFO name="QUERY" value="SELECT TOP 100 source_id,ra,pmra,dec,pmdec,parallax,radial_velocity,phot_g_mean_mag,phot_rp_mean_mag 
  |     FROM gaiadr3.gaia_source_lite
  |     WHERE CONTAINS(POINT(&#039;ICRS&#039;,ra,dec),CIRCLE(&#039;ICRS&#039;, 86.55474, -0.10137, 0.08167))=1
  |     and ((phot_rp_mean_mag &lt; 17.228) or (phot_g_mean_mag &lt; 17.228))
  |     and (ruwe &lt; 1.4)
  |     ORDER BY phot_g_mean_mag
  |      ">
  |            <![CDATA[SELECT TOP 100 source_id,ra,pmra,dec,pmdec,parallax,radial_velocity,phot_g_mean_mag,phot_rp_mean_mag 
  |     FROM gaiadr3.gaia_source_lite
  |     WHERE CONTAINS(POINT('ICRS',ra,dec),CIRCLE('ICRS', 86.55474, -0.10137, 0.08167))=1
  |     and ((phot_rp_mean_mag < 17.228) or (phot_g_mean_mag < 17.228))
  |     and (ruwe < 1.4)
  |     ORDER BY phot_g_mean_mag
  |      ]]>
  |        </INFO>
  |        <INFO name="CAPTION" value="How to cite and acknowledge Gaia: https://gea.esac.esa.int/archive/documentation/credits.html">
  |            <![CDATA[How to cite and acknowledge Gaia: https://gea.esac.esa.int/archive/documentation/credits.html]]>
  |        </INFO>
  |        <INFO name="CITATION" value="How to cite and acknowledge Gaia: https://gea.esac.esa.int/archive/documentation/credits.html" ucd="meta.bib">
  |            <![CDATA[How to cite and acknowledge Gaia: https://gea.esac.esa.int/archive/documentation/credits.html]]>
  |        </INFO>
  |        <INFO name="PAGE" value="" />
  |        <INFO name="PAGE_SIZE" value="" />
  |        <INFO name="JOBID" value="1693412462905O">
  |            <![CDATA[1693412462905O]]>
  |        </INFO>
  |        <INFO name="JOBNAME" value="" />
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
  |                        <TD>3219118640218737920</TD>
  |                        <TD>86.50103315602114</TD>
  |                        <TD>-800.8061180282403659</TD>
  |                        <TD>-0.1406363314163743</TD>
  |                        <TD>10000.0936840305376552</TD>
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

  val gaiaEmptyReponseString = 
  """<?xml version="1.0" encoding="UTF-8"?>
  |<VOTABLE version="1.4" xmlns="http://www.ivoa.net/xml/VOTable/v1.3" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.ivoa.net/xml/VOTable/v1.3 http://www.ivoa.net/xml/VOTable/v1.3">
  |    <RESOURCE type="results">
  |        <INFO name="QUERY_STATUS" value="OK" />
  |        <INFO name="QUERY" value="SELECT TOP 100 source_id,ra,pmra,dec,pmdec,parallax,radial_velocity,phot_g_mean_mag,phot_rp_mean_mag 
  |     FROM gaiadr3.gaia_source_lite
  |     WHERE CONTAINS(POINT(&#039;ICRS&#039;,ra,dec),CIRCLE(&#039;ICRS&#039;, 86.55474, -0.10137, 0.08167))=1
  |     and ((phot_rp_mean_mag &lt; 17.228) or (phot_g_mean_mag &lt; 17.228))
  |     and (ruwe &lt; 1.4)
  |     ORDER BY phot_g_mean_mag
  |      ">
  |            <![CDATA[SELECT TOP 100 source_id,ra,pmra,dec,pmdec,parallax,radial_velocity,phot_g_mean_mag,phot_rp_mean_mag 
  |     FROM gaiadr3.gaia_source_lite
  |     WHERE CONTAINS(POINT('ICRS',ra,dec),CIRCLE('ICRS', 86.55474, -0.10137, 0.08167))=1
  |     and ((phot_rp_mean_mag < 17.228) or (phot_g_mean_mag < 17.228))
  |     and (ruwe < 1.4)
  |     ORDER BY phot_g_mean_mag
  |      ]]>
  |        </INFO>
  |        <INFO name="CAPTION" value="How to cite and acknowledge Gaia: https://gea.esac.esa.int/archive/documentation/credits.html">
  |            <![CDATA[How to cite and acknowledge Gaia: https://gea.esac.esa.int/archive/documentation/credits.html]]>
  |        </INFO>
  |        <INFO name="CITATION" value="How to cite and acknowledge Gaia: https://gea.esac.esa.int/archive/documentation/credits.html" ucd="meta.bib">
  |            <![CDATA[How to cite and acknowledge Gaia: https://gea.esac.esa.int/archive/documentation/credits.html]]>
  |        </INFO>
  |        <INFO name="PAGE" value="" />
  |        <INFO name="PAGE_SIZE" value="" />
  |        <INFO name="JOBID" value="1693412462905O">
  |            <![CDATA[1693412462905O]]>
  |        </INFO>
  |        <INFO name="JOBNAME" value="" />
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

  def guideAvailabilityQuery(oid: Observation.Id, startTime: String, endTime: String) =
    s"""
      query {
        observation(observationId: "$oid") {
          title
          targetEnvironment {
            guideAvailability(start: "$startTime", end: "$endTime") {
              start
              end
              posAngles { degrees }
            }
          }
        }
      }
    """

  val emptyGuideAvailabilityResults =
    json"""
    {
      "observation": {
        "title": "V1647 Orionis",
        "targetEnvironment": {
          "guideAvailability": [
            {
              "start" : "3025-10-31 00:00:00",
              "end" : "3025-12-31 01:00:00",
              "posAngles" : [
              ]
            }
          ]
        }
      }
    }
    """

  val guideAvailabilityResults =
    json"""
    {
      "observation": {
        "title": "V1647 Orionis",
        "targetEnvironment": {
          "guideAvailability": [
            {
              "start" : "2023-10-31 00:00:00",
              "end" : "2024-02-01 00:00:00",
              "posAngles" : [
                {
                  "degrees" : 160.000000
                },
                {
                  "degrees" : 170.000000
                },
                {
                  "degrees" : 180.000000
                },
                {
                  "degrees" : 190.000000
                },
                {
                  "degrees" : 200.000000
                },
                {
                  "degrees" : 250.000000
                },
                {
                  "degrees" : 260.000000
                },
                {
                  "degrees" : 270.000000
                },
                {
                  "degrees" : 280.000000
                },
                {
                  "degrees" : 290.000000
                },
                {
                  "degrees" : 300.000000
                },
                {
                  "degrees" : 310.000000
                }
              ]
            },
            {
              "start" : "2024-02-01 00:00:00",
              "end" : "2024-02-28 00:00:00",
              "posAngles" : [
                {
                  "degrees" : 160.000000
                },
                {
                  "degrees" : 170.000000
                },
                {
                  "degrees" : 180.000000
                },
                {
                  "degrees" : 190.000000
                },
                {
                  "degrees" : 200.000000
                },
                {
                  "degrees" : 260.000000
                },
                {
                  "degrees" : 270.000000
                },
                {
                  "degrees" : 280.000000
                },
                {
                  "degrees" : 290.000000
                },
                {
                  "degrees" : 300.000000
                },
                {
                  "degrees" : 310.000000
                }
              ]
            } 
          ]
        }
      }
    }
    """

  override def httpRequestHandler: Request[IO] => Resource[IO, Response[IO]] =
    req => {
      val respStr =
        if (req.uri.renderString.contains("20-0.10137")) gaiaReponseString
        else gaiaEmptyReponseString
      Resource.eval(IO.pure(Response(body = Stream(respStr).through(utf8.encode))))
    }

  test("successfully obtain guide availability") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      } yield o
    setup.flatMap { oid =>
      expect(pi, guideAvailabilityQuery(oid, successStart, successEnd), expected = guideAvailabilityResults.asRight)
    }
  }

  test("no science targets") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List.empty)
      } yield o
    setup.flatMap { oid =>
      expect(pi, guideAvailabilityQuery(oid, successStart, successEnd),
      expected = List(s"Could not generate a sequence from the observation $oid: target").asLeft)
    }
  }

  test("no guide availability") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      } yield o
    setup.flatMap { oid =>
      expect(pi, guideAvailabilityQuery(oid, emptyStart, emptyEnd), expected = emptyGuideAvailabilityResults.asRight)
    }
  }

  test("no configuration") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithNoModeAs(pi, p, t)
      } yield o
    setup.flatMap { oid =>
      expect(pi, guideAvailabilityQuery(oid, successStart, successEnd),
      expected = List(s"Could not generate a sequence from the observation $oid: observing mode").asLeft)
    }
  }
}
