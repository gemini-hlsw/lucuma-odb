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

class guideEnvironment extends OdbSuite with ObservingModeSetupOperations {

  val pi         = TestUsers.Standard.pi(1, 30)
  val validUsers = List(pi)

  val aug2023 = "2023-08-30T00:00:00Z"
  val aug3000 = "3000-08-30T00:00:00Z"

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
  |                    <TR>
  |                        <TD>3219146093649682944</TD>
  |                        <TD>86.53743300349264</TD>
  |                        <TD>0.5692648114700383</TD>
  |                        <TD>-0.0376789160090541</TD>
  |                        <TD>-1.1071638747425834</TD>
  |                        <TD>2.341690506609991</TD>
  |                        <TD></TD>
  |                        <TD>15.799698</TD>
  |                        <TD>14.392714</TD>
  |                    </TR>
  |                    <TR>
  |                        <TD>3219119189974545024</TD>
  |                        <TD>86.55474182136356</TD>
  |                        <TD>0.9180742337760139</TD>
  |                        <TD>-0.10136166840483617</TD>
  |                        <TD>-1.0570607348960717</TD>
  |                        <TD>2.4220089902661854</TD>
  |                        <TD></TD>
  |                        <TD>17.311058</TD>
  |                        <TD>15.646823</TD>
  |                    </TR>
  |                    <TR>
  |                        <TD>3219118051807091968</TD>
  |                        <TD>86.58944677577558</TD>
  |                        <TD>15.866429710516812</TD>
  |                        <TD>-0.15207273400511523</TD>
  |                        <TD>-10.065610870235203</TD>
  |                        <TD>3.0202124909661983</TD>
  |                        <TD></TD>
  |                        <TD>17.745539</TD>
  |                        <TD>16.56105</TD>
  |                    </TR>
  |                    <TR>
  |                        <TD>3219123240127693440</TD>
  |                        <TD>86.49991695637847</TD>
  |                        <TD>-0.6885542807840347</TD>
  |                        <TD>-0.05177206898508793</TD>
  |                        <TD>0.9481435397955559</TD>
  |                        <TD>-0.15653933934116532</TD>
  |                        <TD></TD>
  |                        <TD>17.92497</TD>
  |                        <TD>16.582914</TD>
  |                    </TR>
  |                    <TR>
  |                        <TD>3219142859538567040</TD>
  |                        <TD>86.62125043949084</TD>
  |                        <TD>1.938773829505391</TD>
  |                        <TD>-0.06343972217577211</TD>
  |                        <TD>-3.1719765448158337</TD>
  |                        <TD>1.4387020523568086</TD>
  |                        <TD></TD>
  |                        <TD>18.051426</TD>
  |                        <TD>16.399946</TD>
  |                    </TR>
  |                    <TR>
  |                        <TD>3219146054993993088</TD>
  |                        <TD>86.56116636793416</TD>
  |                        <TD>-22.496969846693275</TD>
  |                        <TD>-0.0366983015234665</TD>
  |                        <TD>-9.711260712719115</TD>
  |                        <TD>2.5018955357529165</TD>
  |                        <TD></TD>
  |                        <TD>18.473146</TD>
  |                        <TD>17.210957</TD>
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

  def guideEnvironmentQuery(oid: Observation.Id, obsTime: String) =
    s"""
      query {
        observation(observationId: "$oid") {
          title
          targetEnvironment {
            guideEnvironment(observationTime: "$obsTime") {
              posAngle {
                degrees
              }
              guideTargets {
                name
                probe
                sourceProfile {
                  point {
                    bandNormalized {
                      brightnesses {
                        band
                      }
                    }
                  }
                }
                sidereal {
                  catalogInfo {
                    name
                    id
                    objectType
                  }
                  epoch
                  ra {
                    microseconds
                    hms
                    hours
                    degrees
                  }
                  dec {
                    dms
                    degrees
                    microarcseconds
                  }
                  radialVelocity {
                    metersPerSecond
                    centimetersPerSecond
                    kilometersPerSecond
                  }
                  properMotion {
                    ra {
                      microarcsecondsPerYear
                      milliarcsecondsPerYear
                    }
                    dec {
                      microarcsecondsPerYear
                      milliarcsecondsPerYear
                    }
                  }
                  parallax {
                    microarcseconds
                    milliarcseconds
                  }
                }
                nonsidereal {
                  des
                }
              }
            }
          }
        }
      }
    """

  val guideEnvironmentResults =
    json"""
    {
      "observation": {
        "title": "V1647 Orionis",
        "targetEnvironment": {
          "guideEnvironment": {
            "posAngle": {
              "degrees": 160.000000
            },
            "guideTargets": [
              {
                "name": "Gaia DR3 3219118090462918016",
                "probe": "GMOS_OIWFS",
                "sourceProfile": {
                  "point": {
                    "bandNormalized": {
                      "brightnesses": [
                        {
                          "band": "GAIA"
                        }
                      ]
                    }
                  }
                },
                "sidereal": {
                  "catalogInfo": {
                    "name": "GAIA",
                    "id": "3219118090462918016",
                    "objectType": null
                  },
                  "epoch": "J2023.660",
                  "ra": {
                    "microseconds": 20782434012,
                    "hms": "05:46:22.434012",
                    "hours": 5.772898336666666666666666666666667,
                    "degrees": 86.59347505
                  },
                  "dec": {
                    "dms": "-00:08:52.651136",
                    "degrees": 359.8520413511111,
                    "microarcseconds": 1295467348864
                  },
                  "radialVelocity": {
                    "metersPerSecond": 0,
                    "centimetersPerSecond": 0,
                    "kilometersPerSecond": 0
                  },
                  "properMotion": {
                    "ra": {
                      "microarcsecondsPerYear": 438,
                      "milliarcsecondsPerYear": 0.438
                    },
                    "dec": {
                      "microarcsecondsPerYear": -741,
                      "milliarcsecondsPerYear": -0.741
                    }
                  },
                  "parallax": {
                    "microarcseconds": 2432,
                    "milliarcseconds": 2.432
                  }
                },
                "nonsidereal": null
              }
            ]
          }
        }
      }
    }
    """

  override def httpRequestHandler: Request[IO] => Resource[IO, Response[IO]] =
    req => {
      val respStr =
        if (req.uri.renderString.contains("20-0.10137")) gaiaReponseString else gaiaEmptyReponseString
      Resource.eval(IO.pure(Response(body = Stream(respStr).through(utf8.encode))))
    }

  test("successfully obtain guide environment") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      } yield o
    setup.flatMap { oid =>
      expect(pi, guideEnvironmentQuery(oid, aug2023), expected = guideEnvironmentResults.asRight)
    }
  }

  test("no science targets") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List.empty)
      } yield o
    setup.flatMap { oid =>
      expect(pi, guideEnvironmentQuery(oid, aug2023),
      expected = List(s"No targets have been defined for observation $oid.").asLeft)
    }
  }

  test("no guide stars") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      } yield o
    setup.flatMap { oid =>
      expect(pi, guideEnvironmentQuery(oid, aug3000),
      expected = List(s"Error calling Gaia: 'No valid guide star candidates were returned for observation $oid.'").asLeft)
    }
  }
}