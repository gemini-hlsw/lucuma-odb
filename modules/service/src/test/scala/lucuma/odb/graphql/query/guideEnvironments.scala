// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*
import fs2.Stream
import fs2.text.utf8
import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import org.http4s.Request
import org.http4s.Response

class guideEnvironments extends ExecutionTestSupportForGmos with GuideEnvironmentSuite {

  val aug2023 = "2023-08-30T00:00:00Z"
  val aug3000 = "3000-08-30T00:00:00Z"

  override val gaiaResponseString =
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
  |                </TABLEDATA>
  |            </DATA>
  |        </TABLE>
  |    </RESOURCE>
  |</VOTABLE>""".stripMargin

  override val gaiaEmptyReponseString =
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
            guideEnvironments(observationTime: "$obsTime") {
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

  val emptyGuideEnvironmentResults =
    json"""
    {
      "observation": {
        "title": "V1647 Orionis",
        "targetEnvironment": {
          "guideEnvironments": [
          ]
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
          "guideEnvironments": [
            {
              "posAngle": {
                "degrees": 180.000000
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
                            "band": "GAIA_RP"
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
                    "epoch": "J2023.659",
                    "ra": {
                      "microseconds": 20782434012,
                      "hms": "05:46:22.434012",
                      "hours": 5.772898336666666666666666666666667,
                      "degrees": 86.59347505
                    },
                    "dec": {
                      "dms": "-00:08:52.651135",
                      "degrees": 359.8520413513889,
                      "microarcseconds": 1295467348865
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
            },
            {
              "posAngle": {
                "degrees": 270.000000
              },
              "guideTargets": [
                {
                  "name": "Gaia DR3 3219118640218737920",
                  "probe": "GMOS_OIWFS",
                  "sourceProfile": {
                    "point": {
                      "bandNormalized": {
                        "brightnesses": [
                          {
                            "band": "GAIA_RP"
                          }
                        ]
                      }
                    }
                  },
                  "sidereal": {
                    "catalogInfo": {
                      "name": "GAIA",
                      "id": "3219118640218737920",
                      "objectType": null
                    },
                    "epoch": "J2023.659",
                    "ra": {
                      "microseconds": 20760248368,
                      "hms": "05:46:00.248368",
                      "hours": 5.766735657777777777777777777777778,
                      "degrees": 86.50103486666666666666666666666667
                    },
                    "dec": {
                      "dms": "-00:08:26.299164",
                      "degrees": 359.8593613433333,
                      "microarcseconds": 1295493700836
                    },
                    "radialVelocity": {
                      "metersPerSecond": 0,
                      "centimetersPerSecond": 0,
                      "kilometersPerSecond": 0
                    },
                    "properMotion": {
                      "ra": {
                        "microarcsecondsPerYear": 806,
                        "milliarcsecondsPerYear": 0.806
                      },
                      "dec": {
                        "microarcsecondsPerYear": -1093,
                        "milliarcsecondsPerYear": -1.093
                      }
                    },
                    "parallax": {
                      "microarcseconds": 2371,
                      "milliarcseconds": 2.371
                    }
                  },
                  "nonsidereal": null
                }
              ]
            },
            {
              "posAngle": {
                "degrees": 100.000000
              },
              "guideTargets": [
                {
                  "name": "Gaia DR3 3219142829474535424",
                  "probe": "GMOS_OIWFS",
                  "sourceProfile": {
                    "point": {
                      "bandNormalized": {
                        "brightnesses": [
                          {
                            "band": "GAIA_RP"
                          }
                        ]
                      }
                    }
                  },
                  "sidereal": {
                    "catalogInfo": {
                      "name": "GAIA",
                      "id": "3219142829474535424",
                      "objectType": null
                    },
                    "epoch": "J2023.659",
                    "ra": {
                      "microseconds": 20782383801,
                      "hms": "05:46:22.383801",
                      "hours": 5.772884389166666666666666666666667,
                      "degrees": 86.5932658375
                    },
                    "dec": {
                      "dms": "-00:03:38.949866",
                      "degrees": 359.9391805927778,
                      "microarcseconds": 1295781050134
                    },
                    "radialVelocity": {
                      "metersPerSecond": 0,
                      "centimetersPerSecond": 0,
                      "kilometersPerSecond": 0
                    },
                    "properMotion": {
                      "ra": {
                        "microarcsecondsPerYear": -10331,
                        "milliarcsecondsPerYear": -10.331
                      },
                      "dec": {
                        "microarcsecondsPerYear": -29666,
                        "milliarcsecondsPerYear": -29.666
                      }
                    },
                    "parallax": {
                      "microarcseconds": 2497,
                      "milliarcseconds": 2.497
                    }
                  },
                  "nonsidereal": null
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
        if (req.uri.renderString.contains("20-0.10137")) gaiaResponseString else gaiaEmptyReponseString
      Resource.eval(IO.pure(Response(body = Stream(respStr).through(utf8.encode))))
    }

  override def createObservationAs(
    user: User,
    pid: Program.Id,
    tids: List[Target.Id]
  ): IO[Observation.Id] =
    createGmosNorthLongSlitObservationAs(user, pid, tids, offsetArcsec = List(0, 15).some)

  test("successfully obtain guide environment") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t), offsetArcsec = List(0, 15).some)
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
      expected = List(s"Could not generate a sequence for $oid: observation is missing target").asLeft)
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
      expect(pi, guideEnvironmentQuery(oid, aug3000), expected = emptyGuideEnvironmentResults.asRight)
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
      expect(pi, guideEnvironmentQuery(oid, aug3000),
      expected = List(s"Could not generate a sequence for $oid: observation is missing observing mode").asLeft)
    }
  }
}
