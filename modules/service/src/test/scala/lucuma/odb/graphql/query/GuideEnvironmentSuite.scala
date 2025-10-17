// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import lucuma.ags.GuideStarName
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.Timestamp

trait GuideEnvironmentSuite extends ExecutionTestSupport:

  val gaiaSuccess: Timestamp = Timestamp.FromString.getOption("2023-08-30T00:00:00Z").get

  val defaultTargetId: Long = 3219118090462918016L
  val otherTargetId: Long = 3219142829474535424L

  val defaultTargetName: String =
    GuideStarName.gaiaSourceId.reverseGet(defaultTargetId).value.value
  val otherTargetName: String =
    GuideStarName.gaiaSourceId.reverseGet(otherTargetId).value.value

  // Gaia response with multiple candidate stars for AGS to choose from
  val gaiaResponseString: String =
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

  val gaiaDefaultNameResponseString: String =
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
  |                </TABLEDATA>
  |            </DATA>
  |        </TABLE>
  |    </RESOURCE>
  |</VOTABLE>""".stripMargin

  val gaiaOtherNameReponseString =
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

  def guideEnvironmentQuery(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: "$oid") {
          title
          targetEnvironment {
            guideEnvironment {
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

  def createObservationAs(user: User, pid: Program.Id, tids: List[Target.Id]): IO[Observation.Id]
