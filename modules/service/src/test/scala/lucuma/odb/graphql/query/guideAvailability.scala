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
import io.circe.syntax.*
import lucuma.core.math.Angle
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.json.angle.query.given
import lucuma.odb.service.GuideService
import lucuma.odb.service.GuideService.AvailabilityPeriod
import org.http4s.Request
import org.http4s.Response

class guideAvailability extends ExecutionTestSupportForGmos with GuideEnvironmentSuite {

  val oct15_2023 = "2023-10-15T00:00:00Z"
  val oct25_2023 = "2023-10-25T00:00:00Z"
  val oct31_2023 = "2023-10-31T00:00:00Z"
  val nov30_2023 = "2023-11-30T00:00:00Z"
  val feb01_2024 = "2024-02-01T00:00:00Z"
  val feb28_2024 = "2024-02-28T00:00:00Z"
  val mar05_2024 = "2024-03-05T00:00:00Z"
  val mar10_2024 = "2024-03-10T00:00:00Z"

  val emptyStart   = "3025-10-31T00:00:00Z"
  val emptyEnd     = "3025-12-31T01:00:00Z"

  val earlyAngles = List(160, 170, 180, 190, 200, 250, 260, 270, 280, 290, 300, 310)
  val laterAngles = earlyAngles.filter(_ =!= 250)

  // increased the proper motion of the second candidate so we could get multiple availabilities.
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
  |     and ((phot_rp_mean_mag < 17.228) or (phot_g_mean_mag  17.228))
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
              posAngles {
                microarcseconds
                microseconds
                milliarcseconds
                milliseconds
                arcseconds
                seconds
                arcminutes
                minutes
                degrees
                hours
                dms
                hms
              }
            }
          }
        }
      }
    """

  def availabilityResult(title: String, periods: List[AvailabilityPeriod]): Json =
    Json.obj(
      "observation" -> Json.obj(
        "title"            -> title.asJson,
        "targetEnvironment" -> Json.obj(
          "guideAvailability" -> Json.fromValues(
            periods.map(ap =>
              Json.obj(
                "start"     -> ap.period.start.asJson,
                "end"       -> ap.period.end.asJson,
                "posAngles" -> ap.posAngles.asJson
              )
            )
          )
        )
      )
    )

  def makeAvailabilityPeriod(start: String, end: String, angles: List[Int]) =
    AvailabilityPeriod(
      TimestampInterval.between(
        Timestamp.FromString.getOption(start).get,
        Timestamp.FromString.getOption(end).get
      ),
      angles.map(i => Angle.fromBigDecimalDegrees(BigDecimal(i)))
    )

  override def httpRequestHandler: Request[IO] => Resource[IO, Response[IO]] =
    req => {
      val respStr =
        if (req.uri.renderString.contains("20-0.10166")) gaiaEmptyReponseString
        else gaiaResponseString
      Resource.eval(IO.pure(Response(body = Stream(respStr).through(utf8.encode))))
    }

  override def createObservationAs(user: User, pid: Program.Id, tids: List[Target.Id]): IO[Observation.Id] =
    createGmosNorthLongSlitObservationAs(user, pid, tids, offsetArcsec = List(0, 15).some)

  test("successfully obtain guide availability") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
      } yield o
    val periods = List(
      makeAvailabilityPeriod(oct31_2023, feb01_2024, earlyAngles),
      makeAvailabilityPeriod(feb01_2024, feb28_2024, laterAngles)
    )
    val expected = availabilityResult("V1647 Orionis", periods).asRight
    setup.flatMap { oid =>
      expect(pi, guideAvailabilityQuery(oid, oct31_2023, feb28_2024), expected = expected)
    }
  }

  test("no science targets") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List.empty)
      } yield o
    setup.flatMap { oid =>
      expect(pi, guideAvailabilityQuery(oid, oct31_2023, feb28_2024),
      expected = List(s"Could not generate a sequence for $oid: observation is missing target").asLeft)
    }
  }

  test("no guide availability") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      } yield o
    val periods = List(
      makeAvailabilityPeriod(emptyStart, emptyEnd, List.empty)
    )
    val expected = availabilityResult("V1647 Orionis", periods).asRight
    setup.flatMap { oid =>
      expect(pi, guideAvailabilityQuery(oid, emptyStart, emptyEnd), expected = expected)
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
      expect(pi, guideAvailabilityQuery(oid, oct31_2023, feb28_2024),
      expected = List(s"Could not generate a sequence for $oid: observation is missing observing mode").asLeft)
    }
  }

  test("range too big") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithNoModeAs(pi, p, t)
      } yield o
    setup.flatMap { oid =>
      expect(pi, guideAvailabilityQuery(oid, oct31_2023, emptyEnd),
      expected = List(s"Period for guide availability cannot be greater than ${GuideService.maxAvailabilityPeriodDays} days.").asLeft)
    }
  }

  // There isn't a lot of visibility into how the service is deciding what it needs to calculate
  // based on what it already has, but we can make sure the various scenarios don't break things.
  // Much of the decision behavior is encapsulated in ContiguousTimestampMap, which is well tested.

  test("extends both ends") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
      } yield o
    setup.flatMap { oid =>
      val periods1 = List(
        makeAvailabilityPeriod(oct31_2023, feb01_2024, earlyAngles),
        makeAvailabilityPeriod(feb01_2024, feb28_2024, laterAngles)
      )
      val expected1 = availabilityResult("V1647 Orionis", periods1).asRight
      val one = expect(pi, guideAvailabilityQuery(oid, oct31_2023, feb28_2024), expected = expected1)

      val periods2 = List(
        makeAvailabilityPeriod(oct25_2023, feb01_2024, earlyAngles),
        makeAvailabilityPeriod(feb01_2024, mar05_2024, laterAngles)
      )
      val expected2 = availabilityResult("V1647 Orionis", periods2).asRight
      val two = expect(pi, guideAvailabilityQuery(oid, oct25_2023, mar05_2024), expected = expected2)

      one >> two
    }
  }

  test("subset") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
      } yield o
    setup.flatMap { oid =>
      val periods1 = List(
        makeAvailabilityPeriod(oct31_2023, feb01_2024, earlyAngles),
        makeAvailabilityPeriod(feb01_2024, feb28_2024, laterAngles)
      )
      val expected1 = availabilityResult("V1647 Orionis", periods1).asRight
      val one = expect(pi, guideAvailabilityQuery(oid, oct31_2023, feb28_2024), expected = expected1)

      val periods2 = List(
        makeAvailabilityPeriod(nov30_2023, feb01_2024, earlyAngles)
      )
      val expected2 = availabilityResult("V1647 Orionis", periods2).asRight
      val two = expect(pi, guideAvailabilityQuery(oid, nov30_2023, feb01_2024), expected = expected2)

      one >> two
    }
  }

  test("disjoint in front") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
      } yield o
    setup.flatMap { oid =>
      val periods1 = List(
        makeAvailabilityPeriod(oct31_2023, feb01_2024, earlyAngles),
        makeAvailabilityPeriod(feb01_2024, feb28_2024, laterAngles)
      )
      val expected1 = availabilityResult("V1647 Orionis", periods1).asRight
      val one = expect(pi, guideAvailabilityQuery(oid, oct31_2023, feb28_2024), expected = expected1)

      val periods2 = List(
        makeAvailabilityPeriod(oct15_2023, oct25_2023, earlyAngles)
      )
      val expected2 = availabilityResult("V1647 Orionis", periods2).asRight
      val two = expect(pi, guideAvailabilityQuery(oid, oct15_2023, oct25_2023), expected = expected2)

      one >> two
    }
  }

  test("abuts in front") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
      } yield o
    setup.flatMap { oid =>
      val periods1 = List(
        makeAvailabilityPeriod(oct31_2023, feb01_2024, earlyAngles),
        makeAvailabilityPeriod(feb01_2024, feb28_2024, laterAngles)
      )
      val expected1 = availabilityResult("V1647 Orionis", periods1).asRight
      val one = expect(pi, guideAvailabilityQuery(oid, oct31_2023, feb28_2024), expected = expected1)

      val periods2 = List(
        makeAvailabilityPeriod(oct15_2023, oct31_2023, earlyAngles)
      )
      val expected2 = availabilityResult("V1647 Orionis", periods2).asRight
      val two = expect(pi, guideAvailabilityQuery(oid, oct15_2023, oct31_2023), expected = expected2)

      one >> two
    }
  }

  test("overlaps at front") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
      } yield o
    setup.flatMap { oid =>
      val periods1 = List(
        makeAvailabilityPeriod(oct31_2023, feb01_2024, earlyAngles),
        makeAvailabilityPeriod(feb01_2024, feb28_2024, laterAngles)
      )
      val expected1 = availabilityResult("V1647 Orionis", periods1).asRight
      val one = expect(pi, guideAvailabilityQuery(oid, oct31_2023, feb28_2024), expected = expected1)

      val periods2 = List(
        makeAvailabilityPeriod(oct15_2023, nov30_2023, earlyAngles)
      )
      val expected2 = availabilityResult("V1647 Orionis", periods2).asRight
      val two = expect(pi, guideAvailabilityQuery(oid, oct15_2023, nov30_2023), expected = expected2)

      one >> two
    }
  }

  test("disjoint at end") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
      } yield o
    setup.flatMap { oid =>
      val periods1 = List(
        makeAvailabilityPeriod(oct31_2023, feb01_2024, earlyAngles),
        makeAvailabilityPeriod(feb01_2024, feb28_2024, laterAngles)
      )
      val expected1 = availabilityResult("V1647 Orionis", periods1).asRight
      val one = expect(pi, guideAvailabilityQuery(oid, oct31_2023, feb28_2024), expected = expected1)

      val periods2 = List(
        makeAvailabilityPeriod(mar05_2024, mar10_2024, laterAngles)
      )
      val expected2 = availabilityResult("V1647 Orionis", periods2).asRight
      val two = expect(pi, guideAvailabilityQuery(oid, mar05_2024, mar10_2024), expected = expected2)

      one >> two
    }
  }

  test("abuts at end") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
      } yield o
    setup.flatMap { oid =>
      val periods1 = List(
        makeAvailabilityPeriod(oct31_2023, feb01_2024, earlyAngles),
        makeAvailabilityPeriod(feb01_2024, feb28_2024, laterAngles)
      )
      val expected1 = availabilityResult("V1647 Orionis", periods1).asRight
      val one = expect(pi, guideAvailabilityQuery(oid, oct31_2023, feb28_2024), expected = expected1)

      val periods2 = List(
        makeAvailabilityPeriod(feb28_2024, mar10_2024, laterAngles)
      )
      val expected2 = availabilityResult("V1647 Orionis", periods2).asRight
      val two = expect(pi, guideAvailabilityQuery(oid, feb28_2024, mar10_2024), expected = expected2)

      one >> two
    }
  }

  test("overlaps at end") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
      } yield o
    setup.flatMap { oid =>
      val periods1 = List(
        makeAvailabilityPeriod(oct31_2023, feb01_2024, earlyAngles),
        makeAvailabilityPeriod(feb01_2024, feb28_2024, laterAngles)
      )
      val expected1 = availabilityResult("V1647 Orionis", periods1).asRight
      val one = expect(pi, guideAvailabilityQuery(oid, oct31_2023, feb28_2024), expected = expected1)

      val periods2 = List(
        makeAvailabilityPeriod(nov30_2023, feb01_2024, earlyAngles),
        makeAvailabilityPeriod(feb01_2024, mar05_2024, laterAngles)
      )
      val expected2 = availabilityResult("V1647 Orionis", periods2).asRight
      val two = expect(pi, guideAvailabilityQuery(oid, nov30_2023, mar05_2024), expected = expected2)

      one >> two
    }
  }

  test("disjoint and far away") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
      } yield o
    setup.flatMap { oid =>
      val periods1 = List(
        makeAvailabilityPeriod(oct31_2023, feb01_2024, earlyAngles),
        makeAvailabilityPeriod(feb01_2024, feb28_2024, laterAngles)
      )
      val expected1 = availabilityResult("V1647 Orionis", periods1).asRight
      val one = expect(pi, guideAvailabilityQuery(oid, oct31_2023, feb28_2024), expected = expected1)

      val periods2 = List(
        makeAvailabilityPeriod(emptyStart, emptyEnd, List.empty)
      )
      val expected2 = availabilityResult("V1647 Orionis", periods2).asRight
      val two = expect(pi, guideAvailabilityQuery(oid, emptyStart, emptyEnd), expected = expected2)

      one >> two
    }
  }

}
