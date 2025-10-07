// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import lucuma.ags.GuideStarName
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import org.http4s.HttpApp
import org.http4s.Response
import org.http4s.Status
import org.http4s.client.Client

class guideEnvironment extends ExecutionTestSupportForGmos {

  val gaiaSuccess = Timestamp.FromString.getOption("2023-08-30T00:00:00Z").get
  val gaiaEmpty   = Timestamp.FromString.getOption("3000-01-30T04:00:00Z").get
  // for tests in which gaia should not get called
  val gaiaError   = Timestamp.FromString.getOption("4000-12-30T20:00:00Z").get

  val setupTime = TimeSpan.fromMinutes(16).get
  val fullTimeEstimate = TimeSpan.parse("PT36M1.8S").toOption.get
  val durationTooShort = setupTime -| TimeSpan.fromMicroseconds(1).get
  val durationTooLong = fullTimeEstimate +| TimeSpan.fromMicroseconds(1).get

  // For tests in which the actual value of the duration is not validated because
  // the execution digest cannot be calculated.
  val durationNotValidated = TimeSpan.Zero

  val invalidTargetId = 1L
  val defaultTargetId = 3219118090462918016L
  val otherTargetId = 3219142829474535424L

  val invalidTargetName: String = GuideStarName.gaiaSourceId.reverseGet(invalidTargetId).value.value
  val defaultTargetName: String = GuideStarName.gaiaSourceId.reverseGet(defaultTargetId).value.value
  val otherTargetName: String = GuideStarName.gaiaSourceId.reverseGet(otherTargetId).value.value

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
  |                </TABLEDATA>
  |            </DATA>
  |        </TABLE>
  |    </RESOURCE>
  |</VOTABLE>""".stripMargin

  def guideEnvironmentQuery(oid: Observation.Id) =
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

  val emptyGuideEnvironmentResults =
    json"""
    {
      "observation": {
        "title": "V1647 Orionis",
        "targetEnvironment": {
          "guideEnvironments": null
        }
      }
    }
    """.asRight

  val defaultGuideEnvironmentResults =
    json"""
    {
      "observation": {
        "title": "V1647 Orionis",
        "targetEnvironment": {
          "guideEnvironment": {
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
    """.asRight

  val otherGuideEnvironmentResults =
    json"""
    {
      "observation": {
        "title": "V1647 Orionis",
        "targetEnvironment": {
          "guideEnvironment": {
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
                  "epoch": "J2023.660",
                  "ra": {
                    "microseconds": 20782383800,
                    "hms": "05:46:22.383800",
                    "hours": 5.772884388888888888888888888888889,
                    "degrees": 86.59326583333333333333333333333333
                  },
                  "dec": {
                    "dms": "-00:03:38.949911",
                    "degrees": 359.93918058027776,
                    "microarcseconds": 1295781050089
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
        }
      }
    }
    """.asRight

  override def httpClient: Client[IO] =
    Client.fromHttpApp[IO](HttpApp[IO]: request =>
      val renderStr = request.uri.renderString
      val query = request.uri.query.params.getOrElse("QUERY", "")

      // Check for source_id-based queries
      val sourceIdPattern = """WHERE source_id = (\d+)""".r
      val voTableXml = sourceIdPattern.findFirstMatchIn(query) match {
        case Some(m) =>
          val sourceId = m.group(1).toLong
          GaiaTestFixtures.filterVoTableById(gaiaReponseString, sourceId)
        case None =>
          // Check for coordinate-based queries
          if (renderStr.contains("-0.10137")) gaiaReponseString
          else GaiaTestFixtures.gaiaEmptyResponse
      }

      Response[IO](Status.Ok).withEntity(voTableXml).pure[IO])

  private def createObservationAs(user: User, pid: Program.Id, tids: List[Target.Id]): IO[Observation.Id] =
    createGmosNorthLongSlitObservationAs(user, pid, tids, offsetArcsec = List(0, 15).some)

  test("no science targets") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        o <- createObservationAs(pi, p, List.empty)
        _ <- setObservationTimeAndDuration(pi, o, gaiaError.some, durationNotValidated.some)
      } yield o
    setup.flatMap { oid =>
      expect(
        pi,
        guideEnvironmentQuery(oid),
        expected = List(s"No targets have been defined for observation $oid.").asLeft)
    }
  }

  test("no observation time") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
      } yield o
    setup.flatMap { oid =>
      expect(
        pi,
        guideEnvironmentQuery(oid),
        expected = List(s"Observation time not set for observation $oid.").asLeft)
    }
  }

  test("no observation duration") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
        _ <- setObservationTimeAndDuration(pi, o, gaiaError.some, none)
      } yield o
    setup.flatMap { oid =>
      expect(
        pi,
        guideEnvironmentQuery(oid),
        expected = List(s"Observation duration not set for observation $oid.").asLeft)
    }
  }

  test("observation duration too short") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
        _ <- setObservationTimeAndDuration(pi, o, gaiaError.some, durationTooShort.some)
      } yield o
    setup.flatMap { oid =>
      expect(
        pi,
        guideEnvironmentQuery(oid),
        expected = List(s"Observation duration of ${durationTooShort.format} is less than the setup time of ${setupTime.format} for observation $oid.").asLeft)
    }
  }

  // Temporarily(?) disable check for too long of a duration for sc-5322
  // test("observation duration too long") {
  //   val setup: IO[Observation.Id] =
  //     for {
  //       p <- createProgramAs(pi)
  //       t <- createTargetWithProfileAs(pi, p)
  //       o <- createObservationAs(pi, p, List(t))
  //       _ <- setObservationTimeAndDuration(pi, o, gaiaError.some, durationTooLong.some)
  //     } yield o
  //   setup.flatMap { oid =>
  //     expect(
  //       pi,
  //       guideEnvironmentQuery(oid),
  //       expected = List(s"Observation duration of ${durationTooLong.format} exceeds the remaining time of ${fullTimeEstimate.format} for observation $oid.").asLeft)
  //   }
  // }

  test("no guide stars") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
        _ <- setObservationTimeAndDuration(pi, o, gaiaEmpty.some, fullTimeEstimate.some)
      } yield o
    setup.flatMap { oid =>
      expect(
        pi,
        guideEnvironmentQuery(oid),
        expected = List("No potential guidestars found on Gaia.").asLeft)
    }
  }

  test("no configuration") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithNoModeAs(pi, p, t)
        _ <- setObservationTimeAndDuration(pi, o, gaiaError.some, durationNotValidated.some)
      } yield o
    setup.flatMap { oid =>
      expect(
        pi,
        guideEnvironmentQuery(oid),
        expected = List(s"Could not generate a sequence for $oid: observation is missing observing mode").asLeft)
    }
  }

  test("non-existent name set") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
        _ <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
        _ <- setGuideTargetName(pi, o, invalidTargetName.some)
      } yield o
    setup.flatMap { oid =>
      expect(
        pi,
        guideEnvironmentQuery(oid),
        expected = List(s"Error calling Gaia: Star with id $invalidTargetId not found on Gaia.").asLeft)
    }
  }

  test("no name set - successfully obtain guide environment") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
        _ <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
      } yield o
    setup.flatMap { oid =>
      expect(pi, guideEnvironmentQuery(oid), expected = defaultGuideEnvironmentResults)
    }
  }

  test("no name set - successfully obtain guide environment") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
        _ <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
      } yield o
    setup.flatMap { oid =>
      expect(pi, guideEnvironmentQuery(oid), expected = defaultGuideEnvironmentResults)
    }
  }

  test("name set to default - call gaia") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
        _ <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
        _ <- setGuideTargetName(pi, o, defaultTargetName.some)
      } yield o
    setup.flatMap { oid =>
      expect(pi, guideEnvironmentQuery(oid), expected = defaultGuideEnvironmentResults)
    }
  }

  test("name set to other usable - call gaia") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
        _ <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
        _ <- setGuideTargetName(pi, o, otherTargetName.some)
      } yield o
    setup.flatMap { oid =>
      expect(pi, guideEnvironmentQuery(oid), expected = otherGuideEnvironmentResults)
    }
  }

}
