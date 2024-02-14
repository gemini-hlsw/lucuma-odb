// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegShort
import io.circe.ACursor
import io.circe.Decoder
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.CloudExtinction
import lucuma.core.enums.FocalPlane
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.ImageQuality
import lucuma.core.enums.ObsActiveStatus
import lucuma.core.enums.ObsStatus
import lucuma.core.enums.ScienceMode
import lucuma.core.enums.Site
import lucuma.core.enums.SpectroscopyCapabilities
import lucuma.core.math.Angle
import lucuma.core.model.GuestUser
import lucuma.core.model.Partner
import lucuma.core.model.Program
import lucuma.core.model.ServiceUser
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.StandardUser
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.model.sequence.gmos.binning.northSpectralBinning
import lucuma.core.util.Timestamp
import lucuma.odb.data.PosAngleConstraintMode
import lucuma.odb.graphql.input.CoordinatesInput
import lucuma.odb.service.AsterismService

import java.time.LocalDateTime
import scala.collection.immutable.SortedMap

class createObservation extends OdbSuite {

  extension (ac: ACursor)
    def downPath(p: String*): ACursor =
      p.foldLeft(ac) { (aCursor, field) => aCursor.downField(field) }

    def liftIO[A: Decoder]: IO[A] =
      ac.as[A].leftMap(f => new RuntimeException(f.message)).liftTo[IO]

    def downIO[A: Decoder](p: String*): IO[A] =
      downPath(p*).liftIO[A]

  val pi: StandardUser     = TestUsers.Standard.pi(nextId, nextId)
  val pi2: StandardUser    = TestUsers.Standard.pi(nextId, nextId)
  val pi3: StandardUser    = TestUsers.Standard.pi(nextId, nextId)
  val ngo: StandardUser    = TestUsers.Standard.ngo(nextId, nextId, Partner.Ca)
  val staff: StandardUser  = TestUsers.Standard.staff(nextId, nextId)
  val admin: StandardUser  = TestUsers.Standard.admin(nextId, nextId)
  val guest: GuestUser     = TestUsers.guest(nextId)
  val service: ServiceUser = TestUsers.service(nextId)

  lazy val validUsers: List[User] =
    List(pi, pi2, pi3, ngo, staff, admin, guest, service)

  test("[general] default name should be null") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
            }) {
              observation {
                program {
                  id
                }
              }
            }
          }
          """).flatMap { js =>
        val get = js.hcursor
          .downField("createObservation")
          .downField("observation")
          .downField("program")
          .downField("id")
          .as[Program.Id]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
        assertIO(get, pid)
      }
    }
  }

  test("[general] subtitle can't be empty") {
    createProgramAs(pi).flatMap { pid =>
      interceptGraphQL("Argument 'input.SET.subtitle' is invalid: string value must be non-empty.") {
        query(pi,
          s"""
            mutation {
              createObservation(input: {
                programId: ${pid.asJson}
                SET: {
                  subtitle: ""
                }
              }) {
                observation {
                  subtitle
                }
              }
            }
            """
        )
      }
    }
  }

  test("[general] created observation should have specified program as parent") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
            }) {
              observation {
                program {
                  id
                }
              }
            }
          }
          """).flatMap { js =>
        val get = js.hcursor
          .downField("createObservation")
          .downField("observation")
          .downField("program")
          .downField("id")
          .as[Program.Id]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
        assertIO(get, pid)
      }
    }
  }

  test("[general] created observation should have specified subtitle (non-null)") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                subtitle: "crunchy frog"
              }
            }) {
              observation {
                subtitle
              }
            }
          }
          """).flatMap { js =>
        val get = js.hcursor
          .downField("createObservation")
          .downField("observation")
          .downField("subtitle")
          .as[String]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
        assertIO(get, "crunchy frog")
      }
    }
  }

  test("[general] created observation should have specified subtitle (null)") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                subtitle: null
              }
            }) {
              observation {
                subtitle
              }
            }
          }
          """).flatMap { js =>
        val get = js.hcursor
          .downField("createObservation")
          .downField("observation")
          .downField("subtitle")
          .as[Option[String]]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
        assertIO(get, None)
      }
    }
  }

  test("[general] created observation should have specified status") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                status: FOR_REVIEW
              }
            }) {
              observation {
                status
              }
            }
          }
          """).flatMap { js =>
        val get = js.hcursor
          .downField("createObservation")
          .downField("observation")
          .downField("status")
          .as[ObsStatus]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
        assertIO(get, ObsStatus.ForReview)
      }
    }
  }

  test("[general] created observation should have specified visualization time") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        s"""
        mutation {
          createObservation(input: {
            programId: ${pid.asJson}
            SET: {
              visualizationTime: "2022-08-29 18:01:00"
            }
          }) {
            observation {
              visualizationTime
            }
          }
        }
        """).flatMap { js =>
        val get = js.hcursor
          .downField("createObservation")
          .downField("observation")
          .downField("visualizationTime")
          .as[Timestamp]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
        assertIO(get, Timestamp.fromLocalDateTime(LocalDateTime.of(2022, 8, 29, 18, 1, 0, 0)).get)
      }
    }
  }

  test("[general] created observation should have specified active status") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                activeStatus: INACTIVE
              }
            }) {
              observation {
                activeStatus
              }
            }
          }
          """).flatMap { js =>
        val get = js.hcursor
          .downField("createObservation")
          .downField("observation")
          .downField("activeStatus")
          .as[ObsActiveStatus]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
        assertIO(get, ObsActiveStatus.Inactive)
      }
    }
  }

  test("[general] created observation has no explicit base by default") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        s"""
        mutation {
          createObservation(input: {
            programId: ${pid.asJson}
          }) {
            observation {
              targetEnvironment {
                explicitBase {
                  ra { hms }
                  dec { dms }
                }
              }
            }
          }
        }
        """).map { js =>
        val get = js.hcursor
          .downField("createObservation")
          .downField("observation")
          .downField("targetEnvironment")
          .downField("explicitBase")
          .downField("ra")
          .failed
        assert(get, "Expected a failed cursor on ra")
      }
    }
  }

  test("[general] created observation should have specified explicit base") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        s"""
      mutation {
        createObservation(input: {
          programId: ${pid.asJson}
          SET: {
            targetEnvironment: {
              explicitBase: {
                ra: { hms: "1:00:00" }
                dec: { dms: "2:00:00" }
              }
            }
          }
        }) {
          observation {
            targetEnvironment {
              explicitBase {
                ra { hours }
                dec { degrees }
              }
            }
          }
        }
      }
      """).flatMap { js =>
        val c = js.hcursor
          .downField("createObservation")
          .downField("observation")
          .downField("targetEnvironment")
          .downField("explicitBase")

        val ra = c
          .downField("ra")
          .downField("hours")
          .as[Int]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]

        val dec = c
          .downField("dec")
          .downField("degrees")
          .as[Int]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]

        for {
          _ <- assertIO(ra, 1)
          _ <- assertIO(dec, 2)
        } yield ()
      }
    }
  }

  test("[general] both ra and dec are required to set an explicit base") {
    createProgramAs(pi).flatMap { pid =>
      interceptGraphQL("Argument 'input.SET.targetEnvironment.explicitBase' is invalid: Both ra and dec are required in order to specify a coordinate.") {
        query(pi,
          s"""
            mutation {
              createObservation(input: {
                programId: ${pid.asJson}
                SET: {
                  targetEnvironment: {
                    explicitBase: {
                      ra: { hms: "1:00:00" }
                    }
                  }
                }
              }) {
                observation {
                  targetEnvironment {
                    explicitBase {
                      ra { hours }
                    }
                  }
                }
              }
            }
            """
        )
      }
    }
  }

  test("[general] created observation should have specified cloud extinction") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                constraintSet: {
                  cloudExtinction: ONE_POINT_FIVE
                }
              }
            }) {
              observation {
                constraintSet {
                  cloudExtinction
                }
              }
            }
          }
          """).flatMap { js =>
        val get = js.hcursor
          .downField("createObservation")
          .downField("observation")
          .downField("constraintSet")
          .downField("cloudExtinction")
          .as[CloudExtinction]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
        assertIO(get, CloudExtinction.OnePointFive)
      }
    }
  }

  test("[general] created observation can default cloud extinction") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
            }) {
              observation {
                constraintSet {
                  cloudExtinction
                }
              }
            }
          }
          """).flatMap { js =>
        val get = js.hcursor
          .downField("createObservation")
          .downField("observation")
          .downField("constraintSet")
          .downField("cloudExtinction")
          .as[CloudExtinction]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
        assertIO(get, CloudExtinction.PointThree)
      }
    }
  }

    test("[general] created observation can default image quality") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
            }) {
              observation {
                constraintSet {
                  imageQuality
                }
              }
            }
          }
          """).flatMap { js =>
        val get = js.hcursor
          .downField("createObservation")
          .downField("observation")
          .downField("constraintSet")
          .downField("imageQuality")
          .as[ImageQuality]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
        assertIO(get, ImageQuality.PointEight)
      }
    }
  }

  test("[general] created observation should have specified air mass") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                constraintSet: {
                  elevationRange: {
                    airMass: {
                      min: "1.2"
                      max: "1.3"
                    }
                  }
                }
              }
            }) {
              observation {
                constraintSet {
                  elevationRange {
                    airMass {
                      min
                    }
                  }
                }
              }
            }
          }
          """).flatMap { js =>
        val get = js.hcursor
          .downField("createObservation")
          .downField("observation")
          .downField("constraintSet")
          .downField("elevationRange")
          .downField("airMass")
          .downField("min")
          .as[BigDecimal]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
        assertIO(get, BigDecimal("1.2"))
      }
    }
  }

  test("[general] created observation should have specified asterism") {
    def createObs(pid: Program.Id, t0: Target.Id, t1: Target.Id): IO[List[Target.Id]] =
      query(
        pi,
        s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                targetEnvironment: {
                  asterism: [ ${t0.asJson}, ${t1.asJson} ]
                }
              }
            }) {
              observation {
                targetEnvironment {
                  asterism {
                    id
                  }
                }
              }
            }
          }
        """.stripMargin
      ).map { js =>
        js
          .hcursor
          .downField("createObservation")
          .downField("observation")
          .downField("targetEnvironment")
          .downField("asterism")
          .values              // Option[Iterable[Json]]
          .toList              // List[Iterable[Json]]
          .flatMap(_.toList)   // List[Json]
          .flatMap(_.hcursor.downField("id").as[Target.Id].toOption.toList)
      }

    for {
      pid <- createProgramAs(pi)
      t0  <- createTargetAs(pi, pid, "Biff")
      t1  <- createTargetAs(pi, pid, "Henderson")
      res <- createObs(pid, t0, t1)
    } yield assertEquals(res, List(t0, t1))
  }

  test("[general] handle unknown target id") {

    val fakeTarget: Target.Id = Target.Id.fromLong(1).get

    def createObs(pid: Program.Id): IO[Unit] =
      interceptGraphQL(AsterismService.ForeignKeyViolationMessage(pid, NonEmptyList.one(fakeTarget)))(
        query(
          pi,
          s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                targetEnvironment: {
                  asterism: [ ${fakeTarget.asJson} ]
                }
              }
            }) {
              observation {
                targetEnvironment {
                  asterism {
                    id
                  }
                }
              }
            }
          }
        """.stripMargin
        )
      )

    for {
      pid <- createProgramAs(pi)
      _   <- createObs(pid)
    } yield ()
  }

  test("[general] created observation can default pos angle constraint mode") {
    createProgramAs(pi).flatMap { pid =>
      query(pi, s"""
        mutation {
          createObservation(input: {
            programId: ${pid.asJson}
          }) {
            observation {
              posAngleConstraint {
                mode
              }
            }
          }
        }
      """).flatMap { js =>
        val get = js.hcursor
          .downField("createObservation")
          .downField("observation")
          .downField("posAngleConstraint")
          .downField("mode")
          .as[PosAngleConstraintMode]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
        assertIO(get, PosAngleConstraintMode.Default)
      }
    }
  }

  test("[general] created observation can default pos angle constraint angle") {
    createProgramAs(pi).flatMap { pid =>
      query(pi, s"""
        mutation {
          createObservation(input: {
            programId: ${pid.asJson}
          }) {
            observation {
              posAngleConstraint {
                angle { degrees }
              }
            }
          }
        }
      """).flatMap { js =>
        val get = js.hcursor
          .downField("createObservation")
          .downField("observation")
          .downField("posAngleConstraint")
          .downField("angle")
          .downField("degrees")
          .as[BigDecimal]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
        assertIO(get, BigDecimal(0))
      }
    }
  }

  private def siteName(site: Site): String =
    site match {
      case Site.GN => "North"
      case Site.GS => "South"
    }

  private def createObsWithObservingMode(
    pid:      Program.Id,
    site:     Site,
    grating:  String,
    fpu:      String = "LONG_SLIT_0_25",
    iq:       ImageQuality = ImageQuality.TwoPointZero,
    asterism: List[Target.Id] = Nil
  ): String =
    s"""
      mutation {
        createObservation(input: {
          programId: ${pid.asJson}
          SET: {
            constraintSet: {
              imageQuality: ${iq.tag.toUpperCase}
            }
            observingMode: {
              gmos${siteName(site)}LongSlit: {
                grating: $grating
                filter: G_PRIME
                fpu: $fpu
                centralWavelength: {
                  nanometers: 234.56
                },
                explicitYBin: TWO
              }
            }
            targetEnvironment: {
              asterism: [ ${asterism.map(_.asJson).mkString(", ")} ]
            }
          }
        }) {
          observation {
            observingMode {
              gmos${siteName(site)}LongSlit {
                grating
                filter
                fpu
                centralWavelength {
                  nanometers
                }
                xBin,
                yBin,
                explicitYBin
                defaultYBin
                initialGrating
                initialFilter
                initialFpu
                initialCentralWavelength {
                  nanometers
                }
              }
            }
          }
        }
      }
    """

  test("[general] specify gmos north long slit observing mode at observation creation") {
    createProgramAs(pi).flatMap { pid =>
      query(pi, createObsWithObservingMode(pid, Site.GN, "B1200_G5301")).flatMap { js =>
        val longSlit = js.hcursor.downPath("createObservation", "observation", "observingMode", "gmosNorthLongSlit")

        assertIO(
          (longSlit.downIO[GmosNorthGrating]("grating"),
           longSlit.downIO[Option[GmosNorthFilter]]("filter"),
           longSlit.downIO[GmosNorthFpu]("fpu"),
           longSlit.downIO[Double]("centralWavelength", "nanometers"),
           longSlit.downIO[GmosXBinning]("xBin"),
           longSlit.downIO[GmosYBinning]("yBin"),
           longSlit.downIO[Option[GmosYBinning]]("explicitYBin"),
           longSlit.downIO[GmosYBinning]("defaultYBin"),
           longSlit.downIO[GmosNorthGrating]("initialGrating"),
           longSlit.downIO[Option[GmosNorthFilter]]("initialFilter"),
           longSlit.downIO[GmosNorthFpu]("initialFpu"),
           longSlit.downIO[Double]("initialCentralWavelength", "nanometers")
          ).tupled,
          (GmosNorthGrating.B1200_G5301,
           Some(GmosNorthFilter.GPrime),
           GmosNorthFpu.LongSlit_0_25,
           234.56,
           GmosXBinning.One,
           GmosYBinning.Two,
           Some(GmosYBinning.Two),
           GmosYBinning.One,
           GmosNorthGrating.B1200_G5301,
           Some(GmosNorthFilter.GPrime),
           GmosNorthFpu.LongSlit_0_25,
           234.56
          )
        )

      }
    }
  }

  test("[general] specify gmos north long slit with calculated xbin") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid, "Biff",
        """
          sourceProfile: {
            gaussian: {
              fwhm: {
                microarcseconds: 647200
              }
              spectralDefinition: {
                bandNormalized: {
                  sed: {
                    stellarLibrary: B5_III
                  }
                  brightnesses: []
                }
              }
            }
          }
        """.stripMargin
      ).flatMap { tid =>
        query(pi, createObsWithObservingMode(pid, Site.GN, "B1200_G5301", fpu = "LONG_SLIT_5_00", iq = ImageQuality.PointOne, asterism = List(tid))).flatMap { js =>
          val longSlit = js.hcursor.downPath("createObservation", "observation", "observingMode", "gmosNorthLongSlit")

          assertIO(
            (longSlit.downIO[GmosNorthGrating]("grating"),
              longSlit.downIO[Option[GmosNorthFilter]]("filter"),
              longSlit.downIO[GmosNorthFpu]("fpu"),
              longSlit.downIO[Double]("centralWavelength", "nanometers"),
              longSlit.downIO[GmosXBinning]("xBin"),
              longSlit.downIO[GmosYBinning]("yBin"),
              longSlit.downIO[Option[GmosYBinning]]("explicitYBin"),
              longSlit.downIO[GmosYBinning]("defaultYBin"),
              longSlit.downIO[GmosNorthGrating]("initialGrating"),
              longSlit.downIO[Option[GmosNorthFilter]]("initialFilter"),
              longSlit.downIO[GmosNorthFpu]("initialFpu"),
              longSlit.downIO[Double]("initialCentralWavelength", "nanometers")
            ).tupled,
            (GmosNorthGrating.B1200_G5301,
              Some(GmosNorthFilter.GPrime),
              GmosNorthFpu.LongSlit_5_00,
              234.56,
              northSpectralBinning(GmosNorthFpu.LongSlit_5_00, SourceProfile.Gaussian(Angle.fromMicroarcseconds(647200), SpectralDefinition.BandNormalized(none, SortedMap.empty)), ImageQuality.PointOne, GmosNorthGrating.B1200_G5301),
              GmosYBinning.Two,
              Some(GmosYBinning.Two),
              GmosYBinning.Two,
              GmosNorthGrating.B1200_G5301,
              Some(GmosNorthFilter.GPrime),
              GmosNorthFpu.LongSlit_5_00,
              234.56
            )
          )
        }
      }
    }
  }

  test("[general] specify gmos south long slit observing mode at observation creation") {
    createProgramAs(pi).flatMap { pid =>
      query(pi, createObsWithObservingMode(pid, Site.GS, "B1200_G5321")).flatMap { js =>
        val longSlit = js.hcursor.downPath("createObservation", "observation", "observingMode", "gmosSouthLongSlit")

        assertIO(
          (longSlit.downIO[GmosSouthGrating]("grating"),
           longSlit.downIO[Option[GmosSouthFilter]]("filter"),
           longSlit.downIO[GmosSouthFpu]("fpu"),
           longSlit.downIO[Double]("centralWavelength", "nanometers"),
           longSlit.downIO[GmosXBinning]("xBin"),
           longSlit.downIO[GmosYBinning]("yBin"),
           longSlit.downIO[Option[GmosYBinning]]("explicitYBin"),
           longSlit.downIO[GmosYBinning]("defaultYBin"),
           longSlit.downIO[GmosSouthGrating]("initialGrating"),
           longSlit.downIO[Option[GmosSouthFilter]]("initialFilter"),
           longSlit.downIO[GmosSouthFpu]("initialFpu"),
           longSlit.downIO[Double]("initialCentralWavelength", "nanometers")
          ).tupled,
          (GmosSouthGrating.B1200_G5321,
           Some(GmosSouthFilter.GPrime),
           GmosSouthFpu.LongSlit_0_25,
           234.56,
           GmosXBinning.One,
           GmosYBinning.Two,
           Some(GmosYBinning.Two),
           GmosYBinning.One,
           GmosSouthGrating.B1200_G5321,
           Some(GmosSouthFilter.GPrime),
           GmosSouthFpu.LongSlit_0_25,
           234.56
          )
        )

      }
    }
  }

  private def createObsWithObservingModeExplicit(pid: Program.Id, site: Site, grating: String): String =
    s"""
      mutation {
        createObservation(input: {
          programId: ${pid.asJson}
          SET: {
            observingMode: {
              gmos${siteName(site)}LongSlit: {
                grating: $grating
                filter: G_PRIME
                fpu: LONG_SLIT_0_25
                centralWavelength: {
                  nanometers: 234.56
                }
                explicitXBin: FOUR
                explicitYBin: FOUR
                explicitAmpReadMode: FAST
                explicitAmpGain: HIGH
                explicitRoi: CCD2
                explicitWavelengthDithers: [
                  { nanometers: -7.5},
                  { nanometers:  7.1},
                  { nanometers:  7.1},
                  { nanometers: -7.5}
                ],
                explicitSpatialOffsets: [
                  { arcseconds: -10.0 },
                  { arcseconds:  10.0 },
                  { arcseconds:  10.0 },
                  { arcseconds: -10.0 }
                ]
              }
            }
          }
        }) {
          observation {
            observingMode {
              gmos${siteName(site)}LongSlit {
                xBin
                explicitXBin
                defaultXBin
                yBin
                explicitYBin
                defaultYBin
                ampReadMode
                explicitAmpReadMode
                defaultAmpReadMode
                ampGain
                explicitAmpGain
                defaultAmpGain
                roi
                explicitRoi
                defaultRoi
                wavelengthDithers {
                  nanometers
                }
                explicitWavelengthDithers {
                  nanometers
                }
                defaultWavelengthDithers {
                  nanometers
                }
                spatialOffsets {
                  arcseconds
                }
                explicitSpatialOffsets {
                  microarcseconds
                  arcseconds
                }
                defaultSpatialOffsets {
                  arcseconds
                }
              }
            }
          }
        }
      }
    """

  private def testObservingModeExplicitParams(site: Site, grating: String): IO[Unit] = {
    createProgramAs(pi).flatMap { pid =>
      query(pi, createObsWithObservingModeExplicit(pid, site, grating)).flatMap { js =>
        val longSlit = js.hcursor.downPath("createObservation", "observation", "observingMode", s"gmos${siteName(site)}LongSlit")

        assertIO(
          (longSlit.downIO[GmosXBinning]("xBin"),
           longSlit.downIO[Option[GmosXBinning]]("explicitXBin"),
           longSlit.downIO[GmosXBinning]("defaultXBin"),
           longSlit.downIO[GmosYBinning]("yBin"),
           longSlit.downIO[Option[GmosYBinning]]("explicitYBin"),
           longSlit.downIO[GmosYBinning]("defaultYBin"),
           longSlit.downIO[GmosAmpReadMode]("ampReadMode"),
           longSlit.downIO[Option[GmosAmpReadMode]]("explicitAmpReadMode"),
           longSlit.downIO[GmosAmpReadMode]("defaultAmpReadMode"),
           longSlit.downIO[GmosAmpGain]("ampGain"),
           longSlit.downIO[Option[GmosAmpGain]]("explicitAmpGain"),
           longSlit.downIO[GmosAmpGain]("defaultAmpGain"),
           longSlit.downIO[GmosRoi]("roi"),
           longSlit.downIO[Option[GmosRoi]]("explicitRoi"),
           longSlit.downIO[GmosRoi]("defaultRoi"),
           IO(longSlit.downField("wavelengthDithers").values.toList.flatMap(_.toList)),
           IO(longSlit.downField("explicitWavelengthDithers").values.map(_.toList)),
           IO(longSlit.downField("defaultWavelengthDithers").values.toList.flatMap(_.toList)),
           IO(longSlit.downField("spatialOffsets").values.toList.flatMap(_.toList)),
           IO(longSlit.downField("explicitSpatialOffsets").values.map(_.toList)),
           IO(longSlit.downField("defaultSpatialOffsets").values.toList.flatMap(_.toList))
          ).tupled,
          (GmosXBinning.Four,
           Some(GmosXBinning.Four),
           GmosXBinning.One,
           GmosYBinning.Four,
           Some(GmosYBinning.Four),
           GmosYBinning.One,
           GmosAmpReadMode.Fast,
           Some(GmosAmpReadMode.Fast),
           GmosAmpReadMode.Slow,
           GmosAmpGain.High,
           Some(GmosAmpGain.High),
           GmosAmpGain.Low,
           GmosRoi.Ccd2,
           Some(GmosRoi.Ccd2),
           GmosRoi.FullFrame,
           List(
             json"""{ "nanometers": -7.5 }""",
             json"""{ "nanometers":  7.1 }""",
             json"""{ "nanometers":  7.1 }""",
             json"""{ "nanometers": -7.5 }"""
           ),
           Some(List(
             json"""{ "nanometers": -7.5 }""",
             json"""{ "nanometers":  7.1 }""",
             json"""{ "nanometers":  7.1 }""",
             json"""{ "nanometers": -7.5 }"""
           )),
           List(
             json"""{ "nanometers": 0.0 }""",
             json"""{ "nanometers": 5.0 }""",
             json"""{ "nanometers": 5.0 }""",
             json"""{ "nanometers": 0.0 }"""
           ),
           List(
             json"""{ "arcseconds": -10.0}""",
             json"""{ "arcseconds":  10.0}""",
             json"""{ "arcseconds":  10.0}""",
             json"""{ "arcseconds": -10.0}"""
           ),
           Some(List(
             json"""{ "microarcseconds": -10000000, "arcseconds": -10.0 }""",
             json"""{ "microarcseconds":  10000000, "arcseconds":  10.0 }""",
             json"""{ "microarcseconds":  10000000, "arcseconds":  10.0 }""",
             json"""{ "microarcseconds": -10000000, "arcseconds": -10.0 }"""
           )),
           List(
             json"""{ "arcseconds":  0.0}""",
             json"""{ "arcseconds": 15.0}""",
             json"""{ "arcseconds": 15.0}""",
             json"""{ "arcseconds":  0.0}"""
           )
          )
        )

      }
    }
  }

  test("[general] specify gmos north long slit observing mode (with optional explicit parameters) at observation creation") {
    testObservingModeExplicitParams(Site.GN, "B1200_G5301")
  }

  test("[general] specify gmos south long slit observing mode (with optional explicit parameters) at observation creation") {
    testObservingModeExplicitParams(Site.GS, "B1200_G5321")
  }

  test("[general] can't create an observation with two observing modes") {
    createProgramAs(pi).flatMap { pid =>
      interceptGraphQL(s"Argument 'input.SET.observingMode' is invalid: Expected exactly one of gmosNorthLongSlit, gmosSouthLongSlit") {
        query(pi,
          s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                observingMode: {
                  gmosNorthLongSlit: {
                    grating: B1200_G5301
                    fpu: LONG_SLIT_0_25
                    centralWavelength: {
                      nanometers: 234.56
                    }
                  }
                  gmosSouthLongSlit: {
                    grating: B1200_G5321
                    fpu: LONG_SLIT_0_25
                    centralWavelength: {
                      nanometers: 234.56
                    }
                  }
                }
              }
            }) {
              observation {
                observingMode {
                  gmosNorthLongSlit {
                    grating
                  }
                  gmosSouthLongSlit {
                    grating
                  }
                }
              }
            }
          }
          """
        )
      }
    }
  }

  test("[general] created observation should have specified position angle constraint") {
    createProgramAs(pi).flatMap { pid =>
      query(pi, s"""
        mutation {
          createObservation(input: {
            programId: ${pid.asJson}
            SET: {
              posAngleConstraint: {
                mode: ALLOW_FLIP
                angle: { degrees: 14 }
              }
            }
          }) {
            observation {
              posAngleConstraint {
                mode
                angle { degrees }
              }
            }
          }
        }
        """).flatMap { js =>
        val pac = js.hcursor
          .downField("createObservation")
          .downField("observation")
          .downField("posAngleConstraint")

        val mode = pac
          .downField("mode")
          .as[PosAngleConstraintMode]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]

        val angle = pac
          .downField("angle")
          .downField("degrees")
          .as[BigDecimal]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]

        assertIO((mode, angle).tupled, (PosAngleConstraintMode.AllowFlip, BigDecimal(14)))
      }
    }
  }

  test("[general] created observation defaults to spectroscopy") {
    createProgramAs(pi).flatMap { pid =>
      query(pi, s"""
        mutation {
          createObservation(input: {
            programId: ${pid.asJson}
          }) {
            observation {
              scienceRequirements {
                mode
              }
            }
          }
        }
      """).flatMap { js =>
        val get = js.hcursor
          .downField("createObservation")
          .downField("observation")
          .downField("scienceRequirements")
          .downField("mode")
          .as[ScienceMode]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
        assertIO(get, ScienceMode.Spectroscopy)
      }
    }
  }

  test("[general] created observation accepts spectroscopy requirements") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,s"""
        mutation {
          createObservation(input: {
            programId: ${pid.asJson}
            SET: {
              scienceRequirements: {
                mode: SPECTROSCOPY
                spectroscopy: {
                  wavelength: { nanometers: 400 }
                  resolution: 200
                  signalToNoise: 75.5
                  signalToNoiseAt: { micrometers: 2.5 }
                  wavelengthCoverage: { picometers: 100000 }
                  focalPlane: SINGLE_SLIT
                  focalPlaneAngle: { microarcseconds: 3 }
                  capability: null
                }
              }
            }
          }) {
            observation {
              scienceRequirements {
                mode
                spectroscopy {
                  wavelength { picometers }
                  resolution
                  signalToNoise
                  signalToNoiseAt { nanometers }
                  wavelengthCoverage { micrometers }
                  focalPlane
                  focalPlaneAngle { microarcseconds }
                  capability
                }
              }
            }
          }
        }
      """).flatMap { js =>

        val reqs: ACursor =
          js.hcursor.downPath("createObservation", "observation", "scienceRequirements")

        val spectroscopy: ACursor =
          reqs.downField("spectroscopy")

        assertIO(
          (reqs.downIO[ScienceMode]("mode"),
           spectroscopy.downIO[Long]("wavelength", "picometers"),
           spectroscopy.downIO[Int]("resolution"),
           spectroscopy.downIO[BigDecimal]("signalToNoise"),
           spectroscopy.downIO[Long]("signalToNoiseAt", "nanometers"),
           spectroscopy.downIO[BigDecimal]("wavelengthCoverage", "micrometers"),
           spectroscopy.downIO[FocalPlane]("focalPlane"),
           spectroscopy.downIO[Int]("focalPlaneAngle", "microarcseconds"),
           spectroscopy.downIO[Option[SpectroscopyCapabilities]]("capability")
          ).tupled,
          (ScienceMode.Spectroscopy,
           400_000L,
           200,
           BigDecimal("75.50"),
           2_500L,
           BigDecimal("0.1"),
           FocalPlane.SingleSlit,
           3,
           Option.empty[SpectroscopyCapabilities]
          )
        )
      }
    }
  }

  test("[pi] pi can create an observation in their own program") {
    createProgramAs(pi).flatMap { pid =>
      createObservationAs(pi, pid)
    }
  }

  test("[pi] pi can't create an observation in someone else's program") {
    createProgramAs(pi).flatMap { pid =>
      interceptGraphQL(s"User ${pi2.id} is not authorized to perform this action.") {
        createObservationAs(pi2, pid)
      }
    }
  }

  test("[program] create many obs and then select them (in order)") {
    for {
      pid  <- createProgramAs(pi)
      o1   <- createObservationInGroupAs(pi, pid)
      o2   <- createObservationInGroupAs(pi, pid)
      o3   <- createObservationInGroupAs(pi, pid)
      ids  <- groupElementsAs(pi, pid, None)
    } yield assertEquals(ids, List(Right(o1), Right(o2), Right(o3)))
  }

  test("[program] insert obs at beginning") {
    for {
      pid  <- createProgramAs(pi)
      o1   <- createObservationInGroupAs(pi, pid)
      o2   <- createObservationInGroupAs(pi, pid)
      o3   <- createObservationInGroupAs(pi, pid, None, Some(NonNegShort.unsafeFrom(0)))
      ids  <- groupElementsAs(pi, pid, None)
    } yield assertEquals(ids, List(Right(o3), Right(o1), Right(o2)))
  }

  test("[program] insert obs in the middle") {
    for {
      pid  <- createProgramAs(pi)
      o1   <- createObservationInGroupAs(pi, pid)
      o2   <- createObservationInGroupAs(pi, pid)
      o3   <- createObservationInGroupAs(pi, pid, None, Some(NonNegShort.unsafeFrom(1)))
      ids  <- groupElementsAs(pi, pid, None)
    } yield assertEquals(ids, List(Right(o1), Right(o3), Right(o2)))
  }

  test("[program] create many obs and then select them (in order)") {
    for {
      pid  <- createProgramAs(pi)
      gid  <- createGroupAs(pi, pid)
      o1   <- createObservationInGroupAs(pi, pid, Some(gid))
      o2   <- createObservationInGroupAs(pi, pid, Some(gid))
      o3   <- createObservationInGroupAs(pi, pid, Some(gid))
      ids  <- groupElementsAs(pi, pid, Some(gid))
    } yield assertEquals(ids, List(Right(o1), Right(o2), Right(o3)))
  }

  test("[program] insert obs at beginning") {
    for {
      pid  <- createProgramAs(pi)
      gid  <- createGroupAs(pi, pid)
      o1   <- createObservationInGroupAs(pi, pid, Some(gid))
      o2   <- createObservationInGroupAs(pi, pid, Some(gid))
      o3   <- createObservationInGroupAs(pi, pid, Some(gid), Some(NonNegShort.unsafeFrom(0)))
      ids  <- groupElementsAs(pi, pid, Some(gid))
    } yield assertEquals(ids, List(Right(o3), Right(o1), Right(o2)))
  }

  test("[program] insert obs in the middle") {
    for {
      pid  <- createProgramAs(pi)
      gid  <- createGroupAs(pi, pid)
      o1   <- createObservationInGroupAs(pi, pid, Some(gid))
      o2   <- createObservationInGroupAs(pi, pid, Some(gid))
      o3   <- createObservationInGroupAs(pi, pid, Some(gid), Some(NonNegShort.unsafeFrom(1)))
      ids  <- groupElementsAs(pi, pid, Some(gid))
    } yield assertEquals(ids, List(Right(o1), Right(o3), Right(o2)))
  }
}
