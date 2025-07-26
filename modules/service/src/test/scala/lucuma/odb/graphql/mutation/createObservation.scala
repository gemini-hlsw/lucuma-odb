// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegShort
import io.circe.ACursor
import io.circe.Decoder
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.*
import lucuma.core.enums.CallForProposalsType.DemoScience
import lucuma.core.model.CloudExtinction
import lucuma.core.model.GuestUser
import lucuma.core.model.ImageQuality
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Semester
import lucuma.core.model.ServiceUser
import lucuma.core.model.StandardUser
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.syntax.string.*
import lucuma.core.syntax.timespan.*
import lucuma.odb.data.PosAngleConstraintMode
import lucuma.odb.graphql.input.AllocationInput

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
  val ngo: StandardUser    = TestUsers.Standard.ngo(nextId, nextId, Partner.CA)
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

  test("[general] can create observation with a program reference") {
    createProgramAs(pi).flatMap { pid =>
      createCallForProposalsAs(staff, DemoScience, Semester.unsafeFromString("2025A")).flatMap { cid =>
        addDemoScienceProposal(pi, pid, cid)
      } *>
      submitProposal(pi, pid) *>
      query(pi,
        s"""
          mutation {
            createObservation(input: {
              proposalReference: "G-2025A-0001"
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

  test("[general] can create an observation when both ref and pid are supplied if they correspond") {
    createProgramAs(pi).flatMap { pid =>
      createCallForProposalsAs(staff, DemoScience, Semester.unsafeFromString("2025A")).flatMap { cid =>
        addDemoScienceProposal(pi, pid, cid)
      } *>
      submitProposal(pi, pid) *>
      query(pi,
        s"""
          mutation {
            createObservation(input: {
              programId: "$pid"
              proposalReference: "G-2025A-0002"
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

  test("[general] cannot create an observation when both ref and pid are supplied if they do not correspond") {
    createProgramAs(pi).flatMap { pid =>
      createCallForProposalsAs(staff, DemoScience, Semester.unsafeFromString("2025A")).flatMap { cid =>
        addDemoScienceProposal(pi, pid, cid)
      } *>
      submitProposal(pi, pid) *>
      expect(
        pi,
        s"""
          mutation {
            createObservation(input: {
              programId: "p-123"
              proposalReference: "G-2025A-0003"
              SET: {
                subtitle: "crunchy frog"
              }
            }) {
              observation {
                subtitle
              }
            }
          }
        """,
        Left(List(
          "Proposal 'G-2025A-0003' (id p-107) does not correspond to the specified program id p-123."
        ))
      )
    }
  }

  test("[general] cannot create an observation without a ref or pid") {
    createProgramAs(pi).flatMap { pid =>
      createCallForProposalsAs(staff, DemoScience, Semester.unsafeFromString("2025A")).flatMap { cid =>
        addDemoScienceProposal(pi, pid, cid)
      } *>
      submitProposal(pi, pid) *>
      expect(
        pi,
        s"""
          mutation {
            createObservation(input: {
              SET: {
                subtitle: "crunchy frog"
              }
            }) {
              observation {
                subtitle
              }
            }
          }
        """,
        Left(List(
          "One of programId, programReference or proposalReference must be provided."
        ))
      )
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

  test("[general] created observation may have no science band") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: { }
            }) {
              observation {
                scienceBand
              }
            }
          }
          """).flatMap { js =>
        val get = js.hcursor
          .downField("createObservation")
          .downField("observation")
          .downField("scienceBand")
          .as[Option[ScienceBand]]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
        assertIO(get, none[ScienceBand])
      }
    }
  }

  test("[general] created observation should have specified science band") {
    createProgramAs(pi)
      .flatTap(pid => setAllocationsAs(staff, pid, List(AllocationInput(TimeAccountingCategory.US, ScienceBand.Band2, 1.hourTimeSpan))))
      .flatMap { pid =>
        query(pi,
          s"""
            mutation {
              createObservation(input: {
                programId: ${pid.asJson}
                SET: {
                  scienceBand: BAND2
                }
              }) {
                observation {
                  scienceBand
                }
              }
            }
            """).flatMap { js =>
          val get = js.hcursor
            .downField("createObservation")
            .downField("observation")
            .downField("scienceBand")
            .as[Option[ScienceBand]]
            .leftMap(f => new RuntimeException(f.message))
            .liftTo[IO]
          assertIO(get, ScienceBand.Band2.some)
        }
      }
  }

  test("[general] cannot create observation with inappropriate science band") {
    createProgramAs(pi)
      .flatTap(pid => setAllocationsAs(staff, pid, List(AllocationInput(TimeAccountingCategory.US, ScienceBand.Band2, 1.hourTimeSpan))))
      .flatMap { pid =>
        expect(pi,
          s"""
            mutation {
              createObservation(input: {
                programId: ${pid.asJson}
                SET: {
                  scienceBand: BAND1
                }
              }) {
                observation {
                  scienceBand
                }
              }
            }
          """,
          List(s"One or more programs have not been allocated time in BAND1: $pid").asLeft
        )
      }
  }

  test("[general] create observation with a single science band allocation") {
    createProgramAs(pi)
      .flatTap(pid => setAllocationsAs(staff, pid, List(AllocationInput(TimeAccountingCategory.US, ScienceBand.Band2, 1.hourTimeSpan))))
      .flatMap { pid =>
        expect(pi,
          s"""
            mutation {
              createObservation(input: {
                programId: ${pid.asJson}
                SET: { }
              }) {
                observation {
                  scienceBand
                }
              }
            }
          """,
          json"""
            {
              "createObservation": {
                "observation": {
                  "scienceBand": "BAND2"
                }
              }
            }
          """.asRight
        )
      }
  }

  test("[general] create observation with a multiple science band allocation") {
    val allocations = List(
      AllocationInput(TimeAccountingCategory.US, ScienceBand.Band1, 1.hourTimeSpan),
      AllocationInput(TimeAccountingCategory.US, ScienceBand.Band2, 1.hourTimeSpan)
    )
    createProgramAs(pi)
      .flatTap(pid => setAllocationsAs(staff, pid, allocations))
      .flatMap { pid =>
        expect(pi,
          s"""
            mutation {
              createObservation(input: {
                programId: ${pid.asJson}
                SET: { }
              }) {
                observation {
                  scienceBand
                }
              }
            }
          """,
          json"""
            {
              "createObservation": {
                "observation": {
                  "scienceBand": null
                }
              }
            }
          """.asRight
        )
      }
  }

  test("[general] new observation without definition should be UNDEFINED"):
    createProgramAs(pi).flatMap: pid =>
      createObservationAs(pi, pid).flatMap: oid =>
        runObscalcUpdateAs(service, pid, oid) >>
        query(pi, s"""
            query {
              observation(observationId: "$oid") {
                workflow { value { state } }
              }
            }
          """
        ).flatMap: js =>
          val get = js.hcursor
            .downField("observation")
            .downField("workflow")
            .downField("value")
            .downField("state")
            .as[ObservationWorkflowState]
            .leftMap(f => new RuntimeException(f.message))
            .liftTo[IO]
          assertIO(get, ObservationWorkflowState.Undefined)

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
          .as[CloudExtinction.Preset]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
        assertIO(get, CloudExtinction.Preset.OnePointZero)
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
          .as[CloudExtinction.Preset]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
        assertIO(get, CloudExtinction.Preset.PointThree)
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
          .as[ImageQuality.Preset]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
        assertIO(get, ImageQuality.Preset.PointEight)
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
      interceptGraphQL(show"Target(s) $fakeTarget must exist and be associated with Program $pid.")(
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

  private def createObsWithGmosObservingMode(
    pid:      Program.Id,
    site:     Site,
    grating:  String,
    fpu:      String = "LONG_SLIT_0_25",
    iq:       ImageQuality.Preset = ImageQuality.Preset.TwoPointZero,
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
                explicitXBin,
                defaultXBin,
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
      query(pi, createObsWithGmosObservingMode(pid, Site.GN, "B1200_G5301")).flatMap { js =>
        val longSlit = js.hcursor.downPath("createObservation", "observation", "observingMode", "gmosNorthLongSlit")

        assertIO(
          (longSlit.downIO[GmosNorthGrating]("grating"),
           longSlit.downIO[Option[GmosNorthFilter]]("filter"),
           longSlit.downIO[GmosNorthFpu]("fpu"),
           longSlit.downIO[Double]("centralWavelength", "nanometers"),
           longSlit.downIO[GmosXBinning]("xBin"),
           longSlit.downIO[Option[GmosXBinning]]("explicitXBin"),
           longSlit.downIO[GmosXBinning]("defaultXBin"),
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
           None,
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
        query(pi, createObsWithGmosObservingMode(pid, Site.GN, "B1200_G5301", fpu = "LONG_SLIT_5_00", iq = ImageQuality.Preset.PointOne, asterism = List(tid))).flatMap { js =>
          val longSlit = js.hcursor.downPath("createObservation", "observation", "observingMode", "gmosNorthLongSlit")

          assertIO(
            (longSlit.downIO[GmosNorthGrating]("grating"),
              longSlit.downIO[Option[GmosNorthFilter]]("filter"),
              longSlit.downIO[GmosNorthFpu]("fpu"),
              longSlit.downIO[Double]("centralWavelength", "nanometers"),
              longSlit.downIO[GmosXBinning]("xBin"),
              longSlit.downIO[Option[GmosXBinning]]("explicitXBin"),
              longSlit.downIO[GmosXBinning]("defaultXBin"),
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
              GmosXBinning.Two,
              None,
              GmosXBinning.Two,
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
      query(pi, createObsWithGmosObservingMode(pid, Site.GS, "B1200_G5321")).flatMap { js =>
        val longSlit = js.hcursor.downPath("createObservation", "observation", "observingMode", "gmosSouthLongSlit")

        assertIO(
          (longSlit.downIO[GmosSouthGrating]("grating"),
           longSlit.downIO[Option[GmosSouthFilter]]("filter"),
           longSlit.downIO[GmosSouthFpu]("fpu"),
           longSlit.downIO[Double]("centralWavelength", "nanometers"),
           longSlit.downIO[GmosXBinning]("xBin"),
           longSlit.downIO[Option[GmosXBinning]]("explicitXBin"),
           longSlit.downIO[GmosXBinning]("defaultXBin"),
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
           None,
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

  private def createObsWithF2ObservingMode(
    pid:      Program.Id,
    grating:  String,
    fpu:      String = "LONG_SLIT_2",
    iq:       ImageQuality.Preset = ImageQuality.Preset.TwoPointZero,
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
              flamingos2LongSlit: {
                disperser: $grating
                filter: Y
                fpu: $fpu
              }
            }
            targetEnvironment: {
              asterism: [ ${asterism.map(_.asJson).mkString(", ")} ]
            }
          }
        }) {
          observation {
            observingMode {
              flamingos2LongSlit {
                disperser
                filter
                fpu
                explicitReadMode
                explicitReads
                decker
                defaultDecker
                explicitDecker
                readoutMode
                defaultReadoutMode
                explicitReadoutMode
                initialDisperser
                initialFilter
                initialFpu
              }
            }
          }
        }
      }
    """

  test("[general] specify f2 long slit observing mode at observation creation") {
    createProgramAs(pi).flatMap { pid =>
      query(pi, createObsWithF2ObservingMode(pid, "R1200_HK")).flatMap { js =>
        val longSlit = js.hcursor.downPath("createObservation", "observation", "observingMode", "flamingos2LongSlit")

        assertIO(
          (longSlit.downIO[Flamingos2Disperser]("disperser"),
           longSlit.downIO[Option[Flamingos2Filter]]("filter"),
           longSlit.downIO[Flamingos2Fpu]("fpu"),
           longSlit.downIO[Option[Flamingos2ReadMode]]("explicitReadMode"),
           longSlit.downIO[Option[Flamingos2Reads]]("explicitReads"),
           longSlit.downIO[Flamingos2Decker]("decker"),
           longSlit.downIO[Flamingos2Decker]("defaultDecker"),
           longSlit.downIO[Option[Flamingos2Decker]]("explicitDecker"),
           longSlit.downIO[Flamingos2ReadoutMode]("readoutMode"),
           longSlit.downIO[Flamingos2ReadoutMode]("defaultReadoutMode"),
           longSlit.downIO[Option[Flamingos2ReadoutMode]]("explicitReadoutMode"),
           longSlit.downIO[Flamingos2Disperser]("initialDisperser"),
           longSlit.downIO[Option[Flamingos2Filter]]("initialFilter"),
           longSlit.downIO[Flamingos2Fpu]("initialFpu")
          ).tupled,
          (Flamingos2Disperser.R1200HK,
           Some(Flamingos2Filter.Y),
           Flamingos2Fpu.LongSlit2,
           None,
           None,
           Flamingos2Decker.LongSlit,
           Flamingos2Decker.LongSlit,
           None,
           Flamingos2ReadoutMode.Science,
           Flamingos2ReadoutMode.Science,
           None,
           Flamingos2Disperser.R1200HK,
           Some(Flamingos2Filter.Y),
           Flamingos2Fpu.LongSlit2
          )
        )

      }
    }
  }

  private def createObsWithF2ObservingModeAllParams(
    pid:                 Program.Id,
    disperser:           Flamingos2Disperser,
    fpu:                 Flamingos2Fpu,
    explicitReadMode:    Option[Flamingos2ReadMode],
    explicitDecker:      Option[Flamingos2Decker],
    explicitReadoutMode: Option[Flamingos2ReadoutMode],
    explicitReads:       Option[Flamingos2Reads]
  ): String =
    s"""
      mutation {
        createObservation(input: {
          programId: ${pid.asJson}
          SET: {
            observingMode: {
              flamingos2LongSlit: {
                disperser: ${disperser.tag.toScreamingSnakeCase}
                filter: Y
                fpu: ${fpu.tag.toScreamingSnakeCase}
                explicitReadMode: ${explicitReadMode.map(_.tag.toScreamingSnakeCase).getOrElse("null")}
                explicitDecker: ${explicitDecker.map(_.tag.toScreamingSnakeCase).getOrElse("null")}
                explicitReadoutMode: ${explicitReadoutMode.map(_.tag.toScreamingSnakeCase).getOrElse("null")}
                explicitReads: ${explicitReads.map(_.tag.toScreamingSnakeCase).getOrElse("null")}
              }
            }
          }
        }) {
          observation {
            observingMode {
              flamingos2LongSlit {
                disperser
                filter
                fpu
                explicitReadMode
                explicitReads
                decker
                defaultDecker
                explicitDecker
                readoutMode
                defaultReadoutMode
                explicitReadoutMode
              }
            }
          }
        }
      }
    """

  test("[general] specify f2 long slit observing mode at observation creation with explicit read mode") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        createObsWithF2ObservingModeAllParams(
          pid,
          Flamingos2Disperser.R1200HK,
          Flamingos2Fpu.LongSlit2,
          explicitReadMode = Some(Flamingos2ReadMode.Bright),
          explicitReads = None,
          explicitDecker = Some(Flamingos2Decker.MOS),
          explicitReadoutMode = Some(Flamingos2ReadoutMode.Engineering)
        )).flatMap { js =>
          val longSlit = js.hcursor.downPath("createObservation", "observation", "observingMode", "flamingos2LongSlit")

          assertIO((
            longSlit.downIO[Flamingos2Disperser]("disperser"),
            longSlit.downIO[Option[Flamingos2Filter]]("filter"),
            longSlit.downIO[Flamingos2Fpu]("fpu"),
            longSlit.downIO[Option[Flamingos2ReadMode]]("explicitReadMode"),
            longSlit.downIO[Option[Flamingos2Reads]]("explicitReads"),
            longSlit.downIO[Flamingos2Decker]("decker"),
            longSlit.downIO[Flamingos2Decker]("defaultDecker"),
            longSlit.downIO[Option[Flamingos2Decker]]("explicitDecker"),
            longSlit.downIO[Flamingos2ReadoutMode]("readoutMode"),
            longSlit.downIO[Flamingos2ReadoutMode]("defaultReadoutMode"),
            longSlit.downIO[Option[Flamingos2ReadoutMode]]("explicitReadoutMode")
          ).tupled, (
            Flamingos2Disperser.R1200HK,
            Some(Flamingos2Filter.Y),
            Flamingos2Fpu.LongSlit2,
            Some(Flamingos2ReadMode.Bright),        // Explicitly set read mode
            None,                           // Explicit reads is Empty
            Flamingos2Decker.MOS,                   // Explicitly set
            Flamingos2Decker.LongSlit,              // default to long slit
            Some(Flamingos2Decker.MOS),             // Explicitly set
            Flamingos2ReadoutMode.Engineering,      // Explicitly set
            Flamingos2ReadoutMode.Science,          // Science bf default
            Some(Flamingos2ReadoutMode.Engineering) // Explicitly set
          )
        )

      }
    }
  }

  test("[general] specify f2 long slit observing mode at observation creation with explicit reads") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        createObsWithF2ObservingModeAllParams(
          pid,
          Flamingos2Disperser.R1200HK,
          Flamingos2Fpu.LongSlit2,
          explicitReadMode = None,
          explicitReads = Some(Flamingos2Reads.Reads_4),
          explicitDecker = None,
          explicitReadoutMode = None
        )).flatMap { js =>
          val longSlit = js.hcursor.downPath("createObservation", "observation", "observingMode", "flamingos2LongSlit")

          assertIO((
            longSlit.downIO[Flamingos2Disperser]("disperser"),
            longSlit.downIO[Option[Flamingos2Filter]]("filter"),
            longSlit.downIO[Flamingos2Fpu]("fpu"),
            longSlit.downIO[Option[Flamingos2ReadMode]]("explicitReadMode"),
            longSlit.downIO[Option[Flamingos2Reads]]("explicitReads"),
            longSlit.downIO[Flamingos2Decker]("decker"),
            longSlit.downIO[Flamingos2ReadoutMode]("readoutMode")
          ).tupled, (
            Flamingos2Disperser.R1200HK,
            Some(Flamingos2Filter.Y),
            Flamingos2Fpu.LongSlit2,
            None,
            Some(Flamingos2Reads.Reads_4),          // Explicit reads set to 4
            Flamingos2Decker.LongSlit,              // default to long slit
            Flamingos2ReadoutMode.Science           // Science bf default
          )
        )

      }
    }
  }

  test("[general] specify f2 long slit observing mode at observation creation with explicit read mode and reads") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        createObsWithF2ObservingModeAllParams(
          pid,
          Flamingos2Disperser.R1200HK,
          Flamingos2Fpu.LongSlit2,
          explicitReadMode = Some(Flamingos2ReadMode.Medium),
          explicitReads = Some(Flamingos2Reads.Reads_16),
          explicitDecker = None,
          explicitReadoutMode = None
        )).flatMap { js =>
          val longSlit = js.hcursor.downPath("createObservation", "observation", "observingMode", "flamingos2LongSlit")

          assertIO((
            longSlit.downIO[Flamingos2Disperser]("disperser"),
            longSlit.downIO[Option[Flamingos2Filter]]("filter"),
            longSlit.downIO[Flamingos2Fpu]("fpu"),
            longSlit.downIO[Option[Flamingos2ReadMode]]("explicitReadMode"),
            longSlit.downIO[Option[Flamingos2Reads]]("explicitReads"),
            longSlit.downIO[Flamingos2Decker]("decker"),
            longSlit.downIO[Flamingos2ReadoutMode]("readoutMode")
          ).tupled, (
            Flamingos2Disperser.R1200HK,
            Some(Flamingos2Filter.Y),
            Flamingos2Fpu.LongSlit2,
            Some(Flamingos2ReadMode.Medium), // Explicitly set read mode
            Some(Flamingos2Reads.Reads_16),  // Explicit reads is 16
            Flamingos2Decker.LongSlit,       // default to long slit
            Flamingos2ReadoutMode.Science    // Science bf default
          )
        )

      }
    }
  }

  private def createObsWithF2SpatialOffsets(pid: Program.Id): String =
    s"""
      mutation {
        createObservation(input: {
          programId: ${pid.asJson}
          SET: {
            observingMode: {
              flamingos2LongSlit: {
                disperser: R1200_HK
                filter: Y
                fpu: LONG_SLIT_2
                explicitSpatialOffsets: [
                  { p: { arcseconds: 0.0 }, q: { arcseconds: -10.0 } },
                  { p: { arcseconds: 0.0 }, q: { arcseconds:  10.0 } },
                  { p: { arcseconds: 0.0 }, q: { arcseconds:   5.5 } },
                  { p: { arcseconds: 0.0 }, q: { arcseconds:  -2.5 } }
                ]
              }
            }
          }
        }) {
          observation {
            observingMode {
              flamingos2LongSlit {
                disperser
                filter
                fpu
                spatialOffsets {
                  p {
                    microarcseconds
                    arcseconds
                  }
                  q {
                    microarcseconds
                    arcseconds
                  }
                }
                explicitSpatialOffsets {
                  p {
                    microarcseconds
                    arcseconds
                  }
                  q {
                    microarcseconds
                    arcseconds
                  }
                }
                defaultSpatialOffsets {
                  p {
                    microarcseconds
                    arcseconds
                  }
                  q {
                    microarcseconds
                    arcseconds
                  }
                }
              }
            }
          }
        }
      }
    """

  test("[general] specify f2 long slit spatial offsets at observation creation"):
    createProgramAs(pi).flatMap { pid =>
      query(pi, createObsWithF2SpatialOffsets(pid)).flatMap { js =>
        val longSlit = js.hcursor.downPath("createObservation", "observation", "observingMode", "flamingos2LongSlit")

        assertIO(
          (longSlit.downIO[Flamingos2Disperser]("disperser"),
           longSlit.downIO[Option[Flamingos2Filter]]("filter"),
           longSlit.downIO[Flamingos2Fpu]("fpu"),
           longSlit.downIO[List[Json]]("spatialOffsets"),
           longSlit.downIO[List[Json]]("explicitSpatialOffsets"),
           longSlit.downIO[List[Json]]("defaultSpatialOffsets")
          ).tupled,
          (Flamingos2Disperser.R1200HK,
           Some(Flamingos2Filter.Y),
           Flamingos2Fpu.LongSlit2,
           List(
             json"""{ "p": { "microarcseconds": 0, "arcseconds": 0.0 }, "q": { "microarcseconds": -10000000, "arcseconds": -10.0 } }""",
             json"""{ "p": { "microarcseconds": 0, "arcseconds": 0.0 }, "q": { "microarcseconds":  10000000, "arcseconds":  10.0 } }""",
             json"""{ "p": { "microarcseconds": 0, "arcseconds": 0.0 }, "q": { "microarcseconds":   5500000, "arcseconds":   5.5 } }""",
             json"""{ "p": { "microarcseconds": 0, "arcseconds": 0.0 }, "q": { "microarcseconds":  -2500000, "arcseconds":  -2.5 } }"""
           ),
           List(
             json"""{ "p": { "microarcseconds": 0, "arcseconds": 0.0 }, "q": { "microarcseconds": -10000000, "arcseconds": -10.0 } }""",
             json"""{ "p": { "microarcseconds": 0, "arcseconds": 0.0 }, "q": { "microarcseconds":  10000000, "arcseconds":  10.0 } }""",
             json"""{ "p": { "microarcseconds": 0, "arcseconds": 0.0 }, "q": { "microarcseconds":   5500000, "arcseconds":   5.5 } }""",
             json"""{ "p": { "microarcseconds": 0, "arcseconds": 0.0 }, "q": { "microarcseconds":  -2500000, "arcseconds":  -2.5 } }"""
           ),
           List(
             json"""{ "p": { "microarcseconds": 0, "arcseconds": 0.0 }, "q": { "microarcseconds": 15000000, "arcseconds": 15.0 } }""",
             json"""{ "p": { "microarcseconds": 0, "arcseconds": 0.0 }, "q": { "microarcseconds": -15000000, "arcseconds": -15.0 } }""",
             json"""{ "p": { "microarcseconds": 0, "arcseconds": 0.0 }, "q": { "microarcseconds": -15000000, "arcseconds": -15.0 } }""",
             json"""{ "p": { "microarcseconds": 0, "arcseconds": 0.0 }, "q": { "microarcseconds": 15000000, "arcseconds": 15.0 } }"""
           )
          )
        )
      }
    }

  test("createObservation: rejects 3 spatial offsets"):
    createProgramAs(pi).flatMap { pid =>
      interceptGraphQL("Argument 'input.SET.observingMode.flamingos2LongSlit' is invalid: Flamingos2 must have exactly 0 or 4 offsets, but 3 were provided.") {
        query(pi, s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                observingMode: {
                  flamingos2LongSlit: {
                    disperser: R1200_HK
                    filter: Y
                    fpu: LONG_SLIT_2
                    explicitSpatialOffsets: [
                      { p: { arcseconds: 0.0 }, q: { arcseconds: -10.0 } },
                      { p: { arcseconds: 0.0 }, q: { arcseconds:  10.0 } },
                      { p: { arcseconds: 0.0 }, q: { arcseconds:   5.0 } }
                    ]
                  }
                }
              }
            }) {
              observation { id }
            }
          }
        """)
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
             json"""{ "nanometers": -7.500 }""",
             json"""{ "nanometers":  7.100 }""",
             json"""{ "nanometers":  7.100 }""",
             json"""{ "nanometers": -7.500 }"""
           ),
           Some(List(
             json"""{ "nanometers": -7.500 }""",
             json"""{ "nanometers":  7.100 }""",
             json"""{ "nanometers":  7.100 }""",
             json"""{ "nanometers": -7.500 }"""
           )),
           List(
             json"""{ "nanometers": 0.000 }""",
             json"""{ "nanometers": 5.000 }""",
             json"""{ "nanometers": -5.000 }"""
           ),
           List(
             json"""{ "arcseconds": -10.000000}""",
             json"""{ "arcseconds":  10.000000}""",
             json"""{ "arcseconds":  10.000000}""",
             json"""{ "arcseconds": -10.000000}"""
           ),
           Some(List(
             json"""{ "microarcseconds": -10000000, "arcseconds": -10.000000 }""",
             json"""{ "microarcseconds":  10000000, "arcseconds":  10.000000 }""",
             json"""{ "microarcseconds":  10000000, "arcseconds":  10.000000 }""",
             json"""{ "microarcseconds": -10000000, "arcseconds": -10.000000 }"""
           )),
           List(
             json"""{ "arcseconds":  0.000000}""",
             json"""{ "arcseconds": 15.000000}""",
             json"""{ "arcseconds": -15.000000}"""
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
      interceptGraphQL(s"Argument 'input.SET.observingMode' is invalid: Expected exactly one of gmosNorthLongSlit, gmosSouthLongSlit, gmosNorthImaging, gmosSouthImaging, flamingos2LongSlit") {
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
          .as[Option[ScienceMode]]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
        assertIO(get, None)
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
                exposureTimeMode: {
                  signalToNoise: {
                    value: 75.5
                    at: { micrometers: 2.5 }
                  }
                }
                spectroscopy: {
                  wavelength: { nanometers: 400 }
                  resolution: 200
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
                exposureTimeMode {
                  signalToNoise {
                    value
                    at { nanometers }
                  }
                }
                spectroscopy {
                  wavelength { picometers }
                  resolution
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
           reqs.downIO[BigDecimal]("exposureTimeMode", "signalToNoise", "value"),
           reqs.downIO[Long]("exposureTimeMode", "signalToNoise", "at", "nanometers"),
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

  test("[general] created observation accepts imaging gmos north requirements"):
    createProgramAs(pi).flatMap { pid =>
      query(pi,s"""
        mutation {
          createObservation(input: {
            programId: ${pid.asJson}
            SET: {
              scienceRequirements: {
                exposureTimeMode: {
                  signalToNoise: {
                    value: 75.5
                    at: { micrometers: 2.5 }
                  }
                }
                imaging: {
                  minimumFov: { arcseconds: 330 }
                  narrowFilters: true
                  broadFilters: true
                  combinedFilters: true
                }
              }
            }
          }) {
            observation {
              scienceRequirements {
                mode
                exposureTimeMode {
                  signalToNoise {
                    value
                    at { nanometers }
                  }
                }
                imaging {
                  minimumFov { arcseconds }
                  narrowFilters
                  broadFilters
                  combinedFilters
                }
              }
            }
          }
        }
      """).flatMap { js =>

        val reqs: ACursor =
          js.hcursor.downPath("createObservation", "observation", "scienceRequirements")

        val imaging: ACursor =
          reqs.downField("imaging")

        assertIO(
          (reqs.downIO[ScienceMode]("mode"),
           reqs.downIO[BigDecimal]("exposureTimeMode", "signalToNoise", "value"),
           reqs.downIO[Long]("exposureTimeMode", "signalToNoise", "at", "nanometers"),
           imaging.downIO[BigDecimal]("minimumFov", "arcseconds"),
           imaging.downIO[Boolean]("narrowFilters"),
           imaging.downIO[Boolean]("broadFilters"),
           imaging.downIO[Boolean]("combinedFilters"),
          ).tupled,
          (ScienceMode.Imaging,
           BigDecimal("75.50"),
           2_500L,
           BigDecimal("330"),
           true,
           true,
           true
          )
        )
      }
    }

  test("[general] created observation accepts imaging gmos south requirements"):
    createProgramAs(pi).flatMap { pid =>
      query(pi,s"""
        mutation {
          createObservation(input: {
            programId: ${pid.asJson}
            SET: {
              scienceRequirements: {
                exposureTimeMode: {
                  signalToNoise: {
                    value: 75.5
                    at: { micrometers: 2.5 }
                  }
                }
                imaging: {
                  minimumFov: { arcseconds: 330 }
                  narrowFilters: true
                  broadFilters: true
                  combinedFilters: true
                }
              }
            }
          }) {
            observation {
              scienceRequirements {
                mode
                exposureTimeMode {
                  signalToNoise {
                    value
                    at { nanometers }
                  }
                }
                imaging {
                  minimumFov { arcseconds }
                  narrowFilters
                  broadFilters
                  combinedFilters
                }
              }
            }
          }
        }
      """).flatMap { js =>

        val reqs: ACursor =
          js.hcursor.downPath("createObservation", "observation", "scienceRequirements")

        val imaging: ACursor =
          reqs.downField("imaging")

        assertIO(
          (reqs.downIO[ScienceMode]("mode"),
           reqs.downIO[BigDecimal]("exposureTimeMode", "signalToNoise", "value"),
           reqs.downIO[Long]("exposureTimeMode", "signalToNoise", "at", "nanometers"),
           imaging.downIO[BigDecimal]("minimumFov", "arcseconds"),
           imaging.downIO[Boolean]("narrowFilters"),
           imaging.downIO[Boolean]("broadFilters"),
           imaging.downIO[Boolean]("combinedFilters"),
          ).tupled,
          (ScienceMode.Imaging,
           BigDecimal("75.50"),
           2_500L,
           BigDecimal("330"),
           true,
           true,
           true
          )
        )
      }
    }

  test("[general] cannot create observation with both imaging and spectroscopy requirements"):
    createProgramAs(pi).flatMap { pid =>
      interceptGraphQL("Argument 'input.SET.scienceRequirements' is invalid: Expected at most one of spectroscopy, imaging") {
        query(pi, s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                scienceRequirements: {
                  exposureTimeMode: {
                    signalToNoise: {
                      value: 75.5
                      at: { micrometers: 2.5 }
                    }
                  }
                  spectroscopy: {
                    wavelength: { nanometers: 400 }
                    resolution: 200
                    wavelengthCoverage: { picometers: 100000 }
                    focalPlane: SINGLE_SLIT
                    focalPlaneAngle: { microarcseconds: 3 }
                    capability: null
                  }
                  imaging: {
                    minimumFov: { arcseconds: 330 }
                    narrowFilters: true
                    broadFilters: true
                    combinedFilters: true
                  }
                }
              }
            }) {
              observation {
                scienceRequirements {
                  mode
                }
              }
            }
          }
        """)
      }
    }

  test("[pi] pi can create an observation in their own program"):
    createProgramAs(pi).flatMap { pid =>
      createObservationAs(pi, pid)
    }

  test("[pi] pi can't create an observation in someone else's program"):
    createProgramAs(pi).flatMap { pid =>
      interceptGraphQL(s"User ${pi2.id} is not authorized to perform this operation.") {
        createObservationAs(pi2, pid)
      }
    }

  test("[program] create many obs and then select them (in order)"):
    for {
      pid  <- createProgramAs(pi)
      o1   <- createObservationInGroupAs(pi, pid)
      o2   <- createObservationInGroupAs(pi, pid)
      o3   <- createObservationInGroupAs(pi, pid)
      ids  <- groupElementsAs(pi, pid, None)
    } yield assertEquals(ids, List(Right(o1), Right(o2), Right(o3)))

  test("[program] insert obs at beginning"):
    for {
      pid  <- createProgramAs(pi)
      o1   <- createObservationInGroupAs(pi, pid)
      o2   <- createObservationInGroupAs(pi, pid)
      o3   <- createObservationInGroupAs(pi, pid, None, Some(NonNegShort.unsafeFrom(0)))
      ids  <- groupElementsAs(pi, pid, None)
    } yield assertEquals(ids, List(Right(o3), Right(o1), Right(o2)))

  test("[program] insert obs in the middle"):
    for {
      pid  <- createProgramAs(pi)
      o1   <- createObservationInGroupAs(pi, pid)
      o2   <- createObservationInGroupAs(pi, pid)
      o3   <- createObservationInGroupAs(pi, pid, None, Some(NonNegShort.unsafeFrom(1)))
      ids  <- groupElementsAs(pi, pid, None)
    } yield assertEquals(ids, List(Right(o1), Right(o3), Right(o2)))

  test("[program] create many obs and then select them (in order)"):
    for {
      pid  <- createProgramAs(pi)
      gid  <- createGroupAs(pi, pid)
      o1   <- createObservationInGroupAs(pi, pid, Some(gid))
      o2   <- createObservationInGroupAs(pi, pid, Some(gid))
      o3   <- createObservationInGroupAs(pi, pid, Some(gid))
      ids  <- groupElementsAs(pi, pid, Some(gid))
    } yield assertEquals(ids, List(Right(o1), Right(o2), Right(o3)))

  test("[program] insert obs at beginning in a group"):
    for {
      pid  <- createProgramAs(pi)
      gid  <- createGroupAs(pi, pid)
      o1   <- createObservationInGroupAs(pi, pid, Some(gid))
      o2   <- createObservationInGroupAs(pi, pid, Some(gid))
      o3   <- createObservationInGroupAs(pi, pid, Some(gid), Some(NonNegShort.unsafeFrom(0)))
      ids  <- groupElementsAs(pi, pid, Some(gid))
    } yield assertEquals(ids, List(Right(o3), Right(o1), Right(o2)))

  test("[program] insert obs in the middle in a group"):
    for {
      pid  <- createProgramAs(pi)
      gid  <- createGroupAs(pi, pid)
      o1   <- createObservationInGroupAs(pi, pid, Some(gid))
      o2   <- createObservationInGroupAs(pi, pid, Some(gid))
      o3   <- createObservationInGroupAs(pi, pid, Some(gid), Some(NonNegShort.unsafeFrom(1)))
      ids  <- groupElementsAs(pi, pid, Some(gid))
    } yield assertEquals(ids, List(Right(o1), Right(o3), Right(o2)))

  test("[gmos imaging] can create GMOS North imaging observation"):
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid).flatMap { tid =>
        createGmosNorthImagingObservationAs(pi, pid, tid).flatMap { oid =>
          expect(pi, s"""
            query {
              observation(observationId: "$oid") {
                observingMode {
                  gmosNorthImaging {
                    filters
                    bin
                    ampReadMode
                    ampGain
                    roi
                  }
                }
              }
            }
          """, json"""
            {
              "observation": {
                "observingMode": {
                  "gmosNorthImaging": {
                    "filters": ["G_PRIME", "R_PRIME"],
                    "bin": "TWO",
                    "ampReadMode": "SLOW",
                    "ampGain": "LOW",
                    "roi": "FULL_FRAME"
                  }
                }
              }
            }
          """.asRight)
        }
      }
    }

  test("[gmos imaging] can create GMOS South imaging observation"):
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid).flatMap { tid =>
        createGmosSouthImagingObservationAs(pi, pid, tid).flatMap { oid =>
          expect(pi, s"""
            query {
              observation(observationId: "$oid") {
                observingMode {
                  gmosSouthImaging {
                    filters
                    bin
                    ampReadMode
                    ampGain
                    roi
                  }
                }
              }
            }
          """, json"""
            {
              "observation": {
                "observingMode": {
                  "gmosSouthImaging": {
                    "filters": ["G_PRIME", "R_PRIME"],
                    "bin": "TWO",
                    "ampReadMode": "SLOW",
                    "ampGain": "LOW",
                    "roi": "FULL_FRAME"
                  }
                }
              }
            }
          """.asRight)
        }
      }
    }

  test("[gmos imaging] GMOS imaging observations have correct observing mode type"):
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid).flatMap { tid =>
        for {
          oidN <- createGmosNorthImagingObservationAs(pi, pid, tid)
          oidS <- createGmosSouthImagingObservationAs(pi, pid, tid)
          result <- expect(pi, s"""
            query {
              observations(WHERE: { id: { IN: ["$oidN", "$oidS"] } }) {
                matches {
                  id
                  observingMode {
                    mode
                  }
                }
              }
            }
          """, json"""
            {
              "observations": {
                "matches": [
                  {
                    "id": $oidN,
                    "observingMode": {
                      "mode": "GMOS_NORTH_IMAGING"
                    }
                  },
                  {
                    "id": $oidS,
                    "observingMode": {
                      "mode": "GMOS_SOUTH_IMAGING"
                    }
                  }
                ]
              }
            }
          """.asRight)
        } yield result
      }
    }

  test("[gmos imaging] cannot create GMOS North imaging observation with empty filters"):
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid).flatMap { tid =>
        expect(pi, s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                targetEnvironment: {
                  asterism: [${tid.asJson}]
                }
                scienceRequirements: {
                  imaging: {
                    minimumFov: { arcseconds: 100 }
                    narrowFilters: false
                    broadFilters: false
                    combinedFilters: true
                  }
                }
                observingMode: {
                  gmosNorthImaging: {
                    filters: []
                  }
                }
              }
            }) {
              observation {
                observingMode {
                  gmosNorthImaging {
                    filters
                  }
                }
              }
            }
          }
        """, List("Argument 'input.SET.observingMode.gmosNorthImaging' is invalid: At least one filter must be specified for GMOS imaging observations.").asLeft)
      }
    }

  test("[gmos imaging] cannot create GMOS South imaging observation with empty filters"):
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid).flatMap { tid =>
        expect(pi, s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                targetEnvironment: {
                  asterism: [${tid.asJson}]
                }
                scienceRequirements: {
                  imaging: {
                    minimumFov: { arcseconds: 100 }
                    narrowFilters: false
                    broadFilters: false
                    combinedFilters: true
                  }
                }
                observingMode: {
                  gmosSouthImaging: {
                    filters: []
                  }
                }
              }
            }) {
              observation {
                observingMode {
                  gmosSouthImaging {
                    filters
                  }
                }
              }
            }
          }
        """, List("Argument 'input.SET.observingMode.gmosSouthImaging' is invalid: At least one filter must be specified for GMOS imaging observations.").asLeft)
      }
    }

  test("[gmos imaging] can create GMOS North imaging observation with spatial offsets"):
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid).flatMap { tid =>
        expect(pi, s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                targetEnvironment: {
                  asterism: [${tid.asJson}]
                }
                scienceRequirements: {
                  imaging: {
                    minimumFov: { arcseconds: 100 }
                    narrowFilters: false
                    broadFilters: false
                    combinedFilters: true
                  }
                }
                observingMode: {
                  gmosNorthImaging: {
                    filters: [G_PRIME, R_PRIME]
                    explicitSpatialOffsets: [
                      { p: { arcseconds: "1.5" }, q: { arcseconds: "2.0" } },
                      { p: { arcseconds: "-0.5" }, q: { arcseconds: "1.0" } }
                    ]
                  }
                }
              }
            }) {
              observation {
                observingMode {
                  gmosNorthImaging {
                    filters
                    spatialOffsets {
                      p { arcseconds }
                      q { arcseconds }
                    }
                    explicitSpatialOffsets {
                      p { arcseconds }
                      q { arcseconds }
                    }
                    defaultSpatialOffsets {
                      p { arcseconds }
                      q { arcseconds }
                    }
                  }
                }
              }
            }
          }
        """, json"""
          {
            "createObservation": {
              "observation": {
                "observingMode": {
                  "gmosNorthImaging": {
                    "filters": ["G_PRIME", "R_PRIME"],
                    "spatialOffsets": [
                      { "p": { "arcseconds": 1.500000 }, "q": { "arcseconds": 2.000000 } },
                      { "p": { "arcseconds": -0.500000 }, "q": { "arcseconds": 1.000000 } }
                    ],
                    "explicitSpatialOffsets": [
                      { "p": { "arcseconds": 1.500000 }, "q": { "arcseconds": 2.000000 } },
                      { "p": { "arcseconds": -0.500000 }, "q": { "arcseconds": 1.000000 } }
                    ],
                    "defaultSpatialOffsets": []
                  }
                }
              }
            }
          }
        """.asRight)
      }
    }

  test("[gmos imaging] can create GMOS South imaging observation with spatial offsets"):
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid).flatMap { tid =>
        expect(pi, s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                targetEnvironment: {
                  asterism: [${tid.asJson}]
                }
                scienceRequirements: {
                  imaging: {
                    minimumFov: { arcseconds: 100 }
                    narrowFilters: false
                    broadFilters: false
                    combinedFilters: true
                  }
                }
                observingMode: {
                  gmosSouthImaging: {
                    filters: [G_PRIME, R_PRIME]
                    explicitSpatialOffsets: [
                      { p: { arcseconds: "2.5" }, q: { arcseconds: "-1.5" } }
                    ]
                  }
                }
              }
            }) {
              observation {
                observingMode {
                  gmosSouthImaging {
                    filters
                    spatialOffsets {
                      p { arcseconds }
                      q { arcseconds }
                    }
                    explicitSpatialOffsets {
                      p { arcseconds }
                      q { arcseconds }
                    }
                    defaultSpatialOffsets {
                      p { arcseconds }
                      q { arcseconds }
                    }
                  }
                }
              }
            }
          }
        """, json"""
          {
            "createObservation": {
              "observation": {
                "observingMode": {
                  "gmosSouthImaging": {
                    "filters": ["G_PRIME", "R_PRIME"],
                    "spatialOffsets": [
                      { "p": { "arcseconds": 2.500000 }, "q": { "arcseconds": -1.500000 } }
                    ],
                    "explicitSpatialOffsets": [
                      { "p": { "arcseconds": 2.500000 }, "q": { "arcseconds": -1.500000 } }
                    ],
                    "defaultSpatialOffsets": []
                  }
                }
              }
            }
          }
        """.asRight)
      }
    }

  test("[gmos imaging] GMOS imaging observation defaults to empty spatial offsets"):
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid).flatMap { tid =>
        expect(pi, s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                targetEnvironment: {
                  asterism: [${tid.asJson}]
                }
                scienceRequirements: {
                  imaging: {
                    minimumFov: { arcseconds: 100 }
                    narrowFilters: false
                    broadFilters: false
                    combinedFilters: true
                  }
                }
                observingMode: {
                  gmosNorthImaging: {
                    filters: [G_PRIME, R_PRIME]
                  }
                }
              }
            }) {
              observation {
                observingMode {
                  gmosNorthImaging {
                    spatialOffsets {
                      p { arcseconds }
                      q { arcseconds }
                    }
                    explicitSpatialOffsets {
                      p { arcseconds }
                      q { arcseconds }
                    }
                    defaultSpatialOffsets {
                      p { arcseconds }
                      q { arcseconds }
                    }
                  }
                }
              }
            }
          }
        """, json"""
          {
            "createObservation": {
              "observation": {
                "observingMode": {
                  "gmosNorthImaging": {
                    "spatialOffsets": [],
                    "explicitSpatialOffsets": [],
                    "defaultSpatialOffsets": []
                  }
                }
              }
            }
          }
        """.asRight)
      }
    }

  test("[flamingos2] F2 observation defaults to 4 spatial offsets"):
    createProgramAs(pi).flatMap { pid =>
      expect(pi, s"""
        mutation {
          createObservation(input: {
            programId: ${pid.asJson}
            SET: {
              observingMode: {
                flamingos2LongSlit: {
                  disperser: R1200_HK
                  filter: Y
                  fpu: LONG_SLIT_2
                }
              }
            }
          }) {
            observation {
              observingMode {
                flamingos2LongSlit {
                  spatialOffsets {
                    p { arcseconds }
                    q { arcseconds }
                  }
                  explicitSpatialOffsets {
                    p { arcseconds }
                    q { arcseconds }
                  }
                  defaultSpatialOffsets {
                    p { arcseconds }
                    q { arcseconds }
                  }
                }
              }
            }
          }
        }
      """, json"""
        {
          "createObservation": {
            "observation": {
              "observingMode": {
                "flamingos2LongSlit": {
                  "spatialOffsets": [
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": 15.000000 } },
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": -15.000000 } },
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": -15.000000 } },
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": 15.000000 } }
                  ],
                  "explicitSpatialOffsets": null,
                  "defaultSpatialOffsets": [
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": 15.000000 } },
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": -15.000000 } },
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": -15.000000 } },
                    { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": 15.000000 } }
                  ]
                }
              }
            }
          }
        }
      """.asRight)
    }
}
