// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package feature

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Decoder
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.CallForProposalsType
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.CallForProposals
import lucuma.core.model.Observation
import lucuma.core.model.ObservationReference
import lucuma.core.model.Program
import lucuma.core.model.ProgramReference
import lucuma.core.model.ProposalReference
import lucuma.core.model.Semester
import lucuma.core.model.User
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.DatasetReference
import lucuma.core.syntax.string.*


class reference extends OdbSuite {

  val pi       = TestUsers.Standard.pi(1, 101)
  val guest    = TestUsers.guest(2)
  val service  = TestUsers.service(3)
  val staff    = TestUsers.Standard.staff(4, 104)

  val validUsers = List(pi, guest, service, staff)

  // Unsafe convenience for testing.
  extension (s: String) {
    def proposalReference: ProposalReference =
      ProposalReference.fromString.unsafeGet(s)

    def programReference: ProgramReference =
      ProgramReference.fromString.unsafeGet(s)

    def observationReference: ObservationReference =
      ObservationReference.fromString.unsafeGet(s)

    def datasetReference: DatasetReference =
      DatasetReference.fromString.unsafeGet(s)

    def semester: Semester =
      Semester.unsafeFromString(s)
  }

  val sem2024A   = "2024A".semester
  val ref2024A1  = "G-2024A-0001".proposalReference
  val ref2024A1Q = "G-2024A-0001-Q".programReference
  val ref2024A1C = "G-2024A-0001-C".programReference

  val sem2024B   = "2024B".semester
  val ref2024B1  = "G-2024B-0001".proposalReference
  val ref2024B2  = "G-2024B-0002".proposalReference
  val ref2024B3  = "G-2024B-0003".proposalReference

  val sem2025A   = "2025A".semester
  val ref2025A1  = "G-2025A-0001".proposalReference

  val sem2025B   = "2025B".semester
  val ref2025B1  = "G-2025B-0001".proposalReference
  val ref2025B2  = "G-2025B-0002".proposalReference

  val sem2010A   = "2010A".semester
  val ref2010A1  = "G-2010A-0001".proposalReference

  def pidsWhere(where: String): IO[List[Program.Id]] =
    query(pi, s"query { programs(WHERE: $where) { matches { id } } }")
      .flatMap {
        _.hcursor
         .downFields("programs", "matches")
         .values
         .toList
         .flatMap(_.toList)
         .traverse { _.hcursor.downField("id").as[Program.Id] }
         .leftMap(f => new RuntimeException(f.message))
         .liftTo[IO]
      }

  def proposalRefsWhere(where: String): IO[List[ProposalReference]] =
    query(pi, s"query { programs(WHERE: $where) { matches { proposal { reference { label } } } } }")
      .flatMap {
        _.hcursor
         .downFields("programs", "matches")
         .values
         .toList
         .flatMap(_.toList)
         .flatTraverse {
           _.hcursor
            .downFields("proposal", "reference", "label")
            .success
            .toList
            .traverse(_.as[ProposalReference])
         }
         .leftMap(f => new RuntimeException(f.message))
         .liftTo[IO]
      }

  def refsWhere[A: Decoder](name: String, where: String): IO[List[A]] =
    query(pi, s"query { $name(WHERE: $where) { matches { reference { label } } } }")
      .flatMap {
        _.hcursor
         .downFields(name, "matches")
         .values
         .toList
         .flatMap(_.toList)
         .flatTraverse {
           _.hcursor
            .downFields("reference", "label")
            .success
            .toList
            .traverse(_.as[A])
         }
         .leftMap(f => new RuntimeException(f.message))
         .liftTo[IO]
      }

  def programRefsWhere(where: String): IO[List[ProgramReference]] =
    refsWhere("programs", where)

  test("submit proposals") {
    for {
      cid0 <- createCallForProposalsAs(staff, semester = sem2024B)
      pid0 <- createProgramWithUsPi(pi)
      _    <- addQueueProposal(pi, pid0, cid0)
      _    <- addPartnerSplits(pi, pid0)
      _    <- addCoisAs(pi, pid0)
      ref0 <- submitProposal(pi, pid0)

      pid1 <- createProgramWithUsPi(pi)
      _    <- addQueueProposal(pi, pid1, cid0)
      _    <- addPartnerSplits(pi, pid1)
      _    <- addCoisAs(pi, pid1)
      ref1 <- submitProposal(pi, pid1)

      cid1 <- createCallForProposalsAs(staff, semester = sem2025A)
      pid2 <- createProgramWithUsPi(pi)
      _    <- addQueueProposal(pi, pid2, cid1)
      _    <- addPartnerSplits(pi, pid2)
      _    <- addCoisAs(pi, pid2)
      ref2 <- submitProposal(pi, pid2)
    } yield {
      assertEquals(ref0, ref2024B1)
      assertEquals(ref1, ref2024B2)
      assertEquals(ref2, ref2025A1)
    }
  }

  test("lookup via proposal ref") {
    expect(
      user  = pi,
      query = s"""
        query {
          program(proposalReference: "G-2024B-0001") {
            reference { label }
            proposal {
              reference {
                label
                semester
                semesterIndex
              }
            }
          }
        }
      """,
      expected = Right(
        json"""
          {
            "program": {
              "reference": null,
              "proposal": {
                "reference": {
                  "label": ${ref2024B1.label},
                  "semester": "2024B",
                  "semesterIndex": 1
                }
              }
            }
          }
        """
      )
    )
  }

  test("lookup program, no pid, no ref") {
    expect(
      user  = pi,
      query = s"""
        query {
          program {
            proposal { reference { label } }
          }
        }
      """,
      expected = Right(
        json"""
          {
            "program": null
          }
        """
      )
    )
  }

  test("select via WHERE proposal IS_NULL = true") {
    for {
      pid0 <- createProgramAs(pi)
      pid1 <- createProgramAs(pi)
      _    <- addProposal(pi, pid1)
      res0 <- pidsWhere("{ proposal: { IS_NULL: true } }")
      res1 <- pidsWhere("{ proposal: { reference: { IS_NULL: true } } }")
    } yield {
      assertEquals(res0, List(pid0))
      assertEquals(res1, List(pid0, pid1))
    }
  }

  test("select via WHERE proposal IS_NULL = false") {
    assertIO(
      proposalRefsWhere( s"""
        {
          proposal: {
            IS_NULL: false
            reference: { IS_NULL: false }
          }
        }
      """),
      List(ref2024B1, ref2024B2, ref2025A1)
    )
  }

  test("select via WHERE proposal reference label LIKE") {
    assertIO(
      proposalRefsWhere( s"""{ proposal: { reference: { label: { LIKE: "G-2024B-%" } } } }"""),
      List(ref2024B1, ref2024B2)
    )
  }

  test("select via WHERE proposal reference semester") {
    assertIO(
      proposalRefsWhere( s"""{ proposal: { reference: { semester: { EQ: "2024B" } } } }"""),
      List(ref2024B1, ref2024B2)
    )
  }

  test("select via WHERE proposal reference semesterIndex") {
    assertIO(
      proposalRefsWhere( s"""{ proposal: { reference: { semesterIndex: { GT: 1 } } } }"""),
      List(ref2024B2)
    )
  }

  test("submit, unsubmit, resubmit, same reference") {
    for {
      cid  <- createCallForProposalsAs(staff, semester = sem2010A)
      pid  <- createProgramWithUsPi(pi)
      _    <- addQueueProposal(pi, pid, cid)
      _    <- addPartnerSplits(pi, pid)
      _    <- addCoisAs(pi, pid)
      ref0 <- submitProposal(pi, pid)
      _    <- unsubmitProposal(pi, pid)
      ref1 <- submitProposal(pi, pid)
    } yield {
      assertEquals(ref0, ref2010A1)
      assertEquals(ref1, ref2010A1)
    }
  }

  test("accept proposal") {
    for {
      cid  <- createCallForProposalsAs(staff, semester = sem2024A)
      pid  <- createProgramWithUsPi(pi)
      _    <- addQueueProposal(pi, pid, cid)
      _    <- addPartnerSplits(pi, pid)
      _    <- addCoisAs(pi, pid)
      _    <- submitProposal(pi, pid)
      ref0 <- fetchProgramReference(pi, pid)
       _   <- acceptProposal(staff, pid)
      ref1 <- fetchProgramReference(pi, pid)
    } yield {
      assert(ref0.isEmpty)
      assertEquals(ref1, ref2024A1Q.some)
    }
  }

  test("program reference SCI fields") {
    for {
      cid <- createCallForProposalsAs(staff, semester = sem2024A)
      pid <- createProgramWithUsPi(pi)
      _   <- addQueueProposal(pi, pid, cid)
      _   <- addPartnerSplits(pi, pid)
      _   <- addCoisAs(pi, pid)
      _   <- submitProposal(pi, pid)
      _   <- acceptProposal(staff, pid)
      _   <- expect(pi, s"""
          query {
            program(programId: "$pid") {
              reference {
                label
                ... on ScienceProgramReference {
                  scienceSubtype
                  semester
                  semesterIndex
                }
              }
            }
          }
        """,
        json"""
          {
            "program": {
              "reference": {
                "label": "G-2024A-0002-Q",
                "scienceSubtype": "QUEUE",
                "semester": "2024A",
                "semesterIndex": 2
              }
            }
          }
        """.asRight
      )
    } yield ()
  }

  test("change proposal class in accepted proposal") {
    def toClassical(pid: Program.Id, cid: CallForProposals.Id): IO[Json] =
      query(
        staff,
        s"""
          mutation {
            updateProposal (
              input: {
                programId: "$pid"
                SET: {
                  callId: "$cid"
                  type: {
                    classical: {
                      minPercentTime: 50
                      partnerSplits: [
                        {
                          partner: CA,
                          percent: 100
                        }
                      ]
                    }
                  }
                }
              }
            ) { proposal { category } }
          }
        """
      )

    for {
      cid  <- createCallForProposalsAs(staff, CallForProposalsType.RegularSemester, sem2024A)
      pid  <- fetchPid(pi, ref2024A1)
      _    <- toClassical(pid, cid)
      prog <- fetchProgramReference(pi, pid)
    } yield assertEquals(prog, ref2024A1C.some)
  }

  test("setProposalReference illegal attempt to define multiple references") {
    createProgramAs(pi).flatMap { pid =>
      expect(pi,
        s"""
          mutation {
            setProgramReference(input: {
              programId: "$pid"
              SET: {
                calibration: { semester: "2025B", instrument: GMOS_SOUTH },
                engineering: { semester: "2025B", instrument: GMOS_SOUTH }
              }
            }) {
              reference { label }
            }
          }
        """,
        List("Exactly one key must be specified for oneOf input object ProgramReferencePropertiesInput in field 'setProgramReference' of type 'Mutation', but found 'calibration', 'engineering'").asLeft
      )
    }
  }

  test("setProposalReference, illegal semester") {
    createProgramAs(pi).flatMap { pid =>
      expect(staff,
        s"""
          mutation {
            setProgramReference(input: {
              programId: "$pid"
              SET: {
                calibration: { semester: "9999B", instrument: GMOS_SOUTH }
              }
            }) {
              reference { label }
            }
          }
        """,
        List("The maximum semester is capped at the current year +1 (9999B specified).").asLeft
      )
    }
  }

  test("setProposalReference CAL") {
    for {
      pid <- createProgramAs(pi)
      ref <- setProgramReference(staff, pid, """calibration: { semester: "2025B", instrument: GMOS_SOUTH }""")
    } yield assertEquals(ref, "G-2025B-CAL-GMOSS-01".programReference.some)
  }

  test("setProposalReference CAL, different instrument has its own index") {
    for {
      pid <- createProgramAs(pi)
      ref <- setProgramReference(staff, pid, """calibration: { semester: "2025B", instrument: GMOS_NORTH }""")
    } yield assertEquals(ref, "G-2025B-CAL-GMOSN-01".programReference.some)
  }

  test("program reference CAL fields") {
    expect(pi, s"""
      query {
        program(programReference: "G-2025B-CAL-GMOSS-01") {
          reference {
            label
            ... on CalibrationProgramReference {
              semester
              instrument
              semesterIndex
            }
          }
        }
      }""",
      json"""
        {
          "program": {
            "reference": {
              "label": "G-2025B-CAL-GMOSS-01",
              "semester": "2025B",
              "instrument": "GMOS_SOUTH",
              "semesterIndex": 1
            }
          }
        }
      """.asRight
    )
  }

  test("setProposalReference COM") {
    for {
      pid <- createProgramAs(pi)
      ref <- setProgramReference(staff, pid, """commissioning: { semester: "2025B", instrument: GMOS_SOUTH }""")
    } yield assertEquals(ref, "G-2025B-COM-GMOSS-01".programReference.some)
  }

  test("setProposalReference COM, second has increased index") {
    for {
      pid <- createProgramAs(pi)
      ref <- setProgramReference(staff, pid, """commissioning: { semester: "2025B", instrument: GMOS_SOUTH }""")
    } yield assertEquals(ref, "G-2025B-COM-GMOSS-02".programReference.some)
  }

  test("setProposalReference COM, different instrument has its own index") {
    for {
      pid <- createProgramAs(pi)
      ref <- setProgramReference(staff, pid, """commissioning: { semester: "2025B", instrument: GMOS_NORTH }""")
    } yield assertEquals(ref, "G-2025B-COM-GMOSN-01".programReference.some)
  }

  test("program reference COM fields") {
    expect(pi, s"""
      query {
        program(programReference: "G-2025B-COM-GMOSS-01") {
          reference {
            label
            ... on CommissioningProgramReference {
              semester
              instrument
              semesterIndex
            }
          }
        }
      }""",
      json"""
        {
          "program": {
            "reference": {
              "label": "G-2025B-COM-GMOSS-01",
              "semester": "2025B",
              "instrument": "GMOS_SOUTH",
              "semesterIndex": 1
            }
          }
        }
      """.asRight
    )
  }

  test("setProgramReference SYS") {
    for {
      pid <- createProgramAs(pi)
      ref <- setProgramReference(staff, pid, """system: { description: "SPECPHOTO" }""")
    } yield assertEquals(ref, "SYS-SPECPHOTO".programReference.some)
  }

  test("setProgramReference ENG") {
    for {
      pid <- createProgramAs(pi)
      ref <- setProgramReference(staff, pid, """engineering: { semester: "2025B", instrument: GMOS_SOUTH }""")
    } yield assertEquals(ref, "G-2025B-ENG-GMOSS-01".programReference.some)
  }

  test("setProgramReference ENG, second time increases index") {
    for {
      pid <- createProgramAs(pi)
      ref <- setProgramReference(staff, pid, """engineering: { semester: "2025B", instrument: GMOS_SOUTH }""")
    } yield assertEquals(ref, "G-2025B-ENG-GMOSS-02".programReference.some)
  }

  test("program reference ENG fields") {
    expect(pi, s"""
      query {
        program(programReference: "G-2025B-ENG-GMOSS-02") {
          reference {
            label
            ... on EngineeringProgramReference {
              semester
              instrument
              semesterIndex
            }
          }
        }
      }""",
      json"""
        {
          "program": {
            "reference": {
              "label": "G-2025B-ENG-GMOSS-02",
              "semester": "2025B",
              "instrument": "GMOS_SOUTH",
              "semesterIndex": 2
            }
          }
        }
      """.asRight
    )
  }

  test("setProposalReference XPL") {
    for {
      pid <- createProgramAs(pi)
      ref <- setProgramReference(staff, pid, """example: { instrument: GMOS_SOUTH }""")
    } yield assertEquals(ref, "G-XPL-GMOSS".programReference.some)
  }

  test("setProposalReference XPL, already taken") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        staff,
        s"""
          mutation {
            setProgramReference(input: {
              programId: "$pid"
              SET: { example: { instrument: GMOS_SOUTH } }
            }) {
              reference { label }
            }
          }
        """,
        List("Program reference 'G-XPL-GMOSS' already exists.").asLeft
      )
    }
  }

  test("program reference XPL fields") {
    expect(pi, s"""
      query {
        program(programReference: "G-XPL-GMOSS") {
          reference {
            label
            ... on ExampleProgramReference {
              instrument
            }
          }
        }
      }""",
      json"""
        {
          "program": {
            "reference": {
              "label": "G-XPL-GMOSS",
              "instrument": "GMOS_SOUTH"
            }
          }
        }
      """.asRight
    )
  }

  test("setProposalReference LIB") {
    for {
      pid <- createProgramAs(pi)
      ref <- setProgramReference(staff, pid, """library: { instrument: GMOS_SOUTH, description: "FOO" }""")
    } yield assertEquals(ref, "G-LIB-GMOSS-FOO".programReference.some)
  }

  test("setProposalReference LIB, invalid description") {
    createProgramAs(pi).flatMap { pid =>
      expect(staff,
        s"""
          mutation {
            setProgramReference(input: {
              programId: "$pid"
              SET: {
                library: { instrument: GMOS_SOUTH, description: "x" }
              }
            }) {
              reference { label }
            }
          }
        """,
        List("Argument 'input.SET.library.description' is invalid: Predicate failed: \"x\".matches(\"^[A-Z0-9]+\").").asLeft
      )
    }
  }

  test("program reference LIB fields") {
    expect(pi, s"""
      query {
        program(programReference: "G-LIB-GMOSS-FOO") {
          reference {
            label
            ... on LibraryProgramReference {
              instrument
              description
            }
          }
        }
      }""",
      json"""
        {
          "program": {
            "reference": {
              "label": "G-LIB-GMOSS-FOO",
              "instrument": "GMOS_SOUTH",
              "description": "FOO"
            }
          }
        }
      """.asRight
    )
  }

  test("setProposalReference MON") {
    for {
      pid <- createProgramAs(pi)
      ref <- setProgramReference(staff, pid, """monitoring: { semester: "2024A", instrument: GMOS_NORTH }""")
    } yield assertEquals(ref, "G-2024A-MON-GMOSN-01".programReference.some)
  }

  test("program reference MON fields") {
    expect(pi, s"""
      query {
        program(programReference: "G-2024A-MON-GMOSN-01") {
          reference {
            label
            ... on MonitoringProgramReference {
              semester
              instrument
              semesterIndex
            }
          }
        }
      }""",
      json"""
        {
          "program": {
            "reference": {
              "label": "G-2024A-MON-GMOSN-01",
              "semester": "2024A",
              "instrument": "GMOS_NORTH",
              "semesterIndex": 1
            }
          }
        }
      """.asRight
    )
  }

  test("setProposalReference SCI, no proposal") {
    for {
      pid <- createProgramAs(pi)
      ref <- setProgramReference(staff, pid, """science: { semester: "2025B", scienceSubtype: QUEUE }""")
    } yield assertEquals(ref, none)
  }

  test("setProposalReference SCI, yes proposal") {
    for {
      cid <- createCallForProposalsAs(staff, semester = sem2025B)
      pid <- createProgramWithNonPartnerPi(pi)
      _   <- addQueueProposal(pi, pid, cid)
      _   <- addPartnerSplits(pi, pid)
      _   <- addCoisAs(pi, pid)
      _   <- acceptProposal(staff, pid)
      ref <- fetchProgramReference(pi, pid)
    } yield assertEquals(ref, "G-2025B-0001-Q".programReference.some)
  }

  test("setProposalReference SCI, change subtype but index remains the same") {
    for {
      pid <- fetchPid(pi, "G-2025B-0001-Q".programReference)
      ref <- setProgramReference(staff, pid, """science: { semester: "2025B", scienceSubtype: CLASSICAL }""")
    } yield assertEquals(ref, "G-2025B-0001-C".programReference.some)
  }

  test("attempt setProposalReference CAL, though it has a proposal") {
    fetchPid(pi, "G-2025B-0001-C".programReference).flatMap { pid =>
      expect(
        staff,
        s"""
          mutation {
            setProgramReference(input: {
              programId: "$pid"
              SET: { calibration: { semester: "2025B", instrument: GMOS_SOUTH } }
            }) {
              reference { label }
            }
          }
        """,
        List(s"Cannot set the program reference for $pid to CAL until its proposal is removed.").asLeft
      )
    }
  }

  test("setProposalReference SCI -> CAL -> SCI, index increases") {
    for {
      cid <- createCallForProposalsAs(staff, semester = sem2025B)
      pid <- fetchPid(pi, "G-2025B-0001-C".programReference)
      _   <- deleteProposal(staff, pid)
      _   <- setProgramReference(staff, pid, """calibration: { semester: "2025B", instrument: GMOS_SOUTH }""")
      _   <- setProgramReference(staff, pid, """science: { semester: "2025B", scienceSubtype: QUEUE }""")
      _   <- addProposal(pi, pid, cid.some, "queue: { }".some)
      _   <- acceptProposal(staff, pid)
      ref <- fetchProgramReference(pi, pid)
    } yield assertEquals(ref, "G-2025B-0002-Q".programReference.some)
  }

  test("select via WHERE program reference label LIKE") {
    assertIO(
      programRefsWhere( s"""{ reference: { label: { LIKE: "G-2025B-%" } } }"""),
      List(
        "G-2025B-CAL-GMOSS-01",
        "G-2025B-CAL-GMOSN-01",
        "G-2025B-COM-GMOSS-01",
        "G-2025B-COM-GMOSS-02",
        "G-2025B-COM-GMOSN-01",
        "G-2025B-ENG-GMOSS-01",
        "G-2025B-ENG-GMOSS-02",
        "G-2025B-0002-Q"
      ).map(_.programReference)
    )
  }

  test("select via WHERE program reference semester") {
    assertIO(
      programRefsWhere( s"""{ reference: { semester: { EQ: "2025B" } } }"""),
      List(
        "G-2025B-CAL-GMOSS-01",
        "G-2025B-CAL-GMOSN-01",
        "G-2025B-COM-GMOSS-01",
        "G-2025B-COM-GMOSS-02",
        "G-2025B-COM-GMOSN-01",
        "G-2025B-ENG-GMOSS-01",
        "G-2025B-ENG-GMOSS-02",
        "G-2025B-0002-Q"
      ).map(_.programReference)
    )
  }

  test("select via WHERE program reference semester, only ENG") {
    assertIO(
      programRefsWhere( s"""{ type: { EQ: ENGINEERING }, reference: { semester: { EQ: "2025B" } } }"""),
      List(
        "G-2025B-ENG-GMOSS-01",
        "G-2025B-ENG-GMOSS-02"
      ).map(_.programReference)
    )
  }

  test("select via WHERE program reference semesterIndex") {
    assertIO(
      programRefsWhere( s"""{ reference: { semesterIndex: { GT: 1 } } }"""),
      List(
        "G-2024A-0002-Q",
        "G-2025B-COM-GMOSS-02",
        "G-2025B-ENG-GMOSS-02",
        "G-2025B-0002-Q"
      ).map(_.programReference)
    )
  }

  test("select via WHERE program reference semester, only SYS") {
    assertIO(
      programRefsWhere( s"""{ type: { EQ: SYSTEM } }"""),
      List(
        "SYS-SPECPHOTO"
      ).map(_.programReference)
    )
  }

  test("select via WHERE program reference instrument") {
    assertIO(
      programRefsWhere( s"""{ reference: { instrument: { EQ: GMOS_SOUTH } } }"""),
      List(
        "G-2025B-CAL-GMOSS-01",
        "G-2025B-COM-GMOSS-01",
        "G-2025B-COM-GMOSS-02",
        "G-2025B-ENG-GMOSS-01",
        "G-2025B-ENG-GMOSS-02",
        "G-XPL-GMOSS",
        "G-LIB-GMOSS-FOO"
      ).map(_.programReference)
    )
  }

  test("select via WHERE program reference description") {
    assertIO(
      programRefsWhere( s"""{ reference: { description: { EQ: "FOO" } } }"""),
      List("G-LIB-GMOSS-FOO".programReference)
    )
  }

  test("select via WHERE program reference description not matching") {
    assertIO(
      programRefsWhere( s"""{ reference: { description: { EQ: "BAR" } } }"""),
      Nil
    )
  }

  test("select via WHERE program reference science subtype") {
    assertIO(
      programRefsWhere( s"""{ reference: { scienceSubtype: { EQ: CLASSICAL } } }"""),
      List("G-2024A-0001-C".programReference)
    )
  }

  def expectObservationReference(
    oid: Observation.Id,
    ref: ObservationReference
  ): IO[Unit] =
    expect(pi, s"""
      query {
        observation(observationId: "$oid") {
          reference {
            label
            program { label }
            index
          }
        }
      }""",
      json"""
        {
          "observation": {
            "reference": {
              "label": ${ref.label},
              "program": {
                "label": ${ref.programReference.label}
              },
              "index": ${ref.observationIndex.value}
            }
          }
        }
      """.asRight
    )

  def expectObservationIndex(
    oid:   Observation.Id,
    index: Int
  ): IO[Unit] =
    expect(pi,
      s"""query { observation(observationId: "$oid") { index } }""",
      json"""{ "observation": { "index": $index } }""".asRight
    )

  test("no observation reference") {
    for {
      pid <- createProgramAs(pi)
      o   <- createObservationAs(pi, pid)
      _   <- expect(pi, s"""query { observation(observationId: "$o") { reference { label } } }""", json"""{"observation":  { "reference":  null}}""".asRight)
    } yield ()
  }

  test("observation reference SCI") {
    val pRef = "G-2025B-0002-Q".programReference
    for {
      pid <- fetchPid(pi, pRef)
      o   <- createObservationAs(pi, pid)
      _   <- expectObservationReference(o, s"${pRef.label}-0001".observationReference)
      _   <- expectObservationIndex(o, 1)
    } yield ()
  }

  test("observation reference ENG") {
    val pRef = "G-2025B-ENG-GMOSS-02".programReference
    for {
      pid <- fetchPid(pi, pRef)
      o1  <- createObservationAs(pi, pid)
      _   <- expectObservationReference(o1, s"${pRef.label}-0001".observationReference)
      _   <- expectObservationIndex(o1, 1)
      o2  <- createObservationAs(pi, pid)
      _   <- expectObservationReference(o2, s"${pRef.label}-0002".observationReference)
      _   <- expectObservationIndex(o2, 2)
    } yield ()
  }

  def observationRefsWhere(where: String): IO[List[ObservationReference]] =
    refsWhere("observations", where)

  test("select via WHERE observation reference label") {
    assertIO(
      observationRefsWhere( s"""{ reference: { label: { LIKE: "%-Q-%" } } }"""),
      List(
        "G-2025B-0002-Q-0001".observationReference
      )
    )
  }

  test("select via WHERE observation reference index") {
    assertIO(
      observationRefsWhere( s"""{ reference: { index: { EQ: 2 } } }"""),
      List("G-2025B-ENG-GMOSS-02-0002".observationReference)
    )
  }

  test("select via WHERE observation reference program ") {
    assertIO(
      observationRefsWhere( s"""{ reference: { program: { label: { LIKE: "G-2025B-ENG-GMOSS-%" } } } }"""),
      List(
        "G-2025B-ENG-GMOSS-02-0001".observationReference,
        "G-2025B-ENG-GMOSS-02-0002".observationReference
      )
    )
  }

  test("select via WHERE observation reference not null") {
    assertIO(
      observationRefsWhere( s"""{ reference: { IS_NULL: false } }"""),
      List(
        "G-2025B-0002-Q-0001".observationReference,
        "G-2025B-ENG-GMOSS-02-0001".observationReference,
        "G-2025B-ENG-GMOSS-02-0002".observationReference
      )
    )
  }

  test("select via WHERE observation reference is null") {
    // We created a single observation in a program without a program reference
    // above in test 'no observation reference'.
    assertIO(
      query(pi, s"""query { observations(WHERE: { reference: { IS_NULL: true } }) { matches { id } } }""")
        .map {
          _.hcursor
           .downFields("observations", "matches")
           .values
           .toList
           .flatMap(_.toList)
           .size
        },
      1
    )
  }

  test("lookup via observation ref") {
    expect(
      user  = pi,
      query = s"""
        query {
          observation(observationReference: "G-2025B-ENG-GMOSS-02-0001") {
            reference {
              label
              index
              program { label }
            }
          }
        }
      """,
      expected = Right(
        json"""
          {
            "observation": {
              "reference": {
                "label": "G-2025B-ENG-GMOSS-02-0001",
                "index": 1,
                "program": {
                  "label": "G-2025B-ENG-GMOSS-02"
                }
              }
            }
          }
        """
      )
    )
  }

  test("lookup observation, no oid, no ref") {
    expect(
      user     = pi,
      query    = s""" query { observation { reference { label } } } """,
      expected = json"""{ "observation": null }""".asRight
    )
  }

  test("change program reference, updates observation reference") {
    val pRef = "G-2025B-ENG-GMOSS-02".programReference
    for {
      pid <- fetchPid(pi, pRef)
      ref <- setProgramReference(staff, pid, """calibration: { semester: "2025B", instrument: GMOS_SOUTH }""")
      oref1 = s"${ref.get.label}-0001".observationReference
      oref2 = s"${ref.get.label}-0002".observationReference
      _  <- fetchOid(pi, oref1)
      _  <- fetchOid(pi, oref2)
    } yield ()
  }

  test("observation reference after observation creation") {
    val pRef = "G-XPL-GMOSN".programReference
    for {
      pid <- createProgramAs(pi)
      o1  <- createObservationAs(pi, pid)
      o2  <- createObservationAs(pi, pid)
      ref <- setProgramReference(staff, pid, """example: { instrument: GMOS_NORTH }""")
      _   <- expectObservationReference(o1, s"${pRef.label}-0001".observationReference)
      _   <- expectObservationReference(o2, s"${pRef.label}-0002".observationReference)
    } yield ()
  }

  def expectDatasetReference(
    user: User,
    did:  Dataset.Id,
    ref:  DatasetReference
  ): IO[Unit] =
    expect(user, s"""
      query {
        dataset(datasetId: "$did") {
          reference {
            label
            observation { label }
            stepIndex
            exposureIndex
          }
        }
      }""",
      json"""
        {
          "dataset": {
            "reference": {
              "label": ${ref.label},
              "observation": {
                "label": ${ref.observationReference.label}
              },
              "stepIndex": ${ref.stepIndex.value},
              "exposureIndex": ${ref.exposureIndex.value}
            }
          }
        }
      """.asRight
    )

  test("dataset reference") {
    extension (self: Int) {
      def posInt: PosInt = PosInt.unsafeFrom(self)
    }
    val mode = ObservingModeType.GmosNorthLongSlit
    for {
      pid  <- createProgramAs(pi)
      oid  <- createObservationAs(pi, pid, mode.some)
      pRef <- setProgramReference(staff, pid, """calibration: { semester: "2025B", instrument: GMOS_NORTH }""")
      oRef  = ObservationReference(pRef.get, PosInt.unsafeFrom(1))
      vid  <- recordVisitAs(service, mode.instrument, oid)
      aid  <- recordAtomAs(service, mode.instrument, vid)
      sid0 <- recordStepAs(service, mode.instrument, aid)
      did0 <- recordDatasetAs(service, sid0, "N18630101S0010.fits")
      did1 <- recordDatasetAs(service, sid0, "N18630101S0011.fits")
      sid1 <- recordStepAs(service, mode.instrument, aid)
      did2 <- recordDatasetAs(service, sid1, "N18630101S0012.fits")
      did3 <- recordDatasetAs(service, sid1, "N18630101S0013.fits")
      _    <- expectDatasetReference(pi, did0, DatasetReference(oRef, 1.posInt, 1.posInt))
      _    <- expectDatasetReference(pi, did1, DatasetReference(oRef, 1.posInt, 2.posInt))
      _    <- expectDatasetReference(pi, did2, DatasetReference(oRef, 2.posInt, 1.posInt))
      _    <- expectDatasetReference(pi, did3, DatasetReference(oRef, 2.posInt, 2.posInt))
    } yield ()
  }

  test("no dataset reference") {
    val mode = ObservingModeType.GmosNorthLongSlit
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid, mode.some)
      vid <- recordVisitAs(service, mode.instrument, oid)
      aid <- recordAtomAs(service, mode.instrument, vid)
      sid <- recordStepAs(service, mode.instrument, aid)
      did <- recordDatasetAs(service, sid, "N18630101S0006.fits")
      _   <- expect(pi,
               s"""query { dataset(datasetId: "$did") { reference { label } } }""",
               json"""{ "dataset": { "reference": null } }""".asRight
             )
    } yield ()
  }

  test("lookup via dataset ref") {
    expect(
      user  = pi,
      query = s"""
        query {
          dataset(datasetReference: "G-2025B-CAL-GMOSN-02-0001-0001-0001") {
            reference {
              label
              observation { label }
              stepIndex
              exposureIndex
            }
          }
        }
      """,
      expected = Right(
        json"""
          {
            "dataset": {
              "reference": {
                 "label": "G-2025B-CAL-GMOSN-02-0001-0001-0001",
                 "observation": {
                   "label": "G-2025B-CAL-GMOSN-02-0001"
                 },
                 "stepIndex": 1,
                 "exposureIndex": 1
               }
            }
          }
        """
      )
    )
  }

  test("lookup dataset, no oid, no ref") {
    expect(
      user     = pi,
      query    = s""" query { dataset { reference { label } } } """,
      expected = json"""{ "dataset": null }""".asRight
    )
  }

  def datasetRefsWhere(where: String): IO[List[DatasetReference]] =
    refsWhere("datasets", where)

  test("select via WHERE dataset reference label") {
    assertIO(
      datasetRefsWhere( s"""{ reference: { label: { LIKE: "%-0002" } } }"""),
      List(
        "G-2025B-CAL-GMOSN-02-0001-0001-0002".datasetReference,
        "G-2025B-CAL-GMOSN-02-0001-0002-0002".datasetReference
      )
    )
  }

  test("select via WHERE dataset stepIndex") {
    assertIO(
      datasetRefsWhere( s"""{ reference: { stepIndex: { EQ: 2 } } }"""),
      List(
        "G-2025B-CAL-GMOSN-02-0001-0002-0001".datasetReference,
        "G-2025B-CAL-GMOSN-02-0001-0002-0002".datasetReference
      )
    )
  }

  test("select via WHERE dataset exposureIndex") {
    assertIO(
      datasetRefsWhere( s"""{ reference: { exposureIndex: { EQ: 2 } } }"""),
      List(
        "G-2025B-CAL-GMOSN-02-0001-0001-0002".datasetReference,
        "G-2025B-CAL-GMOSN-02-0001-0002-0002".datasetReference
      )
    )
  }

  test("select via WHERE dataset observation reference") {
    assertIO(
      datasetRefsWhere( s"""{ reference: { observation: { label: { LIKE: "G-2025B-CAL-GMOSN-02-0001" } } } }"""),
      List(
        "G-2025B-CAL-GMOSN-02-0001-0001-0001".datasetReference,
        "G-2025B-CAL-GMOSN-02-0001-0001-0002".datasetReference,
        "G-2025B-CAL-GMOSN-02-0001-0002-0001".datasetReference,
        "G-2025B-CAL-GMOSN-02-0001-0002-0002".datasetReference
      )
    )
  }

  test("select via WHERE dataset reference IS NOT NULL") {
    assertIO(
      datasetRefsWhere( s"""{ reference: { IS_NULL: false } }"""),
      List(
        "G-2025B-CAL-GMOSN-02-0001-0001-0001".datasetReference,
        "G-2025B-CAL-GMOSN-02-0001-0001-0002".datasetReference,
        "G-2025B-CAL-GMOSN-02-0001-0002-0001".datasetReference,
        "G-2025B-CAL-GMOSN-02-0001-0002-0002".datasetReference
      )
    )
  }

  test("change subtype to P, index remains the same") {
    for {
      pid <- fetchPid(pi, "G-2025B-0002-Q".programReference)
      ref <- setProgramReference(staff, pid, """science: { semester: "2025B", scienceSubtype: POOR_WEATHER }""")
    } yield assertEquals(ref, "G-2025B-0002-P".programReference.some)
  }

  test("there is a reference name in instrument_reference_name function for every instrument"):
    Instrument.values.toList.traverse: inst =>
      for
        pid <- createProgramAs(pi)
        ref <- setProgramReference(staff, pid, s"""engineering: { semester: "2025B", instrument: ${inst.tag.toScreamingSnakeCase} }""")
      yield assert(ref.exists(_.label.startsWith(s"G-2025B-ENG-${inst.referenceName}-")))

}
