// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.option.*
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.Instrument
import lucuma.core.model.Attachment
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.syntax.string.*

class mosMasks extends OdbSuite with DatabaseOperations with MosMaskSupport:

  val pi    = TestUsers.Standard.pi(1, 101)
  val pi2   = TestUsers.Standard.pi(2, 102)
  val staff = TestUsers.Standard.staff(3, 103)

  override val validUsers = List(pi, pi2, staff)

  // Scoped to the fixture's programs so that tests do not see each other's masks.
  private def select(
    user:        User,
    instruments: List[Instrument],
    f:           Fixture,
    where:       Option[String]        = None,
    after:       Option[Attachment.Id] = None,
    limit:       Option[Int]           = None
  ): IO[(List[Attachment.Id], Boolean)] =
    val scope = s"{ program: { id: { IN: [${f.p1.asJson}, ${f.p2.asJson}] } } }"
    val args  = List(
      s"instruments: [${instruments.map(_.tag.toScreamingSnakeCase).mkString(", ")}]".some,
      s"WHERE: { AND: [${(scope :: where.toList).mkString(", ")}] }".some,
      after.map(a => s"OFFSET_AFTER: ${a.asJson}"),
      limit.map(l => s"LIMIT: $l")
    ).flatten.mkString(", ")
    query(user, s"query { mosMasks($args) { matches { id maskName program { id } } hasMore } }").map: json =>
      val c = json.hcursor.downField("mosMasks")
      (
        c.downField("matches").require[List[Json]].map(_.hcursor.downField("id").require[Attachment.Id]),
        c.downField("hasMore").require[Boolean]
      )

  // Three GMOS North masks in two programs, one Flamingos 2 mask.
  private case class Fixture(p1: Program.Id, p2: Program.Id, n1: Attachment.Id, n2: Attachment.Id, n3: Attachment.Id, f2: Attachment.Id)

  private def fixture: IO[Fixture] =
    for
      p1 <- createProgramAs(pi)
      p2 <- createProgramAs(pi2)
      n1 <- insertMosMaskAttachment(p1, "GN2026AQ001-01_ODF.fits", Instrument.GmosNorth)
      n2 <- insertMosMaskAttachment(p1, "GN2026AQ001-02_ODF.fits", Instrument.GmosNorth)
      n3 <- insertMosMaskAttachment(p2, "GN2026AQ002-01_ODF.fits", Instrument.GmosNorth)
      f2 <- insertMosMaskAttachment(p2, "GS2026AQ002-02_ODF.fits", Instrument.Flamingos2)
      _  <- insertObsAttachment(p1, "finder", "finder.png")
    yield Fixture(p1, p2, n1, n2, n3, f2)

  test("staff see every mask for the instrument, across programs"):
    for
      f <- fixture
      n <- select(staff, List(Instrument.GmosNorth), f)
      s <- select(staff, List(Instrument.Flamingos2), f)
      g <- select(staff, List(Instrument.GmosSouth), f)
    yield
      assertEquals(n, (List(f.n1, f.n2, f.n3), false))
      assertEquals(s, (List(f.f2), false))
      assertEquals(g, (Nil, false))

  test("several instruments are listed together, none lists nothing"):
    for
      f <- fixture
      b <- select(staff, List(Instrument.GmosNorth, Instrument.Flamingos2), f)
      s <- select(staff, List(Instrument.GmosSouth, Instrument.Flamingos2), f)
      e <- select(staff, Nil, f)
    yield
      assertEquals(b, (List(f.n1, f.n2, f.n3, f.f2), false))
      assertEquals(s, (List(f.f2), false))
      assertEquals(e, (Nil, false))

  test("each match tells which instrument it was cut for"):
    for
      f <- fixture
      j <- query(staff, s"""
             query {
               mosMasks(
                 instruments: [GMOS_NORTH, FLAMINGOS2]
                 WHERE: { program: { id: { EQ: ${f.p2.asJson} } } }
               ) { matches { id mask { instrument } } }
             }
           """)
    yield assertEquals(
      j.hcursor.downFields("mosMasks", "matches").require[List[Json]].map: m =>
        (m.hcursor.downField("id").require[Attachment.Id], m.hcursor.downFields("mask", "instrument").require[String]),
      List((f.n3, "GMOS_NORTH"), (f.f2, "FLAMINGOS2"))
    )

  test("a PI sees nothing, not even their own masks"):
    for
      f <- fixture
      r <- select(pi, List(Instrument.GmosNorth), f)
    yield assertEquals(r, (Nil, false))

  test("masks can be narrowed by mask name"):
    for
      f <- fixture
      r <- select(staff, List(Instrument.GmosNorth), f, where = """{ maskName: { LIKE: "GN2026AQ001-%" } }""".some)
    yield assertEquals(r, (List(f.n1, f.n2), false))

  test("masks can be narrowed by program"):
    for
      f <- fixture
      r <- select(staff, List(Instrument.GmosNorth), f, where = s"""{ program: { id: { EQ: ${f.p2.asJson} } } }""".some)
    yield assertEquals(r, (List(f.n3), false))

  test("results are paged"):
    for
      f  <- fixture
      r0 <- select(staff, List(Instrument.GmosNorth), f, limit = 2.some)
      r1 <- select(staff, List(Instrument.GmosNorth), f, after = f.n2.some, limit = 2.some)
    yield
      assertEquals(r0, (List(f.n1, f.n2), true))
      assertEquals(r1, (List(f.n3), false))
