// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.ACursor
import io.circe.Decoder
import io.circe.Json
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import skunk.codec.numeric.int8
import skunk.codec.text.varchar
import skunk.syntax.all.*

// Ids are hex numbers held as text, so as text they fall out of order at each new digit.
class gidPaging extends OdbSuite:

  val pi: User = TestUsers.Standard.pi(1, 30)

  val validUsers: List[User] = List(pi)

  // The next id of the sequence gets this number, so that ids can straddle a new hex digit.
  def restart(sequence: String, next: Long): IO[Unit] =
    withSession: s =>
      s.unique(sql"SELECT setval($varchar, $int8, false)".query(int8))((sequence, next)).void

  // One page of a list of ids, read from the result at `path`.
  def page[A: Decoder](path: List[String])(query: String): IO[(List[A], Boolean)] =
    this.query(user = pi, query = s"query { $query }").map: (json: Json) =>
      val c: ACursor = path.foldLeft[ACursor](json.hcursor)(_.downField(_))
      (
        c.downField("matches").values.toList.flatten.flatMap(_.hcursor.downField("id").as[A].toOption),
        c.downField("hasMore").as[Boolean].getOrElse(false)
      )

  // Pages the way a client does: the last id of a page is the cursor for the next one.
  def drain[A](fetch: Option[A] => IO[(List[A], Boolean)]): IO[List[A]] =
    def go(offset: Option[A], acc: List[A], pages: Int): IO[List[A]] =
      fetch(offset).flatMap:
        case (ids, true) if pages < 50 => go(ids.lastOption, acc ++ ids, pages + 1)
        case (ids, _)                  => IO.pure(acc ++ ids)
    go(none, Nil, 0)

  def after[A](offset: Option[A]): String =
    offset.foldMap(a => s"""OFFSET_AFTER: "$a"""")

  def observations(pid: Program.Id, limit: Int)(offset: Option[Observation.Id]): IO[(List[Observation.Id], Boolean)] =
    page[Observation.Id](List("observations")):
      s"""observations(WHERE: { program: { id: { EQ: "$pid" } } } ${after(offset)} LIMIT: $limit) { hasMore matches { id } }"""

  def programObservations(pid: Program.Id, limit: Int)(offset: Option[Observation.Id]): IO[(List[Observation.Id], Boolean)] =
    page[Observation.Id](List("program", "observations")):
      s"""program(programId: "$pid") { observations(${after(offset)} LIMIT: $limit) { hasMore matches { id } } }"""

  def targets(pid: Program.Id, limit: Int)(offset: Option[Target.Id]): IO[(List[Target.Id], Boolean)] =
    page[Target.Id](List("targets")):
      s"""targets(WHERE: { program: { id: { EQ: "$pid" } } } ${after(offset)} LIMIT: $limit) { hasMore matches { id } }"""

  def programs(limit: Int)(offset: Option[Program.Id]): IO[(List[Program.Id], Boolean)] =
    page[Program.Id](List("programs")):
      s"""programs(${after(offset)} LIMIT: $limit) { hasMore matches { id } }"""

  test("observations page across a new hex digit, each once and in order"):
    for
      pid  <- createProgramAs(pi)
      _    <- restart("s_observation_id", 0xffe)
      oids <- createObservationAs(pi, pid).replicateA(5)
      all  <- drain(observations(pid, 2))
    yield
      assertEquals(oids.map(_.toString), List("o-ffe", "o-fff", "o-1000", "o-1001", "o-1002"))
      assertEquals(all, oids)

  test("observations are limited in the order of their numbers"):
    for
      pid  <- createProgramAs(pi)
      _    <- restart("s_observation_id", 0xfffe)
      oids <- createObservationAs(pi, pid).replicateA(5)
      one  <- observations(pid, 3)(none)
    yield assertEquals(one, (oids.take(3), true))

  test("a program's observations page across a new hex digit"):
    for
      pid  <- createProgramAs(pi)
      _    <- restart("s_observation_id", 0xffffe)
      oids <- createObservationAs(pi, pid).replicateA(5)
      all  <- drain(programObservations(pid, 2))
    yield assertEquals(all, oids)

  test("targets page across a new hex digit"):
    for
      pid  <- createProgramAs(pi)
      _    <- restart("s_target_id", 0xffe)
      tids <- createTargetAs(pi, pid).replicateA(5)
      all  <- drain(targets(pid, 2))
    yield
      assertEquals(tids.map(_.toString), List("t-ffe", "t-fff", "t-1000", "t-1001", "t-1002"))
      assertEquals(all, tids)

  // The programs made by the tests above have lower ids, and come first.
  test("programs page across a new hex digit"):
    for
      _    <- restart("s_program_id", 0xffe)
      pids <- createProgramAs(pi).replicateA(5)
      all  <- drain(programs(2))
    yield
      assertEquals(pids.map(_.toString), List("p-ffe", "p-fff", "p-1000", "p-1001", "p-1002"))
      assertEquals(all.takeRight(5), pids)
      assertEquals(all, all.distinct)
