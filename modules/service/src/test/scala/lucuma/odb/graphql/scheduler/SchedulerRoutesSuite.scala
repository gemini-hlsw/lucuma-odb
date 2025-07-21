// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package scheduler

import cats.Order.catsKernelOrderingForOrder
import cats.effect.IO
import cats.syntax.all.*
import fs2.Stream
import fs2.compression.Compression
import fs2.text
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.AtomDigest
import lucuma.core.syntax.string.*
import lucuma.core.util.Gid
import lucuma.core.util.Uid
import org.http4s.*
import org.http4s.headers.`Accept-Encoding`
import org.http4s.implicits.*

import scala.jdk.CollectionConverters.*

abstract class SchedulerRoutesSuite extends OdbSuite:

  protected  def responseStream(s: String): Stream[IO, Byte] =
    Stream[IO, String](s).through(text.utf8.encode)

  protected def headers(user: User, gzip: Boolean = false): IO[Headers] =
    val headers = authorizationHeader(user).map(Headers(_))
    if gzip then headers.map(_.put(`Accept-Encoding`(ContentCoding.gzip)))
    else headers

  protected def gzipAtomsRequest(user: User, gzip: Boolean, oids: String*): IO[Request[IO]] =
    headers(user, gzip).map: hs =>
      Request[IO](
        method  = Method.POST,
        uri     = uri"scheduler/atoms",
        headers = hs,
        body    = Stream.emits(oids.toList).intersperse("\n").through(fs2.text.utf8.encode)
      )

  protected def atomsRequest(user: User, oids: String*): IO[Request[IO]] =
    gzipAtomsRequest(user, gzip = false, oids*)

  extension (a: AtomDigest)
    def fields: List[String] =
      List(
        Uid[Atom.Id].show(a.id),
        a.observeClass.tag.toScreamingSnakeCase,
        a.timeEstimate.sum.toMicroseconds.toString,
        a.stepTypes.toList.sorted.map(_.tag.toScreamingSnakeCase).mkString("[", ", ", "]"),
        a.lampTypes.toList.sorted.map(_.tag.toScreamingSnakeCase).mkString("[", ", ", "]")
      )

  private def assertAtoms(
    actual:          Response[IO],
    body:            String,
    expectedStatus:  Status,
    expectedDigests: List[(Observation.Id, List[AtomDigest])]
  ): Unit =
    val actualLines = body.lines.toList.asScala.toList.map: line =>
      val fields = line.split('\t').toList
      fields.take(2) ++ fields.drop(3)
    val expectedLines = expectedDigests.flatMap: (oid, atoms) =>
      atoms.zipWithIndex.map: (a, idx) =>
        Gid[Observation.Id].show(oid) :: idx.toString :: a.fields.drop(1)

    def formatLines(lines: List[List[String]]): String =
      lines.map(_.mkString("\t")).mkString("\n")

    val statusCheck = actual.status === expectedStatus
    val bodyCheck   = actualLines === expectedLines
    assert(statusCheck && bodyCheck, s"Expected '$expectedStatus':\n${formatLines(expectedLines)}\nActual: '${actual.status}':\n${formatLines(actualLines)}")

  extension (resp: IO[Response[IO]])
    def assertResponse(expectedStatus: Status, expectedBody: Option[String]): IO[Unit] =
      for
        actual <- resp
        body   <- actual.as[String]
      yield
        val statusCheck = actual.status === expectedStatus
        val bodyCheck   = expectedBody.fold(body.isEmpty)(_ === body)
        assert(statusCheck && bodyCheck, s"Expected '$expectedStatus:$expectedBody' Actual: '${actual.status}:$body'")

    def assertUncompressedAtoms(expectedStatus: Status, expectedDigests: List[(Observation.Id, List[AtomDigest])]): IO[Unit] =
      for
        actual <- resp
        body   <- actual.as[String]
      yield assertAtoms(actual, body, expectedStatus, expectedDigests)

    def assertCompressedAtoms(expectedStatus: Status, expectedDigests: List[(Observation.Id, List[AtomDigest])]): IO[Unit] =
      for
        actual     <- resp
        compressed <- actual.body.compile.to(Array)
        body       <- Stream.emits(compressed)
          .covary[IO]
          .through(Compression.forSync[IO].gunzip(8192))
          .flatMap(_.content)
          .through(fs2.text.utf8.decode)
          .compile
          .string
      yield assertAtoms(actual, body, expectedStatus, expectedDigests)