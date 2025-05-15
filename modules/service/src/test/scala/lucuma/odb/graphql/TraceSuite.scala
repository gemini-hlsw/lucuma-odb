// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.effect.IO
import cats.effect.Resource
import cats.effect.kernel.Ref
import cats.syntax.all.*
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.*
import natchez.Trace
import natchez.log.Log
import org.typelevel.log4cats.Logger

class TraceSuite extends OdbSuite {
  import TraceSuite.*

  val pi = TestUsers.Standard.pi(1, 30)
  val validUsers = List(pi)
  var alog: AccumluatingTraceLogger = null // sorry

  override def trace: Resource[IO, Trace[IO]] =
    Resource.eval:
      Ref[IO].of(List.empty[Json]).flatMap: ref =>
        alog = new AccumluatingTraceLogger(ref)
        val ep = Log.entryPoint[IO](getClass.getName)(using summon, alog)
        Trace.ioTraceForEntryPoint(ep)

  // override def trace: Resource[IO, Trace[IO]] =
  //  import natchez.honeycomb.Honeycomb
  //  import io.honeycomb.libhoney.ResponseObserver
  //  import io.honeycomb.libhoney.responses.*
  //   val ro: ResponseObserver =
  //     new ResponseObserver:
  //       def onClientRejected(r: ClientRejected): Unit = println("onClientRejected")
  //       def onServerAccepted(r: ServerAccepted): Unit = println("onServerAccepted")
  //       def onServerRejected(r: ServerRejected): Unit = println("onServerRejected")
  //       def onUnknown(r: Unknown): Unit = println("onUnknown")  
  //   Honeycomb.entryPoint("trace-suite", ro) { cb =>
  //     IO.delay {
  //       cb.setWriteKey("...")
  //       cb.setDataset("rnorris-local")
  //       cb.build()
  //     }
  //   }.evalMap(Trace.ioTraceForEntryPoint)

  extension (j: Json)

    def hasField(k: String): Boolean =
      j.hcursor.downField(k).succeeded

    def hasField(k: String, v: Json): Boolean =
      j.hcursor.downField(k).as[Json].toOption.exists(_ === v)

    def hasName(s: String): Boolean =
      hasField("name", s.asJson)

  test("test tracing (WS)") {
    alog.clear >>
    createProgramAs(pi, clientOption = ClientOption.Ws).replicateA(5) *>
    alog.roots.map: roots =>
      assert:
        roots.exists(_.hasName("connection.init")) &&    
        roots.exists(_.hasName("connection.execute"))        
  }

  test("test tracing (HTTP)") {
    alog.clear >>
    createProgramAs(pi, clientOption = ClientOption.Http).replicateA(5) *>
    alog.all.map: all =>
      assert:
        all.exists(_.hasName("POST /odb"))
  }

}

object TraceSuite:

  class NoopLogger extends Logger[IO]:
    def debug(t: Throwable)(message: => String) = IO.unit
    def error(t: Throwable)(message: => String) = IO.unit
    def info(t: Throwable)(message: => String) = IO.unit
    def trace(t: Throwable)(message: => String) = IO.unit
    def warn(t: Throwable)(message: => String) = IO.unit
    def debug(message: => String) = IO.unit
    def error(message: => String) = IO.unit
    def info(message: => String) = IO.unit
    def trace(message: => String) = IO.unit
    def warn(message: => String) = IO.unit

  class AccumluatingTraceLogger(ref: Ref[IO, List[Json]]) extends NoopLogger:

    def clear: IO[Unit] = 
      ref.set(Nil)

    override def info(message: => String): IO[Unit] = 
      val j = parse(message).toOption.get
      ref.update(j :: _)    

    def roots: IO[List[Json]] =
      ref.get

    def all: IO[List[Json]] =
      ref.get.map: roots =>
        roots.flatMap: j =>
          j :: j.hcursor.downField("children").require[List[Json]]
  
  object AccumluatingTraceLogger:
    def initial: IO[AccumluatingTraceLogger] =
      Ref[IO].of(List.empty[Json]).map(AccumluatingTraceLogger(_))
