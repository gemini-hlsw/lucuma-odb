// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package scheduler

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import fs2.Stream
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.ChargeClass
import lucuma.core.enums.GcalLampType
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.StepType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.AtomDigest
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import org.http4s.*
import org.http4s.implicits.*

class schedulerRoutes extends SchedulerRoutesSuite with ExecutionTestSupportForGmos:

  val Zero: Atom.Id = Atom.Id.parse("a-00000000-0000-0000-0000-000000000000").get

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      PosInt.unsafeFrom(10)
    )

  test("not service user"):
    atomsRequest(pi).flatMap: request =>
      withRoutes(serviceUser, request).assertResponse(Status.Forbidden, none)

  test("no observation ids"):
    atomsRequest(serviceUser).flatMap: request =>
      withRoutes(serviceUser, request).assertResponse(Status.Ok, "".some)

  test("invalid observation ids"):
    atomsRequest(serviceUser, "foo", "o-123", "bar").flatMap: request =>
      withRoutes(serviceUser, request).assertResponse(Status.BadRequest, "Unable to parse observation ids: foo, bar".some)

  test("not service user, invalid observation ids"):
    // Authorization is checked before the body is parsed.
    atomsRequest(pi, "foo", "bar").flatMap: request =>
      withRoutes(serviceUser, request).assertResponse(Status.Forbidden, none)

  // A body that fails the test if anything reads it.
  private val unreadableBody: Stream[IO, Byte] =
    Stream.raiseError[IO](new RuntimeException("request body was read"))

  test("no authorization header, body not read"):
    val request = Request[IO](method = Method.POST, uri = uri"scheduler/atoms").withBodyStream(unreadableBody)
    withRoutes(serviceUser, request).assertResponse(Status.Forbidden, "Access denied".some)

  test("not service user, body not read"):
    atomsRequest(pi).flatMap: request =>
      withRoutes(serviceUser, request.withBodyStream(unreadableBody)).assertResponse(Status.Forbidden, none)

  test("invalid observation ids, bounded error message"):
    val long = "x" * 50  // longer than the reported length, within the line limit
    val bad  = List("b1", "b2", "b3", "b4", long, "b6", "b7")
    atomsRequest(serviceUser, ("o-123" :: bad)*).flatMap: request =>
      withRoutes(serviceUser, request).assertResponse(
        Status.BadRequest,
        s"Unable to parse observation ids: b1, b2, b3, b4, ${"x" * 32}... (and 2 more)".some
      )

  test("invalid observation ids, unprintable characters escaped"):
    val control = "a\u0000b\u001b[2Jc"              // NUL and an ANSI escape
    val bidi    = "x\u202Ey\\z"                     // bidi override and a backslash
    val emoji   = ("w" * 31) + "\uD83D\uDE00" + "v"  // 33 code points, emoji 32nd
    atomsRequest(serviceUser, control, bidi, emoji).flatMap: request =>
      withRoutes(serviceUser, request).assertResponse(
        Status.BadRequest,
        s"Unable to parse observation ids: a\\u0000b\\u001b[2Jc, x\\u202ey\\\\z, ${"w" * 31}\uD83D\uDE00...".some
      )

  test("line too long"):
    val tooLong = "o-" + ("f" * (SchedulerRoutes.MaxAtomsRequestLineLength - 1))
    atomsRequest(serviceUser, "o-123", tooLong).flatMap: request =>
      withRoutes(serviceUser, request).assertResponse(
        Status.BadRequest,
        s"Unable to parse observation ids: a line exceeds ${SchedulerRoutes.MaxAtomsRequestLineLength} characters".some
      )

  test("line too long across chunks"):
    // No single chunk is too long, but together they make one long line.
    val half = "x" * (SchedulerRoutes.MaxAtomsRequestLineLength / 2 + 1)
    atomsRequest(serviceUser).flatMap: request =>
      val req = request.withBodyStream(Stream(half, half).through(fs2.text.utf8.encode).chunkLimit(half.length).unchunks)
      withRoutes(serviceUser, req).assertResponse(
        Status.BadRequest,
        s"Unable to parse observation ids: a line exceeds ${SchedulerRoutes.MaxAtomsRequestLineLength} characters".some
      )

  // Exactly MaxAtomsRequestBytes of valid lines ("o-1\n" is 4 bytes), followed
  // by `extra`, delivered in 4 KiB chunks so the limit is applied across many
  // chunk boundaries.
  private def limitBody(extra: String): Stream[IO, Byte] =
    val lineCount = SchedulerRoutes.MaxAtomsRequestBytes.toInt / 4
    (Stream.emit("o-1\n").repeatN(lineCount.toLong) ++ Stream.emit(extra))
      .through(fs2.text.utf8.encode)
      .chunkLimit(4096)
      .unchunks

  test("request body at the limit is accepted"):
    atomsRequest(serviceUser).flatMap: request =>
      withRoutes(serviceUser, request.withBodyStream(limitBody(""))).assertResponse(Status.Ok, "".some)

  test("request body one byte over the limit is rejected"):
    atomsRequest(serviceUser).flatMap: request =>
      withRoutes(serviceUser, request.withBodyStream(limitBody("o"))).assertResponse(
        Status.PayloadTooLarge,
        s"Request body exceeds ${SchedulerRoutes.MaxAtomsRequestBytes} bytes".some
      )

  test("unknown observation ids"):
    atomsRequest(serviceUser, "o-123", "o-124", "o-125").flatMap: request =>
      withRoutes(serviceUser, request).assertResponse(Status.Ok, "".some)

  val a0 =
    AtomDigest(
      Zero,  // we skip the atom id comparison
      ObserveClass.Science,
      CategorizedTime(ChargeClass.Program -> TimeSpan.unsafeFromMicroseconds(3906781250L)),
      Set(StepType.Gcal, StepType.Science),
      Set(GcalLampType.Arc, GcalLampType.Flat),
      NonNegInt.MinValue,
      PosInt.unsafeFrom(5)
    )

  val a1 = a0.copy(stepIndex = NonNegInt.unsafeFrom( 5))
  val a2 = a0.copy(stepIndex = NonNegInt.unsafeFrom(10))

  val a3 = a0.copy(
    timeEstimate = CategorizedTime(ChargeClass.Program -> TimeSpan.unsafeFromMicroseconds(1390300000L)),
    stepIndex    = NonNegInt.unsafeFrom(15),
    stepCount    = PosInt.unsafeFrom(3)
  )

  test("one observation"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _ <- runObscalcUpdate(p, o)
      yield o

    setup.flatMap: oid =>
      atomsRequest(serviceUser, oid.toString).flatMap: request =>
        withRoutes(serviceUser, request).assertUncompressedAtoms(Status.Ok, List(oid -> List(a0, a1, a2, a3)))

  test("one observation, partial execution"):
    // Execute the first two steps, which are an arc and a flat, leaving 3
    // science steps.
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v <- recordVisitAs(serviceUser, o)
        s <- firstScienceAtomStepIds(serviceUser, o)
        _ <- addEndStepEvent(s(0), v)  // arc
        _ <- addEndStepEvent(s(1), v)  // flat
        _ <- runObscalcUpdate(p, o)
      yield o

    setup.flatMap: oid =>
      atomsRequest(serviceUser, oid.toString).flatMap: request =>
        val a0ʹ = a0.copy(
          timeEstimate = CategorizedTime(ChargeClass.Program -> TimeSpan.unsafeFromMicroseconds(3767581250L)),
          stepTypes    = Set(StepType.Science),
          lampTypes    = Set.empty,
          stepIndex    = NonNegInt.unsafeFrom(2),
          stepCount    = PosInt.unsafeFrom(3)
        )
        withRoutes(serviceUser, request).assertUncompressedAtoms(Status.Ok, List(oid -> List(a0ʹ, a1, a2, a3)))

  test("two observations"):
    val setup: IO[(Observation.Id, Observation.Id)] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o0 <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _  <- runObscalcUpdate(p, o0)
        o1 <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _  <- runObscalcUpdate(p, o1)
      yield (o0, o1)

    setup.flatMap: (oid0, oid1) =>
      atomsRequest(serviceUser, oid0.toString, oid1.toString).flatMap: request =>
        withRoutes(serviceUser, request).assertUncompressedAtoms(Status.Ok, List(oid0 -> List(a0, a1, a2, a3), oid1 -> List(a0, a1, a2, a3)))

  test("observations read in separate batches"):
    val setup: IO[(Observation.Id, Observation.Id)] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o0 <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _  <- runObscalcUpdate(p, o0)
        o1 <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _  <- runObscalcUpdate(p, o1)
      yield (o0, o1)

    // One observation per batch, with an unknown id (an empty batch) between
    // them and a duplicate that must not be repeated.
    setup.flatMap: (oid0, oid1) =>
      atomsRequest(serviceUser, oid0.toString, "o-fffffff", oid1.toString, oid0.toString).flatMap: request =>
        withRoutes(serviceUser, request, SinglePages)
          .assertUncompressedAtoms(Status.Ok, List(oid0 -> List(a0, a1, a2, a3), oid1 -> List(a0, a1, a2, a3)))

  test("gzip"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _ <- runObscalcUpdate(p, o)
      yield o

    setup.flatMap: oid =>
      gzipAtomsRequest(serviceUser, gzip = true, oid.toString).flatMap: request =>
        withRoutes(serviceUser, request).assertCompressedAtoms(Status.Ok, List(oid -> List(a0, a1, a2, a3)))
