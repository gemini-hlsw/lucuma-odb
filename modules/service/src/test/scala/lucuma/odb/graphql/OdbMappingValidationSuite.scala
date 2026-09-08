// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.effect.IO
import cats.syntax.all.*
import grackle.ValidationFailure
import lucuma.core.model.User
import org.typelevel.otel4s.metrics.Meter
import org.typelevel.otel4s.trace.Tracer

/**
 * Validates the OdbMapping type mappings once. Every other suite builds its mappings unchecked,
 * so this is the only place a stale or unused mapping is caught.
 */
class OdbMappingValidationSuite extends OdbSuite:

  val validUsers: List[User] = Nil

  // Grackle's validator is a StateT over Id whose recursion depth grows with the number of type
  // mappings. Measured for our mapping: under 256 KB once JIT-compiled, but up to 2 MB while
  // those frames still run interpreted, which is how the default 1 MB stack overflows in CI from
  // time to time. 8 MB is 4x the worst case and only reserves address space.
  private def onLargeStack[A](a: => A): IO[A] =
    IO.async_ : cb =>
      val t: Thread = new Thread(null, () => cb(Either.catchNonFatal(a)), "odb-mapping-validation", 8L * 1024 * 1024)
      t.setDaemon(true)
      t.start()

  test("type mappings validate without failures"):
    given Tracer[IO] = Tracer.noop
    given Meter[IO]  = Meter.noop
    mapping(shouldValidate = true).use: map =>
      onLargeStack(map.validate()).map: (failures: List[ValidationFailure]) =>
        assert(failures.isEmpty, failures.map(_.toErrorMessage).mkString("\n"))
