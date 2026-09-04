// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*

// Regression guard for the program resource-limit triggers (V1182): concurrent
// inserts into one program must not deadlock. An earlier design maintained the
// count on t_program itself, which deadlocked under concurrency because child
// inserts hold a FOR KEY SHARE lock on the program row while the counter UPDATE
// wanted FOR NO KEY UPDATE. The count now lives in a dedicated table.
class concurrentInsertDeadlock extends OdbSuite:

  val pi = TestUsers.Standard.pi(nextId, nextId)
  lazy val validUsers = List(pi)

  // Under this test's deliberately high concurrency, a backend's file `open()`
  // can be interrupted by a signal on the CI container filesystem (EINTR).
  // Postgres 15 surfaces that as a XX000 internal error ("Could not open file
  // ...: Interrupted system call") in mdopenfork rather than retrying it. It is a
  // transient infrastructure hiccup, unrelated to what this test guards, so we
  // retry it a few times. A genuine regression -- a deadlock (40P01) or a
  // resource-limit violation (LU001) -- carries different text, does not match,
  // and still fails the test loudly.
  private def isTransientOpenIntr(t: Throwable): Boolean =
    LazyList.iterate(Option(t))(_.flatMap(e => Option(e.getCause).filterNot(_ eq e)))
      .takeWhile(_.isDefined)
      .flatten
      .exists(e => Option(e.getMessage).exists(_.contains("Interrupted system call")))

  private def retryingTransient[A](io: IO[A], remaining: Int = 3): IO[A] =
    io.handleErrorWith:
      case t if remaining > 0 && isTransientOpenIntr(t) => retryingTransient(io, remaining - 1)
      case t                                            => IO.raiseError(t)

  test("concurrent observations into one program"):
    createProgramAs(pi).flatMap: pid =>
      List.range(0, 16).parTraverse_(_ => retryingTransient(createObservationAs(pi, pid)))

  test("concurrent groups into one program"):
    createProgramAs(pi).flatMap: pid =>
      List.range(0, 16).parTraverse_(_ => retryingTransient(createGroupAs(pi, pid)))

  test("concurrent targets into one program"):
    createProgramAs(pi).flatMap: pid =>
      List.range(0, 16).parTraverse_(_ => retryingTransient(createTargetAs(pi, pid)))
