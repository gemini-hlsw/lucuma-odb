// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.*
import cats.arrow.FunctionK
import grackle.Result
import grackle.Result.*

given Parallel[Result] =
  new Parallel[Result]:
    final case class F[A](r: Result[A])
    val parallel = FunctionK.lift([A] => (r: Result[A]) => F(r))
    val sequential = FunctionK.lift([A] => (f: F[A]) => f.r)
    val monad = summon[Monad[Result]]
    val applicative =
      new Applicative[F]:
        def pure[A](x: A) = F(Result(x))
        def ap[A, B](ff: F[A => B])(fa: F[A]) =
          fa.r match
            case rf@Failure(_) => F(rf)
            case re@InternalError(_) => F(re)
            case Warning(ps0, a) =>
              ff.r match
                case rf@Failure(_)       => F(rf)
                case re@InternalError(_) => F(re)
                case Success(fab)        => F(Warning(ps0, fab(a)))
                case Warning(ps1, fab)   => F(Warning(ps0 ++ ps1, fab(a)))
            case Success(a) => 
              ff.r match
                case rf@Failure(_)       => F(rf)
                case re@InternalError(_) => F(re)
                case Success(fab)        => F(Success(fab(a)))
                case Warning(ps, fab)    => F(Warning(ps, fab(a)))


