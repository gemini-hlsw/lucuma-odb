// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.syntax

import cats.Applicative
import cats.Functor
import cats.syntax.applicative.*
import cats.syntax.functor.*
import grackle.Result
import grackle.ResultT
import grackle.syntax.*

trait ToResultTOps {
  extension (x: ResultT.type)
    def unit[F[_]](implicit F: Applicative[F]): ResultT[F, Unit] =
      pure(())

    def pure[F[_], A](a: A)(implicit F: Applicative[F]): ResultT[F, A] =
      fromResult(Result.pure(a))

    def fromResult[F[_], A](a: Result[A])(implicit F: Applicative[F]): ResultT[F, A] =
      ResultT(a.pure[F])

    def liftF[F[_], A](fa: F[A])(implicit F: Functor[F]): ResultT[F, A] =
      ResultT(fa.map(_.success))
}

object resultT extends ToResultTOps