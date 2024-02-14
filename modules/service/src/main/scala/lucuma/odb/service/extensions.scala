// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.Functor
import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import grackle.Result
import grackle.ResultT
import skunk._
import skunk.data.Completion

extension [F[_]: MonadCancelThrow](s: Session[F]) def executeCommand(af: AppliedFragment): F[Completion] =
  s.prepareR(af.fragment.command).use(_.execute(af.argument))

extension (self: ResultT.type)
  def fromResult[F[_]]: FromResultPartiallyApplied[F] = new FromResultPartiallyApplied
  def success[F[_], A](fa: F[A])(using Functor[F]) = ResultT(fa.map(Result.success))

class FromResultPartiallyApplied[F[_]]:
  def apply[A](result: Result[A])(using F: Applicative[F]): ResultT[F, A] =
    ResultT(F.pure(result))

