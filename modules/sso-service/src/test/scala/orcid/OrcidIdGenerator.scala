// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service.orcid

import cats.effect.Sync
import lucuma.core.model.OrcidId

import scala.util.Random

trait OrcidIdGenerator[F[_]]:
  def randomOrcidId(using Sync[F]): F[OrcidId] =
    Sync[F].delay:
      def digit = Random.nextInt(10).toString
      val a, b, c = List.fill(4)(digit).mkString
      val d = List.fill(3)(digit).mkString
      val x = checkDigit(a + b + c + d)
      OrcidId.fromValue(s"$a-$b-$c-$d$x") match
        case Left(s)  => sys.error(s)
        case Right(o) => o

  // Copied from OrcidId.scala, where it is private :-\
  def checkDigit(baseDigits: String): String =
    require(baseDigits.forall(c => c >= '0' && c <= '9'))
    val total = baseDigits.foldLeft(0) { (acc, c) =>
      val digit = c - '0'
      (acc + digit) * 2
    }
    val remainder = total % 11
    val result    = (12 - remainder) % 11
    if (result == 10) "X" else result.toString