// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import edu.gemini.grackle.Cursor
import edu.gemini.grackle.Result
import edu.gemini.grackle.Type
import edu.gemini.grackle.Predicate
import Predicate._
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp

import lucuma.odb.graphql.table.TimingWindowTable

trait TimingWindowEndMapping[F[_]] extends TimingWindowTable[F] {

  lazy val TimingWindowEndMapping =
    SqlUnionMapping(
      tpe = TimingWindowEndType,
      fieldMappings = 
        List(
          SqlField("end_at", TimingWindowTable.EndAt, hidden = true),
          SqlField("end_after", TimingWindowTable.EndAfter, hidden = true)
        ),
      discriminator = endDiscriminator
    )

    object endDiscriminator extends SqlDiscriminator {
      def discriminate(c: Cursor): Result[Type] =
        for {
          at <- c.fieldAs[Option[Timestamp]]("endAt")
          after <- c.fieldAs[Option[TimeSpan]]("endAfter")
        } yield (at, after) match {
          case (Some(_), None) => TimingWindowEndAtType
          case (None, Some(_)) => TimingWindowEndAfterType
          case _ => ???
        }

      def narrowPredicate(subtpe: Type): Option[Predicate] = {
        def mkPredicate(tpe: String): Option[Predicate] =
          Some(IsNull(TimingWindowEndAtType / tpe, false))

        subtpe match {
          case TimingWindowEndAtType => mkPredicate("endAt")
          case TimingWindowEndAfterType => mkPredicate("endAfter")
          case _ => None
        }
      }
    }

  }

