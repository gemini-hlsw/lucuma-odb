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

import lucuma.odb.graphql.table.TimingWindowView
import lucuma.odb.data.TimingWindowEndTypeEnum
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.NullableType

trait TimingWindowEndMapping[F[_]] extends TimingWindowView[F] {

  lazy val TimingWindowEndMapping: ObjectMapping =
    SqlUnionMapping(
      tpe = TimingWindowEndType,
      fieldMappings = 
        List(
          SqlField("id", TimingWindowView.End.SyntheticId, key = true),
          SqlField("endType", TimingWindowView.End.Type, discriminator = true, hidden = true),
        ),
      discriminator = endDiscriminator
    )


  object endDiscriminator extends SqlDiscriminator {
    def discriminate(c: Cursor): Result[Type] = {
      c.fieldAs[TimingWindowEndTypeEnum]("endType").map {
        case TimingWindowEndTypeEnum.At => TimingWindowEndAtType
        case TimingWindowEndTypeEnum.After => TimingWindowEndAfterType
      }
    }

    def narrowPredicate(subtpe: Type): Option[Predicate] = {
      def mkPredicate(tpe: TimingWindowEndTypeEnum): Option[Predicate] =
        Some(Eql(TimingWindowEndType / "endType", Const(tpe)))

      subtpe match {
        case TimingWindowEndAtType => mkPredicate(TimingWindowEndTypeEnum.At)
        case TimingWindowEndAfterType => mkPredicate(TimingWindowEndTypeEnum.After)
        case _ => None
      }
    }
  }

}

