// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.Cursor
import grackle.Predicate
import grackle.Result
import grackle.Type
import lucuma.odb.data.TimingWindowEndTypeEnum
import lucuma.odb.graphql.table.TimingWindowView

import Predicate.*

trait TimingWindowMappings[F[_]] extends TimingWindowView[F] {

  // TimingWindow
  lazy val TimingWindowMapping =
    ObjectMapping(TimingWindowType)(
      SqlField("id", TimingWindowView.Id, key = true, hidden = true),
      SqlField("inclusion", TimingWindowView.Inclusion),
      SqlField("startUtc", TimingWindowView.Start),
      SqlObject("end")
    )

  // TimingWindowEnd
  lazy val TimingWindowEndMapping: ObjectMapping =
    SqlUnionMapping(
      tpe = TimingWindowEndType,
      fieldMappings =
        List(
          SqlField("id", TimingWindowView.End.SyntheticId, key = true, hidden = true),
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

  // TimingWindowEntAt
  lazy val TimingWindowEndAtMapping =
    ObjectMapping(TimingWindowEndAtType)(
      SqlField("id", TimingWindowView.End.SyntheticId, key = true, hidden = true),
      SqlField("atUtc", TimingWindowView.End.At),
    )

  // TimingWindowEntAfter
  lazy val TimingWindowEndAfterMapping =
    ObjectMapping(TimingWindowEndAfterType)(
      SqlField("id", TimingWindowView.End.SyntheticId, key = true, hidden = true),
      SqlObject("after"),
      SqlObject("repeat")
    )

  // TimingWindowRepeat
  lazy val TimingWindowRepeatMapping =
    ObjectMapping(TimingWindowRepeatType)(
      SqlField("id", TimingWindowView.End.Repeat.SyntheticId, key = true, hidden = true),
      SqlObject("period"),
      SqlField("times", TimingWindowView.End.Repeat.Times),
    )

}

