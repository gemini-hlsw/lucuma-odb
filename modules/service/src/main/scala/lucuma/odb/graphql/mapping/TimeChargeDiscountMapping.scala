// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.syntax.option.*
import grackle.Cursor
import grackle.Predicate
import grackle.Predicate.Const
import grackle.Predicate.Eql
import grackle.Result
import grackle.Type
import lucuma.odb.data.TimeCharge.DiscountDiscriminator
import lucuma.odb.graphql.table.TimeChargeDiscountTable
import lucuma.odb.graphql.table.VisitTable

trait TimeChargeDiscountMapping[F[_]] extends TimeChargeDiscountTable[F]
                                         with VisitTable[F] {

  lazy val TimeChargeDiscountMapping: ObjectMapping =
    SqlInterfaceMapping(
      tpe           = TimeChargeDiscountType,
      discriminator = discriminator,
      fieldMappings = List(
        SqlField("id",     TimeChargeDiscountTable.Id, key = true, hidden = true),
        SqlField("type",   TimeChargeDiscountTable.DiscountType, discriminator = true, hidden = true),
        SqlField("start",  TimeChargeDiscountTable.Start),
        SqlField("end",    TimeChargeDiscountTable.End),
        SqlObject("partner"),
        SqlObject("program"),
        SqlField("comment", TimeChargeDiscountTable.Comment)
      )
    )

  private lazy val discriminator: SqlDiscriminator =
    new SqlDiscriminator {
      override def discriminate(c: Cursor): Result[Type] =
        c.fieldAs[DiscountDiscriminator]("type").flatMap {
          case DiscountDiscriminator.Daylight => Result(TimeChargeDaylightDiscountType)
          case d                              => Result.failure(s"No TimeChargeDiscount implementation for ${d.dbTag}")
        }

      private def mkPredicate(discountType: DiscountDiscriminator): Option[Predicate] =
        Eql(TimeChargeDiscountType / "type", Const(discountType)).some

      override def narrowPredicate(tpe: Type): Option[Predicate] =
        tpe match {
          case TimeChargeDaylightDiscountType => mkPredicate(DiscountDiscriminator.Daylight)
          case _                              => none
        }
    }

  lazy val TimeChargeDaylightDiscountMapping: ObjectMapping =
    ObjectMapping(
      tpe = TimeChargeDaylightDiscountType,
      fieldMappings = List(
        SqlField("id",   TimeChargeDiscountTable.Id, key = true),
        SqlField("site", TimeChargeDiscountTable.Daylight.Site)
      )
    )
}
