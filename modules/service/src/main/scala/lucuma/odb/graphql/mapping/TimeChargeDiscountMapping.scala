// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Cursor
import grackle.Predicate
import grackle.Predicate.Const
import grackle.Predicate.Eql
import grackle.Result
import grackle.Type
import grackle.TypeRef
import grackle.syntax.*
import io.circe.syntax.*
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.data.TimeCharge.DiscountDiscriminator
import lucuma.odb.graphql.table.DatasetTable
import lucuma.odb.graphql.table.ObservationView
import lucuma.odb.graphql.table.TimeChargeDiscountTable
import lucuma.odb.graphql.table.VisitTable
import lucuma.odb.json.time.query.given

trait TimeChargeDiscountMapping[F[_]] extends DatasetTable[F]
                                         with ObservationView[F]
                                         with TimeChargeDiscountTable[F]
                                         with VisitTable[F] {

  lazy val TimeChargeDiscountMapping: ObjectMapping =
    SqlInterfaceMapping(
      tpe           = TimeChargeDiscountType,
      discriminator = discriminator,
      fieldMappings = List(
        SqlField("id",     TimeChargeDiscountTable.Id, key = true, hidden = true),
        SqlField("type",   TimeChargeDiscountTable.DiscountType, discriminator = true, hidden = true),

        SqlField("start",  TimeChargeDiscountTable.Start, hidden = true),
        SqlField("end",    TimeChargeDiscountTable.End,   hidden = true),

        CursorFieldJson("interval",
          cursor =>
            for {
              s <- cursor.fieldAs[Timestamp]("start")
              e <- cursor.fieldAs[Timestamp]("end")
            } yield TimestampInterval.between(s, e).asJson,
          List("start", "end")
        ),

        SqlObject("amount"),
        SqlField("comment", TimeChargeDiscountTable.Comment)
      )
    )

  private lazy val discriminator: SqlDiscriminator =
    new SqlDiscriminator {
      override def discriminate(c: Cursor): Result[Type] =
        c.fieldAs[DiscountDiscriminator]("type").flatMap {
          case DiscountDiscriminator.Daylight => Result(TimeChargeDaylightDiscountType)
          case DiscountDiscriminator.NoData   => Result(TimeChargeNoDataDiscountType)
          case DiscountDiscriminator.Overlap  => Result(TimeChargeOverlapDiscountType)
          case DiscountDiscriminator.Qa       => Result(TimeChargeQaDiscountType)
          case d                              => Result.internalError(s"No TimeChargeDiscount implementation for ${d.dbTag}")
        }

      private def mkPredicate(discountType: DiscountDiscriminator): Result[Predicate] =
        Eql(TimeChargeDiscountType / "type", Const(discountType)).success

      override def narrowPredicate(tpe: Type): Result[Predicate] =
        tpe match {
          case TimeChargeDaylightDiscountType => mkPredicate(DiscountDiscriminator.Daylight)
          case TimeChargeNoDataDiscountType   => mkPredicate(DiscountDiscriminator.NoData)
          case TimeChargeOverlapDiscountType  => mkPredicate(DiscountDiscriminator.Overlap)
          case TimeChargeQaDiscountType       => mkPredicate(DiscountDiscriminator.Qa)
          case _                              => Result.internalError(s"Invalid discriminator: $tpe")
        }
    }

  lazy val TimeChargeDaylightDiscountMapping: ObjectMapping =
    ObjectMapping(TimeChargeDaylightDiscountType)(
      SqlField("site", TimeChargeDiscountTable.Daylight.Site)
    )

  lazy val TimeChargeNoDataDiscountMapping: ObjectMapping =
    ObjectMapping(TimeChargeNoDataDiscountType)()

  lazy val TimeChargeOverlapDiscountMapping: ObjectMapping =
    ObjectMapping(TimeChargeOverlapDiscountType)(
      SqlObject("observation", Join(TimeChargeDiscountTable.Id, TimeChargeDiscountOverlapTable.DiscountId), Join(TimeChargeDiscountOverlapTable.ObservationId, ObservationView.Id))
    )

  lazy val TimeChargeQaDiscountMapping: ObjectMapping =
    ObjectMapping(TimeChargeQaDiscountType)(
      SqlObject("datasets", Join(TimeChargeDiscountTable.Id, TimeChargeDiscountDatasetTable.DiscountId), Join(TimeChargeDiscountDatasetTable.DatasetId, DatasetTable.Id))
    )

}
