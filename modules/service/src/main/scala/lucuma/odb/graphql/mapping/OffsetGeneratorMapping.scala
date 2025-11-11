// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Query.Binding
import grackle.Query.OrderBy
import grackle.Query.OrderSelection
import grackle.Query.OrderSelections
import grackle.QueryCompiler.Elab
import grackle.TypeRef

import table.EnumeratedOffsetTable
import table.OffsetGeneratorView

trait OffsetGeneratorMapping[F[_]] extends OffsetGeneratorView[F] with EnumeratedOffsetTable[F]:

  lazy val EnumeratedOffsetGeneratorMapping: ObjectMapping =
    ObjectMapping(EnumeratedOffsetGeneratorType)(
      SqlField("observationId", OffsetGeneratorView.Enumerated.ObservationId, key = true, hidden = true),
      SqlField("role",          OffsetGeneratorView.Enumerated.OffsetGeneratorRole, key = true, hidden = true),
      SqlObject("values",
        Join(
          List(
            OffsetGeneratorView.Enumerated.ObservationId       -> EnumeratedOffsetTable.ObservationId,
            OffsetGeneratorView.Enumerated.OffsetGeneratorRole -> EnumeratedOffsetTable.OffsetGeneratorRole
          )
        )
      )
    )

  lazy val EnumeratedOffsetElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (EnumeratedOffsetGeneratorType, "values", Nil) =>
      Elab.transformChild: child =>
        OrderBy(
          OrderSelections(List(OrderSelection[Int](EnumeratedOffsetGeneratorType / "values" / EnumeratedOffsetTable.Index.column))),
          child
        )

  lazy val GridOffsetGeneratorMapping: ObjectMapping =
    ObjectMapping(GridOffsetGeneratorType)(
      SqlField("observationId", OffsetGeneratorView.Grid.ObservationId, key = true, hidden = true),
      SqlField("role",          OffsetGeneratorView.Grid.OffsetGeneratorRole, key = true, hidden = true),
      SqlObject("cornerA"),
      SqlObject("cornerB")
    )

  lazy val RandomOffsetGeneratorMapping: ObjectMapping =
    ObjectMapping(RandomOffsetGeneratorType)(
      SqlField("observationId", OffsetGeneratorView.Random.ObservationId, key = true, hidden = true),
      SqlField("role",          OffsetGeneratorView.Random.OffsetGeneratorRole, key = true, hidden = true),
      SqlObject("size"),
      SqlObject("center")
    )

  lazy val SpiralOffsetGeneratorMapping: ObjectMapping =
    ObjectMapping(SpiralOffsetGeneratorType)(
      SqlField("observationId", OffsetGeneratorView.Spiral.ObservationId, key = true, hidden = true),
      SqlField("role",          OffsetGeneratorView.Spiral.OffsetGeneratorRole, key = true, hidden = true),
      SqlObject("size"),
      SqlObject("center")
    )

  lazy val OffsetGeneratorMapping: ObjectMapping =
    ObjectMapping(OffsetGeneratorType)(
      SqlField("observationId", OffsetGeneratorView.ObservationId, key = true, hidden = true),
      SqlField("role", OffsetGeneratorView.OffsetGeneratorRole, key = true, hidden = true),

      SqlField("generatorType", OffsetGeneratorView.OffsetGeneratorType),

      SqlObject("enumerated"),
      SqlObject("grid"),
      SqlObject("random"),
      SqlObject("spiral")
    )

  lazy val OffsetGeneratorMappings: List[TypeMapping] =
    List(
      EnumeratedOffsetGeneratorMapping,
      GridOffsetGeneratorMapping,
      OffsetGeneratorMapping,
      RandomOffsetGeneratorMapping,
      SpiralOffsetGeneratorMapping
    )