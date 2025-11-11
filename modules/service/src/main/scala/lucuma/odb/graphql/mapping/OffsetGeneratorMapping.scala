// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path
import grackle.Query.Binding
import grackle.Query.OrderBy
import grackle.Query.OrderSelection
import grackle.Query.OrderSelections
import grackle.QueryCompiler.Elab
import grackle.TypeRef

import table.EnumeratedOffsetView
import table.OffsetGeneratorView

trait OffsetGeneratorMapping[F[_]] extends OffsetGeneratorView[F] with EnumeratedOffsetView[F]:

  def enumeratedOffsetGeneratorMapping(path: Path,
    parentIdColumn: ColumnRef,
    childIdColumn: ColumnRef
  ): ObjectMapping =
    ObjectMapping(path)(
      SqlField("observationId", OffsetGeneratorView.Enumerated.ObservationId, key = true, hidden = true),
      SqlField("role",          OffsetGeneratorView.Enumerated.OffsetGeneratorRole, key = true, hidden = true),
      SqlObject("values",       Join(parentIdColumn, childIdColumn))
    )

  lazy val EnumeratedOffsetElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (EnumeratedOffsetGeneratorType, "values", Nil) =>
      Elab.transformChild: child =>
        OrderBy(
          OrderSelections(List(OrderSelection[Int](TelescopeConfigType / "index"))),
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

      SqlObject("enumerated", Join(OffsetGeneratorView.ObservationId, OffsetGeneratorView.Enumerated.ObservationId)),
      SqlObject("grid",       Join(OffsetGeneratorView.ObservationId, OffsetGeneratorView.Grid.ObservationId)),
      SqlObject("random",     Join(OffsetGeneratorView.ObservationId, OffsetGeneratorView.Random.ObservationId)),
      SqlObject("spiral",     Join(OffsetGeneratorView.ObservationId, OffsetGeneratorView.Spiral.ObservationId))
    )

  lazy val OffsetGeneratorMappings: List[TypeMapping] =
    List(
      enumeratedOffsetGeneratorMapping(GmosNorthImagingType / "objectOffsetGenerator" / "enumerated", OffsetGeneratorView.Enumerated.ObservationId, EnumeratedOffsetView.ObjectObservationId),
      enumeratedOffsetGeneratorMapping(GmosNorthImagingType / "skyOffsetGenerator" / "enumerated", OffsetGeneratorView.Enumerated.ObservationId, EnumeratedOffsetView.SkyObservationId),
      enumeratedOffsetGeneratorMapping(GmosSouthImagingType / "objectOffsetGenerator" / "enumerated", OffsetGeneratorView.Enumerated.ObservationId, EnumeratedOffsetView.ObjectObservationId),
      enumeratedOffsetGeneratorMapping(GmosSouthImagingType / "skyOffsetGenerator" / "enumerated", OffsetGeneratorView.Enumerated.ObservationId, EnumeratedOffsetView.SkyObservationId),
      GridOffsetGeneratorMapping,
      OffsetGeneratorMapping,
      RandomOffsetGeneratorMapping,
      SpiralOffsetGeneratorMapping
    )