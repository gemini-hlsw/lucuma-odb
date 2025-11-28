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

  lazy val UniformOffsetGeneratorMapping: ObjectMapping =
    ObjectMapping(UniformOffsetGeneratorType)(
      SqlField("observationId", OffsetGeneratorView.Uniform.ObservationId, key = true, hidden = true),
      SqlField("role",          OffsetGeneratorView.Uniform.OffsetGeneratorRole, key = true, hidden = true),
      SqlObject("cornerA"),
      SqlObject("cornerB")
    )

  lazy val OffsetGeneratorMapping: ObjectMapping =
    ObjectMapping(OffsetGeneratorType)(
      SqlField("observationId", OffsetGeneratorView.ObservationId, key = true, hidden = true),
      SqlField("role", OffsetGeneratorView.OffsetGeneratorRole, key = true, hidden = true),

      SqlField("generatorType", OffsetGeneratorView.OffsetGeneratorType),

      SqlObject("enumerated", Join(List(
        OffsetGeneratorView.ObservationId       -> OffsetGeneratorView.Enumerated.ObservationId,
        OffsetGeneratorView.OffsetGeneratorRole -> OffsetGeneratorView.Enumerated.OffsetGeneratorRole
      ))),
      SqlObject("random",     Join(List(
        OffsetGeneratorView.ObservationId       -> OffsetGeneratorView.Random.ObservationId,
        OffsetGeneratorView.OffsetGeneratorRole -> OffsetGeneratorView.Random.OffsetGeneratorRole
      ))),
      SqlObject("spiral",     Join(List(
        OffsetGeneratorView.ObservationId       -> OffsetGeneratorView.Spiral.ObservationId,
        OffsetGeneratorView.OffsetGeneratorRole -> OffsetGeneratorView.Spiral.OffsetGeneratorRole
      ))),
      SqlObject("uniform",    Join(List(
        OffsetGeneratorView.ObservationId       -> OffsetGeneratorView.Uniform.ObservationId,
        OffsetGeneratorView.OffsetGeneratorRole -> OffsetGeneratorView.Uniform.OffsetGeneratorRole
      )))
    )

  lazy val OffsetGeneratorMappings: List[TypeMapping] =
    List(
      enumeratedOffsetGeneratorMapping(GmosNorthImagingType / "objectOffsetGenerator" / "enumerated", OffsetGeneratorView.Enumerated.ObservationId, EnumeratedOffsetView.ObjectObservationId),
      enumeratedOffsetGeneratorMapping(GmosNorthImagingType / "skyOffsetGenerator"    / "enumerated", OffsetGeneratorView.Enumerated.ObservationId, EnumeratedOffsetView.SkyObservationId),
      enumeratedOffsetGeneratorMapping(GmosSouthImagingType / "objectOffsetGenerator" / "enumerated", OffsetGeneratorView.Enumerated.ObservationId, EnumeratedOffsetView.ObjectObservationId),
      enumeratedOffsetGeneratorMapping(GmosSouthImagingType / "skyOffsetGenerator"    / "enumerated", OffsetGeneratorView.Enumerated.ObservationId, EnumeratedOffsetView.SkyObservationId),
      OffsetGeneratorMapping,
      RandomOffsetGeneratorMapping,
      SpiralOffsetGeneratorMapping,
      UniformOffsetGeneratorMapping
    )