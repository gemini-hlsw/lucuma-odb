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
import table.TelescopeConfigGeneratorView

trait TelescopeConfigGeneratorMapping[F[_]] extends TelescopeConfigGeneratorView[F] with EnumeratedOffsetView[F]:

  def enumeratedTelescopeConfigGeneratorMapping(path: Path,
    parentIdColumn: ColumnRef,
    childIdColumn: ColumnRef
  ): ObjectMapping =
    ObjectMapping(path)(
      SqlField("observationId", TelescopeConfigGeneratorView.Enumerated.ObservationId, key = true, hidden = true),
      SqlField("role",          TelescopeConfigGeneratorView.Enumerated.Role, key = true, hidden = true),
      SqlObject("values",       Join(parentIdColumn, childIdColumn))
    )

  lazy val EnumeratedOffsetElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (EnumeratedTelescopeConfigGeneratorType, "values", Nil) =>
      Elab.transformChild: child =>
        OrderBy(
          OrderSelections(List(OrderSelection[Int](TelescopeConfigType / "index"))),
          child
        )

  lazy val RandomTelescopeConfigGeneratorMapping: ObjectMapping =
    ObjectMapping(RandomTelescopeConfigGeneratorType)(
      SqlField("observationId", TelescopeConfigGeneratorView.Random.ObservationId, key = true, hidden = true),
      SqlField("role",          TelescopeConfigGeneratorView.Random.Role, key = true, hidden = true),
      SqlObject("size"),
      SqlObject("center"),
      SqlField("seed",          TelescopeConfigGeneratorView.Random.Seed)
    )

  lazy val SpiralTelescopeConfigGeneratorMapping: ObjectMapping =
    ObjectMapping(SpiralTelescopeConfigGeneratorType)(
      SqlField("observationId", TelescopeConfigGeneratorView.Spiral.ObservationId, key = true, hidden = true),
      SqlField("role",          TelescopeConfigGeneratorView.Spiral.Role, key = true, hidden = true),
      SqlObject("size"),
      SqlObject("center"),
      SqlField("seed",          TelescopeConfigGeneratorView.Spiral.Seed)
    )

  lazy val UniformTelescopeConfigGeneratorMapping: ObjectMapping =
    ObjectMapping(UniformTelescopeConfigGeneratorType)(
      SqlField("observationId", TelescopeConfigGeneratorView.Uniform.ObservationId, key = true, hidden = true),
      SqlField("role",          TelescopeConfigGeneratorView.Uniform.Role, key = true, hidden = true),
      SqlObject("cornerA"),
      SqlObject("cornerB")
    )

  lazy val TelescopeConfigGeneratorMapping: ObjectMapping =
    ObjectMapping(TelescopeConfigGeneratorType)(
      SqlField("observationId", TelescopeConfigGeneratorView.ObservationId, key = true, hidden = true),
      SqlField("role",          TelescopeConfigGeneratorView.Role, key = true, hidden = true),

      SqlField("generatorType", TelescopeConfigGeneratorView.TelescopeConfigGeneratorType),

      SqlObject("enumerated", Join(List(
        TelescopeConfigGeneratorView.ObservationId -> TelescopeConfigGeneratorView.Enumerated.ObservationId,
        TelescopeConfigGeneratorView.Role          -> TelescopeConfigGeneratorView.Enumerated.Role
      ))),
      SqlObject("random",     Join(List(
        TelescopeConfigGeneratorView.ObservationId -> TelescopeConfigGeneratorView.Random.ObservationId,
        TelescopeConfigGeneratorView.Role          -> TelescopeConfigGeneratorView.Random.Role
      ))),
      SqlObject("spiral",     Join(List(
        TelescopeConfigGeneratorView.ObservationId -> TelescopeConfigGeneratorView.Spiral.ObservationId,
        TelescopeConfigGeneratorView.Role          -> TelescopeConfigGeneratorView.Spiral.Role
      ))),
      SqlObject("uniform",    Join(List(
        TelescopeConfigGeneratorView.ObservationId -> TelescopeConfigGeneratorView.Uniform.ObservationId,
        TelescopeConfigGeneratorView.Role          -> TelescopeConfigGeneratorView.Uniform.Role
      )))
    )

  lazy val TelescopeConfigGeneratorMappings: List[TypeMapping] =
    List(
      enumeratedTelescopeConfigGeneratorMapping(GmosNorthImagingType / "variant" / "grouped" / "offsets"    / "enumerated", TelescopeConfigGeneratorView.Enumerated.ObservationId, EnumeratedOffsetView.ObjectObservationId),
      enumeratedTelescopeConfigGeneratorMapping(GmosNorthImagingType / "variant" / "grouped" / "skyOffsets" / "enumerated", TelescopeConfigGeneratorView.Enumerated.ObservationId, EnumeratedOffsetView.SkyObservationId),
      enumeratedTelescopeConfigGeneratorMapping(GmosSouthImagingType / "variant" / "grouped" / "offsets"    / "enumerated", TelescopeConfigGeneratorView.Enumerated.ObservationId, EnumeratedOffsetView.ObjectObservationId),
      enumeratedTelescopeConfigGeneratorMapping(GmosSouthImagingType / "variant" / "grouped" / "skyOffsets" / "enumerated", TelescopeConfigGeneratorView.Enumerated.ObservationId, EnumeratedOffsetView.SkyObservationId),
      enumeratedTelescopeConfigGeneratorMapping(GmosNorthImagingType / "variant" / "interleaved" / "offsets"    / "enumerated", TelescopeConfigGeneratorView.Enumerated.ObservationId, EnumeratedOffsetView.ObjectObservationId),
      enumeratedTelescopeConfigGeneratorMapping(GmosNorthImagingType / "variant" / "interleaved" / "skyOffsets" / "enumerated", TelescopeConfigGeneratorView.Enumerated.ObservationId, EnumeratedOffsetView.SkyObservationId),
      enumeratedTelescopeConfigGeneratorMapping(GmosSouthImagingType / "variant" / "interleaved" / "offsets"    / "enumerated", TelescopeConfigGeneratorView.Enumerated.ObservationId, EnumeratedOffsetView.ObjectObservationId),
      enumeratedTelescopeConfigGeneratorMapping(GmosSouthImagingType / "variant" / "interleaved" / "skyOffsets" / "enumerated", TelescopeConfigGeneratorView.Enumerated.ObservationId, EnumeratedOffsetView.SkyObservationId),
      TelescopeConfigGeneratorMapping,
      RandomTelescopeConfigGeneratorMapping,
      SpiralTelescopeConfigGeneratorMapping,
      UniformTelescopeConfigGeneratorMapping
    )