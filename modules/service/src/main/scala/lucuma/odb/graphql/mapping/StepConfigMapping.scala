// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.syntax.functorFilter.*
import cats.syntax.option.*
import grackle.Cursor
import grackle.Predicate
import grackle.Predicate.Const
import grackle.Predicate.Eql
import grackle.Result
import grackle.Type
import grackle.TypeRef
import lucuma.core.enums.GcalArc
import lucuma.core.enums.StepType

import table.StepRecordView

trait StepConfigMapping[F[_]] extends StepRecordView[F] {

  // Defines a switch mapping from the step record root to prevent the mapping
  // from being picked up in the context of a generated sequence.
  private def stepConfigSwitchMapping(
    typeRef:    TypeRef,
    underlying: ObjectMapping
  ): TypeMapping =
    SwitchMapping(
      typeRef,
      List(StepRecordType / "stepConfig" -> underlying)
    )

  private lazy val stepConfigInterfaceMapping: ObjectMapping =
    SqlInterfaceMapping(
      tpe           = StepConfigType,
      discriminator = stepConfigDiscriminator,
      fieldMappings = List(
        SqlField("id",       StepRecordView.Id,       key = true, hidden = true),
        SqlField("stepType", StepRecordView.StepType, discriminator = true)
      )
    )

  private lazy val stepConfigDiscriminator: SqlDiscriminator =
    new SqlDiscriminator {
      import StepType.*

      override def discriminate(c: Cursor): Result[Type] =
        c.fieldAs[StepType]("stepType").map {
          case Bias      => BiasType
          case Dark      => DarkType
          case Gcal      => GcalType
          case Science   => ScienceType
          case SmartGcal => SmartGcalType
        }

      private def mkPredicate(stepType: StepType): Option[Predicate] =
        Eql(StepConfigType / "stepType", Const(stepType)).some

      override def narrowPredicate(tpe: Type): Option[Predicate] =
        tpe match {
          case BiasType      => mkPredicate(Bias)
          case DarkType      => mkPredicate(Dark)
          case GcalType      => mkPredicate(Gcal)
          case ScienceType   => mkPredicate(Science)
          case SmartGcalType => mkPredicate(SmartGcal)
          case _             => none
        }
    }

  private lazy val stepConfigBiasMapping: ObjectMapping =
    ObjectMapping(
      tpe           = BiasType,
      fieldMappings = List(
        SqlField("id", StepRecordView.Id, key = true, hidden = true)
      )
    )

  private lazy val stepConfigDarkMapping: ObjectMapping =
    ObjectMapping(
      tpe           = DarkType,
      fieldMappings = List(
        SqlField("id", StepRecordView.Id, key = true, hidden = true)
      )
    )

  private lazy val stepConfigGcalMapping: ObjectMapping = {
    def arc(c: Cursor, n: String, a: GcalArc): Result[Option[GcalArc]] =
      c.fieldAs[Boolean](n).map(b => Option.when(b)(a))

    ObjectMapping(
      tpe           = GcalType,
      fieldMappings = List(
        SqlField("id", StepRecordView.Id, key = true, hidden = true),
        SqlField("continuum", StepRecordView.Gcal.Continuum),
        SqlField("arArc",     StepRecordView.Gcal.ArArc,   hidden = true),
        SqlField("cuarArc",   StepRecordView.Gcal.CuarArc, hidden = true),
        SqlField("tharArc",   StepRecordView.Gcal.TharArc, hidden = true),
        SqlField("xeArc",     StepRecordView.Gcal.XeArc,   hidden = true),
        CursorField(
          "arcs",
          cursor =>
            for {
              ar   <- arc(cursor, "arArc",   GcalArc.ArArc)
              cuar <- arc(cursor, "cuarArc", GcalArc.CuArArc)
              thar <- arc(cursor, "tharArc", GcalArc.ThArArc)
              xe   <- arc(cursor, "xeArc",   GcalArc.XeArc)
            } yield List(ar, cuar, thar, xe).flattenOption,
          List("arArc", "cuarArc", "tharArc", "xeArc")
        ),
        SqlField("filter",    StepRecordView.Gcal.Filter),
        SqlField("diffuser",  StepRecordView.Gcal.Diffuser),
        SqlField("shutter",   StepRecordView.Gcal.Shutter)
      )
    )
  }

  private lazy val stepConfigScienceMapping: ObjectMapping =
    ObjectMapping(
      tpe           = ScienceType,
      fieldMappings = List(
        SqlField("synthetic_id", StepRecordView.Id, key = true, hidden = true),
        SqlObject("offset"),
        SqlField("guiding", StepRecordView.Science.GuideState)
      )
    )

  private lazy val stepConfigSmartGcalMapping: ObjectMapping =
    ObjectMapping(
      tpe           = SmartGcalType,
      fieldMappings = List(
        SqlField("id", StepRecordView.Id, key = true, hidden = true),
        SqlField("smartGcalType", StepRecordView.SmartGcal.Type)
      )
    )

  lazy val StepConfigMapping: TypeMapping =
    stepConfigSwitchMapping(StepConfigType, stepConfigInterfaceMapping)

  lazy val StepConfigBiasMapping: TypeMapping =
    stepConfigSwitchMapping(BiasType, stepConfigBiasMapping)

  lazy val StepConfigDarkMapping: TypeMapping =
    stepConfigSwitchMapping(DarkType, stepConfigDarkMapping)

  lazy val StepConfigGcalMapping: TypeMapping =
    stepConfigSwitchMapping(GcalType, stepConfigGcalMapping)

  lazy val StepConfigScienceMapping: TypeMapping =
    stepConfigSwitchMapping(ScienceType, stepConfigScienceMapping)

  lazy val StepConfigSmartGcalMapping: TypeMapping =
    stepConfigSwitchMapping(SmartGcalType, stepConfigSmartGcalMapping)

}
