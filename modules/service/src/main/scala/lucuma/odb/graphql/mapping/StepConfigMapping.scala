// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.syntax.functorFilter.*
import cats.syntax.option.*
import edu.gemini.grackle.Cursor
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate.Const
import edu.gemini.grackle.Predicate.Eql
import edu.gemini.grackle.Result
import edu.gemini.grackle.Type
import edu.gemini.grackle.TypeRef
import lucuma.core.enums.GcalArc
import lucuma.core.enums.StepType

import table.StepRecordTable

trait StepConfigMapping[F[_]] extends StepRecordTable[F] {

  // Defines a switch mapping from the step record root to prevent the mapping
  // from being picked up in the context of a generated sequence.
  private def stepConfigSwitchMapping(
    typeRef:    TypeRef,
    underlying: ObjectMapping
  ): TypeMapping =
    SwitchMapping(
      typeRef,
      List(
        GmosNorthStepRecordType / "stepConfig" -> underlying,
        GmosSouthStepRecordType / "stepConfig" -> underlying
      )
    )

  private lazy val stepConfigInterfaceMapping: ObjectMapping =
    SqlInterfaceMapping(
      tpe           = StepConfigType,
      discriminator = stepTypeDiscriminator,
      fieldMappings = List(
        SqlField("id",       StepRecordTable.Id,       key = true, hidden = true),
        SqlField("stepType", StepRecordTable.StepType, discriminator = true)
      )
    )

  private lazy val stepTypeDiscriminator: SqlDiscriminator =
    new SqlDiscriminator {
      override def discriminate(c:  Cursor): Result[Type] =
        c.fieldAs[StepType]("stepType").map {
          case StepType.Bias      => BiasType
          case StepType.Dark      => DarkType
          case StepType.Gcal      => GcalType
          case StepType.Science   => ScienceType
          case StepType.SmartGcal => SmartGcalType
        }

      private def mkPredicate(tpe: StepType): Option[Predicate] =
        Eql(StepTypeType / "stepType", Const(tpe)).some

      override def narrowPredicate(tpe: Type): Option[Predicate] =
        tpe match {
          case BiasType      => mkPredicate(StepType.Bias)
          case DarkType      => mkPredicate(StepType.Dark)
          case GcalType      => mkPredicate(StepType.Gcal)
          case ScienceType   => mkPredicate(StepType.Science)
          case SmartGcalType => mkPredicate(StepType.SmartGcal)
          case _             => none
        }
    }

  private lazy val stepConfigBiasMapping: ObjectMapping =
    ObjectMapping(
      tpe           = BiasType,
      fieldMappings = List(
        SqlField("id", StepRecordTable.Id, key = true, hidden = true)
      )
    )

  private lazy val stepConfigDarkMapping: ObjectMapping =
    ObjectMapping(
      tpe           = DarkType,
      fieldMappings = List(
        SqlField("id", StepRecordTable.Id, key = true, hidden = true)
      )
    )

  private lazy val stepConfigGcalMapping: ObjectMapping = {
    def arc(c: Cursor, n: String, a: GcalArc): Result[Option[GcalArc]] =
      c.fieldAs[Boolean](n).map(b => Option.when(b)(a))

    ObjectMapping(
      tpe           = GcalType,
      fieldMappings = List(
        SqlField("id", StepRecordTable.Id, key = true, hidden = true),
        SqlField("continuum", StepRecordTable.Gcal.Continuum),
        SqlField("arArc",     StepRecordTable.Gcal.ArArc,   hidden = true),
        SqlField("cuarArc",   StepRecordTable.Gcal.CuarArc, hidden = true),
        SqlField("tharArc",   StepRecordTable.Gcal.TharArc, hidden = true),
        SqlField("xeArc",     StepRecordTable.Gcal.XeArc,   hidden = true),
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
        SqlField("filter",    StepRecordTable.Gcal.Filter),
        SqlField("diffuser",  StepRecordTable.Gcal.Diffuser),
        SqlField("shutter",   StepRecordTable.Gcal.Shutter)
      )
    )
  }

  private lazy val stepConfigScienceMapping: ObjectMapping =
    ObjectMapping(
      tpe           = ScienceType,
      fieldMappings = List(
        SqlField("synthetic_id", StepRecordTable.Id, key = true, hidden = true),
        SqlObject("offset"),
        SqlField("guiding", StepRecordTable.Science.GuideState)
      )
    )

  private lazy val stepConfigSmartGcalMapping: ObjectMapping =
    ObjectMapping(
      tpe           = SmartGcalType,
      fieldMappings = List(
        SqlField("id", StepRecordTable.Id, key = true, hidden = true),
        SqlField("smartGcalType", StepRecordTable.SmartGcal.Type)
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
