// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.syntax.functorFilter.*
import grackle.Cursor
import grackle.Predicate
import grackle.Predicate.Const
import grackle.Predicate.Eql
import grackle.Result
import grackle.Type
import grackle.syntax.*
import lucuma.core.enums.GcalArc
import lucuma.core.enums.StepType

import table.StepView

trait StepConfigMapping[F[_]] extends StepView[F] {

  lazy val StepConfigMapping: ObjectMapping =
    SqlInterfaceMapping(StepRecordType / "stepConfig" % StepConfigType, stepConfigDiscriminator)(
      SqlField("id",       StepView.Id,       key = true, hidden = true),
      SqlField("stepType", StepView.StepType, discriminator = true)
    )

  private lazy val stepConfigDiscriminator: SqlDiscriminator =
    new SqlDiscriminator {
      import StepType.*

      override def discriminate(c: Cursor): Result[Type] =
        c.fieldAs[StepType]("stepType").map:
          case Bias      => BiasType
          case Dark      => DarkType
          case Gcal      => GcalType
          case Science   => ScienceType
          case SmartGcal => SmartGcalType

      private def mkPredicate(stepType: StepType): Result[Predicate] =
        Eql(StepConfigType / "stepType", Const(stepType)).success

      override def narrowPredicate(tpe: Type): Result[Predicate] =
        tpe match
          case BiasType      => mkPredicate(Bias)
          case DarkType      => mkPredicate(Dark)
          case GcalType      => mkPredicate(Gcal)
          case ScienceType   => mkPredicate(Science)
          case SmartGcalType => mkPredicate(SmartGcal)
          case _             => Result.internalError(s"Invalid discriminator: $tpe")
    }

  lazy val StepConfigBiasMapping: ObjectMapping =
    ObjectMapping(StepRecordType / "stepConfig" % BiasType)(
      SqlField("id", StepView.Id, key = true, hidden = true)
    )

  lazy val StepConfigDarkMapping: ObjectMapping =
    ObjectMapping(StepRecordType / "stepConfig" % DarkType)(
      SqlField("id", StepView.Id, key = true, hidden = true)
    )

  lazy val StepConfigGcalMapping: ObjectMapping = {
    def arc(c: Cursor, n: String, a: GcalArc): Result[Option[GcalArc]] =
      c.fieldAs[Boolean](n).map(b => Option.when(b)(a))

    ObjectMapping(StepRecordType / "stepConfig" % GcalType)(
      SqlField("id", StepView.Id, key = true, hidden = true),
      SqlField("continuum", StepView.Gcal.Continuum),
      SqlField("arArc",     StepView.Gcal.ArArc,   hidden = true),
      SqlField("cuarArc",   StepView.Gcal.CuarArc, hidden = true),
      SqlField("tharArc",   StepView.Gcal.TharArc, hidden = true),
      SqlField("xeArc",     StepView.Gcal.XeArc,   hidden = true),
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
      SqlField("filter",    StepView.Gcal.Filter),
      SqlField("diffuser",  StepView.Gcal.Diffuser),
      SqlField("shutter",   StepView.Gcal.Shutter)
    )
  }

  lazy val StepConfigScienceMapping: ObjectMapping =
    ObjectMapping(StepRecordType / "stepConfig" % ScienceType)(
      SqlField("id", StepView.Id, key = true, hidden = true)
    )

  lazy val StepConfigSmartGcalMapping: ObjectMapping =
    ObjectMapping(StepRecordType / "stepConfig" % SmartGcalType)(
      SqlField("id", StepView.Id, key = true, hidden = true),
      SqlField("smartGcalType", StepView.SmartGcal.Type)
    )

}
