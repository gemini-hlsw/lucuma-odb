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

import table.ProgramReferenceView

trait ProgramReferenceMapping[F[_]]
  extends BaseMapping[F]
     with ProgramReferenceView[F] {

  lazy val ProgramReferenceMapping: ObjectMapping =
    SqlInterfaceMapping(
      tpe           = ProgramReferenceType,
      discriminator = programReferenceTypeDiscriminator,
      fieldMappings = List(
        SqlField("id",    ProgramReferenceView.Id,          key = true, hidden = true),
        SqlField("type",  ProgramReferenceView.ProgramType, discriminator = true),
        SqlField("label", ProgramReferenceView.ProgramReference),

        // Used for WHERE clause matching
        SqlField("labelString", ProgramReferenceView.ProgramReferenceString, hidden = true),
        SqlField("whereSemester", ProgramReferenceView.Semester, hidden = true),
        SqlField("whereSemesterIndex", ProgramReferenceView.SemesterIndex, hidden = true),
        SqlField("whereInstrument", ProgramReferenceView.Instrument, hidden = true),
        SqlField("whereDescription", ProgramReferenceView.LibraryDesciption, hidden = true),
        SqlField("whereScienceSubtype", ProgramReferenceView.ScienceSubtype, hidden = true)
      )
    )

  private lazy val programReferenceTypeDiscriminator: SqlDiscriminator =
    new SqlDiscriminator {
      import lucuma.odb.data.{ProgramType => PT}

      override def discriminate(c: Cursor): Result[Type] =
        c.fieldAs[PT]("type").flatMap {
          case PT.Calibration => Result(CalibrationProgramReferenceType)
          case PT.Engineering => Result(EngineeringProgramReferenceType)
          case PT.Example     => Result(ExampleProgramReferenceType)
          case PT.Library     => Result(LibraryProgramReferenceType)
          case PT.Science     => Result(ScienceProgramReferenceType)
        }

      private def mkPredicate(tpe: PT): Option[Predicate] =
        Eql(ProgramReferenceType / "type", Const(tpe)).some

      override def narrowPredicate(tpe: Type): Option[Predicate] =
        tpe match {
          case CalibrationProgramReferenceType => mkPredicate(PT.Calibration)
          case EngineeringProgramReferenceType => mkPredicate(PT.Engineering)
          case ExampleProgramReferenceType     => mkPredicate(PT.Example)
          case LibraryProgramReferenceType     => mkPredicate(PT.Library)
          case ScienceProgramReferenceType     => mkPredicate(PT.Science)
          case _                               => none
        }
    }

  lazy val CalibrationProgramReferenceMapping: ObjectMapping =
    ObjectMapping(
      tpe = CalibrationProgramReferenceType,
      fieldMappings = List(
        SqlField("id",            ProgramReferenceView.Id, key = true, hidden = true),
        SqlField("instrument",    ProgramReferenceView.Instrument),
        SqlField("semester",      ProgramReferenceView.Semester),
        SqlField("semesterIndex", ProgramReferenceView.SemesterIndex)
      )
    )

  lazy val EngineeringProgramReferenceMapping: ObjectMapping =
    ObjectMapping(
      tpe = EngineeringProgramReferenceType,
      fieldMappings = List(
        SqlField("id",            ProgramReferenceView.Id, key = true, hidden = true),
        SqlField("instrument",    ProgramReferenceView.Instrument),
        SqlField("semester",      ProgramReferenceView.Semester),
        SqlField("semesterIndex", ProgramReferenceView.SemesterIndex)
      )
    )

  lazy val ExampleProgramReferenceMapping: ObjectMapping =
    ObjectMapping(
      tpe = ExampleProgramReferenceType,
      fieldMappings = List(
        SqlField("id",            ProgramReferenceView.Id, key = true, hidden = true),
        SqlField("instrument",    ProgramReferenceView.Instrument)
      )
    )

  lazy val LibraryProgramReferenceMapping: ObjectMapping =
    ObjectMapping(
      tpe = LibraryProgramReferenceType,
      fieldMappings = List(
        SqlField("id",            ProgramReferenceView.Id, key = true, hidden = true),
        SqlField("description",   ProgramReferenceView.LibraryDesciption),
        SqlField("instrument",    ProgramReferenceView.Instrument)
      )
    )

  lazy val ScienceProgramReferenceMapping: ObjectMapping =
    ObjectMapping(
      tpe = ScienceProgramReferenceType,
      fieldMappings = List(
        SqlField("id",             ProgramReferenceView.Id, key = true, hidden = true),
        SqlField("scienceSubtype", ProgramReferenceView.ScienceSubtype),
        SqlField("semester",       ProgramReferenceView.Semester),
        SqlField("semesterIndex",  ProgramReferenceView.SemesterIndex)
      )
    )
}