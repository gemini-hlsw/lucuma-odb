// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

import cats.data.NonEmptyList
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.functorFilter.*
import cats.syntax.option.*
import lucuma.core.enums.Instrument
import lucuma.core.syntax.string.*
import lucuma.odb.smartgcal.data.SmartGcalValue
import lucuma.odb.util.Codecs.*
import skunk.Encoder

trait SmartGcalTable {

  import SmartGcalTable.Col
  import SmartGcalTable.InstrumentCol
  import SmartGcalTable.GcalIdCol

  def name: String

  def cols: NonEmptyList[Col]

  def truncate: String =
    s"TRUNCATE TABLE $name RESTART IDENTITY"

  def colNames: String =
    cols.map(_.name).intercalate(", ")

  def alterTable(actions: List[String]): String =
    actions.mkString(s"ALTER TABLE $name\n\t", ",\n\t", "")

  def insertFromTemp(tempTable: String): String = {
    val cs = s"${InstrumentCol.name}, ${GcalIdCol.name}, $colNames"
    s"""
       |INSERT INTO $name ( $cs )
       |  SELECT $cs
       |  FROM $tempTable
       |""".stripMargin
  }

}

object SmartGcalTable {

  case class Col(
    name:     String,
    dataType: String,
    isIndex:  Boolean,
    fTable:   Option[String]
  ) {

    def fkeyConstraintName(tableName: String): String =
      s"${tableName}_${name}_fkey"

    def dropFkeyConstraint(tableName: String): Option[String] =
      fTable.as(s"DROP CONSTRAINT ${fkeyConstraintName(tableName)}")

    def addFkeyConstraint(tableName: String): Option[String] =
      fTable.map { ft =>
        s"ADD CONSTRAINT ${fkeyConstraintName(tableName)} FOREIGN KEY ( $name ) REFERENCES $ft(c_tag)"
      }

    def index: Col =
      copy(isIndex = true)

    def reference(n: String): Col =
      copy(fTable = n.some)

  }

  object Col {

    def apply(name: String, dataType: String): Col =
      Col(name, dataType, isIndex = false, fTable = none)

    def bool(name: String): Col =
      Col(name, "boolean")

    def fkey(name: String, references: String): Col =
      Col(name, "d_tag", isIndex = false, references.some)

  }

  val InstrumentCol: Col = Col.fkey("c_instrument", "t_instrument")
  val GcalIdCol: Col     = Col("c_gcal_id", "serial")

  def valueEncoder[A](using e: Encoder[A]): Encoder[SmartGcalValue[A]] =
    (
      step_config_gcal *:
      int4_pos         *:
      gcal_baseline    *:
      e
    ).contramap[SmartGcalValue[A]] { v => (
      v.gcalConfig   ,
      v.stepCount    ,
      v.baselineType ,
      v.instrumentConfig
    )}

  object Gcal extends SmartGcalTable {

    def name: String =
      "t_gcal"

    def cols: NonEmptyList[Col] =
      NonEmptyList.of(
        Col.fkey("c_gcal_continuum", "t_gcal_continuum"),
        Col.bool("c_gcal_ar_arc"),
        Col.bool("c_gcal_cuar_arc"),
        Col.bool("c_gcal_thar_arc"),
        Col.bool("c_gcal_xe_arc"),
        Col.fkey("c_gcal_filter", "t_gcal_filter"),
        Col.fkey("c_gcal_diffuser", "t_gcal_diffuser"),
        Col.fkey("c_gcal_shutter", "t_gcal_shutter"),
        Col("c_gcal_step_count", "int4"),
        Col("c_gcal_baseline", "e_gcal_baseline_type")
      )

    def deleteWhereInstrumentEquals(inst: Instrument): String =
      s"DELETE FROM $name WHERE c_instrument = '${inst.tag}'"

    def dropFkeyConstraints: String =
      alterTable((InstrumentCol :: cols).toList.mapFilter(_.dropFkeyConstraint(name)))

    def addFkeyConstraints: String =
      alterTable((InstrumentCol :: cols).toList.mapFilter(_.addFkeyConstraint(name)))

  }

  case class Temp(inst: Instrument, cols: NonEmptyList[Col]) extends SmartGcalTable {

    def name: String =
      s"t_temp_smart_${inst.tag.toSnakeCase}"

    def create: String =
      s"""
         |CREATE TEMP TABLE $name (
         |  ${InstrumentCol.name} ${InstrumentCol.dataType} DEFAULT '${inst.tag}',
         |  ${GcalIdCol.name} ${GcalIdCol.dataType},
         |  PRIMARY KEY (${InstrumentCol.name}, ${GcalIdCol.name}),
         |  ${cols.map(c => s"${c.name} ${c.dataType}").intercalate(",\n  ")}
         |) ON COMMIT DROP
         |""".stripMargin

    def copyFromStdin: String =
      s"COPY $name ( $colNames ) FROM STDIN WITH ( DELIMITER '|', NULL 'NULL' )"

  }

  case class Inst(inst: Instrument, cols: NonEmptyList[Col]) extends SmartGcalTable {

    def name: String =
      s"t_smart_${inst.tag.toSnakeCase}"

    def index: String =
      s"i_smart_${inst.tag.toSnakeCase}"

    def dropIndex: String =
      s"DROP INDEX IF EXISTS $index"

    def createIndex: String =
      s"CREATE INDEX $index ON $name ( ${cols.collect { case c@Col(_, _, true, _) => c.name }.mkString(", ")} )"

    private def gcalTableRefConstraint: String =
      s"${name}_${InstrumentCol.name}_${GcalIdCol.name}_fkey"

    def dropFkeyConstraints: String =
      alterTable(
        s"DROP CONSTRAINT $gcalTableRefConstraint" :: cols.toList.mapFilter(_.dropFkeyConstraint(name))
      )

    def addFkeyConstraints: String =
      alterTable(
        s"ADD CONSTRAINT $gcalTableRefConstraint FOREIGN KEY ( ${InstrumentCol.name}, ${GcalIdCol.name} ) REFERENCES ${Gcal.name}(${InstrumentCol.name}, ${GcalIdCol.name})" ::
          cols.toList.mapFilter(_.addFkeyConstraint(name))
      )

  }

  def forInstrument(
    inst:      Instrument,
    orderCol:  Col,
    keyCols:   NonEmptyList[Col],
    valueCols: NonEmptyList[Col]
  ): (Temp, Inst) = (
    Temp(inst, (orderCol :: keyCols) ::: (Gcal.cols ::: valueCols)),
    Inst(inst, orderCol :: keyCols ::: valueCols)
  )

}
