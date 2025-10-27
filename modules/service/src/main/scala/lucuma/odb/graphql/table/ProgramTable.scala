// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs.*
import skunk.codec.boolean.bool
import skunk.codec.temporal.date

trait ProgramTable[F[_]] extends BaseMapping[F]:

  object ProgramTable extends TableDef("t_program"):
    val Id              = col("c_program_id", program_id)
    val Existence       = col("c_existence", existence)
    val Name            = col("c_name", text_nonempty.opt)
    val Description     = col("c_description", text_nonempty.opt)
    val ProposalStatus  = col("c_proposal_status", tag)
    val CalibrationRole = col("c_calibration_role", calibration_role.opt)
    val ActiveStart     = col("c_active_start", date)
    val ActiveEnd       = col("c_active_end", date)

    object Goa:
      val Proprietary   = col("c_goa_proprietary", int4_nonneg)
      val ShouldNotify  = col("c_goa_should_notify", bool)
      val PrivateHeader = col("c_goa_private_header", bool)

    val ProgramType    = col("c_program_type", program_type)

    object Reference:
      val Instrument        = col("c_instrument",         instrument.opt)
      val ProgramReference  = col("c_program_reference",  program_reference.opt)
      val ProposalReference = col("c_proposal_reference", proposal_reference.opt)
      val ScienceSubtype    = col("c_science_subtype",    science_subtype.opt)
      val Semester          = col("c_semester",           semester.opt)
      val SemesterIndex     = col("c_semester_index",     int4_pos.opt)