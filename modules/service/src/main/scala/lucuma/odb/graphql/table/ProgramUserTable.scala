// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs.*
import skunk.codec.all.*

trait ProgramUserTable[F[_]] extends BaseMapping[F] {

  object ProgramUserTable extends TableDef("t_program_user") {
    val ProgramId         = col("c_program_id", program_id)
    val UserId            = col("c_user_id", user_id)
    val Role              = col("c_role", program_user_role)
    val Partner           = col("c_partner", partner.opt)
    val PartnerLink       = col("c_partner_link", partner_link_type)
    val EducationalStatus = col("c_educational_status", educational_status.opt)
    val Thesis            = col("c_thesis", bool.opt)
    val Gender            = col("c_gender", gender.opt)
  }

}
