// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs._
import skunk.codec.all._

trait UserTable[F[_]] extends BaseMapping[F] {

  object UserTable extends TableDef("t_user") {
    val UserId          = col("c_user_id", user_id)
    val UserType        = col("c_user_type", user_type)
    val ServiceName     = col("c_service_name", varchar.opt)
    val OrcidId         = col("c_orcid_id", varchar.opt)
    val OrcidGivenName  = col("c_orcid_given_name", varchar.opt)
    val OrcidCreditName = col("c_orcid_credit_name", varchar.opt)
    val OrcidFamilyName = col("c_orcid_family_name", varchar.opt)
    val OrcidEmail      = col("c_orcid_email", varchar.opt)
  }

}