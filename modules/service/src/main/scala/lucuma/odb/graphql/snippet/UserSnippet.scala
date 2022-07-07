// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet

import edu.gemini.grackle.skunk.SkunkMapping
import edu.gemini.grackle.syntax._
import lucuma.core.model
import lucuma.odb.data
import lucuma.odb.graphql.util._
import lucuma.odb.util.Codecs._
import skunk.codec.all._

object UserSnippet {

  def apply[F[_]](m: SnippetMapping[F] with SkunkMapping[F]): m.Snippet = {
    import m.{ TableDef, ObjectMapping, Snippet, SqlField, LeafMapping }

    val schema =
      schema"""
        scalar UserId

        enum UserType {
          GUEST
          STANDARD
          SERVICE
        }

        type User {
          id:              UserId!
          type:            UserType!
          serviceName:     String,
          orcidId:         String,
          orcidGivenName:  String,
          orcidCreditName: String,
          orcidFamilyName: String,
          orcidEmail:      String,
        }
      """

    object User extends TableDef("t_user") {
      val UserId          = col("c_user_id", user_id)
      val UserType        = col("c_user_type", user_type)
      val ServiceName     = col("c_service_name", varchar.opt)
      val OrcidId         = col("c_orcid_id", varchar.opt)
      val OrcidGivenName  = col("c_orcid_given_name", varchar.opt)
      val OrcidCreditName = col("c_orcid_credit_name", varchar.opt)
      val OrcidFamilyName = col("c_orcid_family_name", varchar.opt)
      val OrcidEmail      = col("c_orcid_email", varchar.opt)
    }

    val UserType     = schema.ref("User")
    val UserIdType   = schema.ref("UserId")
    val UserTypeType = schema.ref("UserType")

    val typeMappings =
      List(
        ObjectMapping(
          tpe = UserType,
          fieldMappings = List(
            SqlField("id", User.UserId, key = true),
            SqlField("type", User.UserType),
            SqlField("serviceName", User.ServiceName),
            SqlField("orcidId", User.OrcidId),
            SqlField("orcidGivenName", User.OrcidGivenName),
            SqlField("orcidCreditName", User.OrcidCreditName),
            SqlField("orcidFamilyName", User.OrcidFamilyName),
            SqlField("orcidEmail", User.OrcidEmail)
          ),
        ),
        LeafMapping[model.User.Id](UserIdType),
        LeafMapping[data.UserType](UserTypeType)
      )

    Snippet(schema, typeMappings)

  }

}
