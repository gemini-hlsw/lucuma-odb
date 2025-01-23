// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import lucuma.core.model.StandardRole
import lucuma.core.util.Gid
import lucuma.odb.util.Codecs.*
import skunk.codec.all.*
import skunk.syntax.all.*

class ShortCut_3485 extends OdbSuite:

  val pi = TestUsers.Standard(
    id    = 101, 
    role  = StandardRole.Pi(Gid[StandardRole.Id].fromLong.getOption(102).get),
    email = Some("pi@pi.org") 
  )

  val validUsers = List(pi)

  test("user email should be included when canonicalized implicitly"):
    for
      _ <- query(pi, "query { callsForProposals { matches { id } } }")
      e <- session.use(_.unique(sql"select c_orcid_email from t_user where c_user_id = $user_id".query(varchar.opt))(pi.id))
    yield assert(e == Some("pi@pi.org"))

