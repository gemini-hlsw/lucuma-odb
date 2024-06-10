// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

import cats.effect.IO
import io.circe.syntax.*
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import lucuma.core.util.Gid
import lucuma.odb.json.angle.query.given
import lucuma.odb.json.sourceprofile.given
import lucuma.odb.json.wavelength.query.given
import org.flywaydb.core.api.migration.BaseJavaMigration
import org.flywaydb.core.api.migration.Context
import org.postgresql.core.BaseConnection
import org.postgresql.util.PGobject

class V0879__SourceProfile extends BaseIOMigration:
  def ioMigrate(ctx: Context, baseConnection: BaseConnection): IO[Unit] =
    IO.delay:
      val rs = baseConnection.execSQLQuery("select c_target_id, c_source_profile from t_target")
      val ps = baseConnection.prepareStatement(s"update t_target set c_source_profile = ? where c_target_id = ?")
      while (rs.next)
        val tid  = Gid[Target.Id].fromString.getOption(rs.getString(1)).getOrElse(sys.error("bogus"))
        val json = io.circe.parser.decode[SourceProfile](rs.getString(2)).getOrElse(sys.error("bogus")).asJson
        ps.setObject(1, {
          val o = new PGobject
          o.setType("jsonb")
          o.setValue(json.noSpaces)
          o
        })
        ps.setString(2, Gid[Target.Id].fromString.reverseGet(tid))
        ps.execute()
        ()
      ps.close()
      rs.close()
              
