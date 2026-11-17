// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.GroupConstraintWaiverView

// Waivers listed by Group.waivers and returned by the waiver mutations.  The
// waivers in GroupProgress.waived are the same type, encoded as JSON with the
// rest of the progress instead (lucuma.odb.json.groupprogress), and the two
// must agree.  The nullable element, night and revocation are keyed by
// synthetic ids that are null when they are absent.  Waivers are always
// reached through a join (from the group or a mutation result), so that
// Grackle reads their columns as nullable.
trait GroupConstraintWaiverMapping[F[_]] extends GroupConstraintWaiverView[F]:

  private lazy val Path = GroupConstraintWaiverType

  lazy val GroupConstraintWaiverMappings: List[TypeMapping] =
    List(
      ObjectMapping(Path)(
        SqlField("id",        GroupConstraintWaiverView.Id, key = true),
        SqlField("groupId",   GroupConstraintWaiverView.GroupId),
        SqlObject("violation"),
        SqlObject("granted"),
        SqlObject("revoked"),
        SqlField("grantedAt", GroupConstraintWaiverView.GrantedAt, hidden = true),
        SqlField("active",    GroupConstraintWaiverView.Active, hidden = true)
      ),
      ObjectMapping(Path / "violation")(
        SqlField("synthetic_id", GroupConstraintWaiverView.Id, key = true, hidden = true),
        SqlField("code",         GroupConstraintWaiverView.Code),
        SqlObject("element"),
        SqlObject("night"),
        SqlField("subgroupId",   GroupConstraintWaiverView.SubgroupId)
      ),
      ObjectMapping(Path / "violation" / "element")(
        SqlField("synthetic_id",  GroupConstraintWaiverView.ElementId, key = true, hidden = true),
        SqlField("groupId",       GroupConstraintWaiverView.ElementGroupId),
        SqlField("observationId", GroupConstraintWaiverView.ElementObservationId)
      ),
      ObjectMapping(Path / "violation" / "night")(
        SqlField("synthetic_id", GroupConstraintWaiverView.NightId, key = true, hidden = true),
        SqlField("site",         GroupConstraintWaiverView.NightSite),
        SqlField("date",         GroupConstraintWaiverView.NightDate)
      ),
      ObjectMapping(Path / "granted")(
        SqlField("synthetic_id", GroupConstraintWaiverView.Id, key = true, hidden = true),
        SqlField("by",           GroupConstraintWaiverView.GrantedBy),
        SqlField("at",           GroupConstraintWaiverView.GrantedAt),
        SqlField("reason",       GroupConstraintWaiverView.GrantedReason)
      ),
      ObjectMapping(Path / "revoked")(
        SqlField("synthetic_id", GroupConstraintWaiverView.RevokedId, key = true, hidden = true),
        SqlField("by",           GroupConstraintWaiverView.RevokedBy),
        SqlField("at",           GroupConstraintWaiverView.RevokedAt),
        SqlField("reason",       GroupConstraintWaiverView.RevokedReason)
      ),
      ObjectMapping(WaiveGroupConstraintResultType)(
        SqlField("id", GroupConstraintWaiverTable.Id, key = true, hidden = true),
        SqlObject("waiver", Join(GroupConstraintWaiverTable.Id, GroupConstraintWaiverView.Id))
      ),
      ObjectMapping(RevokeGroupConstraintWaiverResultType)(
        SqlField("id", GroupConstraintWaiverTable.Id, key = true, hidden = true),
        SqlObject("waiver", Join(GroupConstraintWaiverTable.Id, GroupConstraintWaiverView.Id))
      )
    )
