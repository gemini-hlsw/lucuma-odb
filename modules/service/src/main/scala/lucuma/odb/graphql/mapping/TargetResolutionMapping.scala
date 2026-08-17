// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping

import table.TargetView

/**
 * What a Target of Opportunity resolved to. The tracking lives in the same `c_sid_*` / `c_nsid_*`
 * columns an ordinary target of that subtype would use, so these mappings differ from
 * `SiderealMapping` and `NonsiderealMapping` only in their synthetic key -- which is null unless
 * this row is an opportunity target resolved that way. That is what keeps a resolved ToO from
 * also surfacing as a top-level `sidereal`, so exactly one of the three target subtype fields
 * stays non-null.
 *
 * The field lists are duplicated from those two mappings rather than shared, because sharing
 * would mean converting them to aggregate mapping lists and re-registering them -- registering
 * both an aggregate list and its members is a runtime `ValidationException`.
 */
trait TargetResolutionMapping[F[_]] extends TargetView[F] {

  lazy val TargetResolutionMappings: List[TypeMapping] =
    List(
      ObjectMapping(TargetResolutionType)(
        SqlField("synthetic_id", TargetView.Opportunity.Resolution.SyntheticId, key = true, hidden = true),
        SqlObject("sidereal"),
        SqlObject("nonsidereal"),
      ),
      ObjectMapping(TargetResolutionType / "sidereal")(
        SqlField("synthetic_id", TargetView.Opportunity.Resolution.Sidereal.SyntheticId, key = true, hidden = true),
        SqlObject("ra"),
        SqlObject("dec"),
        SqlField("epoch", TargetView.Sidereal.Epoch),
        SqlObject("properMotion"),
        SqlObject("radialVelocity"),
        SqlObject("parallax"),
        SqlObject("catalogInfo"),
      ),
      ObjectMapping(TargetResolutionType / "nonsidereal")(
        SqlField("synthetic_id", TargetView.Opportunity.Resolution.Nonsidereal.SyntheticId, key = true, hidden = true),
        SqlField("des", TargetView.Nonsidereal.Des),
        SqlField("keyType", TargetView.Nonsidereal.KeyType),
        SqlField("key", TargetView.Nonsidereal.Key),
      ),
    )

}
