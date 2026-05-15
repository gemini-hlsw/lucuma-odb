// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql

import grackle.TypeRef

trait SubscriptionMapping[F[_]] extends BaseMapping[F] {
  val SubscriptionType: TypeRef = schema.ref("Subscription")

  lazy val SubscriptionMapping =
    ObjectMapping(SubscriptionType)(
    )
}
