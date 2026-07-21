// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service
package graphql
package predicate

import grackle.Path
import lucuma.sso.service.graphql.mapping.BaseMapping

trait Predicates[F[_]] extends BaseMapping[F]:
  object Predicates:
    val user = UserPredicates(Path.from(UserType))


