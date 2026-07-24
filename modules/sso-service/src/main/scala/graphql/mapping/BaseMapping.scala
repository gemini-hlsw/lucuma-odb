// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service.graphql.mapping

import grackle.Mapping

trait BaseMapping[F[_]] extends Mapping[F]:

  lazy val ApiKeyIdType         = schema.ref("ApiKeyId")
  lazy val ApiKeyType           = schema.ref("ApiKey")
  lazy val MutationType         = schema.ref("Mutation")
  lazy val OrcidIdType          = schema.ref("OrcidId")
  lazy val PartnerType          = schema.ref("Partner")
  lazy val QueryType            = schema.ref("Query")
  lazy val RoleIdType           = schema.ref("RoleId")
  lazy val RoleType             = schema.ref("Role")
  lazy val RoleTypeType         = schema.ref("RoleType")
  lazy val SubscriptionType     = schema.ref("Subscription")
  lazy val UserIdType           = schema.ref("UserId")
  lazy val UserType             = schema.ref("User")
  lazy val UserSelectResultType = schema.ref("UserSelectResult")
  lazy val UserTypeType         = schema.ref("UserType")
  lazy val UserProfileType      = schema.ref("UserProfile")
