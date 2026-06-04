// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql

import cats.Monoid
import grackle.Env
import grackle.Path
import grackle.Query.Binding
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.TypeRef
import grackle.circe.CirceMappingLike
import grackle.skunk.SkunkMapping
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait BaseMapping[F[_]] extends SkunkMapping[F] with CirceMappingLike[F]:
  given Logger[F] = Slf4jLogger.getLogger[F]

  lazy val MutationType     = schema.ref("Mutation")
  lazy val QueryType        = schema.ref("Query")
  lazy val SubscriptionType = schema.ref("Subscription")

  lazy val BigDecimalType                  = schema.ref("BigDecimal")
  lazy val DateType                        = schema.ref("Date")
  lazy val LongType                        = schema.ref("Long")
  lazy val ProgramReferenceLabelType       = schema.ref("ProgramReferenceLabel")
  lazy val SiteType                        = schema.ref("Site")
  lazy val TelescopeAvailabilityStatusType = schema.ref("TelescopeAvailabilityStatus")
  lazy val TelescopeAvailabilityType       = schema.ref("TelescopeAvailability")
  lazy val TelescopeModeStatusType         = schema.ref("TelescopeModeStatus")
  lazy val TelescopeModeType               = schema.ref("TelescopeModeType")
  lazy val TelescopeNightTimelineType      = schema.ref("TelescopeNightTimeline")
  lazy val TelescopeTooStatusType          = schema.ref("TelescopeTooStatus")
  lazy val TimestampType                   = schema.ref("Timestamp")
  lazy val TooSupportType                  = schema.ref("TooSupport")

  type ElaboratorPF = PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]]
  type ComputeF[A]  = (Path, Env) => F[Result[A]]

  given Monoid[ElaboratorPF] with
    def empty: ElaboratorPF                                     = PartialFunction.empty
    def combine(x: ElaboratorPF, y: ElaboratorPF): ElaboratorPF = x.orElse(y)
