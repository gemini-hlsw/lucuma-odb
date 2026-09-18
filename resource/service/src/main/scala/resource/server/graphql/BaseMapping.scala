// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql

import cats.Monoid
import cats.syntax.all.*
import grackle.Query.Binding
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.TypeRef
import grackle.circe.CirceMappingLike
import grackle.skunk.SkunkMapping
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait BaseMapping[F[_]] extends SkunkMapping[F] with CirceMappingLike[F]:
  given Logger[F] = Slf4jLogger.getLogger[F]

  lazy val MutationType     = schema.ref("Mutation")
  lazy val QueryType        = schema.ref("Query")
  lazy val SubscriptionType = schema.ref("Subscription")

  lazy val BigDecimalType                           = schema.ref("BigDecimal")
  lazy val ComponentLocationType                    = schema.ref("ComponentLocation")
  lazy val DateType                                 = schema.ref("Date")
  lazy val ExistenceType                            = schema.ref("Existence")
  lazy val InstrumentComponentAvailabilityBlockType =
    schema.ref("InstrumentComponentAvailabilityBlock")
  lazy val InstrumentComponentType                  = schema.ref("InstrumentComponent")
  lazy val InstrumentComponentTypeType              = schema.ref("InstrumentComponentType")
  lazy val LongType                                 = schema.ref("Long")
  lazy val MoonEventType                            = schema.ref("MoonEvent")
  lazy val MoonPhaseType                            = schema.ref("MoonPhase")
  lazy val NonEmptyStringType                       = schema.ref("NonEmptyString")
  lazy val PosIntType                               = schema.ref("PosInt")
  lazy val PublishedSemesterType                    = schema.ref("PublishedSemester")
  lazy val SemesterType                             = schema.ref("Semester")
  lazy val SiteType                                 = schema.ref("Site")
  lazy val TimestampType                            = schema.ref("Timestamp")
  lazy val PartnerType                              = schema.ref("Partner")
  lazy val ProgramReferenceLabelType                = schema.ref("ProgramReferenceLabel")
  lazy val TelescopeAvailabilityBlockType           = schema.ref("TelescopeAvailabilityBlock")
  lazy val TelescopeAvailabilityType                = schema.ref("TelescopeAvailability")
  lazy val TelescopeModeBlockType                   = schema.ref("TelescopeModeBlock")
  lazy val TelescopeModeType                        = schema.ref("TelescopeModeType")
  lazy val TooSupportBlockType                      = schema.ref("TooSupportBlock")
  lazy val TooSupportType                           = schema.ref("TooSupport")
  lazy val InstrumentAvailabilityBlockType          = schema.ref("InstrumentAvailabilityBlock")
  lazy val InstrumentPlaceType                      = schema.ref("InstrumentPlace")
  lazy val ResourceInstrumentType                   = schema.ref("ResourceInstrument")
  lazy val ResourceUsageType                        = schema.ref("ResourceUsage")
  lazy val PowerSourceType                          = schema.ref("PowerSource")
  lazy val TelescopeSubsystemType                   = schema.ref("TelescopeSubsystem")
  lazy val TelescopeSubsystemAvailabilityBlockType  =
    schema.ref("TelescopeSubsystemAvailabilityBlock")

  /**
   * The longest window any range query may cover. It bounds both the night queries and the flat
   * block queries, so no single request can stream a site's whole history.
   */
  protected val MaxWindow = 400L

  /**
   * Rejects a query window that is empty or longer than `MaxWindow`. `unit` names what the window
   * counts, for the message only.
   */
  protected def validateWindow(
    startIsBeforeEnd: Boolean,
    length:           Long,
    unit:             String
  ): Result[Unit] =
    if !startIsBeforeEnd then
      OdbError.InvalidArgument("Argument 'start' must be before 'end'.".some).asFailure
    else if length > MaxWindow then
      OdbError
        .InvalidArgument(s"Argument 'end' must be at most $MaxWindow $unit after 'start'.".some)
        .asFailure
    else Result.unit

  type ElaboratorPF = PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]]

  given Monoid[ElaboratorPF] with
    def empty: ElaboratorPF                                     = PartialFunction.empty
    def combine(x: ElaboratorPF, y: ElaboratorPF): ElaboratorPF = x.orElse(y)
