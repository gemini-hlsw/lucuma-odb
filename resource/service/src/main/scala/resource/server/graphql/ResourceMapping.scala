// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql

import _root_.skunk.Session
import cats.effect.*
import cats.syntax.all.*
import grackle.*
import grackle.QueryCompiler.SelectElaborator
import grackle.skunk.SkunkMapping
import grackle.skunk.SkunkMonitor

class ResourceMapping[F[_]: Async](
  pool:    Resource[F, Session[F]],
  monitor: SkunkMonitor[F]
)(override val schema: Schema)
    extends SkunkMapping[F](pool, monitor)
    with BaseMapping[F]
    with QueryMapping[F]
    with LeafMappings[F]
    with TelescopeAvailabilityBlockMapping[F]
    with TelescopeModeBlockMapping[F]
    with TooSupportBlockMapping[F]
    with InstrumentAvailabilityBlockMapping[F]
    with TelescopeSubsystemBlockMapping[F]
    with InstrumentComponentBlockMapping[F]:
  // with SubscriptionMapping[F]
  // with MutationMapping[F]:

  override val nightPool: Resource[F, Session[F]] = pool

  override val typeMappings: TypeMappings = TypeMappings(
    List(
      // MutationMapping,
      QueryMapping
      // SubscriptionMapping,

    ) ++ PublishedSemesterMappings ++ TelescopeAvailabilityBlockMappings ++ TelescopeModeBlockMappings ++ TooSupportBlockMappings ++ InstrumentAvailabilityBlockMappings ++ TelescopeSubsystemBlockMappings ++ InstrumentComponentBlockMappings ++ ComponentCatalogMappings ++ LeafMappings
  )

  override val selectElaborator: SelectElaborator =
    SelectElaborator(
      List(
        // MutationElaborator,
        QueryElaborator
      ).combineAll
    )
