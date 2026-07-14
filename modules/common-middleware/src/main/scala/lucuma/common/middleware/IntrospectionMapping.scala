// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.common.middleware

import cats.MonadThrow
import grackle.Mapping
import grackle.QueryCompiler.Elab
import grackle.QueryCompiler.SelectElaborator
import grackle.Schema

/**
 * A minimal, read-only Grackle mapping that supports GraphQL schema introspection and nothing else.
 * It exposes the schema so that clients can fetch it without credentials, but rejects other queries
 */
object IntrospectionMapping:

  def apply[F[_]: MonadThrow](loadedSchema: Schema): Mapping[F] =
    new Mapping[F]:
      val M: MonadThrow[F] = MonadThrow[F]
      val schema: Schema   = loadedSchema

      private val QueryType = schema.ref(schema.queryType.name)

      val typeMappings: TypeMappings =
        TypeMappings.unchecked(
          ObjectMapping(QueryType, Nil)
        )

      override val selectElaborator: SelectElaborator =
        SelectElaborator:
          case (QueryType, field, _) =>
            Elab.failure(s"Field '$field' requires authentication.")
