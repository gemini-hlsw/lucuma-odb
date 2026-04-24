// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql

import cats.Monoid
import grackle.Query.Binding
import grackle.QueryCompiler.Elab
import grackle.TypeRef
import grackle.circe.CirceMappingLike
import grackle.skunk.SkunkMapping
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait BaseMapping[F[_]] extends SkunkMapping[F] with CirceMappingLike[F]:
  given Logger[F] = Slf4jLogger.getLogger[F]

  type ElaboratorPF = PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]]

  given Monoid[ElaboratorPF] with
    def empty: ElaboratorPF                                     = PartialFunction.empty
    def combine(x: ElaboratorPF, y: ElaboratorPF): ElaboratorPF = x.orElse(y)
