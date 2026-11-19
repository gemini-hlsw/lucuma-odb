// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import lucuma.ags.GuideStarId
import lucuma.core.model.Observation
import lucuma.core.model.ObservationReference
import lucuma.core.model.Target
import lucuma.odb.graphql.binding.*
import lucuma.odb.service.GuideService

final case class SetGuideTargetInput(
  observationId:  Option[Observation.Id],
  observationRef: Option[ObservationReference],
  guideTarget:    Option[GuideStarId]
)

object SetGuideTargetInput:

  private val CatalogNameBinding: Matcher[GuideStarId] =
    NonEmptyStringBinding.rmap(name => GuideService.catalogGuideStarId(name))

  private val ObservationTargetBinding: Matcher[GuideStarId] =
    ObjectFieldsBinding.rmap:
      case List(TargetIdBinding.Option("targetId", rTargetId)) =>
        rTargetId.map((targetId: Option[Target.Id]) => GuideStarId.ObservationTarget(targetId))

  private val GuideTargetSelectionBinding: Matcher[GuideStarId] =
    OneOfBinding(
      "catalogName"       -> CatalogNameBinding,
      "observationTarget" -> ObservationTargetBinding
    )

  val Binding: Matcher[SetGuideTargetInput] =
    ObjectFieldsBinding.rmap:
      case List(
        ObservationIdBinding.Option("observationId", rObsId),
        ObservationReferenceBinding.Option("observationReference", rObsRef),
        GuideTargetSelectionBinding.Option("guideTarget", rGuideTarget)
      ) => (rObsId, rObsRef, rGuideTarget).parMapN(apply)
