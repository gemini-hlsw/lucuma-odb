package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import grackle.syntax.*
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.asFailure
import lucuma.odb.data.PartnerLink
import lucuma.odb.graphql.binding.BooleanBinding
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.PartnerBinding


object PartnerAssociationInput:

  val Binding: Matcher[PartnerLink] =
    ObjectFieldsBinding.rmap:
      case List(
        PartnerBinding.Option("partner", rPartner),
        BooleanBinding.Option("isNonPartner", rNoPartner)
      ) =>
        (rPartner, rNonPartner).parTupled.flatMap:
          case (None,    None)        => PartnerLink.NoPartnerLink.success
          case (None,    Some(b))     => (if (b) PartnerLink.HasNoPartner else PartnerLink.NoPartnerLink).success
          case (Some(p), None)        => PartnerLink.HasPartner(p).success
          case (Some(p), Some(false)) => PartnerLink.HasPartner(p).success
          case (Some(p), Some(true))  => OdbError.InvalidArgument("Specify either 'partner' or 'isNonPartner'.").asFailure

