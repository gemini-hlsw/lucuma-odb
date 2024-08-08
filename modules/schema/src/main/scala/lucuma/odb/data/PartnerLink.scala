// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Eq
import cats.syntax.either.*
import lucuma.core.enums.Partner
import lucuma.core.util.Enumerated
import monocle.Iso

/**
 * Embodies the association between a user and a partner, if any.  There are
 * three possible values: `HasPartner` (meaning the user is tied to a particular
 * `Partner`), `HasNonPartner` (the user is explicitly not associated with any
 * `Partner`) and `HasUnspecifiedPartner` (the relationship is not yet
 * determined).
 */
sealed trait PartnerLink extends Product with Serializable {

  import PartnerLink.LinkType

  def linkType: LinkType =
    fold(
      LinkType.HasUnspecifiedPartner,
      LinkType.HasNonPartner,
      _ => LinkType.HasPartner
    )

  /**
   * Converts to an either where Left(true) is `HasNonPartner`, Left(false) is
   * `HasUnspecifiedPartner` and Right(partner) is `HasPartner`.
   */
  def toEither: Either[Boolean, Partner] =
    fold(false.asLeft, true.asLeft, _.asRight)

  /**
   * Creates an Option[Partner] which is Some when this link is `HasPartner`.
   */
  def toOption: Option[Partner] =
    toEither.toOption

  /**
   * True only if HasNonPartner.
   */
  def isNonPartner: Boolean =
    fold(false, true, _ => false)

  /**
   * True if HasPartner or HasNonPartner.
   */
  def isSet: Boolean =
    fold(false, true, _ => true)

  /**
   * True only if HasUnspecifiedPartner.  Same as `!isSet`.
   */
  def isUnspecified: Boolean =
    !isSet

  def fold[A](unspecified: => A, hasNonPartner: => A, hasPartner: Partner => A): A =
    this match {
      case PartnerLink.HasUnspecifiedPartner => unspecified
      case PartnerLink.HasNonPartner      => hasNonPartner
      case PartnerLink.HasPartner(p)      => hasPartner(p)
    }

}

object PartnerLink {

  enum LinkType(val tag: String) derives Enumerated:
    case HasPartner            extends LinkType("has_partner")
    case HasNonPartner         extends LinkType("has_non_partner")
    case HasUnspecifiedPartner extends LinkType("has_unspecified_partner")

  case class HasPartner(partner: Partner) extends PartnerLink
  case object HasNonPartner               extends PartnerLink
  case object HasUnspecifiedPartner       extends PartnerLink

  given Eq[PartnerLink] =
    Eq.by(_.toEither)

  private def fromEither(e: Either[Boolean, Partner]): PartnerLink =
    e.fold(b => if (b) HasNonPartner else HasUnspecifiedPartner, HasPartner.apply)

  /**
   * Iso for an Either where Left(true) is HasNoPartner and Left(false) is
   * HasUnspecifiedPartner.
   */
  val either: Iso[PartnerLink, Either[Boolean, Partner]] =
    Iso[PartnerLink, Either[Boolean, Partner]](_.toEither)(fromEither)

  /**
   * Creates a `PartnerLink` where a None value for `Partner` is interpreted as
   * `HasNonPartner`.
   */
  def asNonPartner(p: Option[Partner]): PartnerLink =
    p.fold(HasNonPartner)(HasPartner.apply)

  def fromLinkType(linkType: LinkType, partner: Option[Partner] = None): Either[String, PartnerLink] =
    linkType match
      case LinkType.HasPartner            => partner.map(HasPartner.apply).toRight("HasPartner instance missing partner")
      case LinkType.HasNonPartner         => HasNonPartner.asRight
      case LinkType.HasUnspecifiedPartner => HasUnspecifiedPartner.asRight

}
