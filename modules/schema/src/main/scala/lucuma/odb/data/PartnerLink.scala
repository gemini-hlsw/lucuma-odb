// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Eq
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.option.*
import lucuma.core.enums.Partner
import lucuma.core.util.Enumerated
import monocle.Iso


sealed trait PartnerLink extends Product with Serializable {

  import PartnerLink.LinkType

  def linkType: LinkType =
    fold(
      LinkType.UnspecifiedPartner,
      LinkType.HasNoPartner,
      _ => LinkType.HasPartner
    )

  def toEither: Either[Boolean, Partner] =
    this match {
      case PartnerLink.HasPartner(p) => p.asRight
      case PartnerLink.HasNonPartner => true.asLeft
      case PartnerLink.NoPartnerLink => false.asLeft
    }

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
   * True only if UnspecifiedPartner.  Same as `!isSet`.
   */
  def isUnspecified: Boolean =
    !isSet

  def fold[A](unspecified: => A, hasNonPartner: => A, hasPartner: Partner => A): A =
    this match {
      case PartnerLink.UnspecifiedPartner => unspecified
      case PartnerLink.HasNonPartner      => hasNonPartner
      case PartnerLink.HasPartner(p)      => hasPartner(p)
    }

}

object PartnerLink {

  enum LinkType(val tag: String) derives Enumerated:
    case HasPartner         extends LinkType("has_partner"),
    case HasNoPartner       extends LinkType("has_no_partner"),
    case UnspecifiedPartner extends LinkType("unspecified_partner")

  case class HasPartner(partner: Partner) extends PartnerLink
  case object HasNonPartner               extends PartnerLink
  case object UnspecifiedPartner          extends PartnerLink

  given Eq[PartnerLink] =
    Eq.by(_.toEither)

  private def fromEither(e: Either[Boolean, Partner]): PartnerLink =
    e.fold(b => if (b) HasNonPartner else NoPartnerLink, HasPartner.apply)

  /**
   * Iso for an Either where Left(true) is HasNoPartner and Left(false) is
   * UnspecifiedPartner.
   */
  val either: Iso[PartnerLink, Either[Boolean, Partner]] =
    Iso[PartnerLink, Either[Boolean, Partner]](_.toEither, fromEither)

  /**
   * Creates a `PartnerLink` where a None value for `Partner` is interpreted as
   * `HasNonPartner`.
   */
  def asNonPartner(p: Option[Partner]): PartnerLink =
    p.fold(HasNonPartner)(HasPartner.apply)

}
