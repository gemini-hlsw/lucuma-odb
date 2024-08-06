// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Eq
import cats.syntax.option.*
import lucuma.core.enums.Partner


sealed trait PartnerAssociation extends Product with Serializable {

  def isSet: Boolean

  def partnerOption: Option[Partner]

}

object PartnerAssociation {

  case class HasPartner(p: Partner) extends PartnerAssociation {
    override def isSet: Boolean = true
    override def partnerOption: Option[Partner] = p.some
  }

  case object HasNoPartner extends PartnerAssociation {
    override def isSet: Boolean = true
    override def partnerOption: Option[Partner] = none
  }

  case object NoAssociation extends PartnerAssociation {
    override def isSet: Boolean = false
    override def partnerOption: Option[Partner] = none
  }

  given Eq[PartnerAssociation] =
    Eq.by { a => (
      a.isSet,
      a.partnerOption
    )}

  def fromFields(isSet: Boolean, partner: Option[Partner]): PartnerAssociation =
    if (isSet) partner.fold(NoAssociation)(HasPartner.apply)
    else NoAssociation

}
