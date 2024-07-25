// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.syntax

import lucuma.core.enums.Partner
import lucuma.odb.data.TimeAccountingCategory

trait ToPartnerOps {

  extension (self: Partner)

    def timeAccountingCategory: TimeAccountingCategory =
      self match {
        case Partner.AR => TimeAccountingCategory.AR
        case Partner.BR => TimeAccountingCategory.BR
        case Partner.CA => TimeAccountingCategory.CA
        case Partner.CL => TimeAccountingCategory.CL
        case Partner.KR => TimeAccountingCategory.KR
        case Partner.UH => TimeAccountingCategory.UH
        case Partner.US => TimeAccountingCategory.US
      }

}

object partner extends ToPartnerOps