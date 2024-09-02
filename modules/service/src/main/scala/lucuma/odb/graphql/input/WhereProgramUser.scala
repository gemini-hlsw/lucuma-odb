// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.enums.EducationalStatus
import lucuma.core.enums.ProgramUserRole
import lucuma.odb.graphql.binding.*

object WhereProgramUser {

  def binding(path: Path): Matcher[Predicate] =

    lazy val WhereProgramBinding      = WhereProgram.binding(path / "program")
    val WhereUserBinding              = WhereUser.binding(path / "user")
    val WhereRoleBinding              = WhereEq.binding(path / "role", ProgramUserRoleBinding)
    val WherePartnerLinkBinding       = WherePartnerLink.binding(path)
    val WhereEducationalStatusBinding = WhereOptionEq.binding(path / "educationalStatus", EducationalStatusBinding)
    val WhereThesisBinding            = WhereOptionBoolean.binding(path / "thesis", BooleanBinding)

    lazy val WhereProgramUserBinding = binding(path)
    ObjectFieldsBinding.rmap {
      case List(
        WhereProgramUserBinding.List.Option("AND", rAND),
        WhereProgramUserBinding.List.Option("OR", rOR),
        WhereProgramUserBinding.Option("NOT", rNOT),
        WhereProgramBinding.Option("program", rProgram),
        WhereUserBinding.Option("user", rUser),
        WhereRoleBinding.Option("role", rRole),
        WherePartnerLinkBinding.Option("partnerLink", rPartnerLink),
        WhereEducationalStatusBinding.Option("educationalStatus", rEducationalStatus),
        WhereThesisBinding.Option("thesis", rThesis)
      ) =>
        (rAND, rOR, rNOT, rProgram, rUser, rRole, rPartnerLink, rEducationalStatus, rThesis).parMapN {
          (AND, OR, NOT, program, user, role, partnerLink, educationalStatus, thesis) =>
            and(List(
              AND.map(and),
              OR.map(or),
              NOT.map(Not(_)),
              program,
              user,
              role,
              partnerLink,
              educationalStatus,
              thesis
            ).flatten)
        }
    }
}
