// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.enums.EducationalStatus
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.ProgramUser
import lucuma.odb.graphql.binding.*

object WhereProgramUser {

  /**
   * @param path path to program user
   * @param onlyRole set to restrict to a particular role even if the "role"
   *                 field is not set
   */
  def binding(path: Path, onlyRole: Option[ProgramUserRole] = None): Matcher[Predicate] =

    val WhereOrderProgramUserId       = WhereOrder.binding[ProgramUser.Id](path / "id", ProgramUserIdBinding)
    lazy val WhereProgramBinding      = WhereProgram.binding(path / "program")
    val WhereUserBinding              = WhereUser.binding(path / "user")
    val WhereRoleBinding              = WhereEq.binding(path / "role", ProgramUserRoleBinding)
    val WherePartnerLinkBinding       = WherePartnerLink.binding(path)
    val WherePreferredProfileBinding   = WhereUserProfile.binding(path / "preferredProfile")
    val WhereEducationalStatusBinding = WhereOptionEq.binding(path / "educationalStatus", EducationalStatusBinding)
    val WhereThesisBinding            = WhereOptionBoolean.binding(path / "thesis", BooleanBinding)
    val WhereGenderBinding            = WhereOptionEq.binding(path / "gender", GenderBinding)
    val WhereHasDataAccessBinding     = WhereBoolean.binding(path / "hasDataAccess", BooleanBinding)

    lazy val WhereProgramUserBinding = binding(path, onlyRole)
    ObjectFieldsBinding.rmap {
      case List(
        WhereProgramUserBinding.List.Option("AND", rAND),
        WhereProgramUserBinding.List.Option("OR", rOR),
        WhereProgramUserBinding.Option("NOT", rNOT),
        WhereOrderProgramUserId.Option("id", rId),
        WhereProgramBinding.Option("program", rProgram),
        WhereUserBinding.Option("user", rUser),
        WhereRoleBinding.Option("role", rRole),
        WherePartnerLinkBinding.Option("partnerLink", rPartnerLink),
        WherePreferredProfileBinding.Option("preferredProfile", rPreferredProfile),
        WhereEducationalStatusBinding.Option("educationalStatus", rEducationalStatus),
        WhereThesisBinding.Option("thesis", rThesis),
        WhereGenderBinding.Option("gender", rGender),
        WhereHasDataAccessBinding.Option("hasDataAccess", rDataAccess)
      ) =>
        (rAND, rOR, rNOT, rId, rProgram, rUser, rRole, rPartnerLink, rPreferredProfile, rEducationalStatus, rThesis, rGender, rDataAccess).parMapN {
          (AND, OR, NOT, id, program, user, role, partnerLink, preferredProfile, educationalStatus, thesis, gender, hasDataAccess) =>
            and(List(
              AND.map(and),
              OR.map(or),
              NOT.map(Not(_)),
              id,
              program,
              user,
              role,
              onlyRole.map(r => Eql(path / "role", Const(r))),
              partnerLink,
              preferredProfile,
              educationalStatus,
              thesis,
              gender,
              hasDataAccess
            ).flatten)
        }
    }
}
