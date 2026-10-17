-- Every index leading with c_program_id is partial (on c_user_id IS NOT NULL
-- or on the role), so the program members lookups, which filter by program and
-- by a role bound as a parameter (c_role = $n or c_role <> $n), scan the whole
-- table.
CREATE INDEX i_program_user_program
  ON t_program_user (c_program_id);
