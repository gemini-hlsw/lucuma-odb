DROP INDEX unique_program_user;

-- A user cannot appear twice in the same role in the same program.
CREATE UNIQUE INDEX unique_program_user_per_role ON t_program_user (
    c_program_id,
    c_user_id,
    c_role
) WHERE c_user_id IS NOT NULL;

-- A user can only be the pi, coi or coi_ro, never two of these at once.
CREATE UNIQUE INDEX unique_program_user_investigator ON t_program_user (
  c_program_id,
  c_user_id
) WHERE c_user_id IS NOT NULL
    AND c_role IN ('pi', 'coi', 'coi_ro');

-- And there can only be one pi (whether linked to a user or not).
CREATE UNIQUE INDEX unique_program_user_one_pi ON t_program_user (
  c_program_id
) WHERE c_role = 'pi';
