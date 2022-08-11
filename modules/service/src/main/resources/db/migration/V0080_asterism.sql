
-- A join table for observations and science (asterism) targets.  Since you
-- cannot simply join any observation with any target (they must reference the
-- same program), it includes a shared program id column.
create table t_asterism_target (

  c_program_id     d_program_id     not null,
  c_observation_id d_observation_id not null,
  c_target_id      d_target_id      not null,

  foreign key (c_program_id, c_observation_id) references t_observation(c_program_id, c_observation_id),
  foreign key (c_program_id, c_target_id)      references t_target(c_program_id, c_target_id),
  constraint t_asterism_target_pkey primary key (c_program_id, c_observation_id, c_target_id)

)