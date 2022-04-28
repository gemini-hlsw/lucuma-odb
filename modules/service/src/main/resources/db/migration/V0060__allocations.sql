
-- time allocations
create table t_allocation (
  c_program_id d_program_id  not null references t_program(c_program_id),
  c_partner    d_tag         not null references t_partner(c_tag),
  c_duration   interval      not null,
  unique(c_program_id, c_partner)
)

