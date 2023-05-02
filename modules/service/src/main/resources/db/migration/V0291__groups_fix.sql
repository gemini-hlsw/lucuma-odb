-- Renumber observations, starting at zero this time!
with helper as (
  select
    c_program_id p_id,
    c_observation_id o_id,
    -- row_number() starts at 1, not zero!
    row_number() over (partition by c_program_id order by c_observation_id) - 1 idx
  from t_observation
  order by c_program_id
)
update t_observation
set c_group_index = helper.idx
from helper
where c_program_id = helper.p_id and c_observation_id = helper.o_id;

