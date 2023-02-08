
-- A view that contains every pair of target ids. When joined to the target table
-- with conditions on the ids it all compiles down to hash lookups, which are fast.

CREATE OR REPLACE VIEW v_target_pairs AS
  SELECT
    a.c_target_id c_left,
    b.c_target_id c_right
  FROM
    t_target a,
    t_target b
    
