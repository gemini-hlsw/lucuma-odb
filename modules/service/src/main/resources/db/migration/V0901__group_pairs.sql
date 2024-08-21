
-- A view that contains every pair of group ids.

CREATE OR REPLACE VIEW v_group_pairs AS
  SELECT
    a.c_group_id c_left,
    b.c_group_id c_right
  FROM
    t_group a,
    t_group b
    
