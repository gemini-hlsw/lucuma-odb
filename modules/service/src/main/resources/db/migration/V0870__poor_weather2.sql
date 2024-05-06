-- Continue V0869, which must be in a separate migration so that it is also in
-- a separate transaction that is committed before being used below.
INSERT INTO t_science_subtype VALUES ('poor_weather', 'P', 'Poor Weather');
