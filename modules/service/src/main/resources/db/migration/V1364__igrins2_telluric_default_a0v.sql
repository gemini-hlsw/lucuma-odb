-- New IGRINS-2 observations default to an A0V telluric.
-- Existing observations will not be migrated.
ALTER TABLE t_igrins_2_long_slit
ALTER COLUMN c_telluric_type SET DEFAULT '{"tag":"A0V","starTypes":null,"count":null}'::jsonb;
