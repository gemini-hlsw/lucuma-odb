--------------------------------------------------------
-- From V1042
-- This set the c_blind_offset_type to MANUAL when a blind offset target is deleted, which we don't
-- want because sometimes we delete the blind offset target to initialize automatic blind offsetting.
--------------------------------------------------------
DROP FUNCTION IF EXISTS blind_offset_target_delete() CASCADE;
