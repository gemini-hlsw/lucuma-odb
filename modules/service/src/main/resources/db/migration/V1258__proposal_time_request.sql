-- The observing time a proposal asks for.
--
-- Until now this was derived and nothing else: the sum of the time estimates of
-- the program's observations, folded up the group tree in TimeEstimateService.
-- An observation without an estimate contributes nothing to that sum, which is
-- fine while every observation is defined and wrong as soon as one isn't.  A
-- Target of Opportunity proposal is the case that forces the issue -- its
-- targets are unknown at submission time, so the observations that will consume
-- most of its time cannot be written down yet, and the derived sum understates
-- the real ask.
--
-- So the request becomes explicit-or-derived:
--
--   NULL     -> not chosen; the sum over the program's observations stands
--   non-NULL -> chosen, by the PI before submission or by staff during review
--
-- Unlike the ToO activation ceiling of V1245, this is not frozen on acceptance.
-- The ceiling is *enforced* against observations at execution time, so leaving
-- it derived afterwards would let a PI raise their own ceiling; the request
-- authorizes nothing (allocations do that), so there is nothing to freeze.
--
-- The column applies to every kind of proposal, so no proposal-type check
-- changes.  Note it is a different quantity from c_total_time, which is the
-- multi-semester total of a Large Program.
ALTER TABLE t_proposal
  ADD COLUMN c_time_request interval NULL;

-- v_proposal as in V1245, plus a synthetic key for the explicit request.  The
-- view has to be recreated in any case: `SELECT p.*` is expanded when the view
-- is created, so an existing view never picks up a column added afterwards.
DROP VIEW v_proposal;

CREATE VIEW v_proposal AS
  SELECT
    p.*,
    -- Explicit / default / effective.  Unlike the observation's execution
    -- requirement, this is a plain COALESCE: an explicit ceiling replaces the
    -- derived one outright rather than acting as a floor beneath it.  Note the
    -- default keeps tracking the observations even after acceptance, when it no
    -- longer has any effect -- see the freeze in ProposalService.
    too_activation_ceiling_default(p.c_program_id, p.c_observatory, p.c_science_subtype)
                                                                             AS c_too_activation_default,
    COALESCE(
      p.c_too_activation,
      too_activation_ceiling_default(p.c_program_id, p.c_observatory, p.c_science_subtype)
    )                                                                        AS c_too_activation_effective,
    -- Key for the nullable explicit time request: null when no request was
    -- stated, so the GraphQL object is null rather than a zero TimeSpan.
    CASE WHEN p.c_time_request IS NOT NULL              THEN c_program_id END AS c_time_request_id,
    CASE WHEN p.c_observatory = 'gemini'               THEN c_program_id END AS c_program_id_gemini,
    CASE WHEN p.c_observatory = 'keck'                 THEN c_program_id END AS c_program_id_keck,
    CASE WHEN p.c_observatory = 'subaru'               THEN c_program_id END AS c_program_id_subaru,
    -- Non-null discriminator for the GeminiProposalType interface mapping.  Only
    -- meaningful for Gemini proposals; others get a placeholder that is never
    -- rendered (their c_program_id_gemini key is null).
    COALESCE(p.c_science_subtype, 'queue')                                   AS c_gemini_science_subtype,
    CASE WHEN p.c_science_subtype = 'classical'           THEN c_program_id END AS c_program_id_c,
    CASE WHEN p.c_science_subtype = 'demo_science'        THEN c_program_id END AS c_program_id_s,
    CASE WHEN p.c_science_subtype = 'directors_time'      THEN c_program_id END AS c_program_id_d,
    CASE WHEN p.c_science_subtype = 'fast_turnaround'     THEN c_program_id END AS c_program_id_f,
    CASE WHEN p.c_science_subtype = 'large_program'       THEN c_program_id END AS c_program_id_l,
    CASE WHEN p.c_science_subtype = 'poor_weather'        THEN c_program_id END AS c_program_id_p,
    CASE WHEN p.c_science_subtype = 'queue'               THEN c_program_id END AS c_program_id_q,
    CASE WHEN p.c_science_subtype = 'system_verification' THEN c_program_id END AS c_program_id_v
  FROM
    t_proposal p;
