-- Part 2 of 3 (see V1325, V1327).  Performs, once and retroactively, the swap
-- that the new design asks a PI to make: every opportunity target that had been
-- resolved becomes the ordinary target it resolved to.
--
-- Under V1258 a resolved Target of Opportunity kept c_type = 'opportunity' and
-- carried both its region and the coordinates it resolved to.  With resolution
-- gone there is no such state: an opportunity target is a placeholder with no
-- coordinates at all, and V1327 makes that structural by deriving the tracking
-- type from c_type alone.  Left alone, these rows would then read as opportunity
-- targets holding sidereal coordinates and fail sidereal_or_all_columns_null.
--
-- So they are converted rather than emptied.  Someone identified these objects;
-- discarding the identification to leave a placeholder behind would throw away
-- the more valuable half.  The region goes instead, which it must -- the region
-- columns belong to opportunity targets and only to them.
--
-- What is lost is the record that this target was ever a Target of Opportunity,
-- and the patch of sky it was approved for.  Neither is load-bearing any more.
-- Whether the *observation* is a ToO is recorded on the observation, where V1325
-- froze it, and approval lives in t_configuration_request independently of any
-- target -- a stored region there still covers these coordinates exactly as it
-- did before.
--
-- This is data, not schema, so it stands alone: the UPDATE queues deferred
-- trigger events and no ALTER may follow it in the same transaction.  It runs
-- after V1325 deliberately.  While the activation was still generated, dropping
-- an observation's last opportunity target drove its activation to 'none' and
-- withdrew its trigger -- which would have quietly demoted exactly the
-- observations that had done everything right.  With the activation frozen as a
-- plain column, converting a target leaves it untouched, and the repredicated
-- trigger from V1325 sees a Ready ToO that now has somewhere to point.

UPDATE t_target
   SET c_type = CASE c_resolved_type
                  WHEN 'sidereal'    THEN 'sidereal'::e_target_type
                  WHEN 'nonsidereal' THEN 'nonsidereal'::e_target_type
                END,
       c_resolved_type    = NULL,
       c_opp_ra_arc_type  = NULL,
       c_opp_ra_arc_start = NULL,
       c_opp_ra_arc_end   = NULL,
       c_opp_dec_arc_type  = NULL,
       c_opp_dec_arc_start = NULL,
       c_opp_dec_arc_end   = NULL
 WHERE c_type = 'opportunity'
   AND c_resolved_type IS NOT NULL;
