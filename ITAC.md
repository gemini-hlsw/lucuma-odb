

Much of this is undefined, so let's define it.

At the end of Phase 1 all the observations we're concerned with are in the `Unapproved` state. We know their coordinates and estimated times, and we can estimate the expected time per band proportionally. For OR groups we just divide everything by the size of the group.

We can fill RA, Dec, and conditions buckets and report overages, showing all the proposals/observations in each bucket. It would be nice if we could do this without buckets. Can we do this with 1% buckets that flow upward as they fill?


We can likewise look for duplicate targets and report on these.

Other reports should be straightforward.

We also know the amount of time allocated per band, per partner, and can tell whether they're over or under, and by how much.

- We need to change the workflow state such that the science band cannot be set until the observation is `Defined`, but must be set before it can transition to `Ready`. Does this mean we need to add a new state?
- We need to ensure, when setting the science band, that there is enough remaining time in that band.

---

Ok we may be able to use existing code by placing every proposal in every queue band, sliced according to the allocated time per band.