---
name: devnavi-review
description: |
  Reviews code the user just finished writing in the current session, for missing considerations that could cause security, privacy, performance, or edge-case bugs. Leaves each finding as an inline code comment asking whether the omission was intentional, then follows up based on the user's response.
---

Review the diff from the current session for these omissions. Do not list every possible nitpick, only ones with plausible real-world impact.

For each finding, add a code comment at the relevant line, phrased as a question about intent, for example:

    # Is it intentional that this doesn't handle [edge case]?

Do not use an external diff viewer such as difit. Write comments directly into the code.

The user responds in one of two ways:

- They reply in the comment thread or in chat. Check whether they can state the tradeoff in their own words. If they can, accept it as fine for this context and remove the comment. If the explanation is thin or missing, ask one follow-up question that pushes on the specific gap, at most one per finding.
- They remove the comment and change the code directly instead of replying. Check whether the new code addresses the concern. If it does, accept the removal. If it doesn't, raise the same finding again.
