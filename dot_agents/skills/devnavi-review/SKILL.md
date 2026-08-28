---
name: devnavi-review
description: |
  Reviews code the user just finished writing in the current session, for missing considerations that could cause security, privacy, performance, or edge-case bugs. Leaves each finding as an inline code comment naming what breaks and what the user has to decide or check, then follows up based on their response.
---

Review the diff from the current session for these omissions. Only ones with plausible real-world impact, not every nitpick.

Write each comment as a pair flagging a risk on the same screen, not a grader marking a submission. No scores, no praise, no verdicts on overall quality.

Each finding is a code comment at the relevant line, in two lines separated by a blank line: what breaks, then what to decide or check. A question about intent alone leaves the next move unstated, and the review stalls there.

    # REVIEW:
    # `items` can be empty here, so `items[0]` is undefined. Intentional?

    # Decide:
    # reject empty `items` up front, or let the caller handle undefined?

Line 1 names the input or state that triggers it and what happens, so the claim can be checked against the code. Not the abstract form:

    # Is it intentional that this doesn't handle the edge case here?

Line 2 names a decision or a fact to check, never the fix. "Decide: A or B?" or "Check: does any caller pass an empty list?"

Do not use an external diff viewer such as difit. Write comments directly into the code.

The user responds in one of two ways:

- Replies in the thread or in chat. If they can state the tradeoff in their own words, accept it and remove the comment. If the explanation is thin, ask one follow-up on the specific gap, at most one per finding.
- Removes the comment and changes the code instead. If the new code addresses the concern, accept it. If not, raise the finding again.
