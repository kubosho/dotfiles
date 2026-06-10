---
name: spec-implement
description: Implement one acceptance criterion from a docs/specs spec as a transcription of the settled design. Use when starting implementation work on a spec, or when asked to implement an AC.
argument-hint: "<docs/specs/spec-file.md> [AC-N]"
---

Implement exactly one AC from the given spec.

If no spec path is provided, ask for it. If no AC is specified, pick the first incomplete AC whose Depends On entries are all done.

1. Read the entire spec, not just the target AC. Mark the AC as 進行中 in the task list.
2. Collect transcription context before writing anything: find existing code closest to what this AC touches (same layer, similar feature), and note its naming, error handling, and test style. New code follows these observed patterns.

   If an observed pattern appears inferior to a general best practice, present a discussion draft BEFORE starting implementation:

   - 観測パターン：file:line と要約
   - 一般論：代替案と、それが優れるとされる根拠
   - 具体的な不利益：このコードベースで実際に起きている、または起きうる問題。挙げられない場合は好みの差なので、提示せず観測パターンに従う
   - 影響範囲：一般論に切り替えた場合に揃え直しが必要な箇所の見積もり
   - 推奨：観測パターンに従う / このACから一般論を採用 / 別specとして移行を起こす

   Wait for the user's decision. If the user defers, follow the observed pattern for this AC.
3. Write a failing test transcribed directly from the AC's 入力 → 結果 line.
4. Implement in one pass. If a decision the spec does not determine arises mid-implementation, stop. Report it as a spec defect and return to spec-authoring. Do not resolve it inline and keep typing.
5. Run the narrowest relevant test until green.
6. Run spec-implementation-check and follow its judgement.
