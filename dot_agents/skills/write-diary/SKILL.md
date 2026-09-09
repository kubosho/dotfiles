---
name: write-diary
description: Append a work observation entry to today's AI diary file only when explicitly requested by the user.
---

1. Use vault `${XDG_DATA_HOME:-$HOME/.local/share}/obsidian/personal`. Read `00_Templates/AI作業日報.md` for the format and intro.
2. Open `90_Journal/AI_diary/YYYY-MM-DD.md` for today in the user's local timezone. If missing, create it with the template's header and intro, without unfilled placeholders.
3. Add a timestamped session entry in chronological order. Organize by work topic, not request order. Split only unrelated work that cannot share a title.
   - State requests, actions, and decisions without relying on conversation context. Name relevant tools, skills, files, or components and clarify their roles or relationships as needed. For errors, identify the attempted operation, failure, and unverified outcome.
   - Cover material points, merge related observations, and omit irrelevant details. Separate facts from interpretations and completed work from proposals, unverified outcomes, and remaining work. Support claims with file paths, commands and results, or artifact links.
   - Omit inapplicable optional sections. Include 仕組み化の候補 only with session evidence, following the template's Hooks / Skills / AGENTS.md categories and metadata.
4. Do not add daily reflections or generate reports. Cross-entry evaluation and prioritization belong to `diary-report`, which writes a separate HTML file for a requested period. Recording proposals does not mean human review or acceptance.
