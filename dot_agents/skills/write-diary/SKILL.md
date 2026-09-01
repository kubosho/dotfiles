---
name: write-diary
description: Append a work observation entry to today's AI diary file. Use at the end of any session where files were edited or commands were run.
---

1. Read `$XDG_DATA_HOME/obsidian/personal/00_Templates/AI作業日報.md` for the entry format and intro text.
2. Open `$XDG_DATA_HOME/obsidian/personal/90_Journal/AI_diary/YYYY-MM-DD.md` using today's date. If it does not exist, create it with the header and intro paragraph from the template.
3. Append one entry that summarizes the session, organized around the main points of the work rather than the order of requests. Split into multiple entries only when the session covered unrelated work that does not fit under one title.
   - Write the entry to stand on its own: state what was requested, done, and decided as facts, not as a transcript of the conversation.
   - Cover every material point, merge observations that belong to the same point, and omit details that do not affect the claims.
4. Do not edit the `# 日次掘り下げ` section at the end of the file. That section is out of scope for this skill.
