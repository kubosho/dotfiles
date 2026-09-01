---
name: write-diary
description: Append a work observation entry to today's AI diary file. Use at the end of any session where files were edited or commands were run.
---

1. Read `$XDG_DATA_HOME/obsidian/personal/00_Templates/AI作業日報.md` for the entry format and intro text.
2. Open `$XDG_DATA_HOME/obsidian/personal/90_Journal/AI_diary/YYYY-MM-DD.md` using today's date. If it does not exist, create it with the header and intro paragraph from the template.
3. List the distinct requests handled this session, even if they happened back-to-back in one session. Append one entry section per distinct request, not one section for the whole session. Fill each field from that request's own context.
   - Write each entry so it stands on its own for a reader who did not see the session. Do not reenact the conversation (e.g. "there was a question about...", "the user pointed out that..."). State what was requested, done, and decided as facts, not as a transcript of exchanges.
   - Cover the request by making sure no material point is missing, not by maximizing word count. When multiple observations belong to the same point, merge them into one statement inside the relevant field instead of listing each separately.
   - Omit details that carry little weight for the claims and decisions in the entry. Length is not a goal. Do not copy the work log verbatim into the entry.
4. Do not edit the `# 日次掘り下げ` section at the end of the file. That section is out of scope for this skill.
