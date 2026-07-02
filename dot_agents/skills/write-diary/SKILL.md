---
name: write-diary
description: Append a work observation entry to today's AI diary file. Use at the end of any session where files were edited or commands were run.
---

1. Read `$XDG_DATA_HOME/obsidian/personal/90_Journal/AI_diary/_template.md` for the entry format and intro text.
2. Open `$XDG_DATA_HOME/obsidian/personal/90_Journal/AI_diary/YYYY-MM-DD.md` using today's date. If it does not exist, create it with the header and intro paragraph from the template.
3. Append one entry section. Fill each field from this session's context. Set `人間レビュー: 未` for any 明文化候補.
