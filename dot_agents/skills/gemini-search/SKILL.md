---
name: gemini-search
description: |
  Gemini CLI の google_web_search ツールを使ってWeb検索を行う。
  Triggers: "検索して", "調べて", "ググって", "web search", "search for"
  Use cases: (1) 最新情報の取得, (2) 技術ドキュメントの調査, (3) エラーメッセージの検索, (4) ライブラリ・ツールの比較調査
argument-hint: "<search query>"
---

# Gemini Web Search Skill

Use Gemini CLI's `google_web_search` tool to search the web and get summarized results with citations.

## Execution

Run Gemini CLI in non-interactive mode with the google_web_search tool:

```bash
gemini -p "google_web_search ツールを使って次のクエリをWeb検索し、結果を日本語で要約してください: $ARGUMENTS" --allowed-tools google_web_search -o text
```

## Rules

1. Always use `-p` for non-interactive (headless) execution
2. Always use `--allowed-tools google_web_search` to restrict to the web search tool only
3. Always use `-o text` for plain text output
4. Display the full Gemini output to the user without summarizing or filtering
5. If the user provides a specific topic or question, include it verbatim in the search query
6. For English technical terms, keep them in English within the search query for better results
