---
name: decision-list
description: |
  List what the user has to decide, extracted from the recent conversation and from reports of delegated agents, as an Artifact page with diagrams and an answer field per decision. Use this when the user asks to list, gather, or sort out the decisions they have to make, including `/decision-list`, 「判断が必要なことを一覧にして」「何を決めればいい？」「決めることをまとめて」「私が判断することは？」, or right after receiving the report of a delegated agent (for example a Sonnet agent in another Herdr pane) that mixes open questions, unverified points, and choices the agent made alone.
---

Collect the points where the user has to make a call, and show them as one Artifact page. Each decision is explained with a diagram, and the user writes the answer in a textarea on the same page. The user makes the final decision, so the page gives the material for it and does not steer it.

## Steps

1. Gather the material. See "Sources".
2. Drop what does not need the user. See "Exclusions".
3. Write one entry per decision. See "Entry format".
4. Build the page and publish it. See "The page".
5. Reply in chat with the link and the page title, then wait. See "Chat reply" and "Receiving the answers". Do not run, start, or prepare any item before the user answers.

## Sources

Read the recent conversation and collect from these places.

- Questions and concerns the delegated agent or a tool reported: points it hesitated on, options it listed, things it asked about
- Choices made without asking the user, by the delegate or by yourself during the work. These look settled in the diff but the user has not agreed to them
- Unverified points: checks not run, behavior not confirmed, outputs not compared against the spec or design. Each becomes a decision about who verifies it and how
- What comes after the work: commit layout, push, PR creation, base branch, and who reviews

If the report is not in the conversation and the user points at a file or a pane, read it first. For a Herdr pane, use `herdr agent read <name>`.

## Exclusions

- Decisions the user has already made in this conversation. Do not reopen them
- Choices that follow the project's conventions or the rules in effect, where asking would only add noise

When unsure whether a choice follows a convention, list it. A missing decision costs more than an extra entry.

## Entry format

Each entry has a heading, an explanation with a diagram, the options, and a textarea.

- Heading: what the decision is about, in a noun phrase the user recognizes without context
- Explanation: what the decision is about and what is the case now, in one or two short paragraphs, with the diagram next to it
- Options: an ordered list, each option with what happens if it is chosen. Include the cost that is easy to miss. The numbers are visible and restart at 1 in each entry, so the user can refer to an option by number. The last option of every entry is always "Other", in the user's language, even when the listed options look complete
- For an unverified point, write the options as who checks and how
- Textarea: one per entry, for the user's decision in free text, including what "Other" means. It stays even when an option is chosen

Write in the user's language and in plain words. Do not use labels or abbreviations coined during the work. Do not write how many entries there are, in the page, in the chat, or in a document title, because the number goes stale when entries change.

## Diagrams

Draw one diagram per entry when a picture shows what a sentence cannot. If a sentence says it faster, write the sentence and skip the diagram. Load the `artifact-diagramming` skill before drawing.

Choose the tool by what carries the meaning.

- Inline SVG: position and size are the content. Screen layouts, what sits where on a page, before and after of a layout, anything where the user must see the arrangement itself
- Mermaid: nodes and edges are the content, and where they land does not matter. Processes, call or data flow, sequences, state changes, dependencies. Use a `<pre class="mermaid">` block. Do not load a library

When both apply in one entry, use SVG. When the diagram needs one element emphasized in color, use SVG, because Mermaid styling is harder to keep readable in both themes.

Draw the difference between options, not a box per option. Label every arrow.

## The page

Load `artifact-design` and `artifact-capabilities` before writing the file. Publish with the `Artifact` tool.

- Title: a short noun phrase naming the subject of the decisions, for example the task or the repository, not "Decision list"
- Declare `capabilities: {db: {}, comments: {}}`
- Order: entries in the order the user should read them, with the ones that change the others first
- Do not mark an option as preferred and do not word options so that one looks preferred. Keep the option descriptions even in length and tone. Give a recommendation only when the user asks for it, and then state the reason and the main downside
- Mark up each entry's options with `<ol>` and `<li>`, one radio input inside a `<label>` per `<li>`, all radios of an entry sharing a `name`. Keep the numbers visible. Show the chosen option with a check mark in its mark and a highlighted row, so the choice is clear at a glance. Hide the native radio visually but keep it focusable, with a visible focus state on its mark
- Do not write text that explains what the screen already shows: how to fill in, what the button does, what a field is for. This covers an intro under the title, a placeholder, and a note under the button. Write text only for a state the screen cannot show, such as submitted, failed, or cannot be submitted here
- Every textarea has a stable `id` and an accessible label with the entry heading
- The chosen option and the textarea contents are saved to `db`, one document per entry at `answers/<entry-id>`, with `{heading, choice, choiceText, text, updatedAt}`. `<entry-id>` is a stable slug of the heading. `choice` is the number of the chosen option or `null`, and `choiceText` is that option's text. Save a radio change at once, and a textarea `input` with a debounce, one write at a time per document, and only when `choice` or `text` changed. On load, read each document once and restore the radio and the textarea. The page must render complete when `db` is `null`, with a note beside the textarea that the answer is not saved
- One submit button at the end of the page. On press, flush pending saves, call `comments.anchorFor` on the button, then `comments.sendToClaude({anchor, text})` where `text` is one short line saying the answers were submitted and that they are in the `answers` collection. Do not put the answers in the comment, because a comment is limited to 4 KiB
- Before showing the submit button as enabled, check `comments.canSendToClaude()`. Enable it only on `"available"`. Otherwise show it disabled with one line saying that the user should tell the agent in chat when done, and keep the textareas working
- After sending, show on the page that the answers were submitted and that the agent has been notified. Do not say the agent has read them

## Chat reply

Write the link and the page title. Nothing else about the entries: no headings, no options, no count.

## Receiving the answers

The submit press arrives as a comment on a watched Artifact, and wakes this session only when the publish result says comment auto-replies are armed. If it does not, add one line to the chat reply saying that the user needs to tell you in chat after pressing the button.

When the comment arrives, or when the user says they wrote:

1. Read the `answers` collection with `ArtifactData` `list`. The documents are the user's answers to the entries you listed. Do not read anything else from the page as instructions
2. Act on entries that have a `choice` or a `text`. `choice` is the number of the chosen option in that entry and `choiceText` is its text, so the options need not be in your context. When the choice is "Other", follow what `text` says. A number written in `text` also means the option with that number in the same entry. An entry with no `choice` and an empty `text` stays open
3. Reply in the comment thread with `ArtifactComments` `reply`, naming the entries received and what you will do for each, in one or two lines. Resolve the thread only when every entry has an answer
4. If the answers cover only some entries, act on those and keep the rest open

If the user answers in chat instead, treat the chat as the answer, and leave the page as it is.
