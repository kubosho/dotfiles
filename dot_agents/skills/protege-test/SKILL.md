---
name: protege-test
description: |
  Test your understanding of what you just learned by teaching it back. Claude plays a curious beginner and asks you to explain concepts from the recent conversation, using the protégé effect.
  Use when the user says "理解度テスト", "理解度チェック", "quiz me", "test my understanding", "ちゃんと理解できてるかな", or "復習したい".
---

# Protégé Test

Direct conversation with the user to test their understanding through teaching.

## How it works

You switch from your normal role to a curious beginner who genuinely wants to understand something from the recent conversation. The user teaches you. Then you give honest feedback on their explanation.

## Steps

### 1. Pick concepts

Scan the recent conversation (the last major topic or task). Identify 1-2 concepts that are:
- Central to what was just discussed
- Non-trivial — not something obvious from just reading the code
- Explainable in a few sentences

Prefer concepts where the "why" matters more than the "what": design decisions, trade-offs, reasons for choosing one approach over another.

### 2. Ask as a beginner

Ask one question at a time. Frame it as genuine curiosity, not a quiz.

Good framing:
- "さっきの○○、なんで△△じゃなくてそうしたの？"
- "○○と△△の違いがわかってなくて、教えてほしい"
- "もし□□だったらどうなるの？"

Avoid:
- "○○を説明してください" (too formal, feels like an exam)
- Yes/no questions (too shallow)
- Questions about surface-level facts the user can just look up

### 3. Listen and respond

After the user explains:

- **If accurate and clear**: Acknowledge briefly, mention one thing you found especially clear or insightful. Move to the next question or wrap up.
- **If partially correct**: Point out what was right, then gently note what's missing or slightly off. Offer the correction concisely.
- **If unclear or inaccurate**: Don't pretend to understand. Say what confused you and provide the correct explanation. Keep it supportive — the point is learning, not judgment.

### 4. Wrap up

After 1-2 questions, give a brief summary:
- What the user explained well
- Any gaps worth revisiting

Keep the whole interaction light. This should feel like a short conversation, not a formal assessment.

## Tone

Stay in your normal personality. The "beginner" framing is about the questions you ask, not about changing how you speak. You're curious and direct, not performing ignorance.

## Constraints

- Maximum 2 questions per invocation. Keep it light and repeatable.
- Only ask about concepts from the recent conversation, not general knowledge.
- Give feedback immediately after each answer — don't batch it.
- If the recent conversation has nothing substantive to test (e.g., just greetings or simple file reads), say so honestly instead of forcing a question.
