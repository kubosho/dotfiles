import type { Message } from "@earendil-works/pi-ai";
import { expect, test } from "vitest";

import { buildAdvisorRequest } from "../transcript.ts";

const usage = {
  input: 1,
  output: 1,
  cacheRead: 0,
  cacheWrite: 0,
  totalTokens: 2,
  cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, total: 0 },
};

test("advisor request includes the active constraints, conversation, tool activity, and question", () => {
  const messages: Message[] = [
    {
      role: "system",
      content: "You are a coding agent.",
      sections: { project: "Do not edit generated files." },
      timestamp: 1,
    },
    { role: "user", content: "Refactor the parser.", timestamp: 2 },
    {
      role: "assistant",
      content: [
        { type: "text", text: "I will inspect it." },
        { type: "toolCall", id: "call-1", name: "read", arguments: { path: "parser.ts" } },
      ],
      api: "openai-responses",
      provider: "openai",
      model: "gpt-main",
      usage,
      stopReason: "toolUse",
      timestamp: 3,
    },
    {
      role: "toolResult",
      toolCallId: "call-1",
      toolName: "read",
      content: [{ type: "text", text: "export function parse() {}" }],
      isError: false,
      timestamp: 4,
    },
  ];

  const request = buildAdvisorRequest(messages, "Which design should I choose?");

  expect(request).toMatch(/You are a coding agent\./);
  expect(request).toMatch(/Do not edit generated files\./);
  expect(request).toMatch(/Refactor the parser\./);
  expect(request).toMatch(/TOOL CALL read/);
  expect(request).toMatch(/"path": "parser\.ts"/);
  expect(request).toMatch(/TOOL RESULT read \(success\)/);
  expect(request).toMatch(/export function parse\(\) \{\}/);
  expect(request).toMatch(/Which design should I choose\?/);
});

test("image content is represented without copying base64 data", () => {
  const request = buildAdvisorRequest(
    [
      {
        role: "user",
        content: [
          { type: "text", text: "Review this screenshot." },
          { type: "image", data: "sensitive-base64-data", mimeType: "image/png" },
        ],
        timestamp: 1,
      },
    ],
    "What am I missing?",
  );

  expect(request).toMatch(/\[image: image\/png\]/);
  expect(request).not.toMatch(/sensitive-base64-data/);
});
