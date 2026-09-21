import type { Message } from "@earendil-works/pi-ai";
import {
  buildSessionContext,
  convertToLlm,
  type ExtensionAPI,
} from "@earendil-works/pi-coding-agent";
import { Type } from "typebox";

import { assistantText, buildAdvisorRequest } from "./transcript.ts";

const ADVISOR_PROVIDER = "openai-codex";
const ADVISOR_MODEL = "gpt-6-astra";

export default function (pi: ExtensionAPI) {
  pi.registerTool({
    name: "advisor",
    label: "Advisor",
    description:
      "Consult gpt-6-astra for an independent review of a hard decision. The advisor receives the full active conversation, including tool calls and results, and returns guidance only.",
    promptSnippet: "Escalate hard decisions to gpt-6-astra for an independent review",
    promptGuidelines: [
      "Use advisor before committing to an approach in a long, ambiguous, high-impact task; when repeated attempts fail; or before declaring substantial work complete.",
      "Do not use advisor for short or routine tasks, and do not call it repeatedly without new evidence.",
      "Treat advisor guidance as a second opinion: verify its claims against repository evidence and surface any conflict.",
    ],
    parameters: Type.Object({
      question: Type.String({
        description:
          "The specific decision, failure, or completion concern to review. Include competing options or uncertainty.",
      }),
    }),

    async execute(_toolCallId, params, signal, onUpdate, ctx) {
      const model = ctx.modelRegistry.find(ADVISOR_PROVIDER, ADVISOR_MODEL);
      if (!model) {
        throw new Error(`Advisor model is unavailable: ${ADVISOR_PROVIDER}/${ADVISOR_MODEL}`);
      }

      const session = buildSessionContext(
        ctx.sessionManager.getEntries(),
        ctx.sessionManager.getLeafId(),
      );
      const messages = convertToLlm(session.messages);
      const request = buildAdvisorRequest(messages, params.question);
      const advisorContext: { messages: Message[] } = {
        messages: [{ role: "user", content: request, timestamp: Date.now() }],
      };
      const stream = ctx.modelRegistry.streamSimple(model, advisorContext, {
        reasoning: "high",
        toolChoice: "none",
        signal,
      });

      let streamedText = "";
      for await (const event of stream) {
        if (event.type !== "text_delta") continue;
        streamedText += event.delta;
        onUpdate?.({
          content: [{ type: "text", text: streamedText }],
          details: { model: `${ADVISOR_PROVIDER}/${ADVISOR_MODEL}`, question: params.question },
        });
      }

      const response = await stream.result();
      if (response.stopReason === "error" || response.stopReason === "aborted") {
        throw new Error(response.errorMessage ?? `Advisor request ${response.stopReason}`);
      }

      const guidance = assistantText(response.content);
      if (!guidance) throw new Error("Advisor returned no guidance");

      return {
        content: [{ type: "text", text: guidance }],
        details: { model: `${ADVISOR_PROVIDER}/${ADVISOR_MODEL}`, question: params.question },
        usage: response.usage,
      };
    },
  });
}
