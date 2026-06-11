---
name: herdr
description: "Control herdr from inside it. Manage workspaces, tabs, and panes, split panes, spawn agents, read other panes, and wait for output or agent status via the `herdr` CLI. Use whenever HERDR_ENV=1 and the task needs another pane: running a server or tests beside you, watching logs, coordinating with or spawning other agents."
---

Check that `HERDR_ENV=1`. If it is not, you are not inside a herdr-managed pane. Say so and stop.

You are one pane inside herdr, a terminal multiplexer for agents. Structure: workspace (project context) > tab (subcontext) > pane (one terminal process). Discover your surroundings with `herdr pane list` (the focused pane is you). All command syntax is self-documented: run `herdr pane --help`, `herdr tab --help`, `herdr workspace --help`, `herdr wait --help`. Raw protocol reference: https://herdr.dev/docs/socket-api/

Things `--help` will not tell you:

- Treat ids as opaque and non-durable. They can change when tabs, panes, or workspaces close. Never trust a remembered id. Re-read from `pane list` / `tab list` / `workspace list`, or parse the JSON that create/split commands print: `result.pane.pane_id` (pane split), `result.tab` and `result.root_pane` (tab create), `result.workspace`, `result.tab`, `result.root_pane` (workspace create).
- Panes expose `agent_status`: `idle`, `working`, `blocked`, `done`, `unknown`. `done` means the agent finished but that pane has not been looked at yet.
- `pane read` is for output that already exists. `wait output` is for output you expect next. `wait output --source recent` matches unwrapped text (soft wraps joined), so pane width never breaks a match. To inspect exactly what the waiter saw, use `pane read --source recent-unwrapped`.
- `pane run` sends text plus a real Enter in one request. `send-text` / `send-keys` are the split-apart versions.
- `--no-focus` on split / tab create / workspace create keeps your own pane focused. Use it by default.
- `wait` exits 1 on timeout. `pane read` prints text, everything else that succeeds prints JSON (mutating sends print nothing).

The core pattern. Servers, tests, log watching, and agent spawning are all this shape:

```bash
NEW=$(herdr pane split --current --direction right --no-focus | python3 -c 'import sys,json; print(json.load(sys.stdin)["result"]["pane"]["pane_id"])')
herdr pane run "$NEW" "npm run dev"
herdr wait output "$NEW" --match "ready" --timeout 30000
herdr pane read "$NEW" --source recent --lines 20
```

To spawn another agent: split, `pane run` the agent command, `wait output` for its prompt, then `pane run` the task. To coordinate with one: `herdr wait agent-status <pane> --status done --timeout 120000`, then `pane read` its result.
