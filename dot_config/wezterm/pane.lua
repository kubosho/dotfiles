local wezterm = require "wezterm"
local act = wezterm.action

local M = {}

function M.get_keys()
  return {
    -- Split pane horizontally (tmux.conf: bind |)
    {
      key = "|",
      mods = "LEADER|SHIFT",
      action = act.SplitHorizontal { domain = "CurrentPaneDomain" },
    },

    -- Split pane vertically (tmux.conf: bind -)
    {
      key = "-",
      mods = "LEADER",
      action = act.SplitVertical { domain = "CurrentPaneDomain" },
    },

    -- Navigate panes (tmux: Ctrl-b + arrow keys)
    {
      key = "LeftArrow",
      mods = "LEADER",
      action = act.ActivatePaneDirection "Left",
    },
    {
      key = "RightArrow",
      mods = "LEADER",
      action = act.ActivatePaneDirection "Right",
    },
    {
      key = "UpArrow",
      mods = "LEADER",
      action = act.ActivatePaneDirection "Up",
    },
    {
      key = "DownArrow",
      mods = "LEADER",
      action = act.ActivatePaneDirection "Down",
    },

    -- Navigate panes with hjkl (vim-style)
    {
      key = "h",
      mods = "LEADER",
      action = act.ActivatePaneDirection "Left",
    },
    {
      key = "l",
      mods = "LEADER",
      action = act.ActivatePaneDirection "Right",
    },
    {
      key = "k",
      mods = "LEADER",
      action = act.ActivatePaneDirection "Up",
    },
    {
      key = "j",
      mods = "LEADER",
      action = act.ActivatePaneDirection "Down",
    },

    -- Close current pane (tmux: Ctrl-b x)
    {
      key = "x",
      mods = "LEADER",
      action = act.CloseCurrentPane { confirm = true },
    },

    -- Toggle pane zoom (tmux: Ctrl-b z)
    {
      key = "z",
      mods = "LEADER",
      action = act.TogglePaneZoomState,
    },

    -- Cycle through panes (tmux: Ctrl-b o)
    {
      key = "o",
      mods = "LEADER",
      action = act.ActivatePaneDirection "Next",
    },

    -- Switch to last active pane (tmux: Ctrl-b ;)
    {
      key = ";",
      mods = "LEADER",
      action = act.ActivatePaneDirection "Prev",
    },

    -- Window navigation (tmux.conf: bind -r C-h/C-l)
    {
      key = "h",
      mods = "LEADER|CTRL",
      action = act.ActivateTabRelative(-1),
    },
    {
      key = "l",
      mods = "LEADER|CTRL",
      action = act.ActivateTabRelative(1),
    },

    -- Resize panes (tmux.conf: bind -r H/J/K/L)
    {
      key = "H",
      mods = "LEADER|SHIFT",
      action = act.AdjustPaneSize { "Left", 5 },
    },
    {
      key = "J",
      mods = "LEADER|SHIFT",
      action = act.AdjustPaneSize { "Down", 5 },
    },
    {
      key = "K",
      mods = "LEADER|SHIFT",
      action = act.AdjustPaneSize { "Up", 5 },
    },
    {
      key = "L",
      mods = "LEADER|SHIFT",
      action = act.AdjustPaneSize { "Right", 5 },
    },
  }
end

return M