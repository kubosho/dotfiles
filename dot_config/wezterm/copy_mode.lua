local wezterm = require "wezterm"
local act = wezterm.action

local M = {}

function M.get_keys()
  return {
    -- Copy mode
    {
      key = "[",
      mods = "LEADER",
      action = act.ActivateCopyMode,
    },

    -- Paste
    {
      key = "]",
      mods = "LEADER",
      action = act.PasteFrom "Clipboard",
    },
  }
end

function M.get_key_tables()
  return {
    copy_mode = {
      -- Movement
      { key = "h", mods = "NONE", action = act.CopyMode "MoveLeft" },
      { key = "j", mods = "NONE", action = act.CopyMode "MoveDown" },
      { key = "k", mods = "NONE", action = act.CopyMode "MoveUp" },
      { key = "l", mods = "NONE", action = act.CopyMode "MoveRight" },

      -- Word movement
      { key = "w", mods = "NONE", action = act.CopyMode "MoveForwardWord" },
      { key = "b", mods = "NONE", action = act.CopyMode "MoveBackwardWord" },

      -- Line movement
      { key = "0", mods = "NONE", action = act.CopyMode "MoveToStartOfLine" },
      { key = "$", mods = "NONE", action = act.CopyMode "MoveToEndOfLineContent" },

      -- Page movement
      { key = "g", mods = "NONE", action = act.CopyMode "MoveToScrollbackTop" },
      { key = "G", mods = "NONE", action = act.CopyMode "MoveToScrollbackBottom" },

      -- Selection modes
      { key = "v", mods = "NONE", action = act.CopyMode { SetSelectionMode = "Cell" } },
      { key = "V", mods = "NONE", action = act.CopyMode { SetSelectionMode = "Line" } },
      { key = "v", mods = "CTRL", action = act.CopyMode { SetSelectionMode = "Block" } },

      -- Copy and exit
      { key = "y", mods = "NONE", action = act.Multiple {
        act.CopyTo "ClipboardAndPrimarySelection",
        act.CopyMode "Close",
      } },

      -- Search
      { key = "/", mods = "NONE", action = act.Search { CaseSensitiveString = "" } },
      { key = "?", mods = "NONE", action = act.Search { CaseInSensitiveString = "" } },
      { key = "n", mods = "NONE", action = act.CopyMode "NextMatch" },
      { key = "N", mods = "NONE", action = act.CopyMode "PriorMatch" },

      -- Exit copy mode
      { key = "q", mods = "NONE", action = act.CopyMode "Close" },
      { key = "Escape", mods = "NONE", action = act.CopyMode "Close" },
      { key = "c", mods = "CTRL", action = act.CopyMode "Close" },
    },
  }
end

return M
