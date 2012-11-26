" Vim color file
" Name:       h2u_black
" Maintainer: Kyo Nagashima <kyo@hail2u.net>
" URL:        http://hail2u.net/
" Version:    12.4.21
" License:    http://hail2u.mit-license.org/2010


set background=dark

hi clear

if exists("syntax_on")
  syntax reset
endif

let colors_name = "h2u_black"


" General
hi ColorColumn   guifg=NONE     guibg=#ff6666  gui=NONE
hi Conceal       guifg=#cccccc  guibg=#666666  gui=NONE
hi Cursor        guifg=#000000  guibg=#99ccff  gui=NONE
hi CursorIM      guifg=#ffffff  guibg=#ff6666  gui=NONE
hi CursorColumn  guifg=NONE     guibg=#222222  gui=NONE
hi CursorLine    guifg=NONE     guibg=#222222  gui=NONE
hi CursorLineNr  guifg=#444444  guibg=#222222  gui=NONE
hi Directory     guifg=#99ccff  guibg=NONE     gui=NONE
hi DiffAdd       guifg=NONE     guibg=#331166  gui=NONE
hi DiffChange    guifg=NONE     guibg=#443344  gui=NONE
hi DiffDelete    guifg=#222222  guibg=#333366  gui=NONE
hi DiffText      guifg=NONE     guibg=#772266  gui=NONE
hi ErrorMsg      guifg=#ffffff  guibg=#ff6666  gui=NONE
hi VertSplit     guifg=#222222  guibg=#222222  gui=NONE
hi Folded        guifg=#99ccff  guibg=#222222  gui=NONE
hi FoldColumn    guifg=#99ccff  guibg=#666666  gui=NONE
hi SignColumn    guifg=#99ccff  guibg=#666666  gui=NONE
hi IncSearch     guifg=#000000  guibg=#ff9900  gui=NONE
hi LineNr        guifg=#444444  guibg=#000000  gui=NONE
hi MatchParen    guifg=#000000  guibg=#ff9900  gui=bold
hi ModeMsg       guifg=#6699cc  guibg=NONE     gui=NONE
hi MoreMsg       guifg=#000000  guibg=#99ccff  gui=NONE
hi NonText       guifg=#666666  guibg=NONE     gui=NONE
hi Normal        guifg=#ffffff  guibg=#000000  gui=NONE
hi Pmenu         guifg=#999999  guibg=#222222  gui=NONE
hi PmenuSel      guifg=#000000  guibg=#99ccff  gui=NONE
hi PmenuSbar     guifg=#000000  guibg=#cccccc  gui=NONE
hi PmenuThumb    guifg=#000000  guibg=#666666  gui=NONE
hi Question      guifg=#99ccff  guibg=NONE     gui=NONE
hi Search        guifg=#000000  guibg=#ffff00  gui=NONE
hi SpecialKey    guifg=#666666  guibg=#000000  gui=NONE
hi SpellBad      guifg=NONE     guibg=NONE     gui=undercurl  guisp=#ff6666
hi SpellCap      guifg=NONE     guibg=NONE     gui=undercurl  guisp=#99ccff
hi SpellLocal    guifg=NONE     guibg=NONE     gui=undercurl  guisp=#99ff33
hi SpellRare     guifg=NONE     guibg=NONE     gui=undercurl  guisp=#ff99ff
hi StatusLine    guifg=#ffffff  guibg=#333333  gui=NONE
hi StatusLineNC  guifg=#666666  guibg=#333333  gui=NONE
hi TabLine       guifg=#666666  guibg=#222222  gui=underline
hi TabLineFill   guifg=#666666  guibg=#222222  gui=underline
hi TabLineSel    guifg=#99ccff  guibg=#000000  gui=NONE
hi Title         guifg=#ffffcc  guibg=NONE     gui=NONE
hi Visual        guifg=NONE     guibg=#444444  gui=NONE
" hi VisualNOS
hi WarningMsg    guifg=#ff6666  guibg=NONE     gui=NONE
hi WildMenu      guifg=#000000  guibg=#99ccff  gui=NONE


" Syntax
hi Comment       guifg=#666666  guibg=NONE     gui=NONE
hi Constant      guifg=#99ff33  guibg=NONE     gui=NONE
hi Character     guifg=#ff9999  guibg=NONE     gui=NONE
hi Number        guifg=#ff99ff  guibg=NONE     gui=NONE
hi Identifier    guifg=#6699cc  guibg=NONE     gui=NONE
hi Statement     guifg=#336699  guibg=NONE     gui=NONE
hi PreProc       guifg=#99ccff  guibg=NONE     gui=NONE
hi Type          guifg=#ffff99  guibg=NONE     gui=NONE
hi Special       guifg=#ff9933  guibg=NONE     gui=NONE
hi Underlined    guifg=NONE     guibg=NONE     gui=underline
" hi Ignore
hi Error         guifg=NONE     guibg=NONE     gui=undercurl  guisp=#ff6666
hi Todo          guifg=#ffff00  guibg=NONE     gui=underline

hi link Float   Number
hi link Boolean Character
