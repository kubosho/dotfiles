if $SHELL =~ '/fish$'
  set shell=zsh
endif

set encoding=utf-8
scriptencoding utf-8

""""""""""""""""""""""""""""""""""""""""""""""""""

set background=dark
syntax on

""""""""""""""""""""""""""""""""""""""""""""""""""

set cursorline
set number
" color number
" ref: http://vim.wikia.com/wiki/Xterm256_color_names_for_console_Vim
hi LineNr    ctermfg=231 ctermbg=234=
hi VertSplit ctermfg=231 ctermbg=234
set ruler
set wrap
set list
set listchars=tab:▸\ ,trail:-,eol:¬

""""""""""""""""""""""""""""""""""""""""""""""""""

set backspace=2

""""""""""""""""""""""""""""""""""""""""""""""""""

set showmatch
set showcmd
set noshowmode

""""""""""""""""""""""""""""""""""""""""""""""""""

set hlsearch
set ignorecase
set smartcase

""""""""""""""""""""""""""""""""""""""""""""""""""

set swapfile
set directory=$XDG_CACHE_HOME
set backupdir=$XDG_CACHE_HOME
set undodir=$XDG_CACHE_HOME
set tags=./tags;,tags;
set clipboard=unnamed,autoselect

""""""""""""""""""""""""""""""""""""""""""""""""""

set confirm

""""""""""""""""""""""""""""""""""""""""""""""""""

set autoindent
set smartindent
set tabstop=4
set shiftwidth=2
set smarttab

""""""""""""""""""""""""""""""""""""""""""""""""""

if !has('gui_running')
  set t_Co=256
endif

""""""""""""""""""""""""""""""""""""""""""""""""""
" .vimrc

nnoremap <silent> <C-]> :<C-u>edit $MYVIMRC<CR>

augroup reload_vimrc
  autocmd!
  autocmd bufwritepost $MYVIMRC nested source $MYVIMRC
augroup END

""""""""""""""""""""""""""""""""""""""""""""""""""
" show zenkaku space
" ref: http://inari.hatenablog.com/entry/2014/05/05/231307

function! ShowZenkakuSpace()
  highlight ShowZenkakuSpace cterm=underline ctermfg=lightblue guibg=darkgray
endfunction

if has('syntax')
  augroup ShowZenkakuSpace
    autocmd!
    autocmd ColorScheme * call ShowZenkakuSpace()
    autocmd VimEnter,WinEnter,BufRead * let w:m1=matchadd('ShowZenkakuSpace', '　')
  augroup END
  call ShowZenkakuSpace()
endif

""""""""""""""""""""""""""""""""""""""""""""""""""
" auto cursorline
" http://thinca.hatenablog.com/entry/20090530/1243615055

augroup vimrc-auto-cursorline
  autocmd!
  autocmd CursorMoved,CursorMovedI * call s:auto_cursorline('CursorMoved')
  autocmd CursorHold,CursorHoldI * call s:auto_cursorline('CursorHold')
  autocmd WinEnter * call s:auto_cursorline('WinEnter')
  autocmd WinLeave * call s:auto_cursorline('WinLeave')

  let s:cursorline_lock = 0
  function! s:auto_cursorline(event)
    if a:event ==# 'WinEnter'
      setlocal cursorline
      let s:cursorline_lock = 2
    elseif a:event ==# 'WinLeave'
      setlocal nocursorline
    elseif a:event ==# 'CursorMoved'
      if s:cursorline_lock
        if 1 < s:cursorline_lock
          let s:cursorline_lock = 1
        else
          setlocal nocursorline
          let s:cursorline_lock = 0
        endif
      endif
    elseif a:event ==# 'CursorHold'
      setlocal cursorline
      let s:cursorline_lock = 1
    endif
  endfunction
augroup END

""""""""""""""""""""""""""""""""""""""""""""""""""
" autosave

autocmd CursorHold * wall
set updatetime=5000
