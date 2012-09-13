" 色の有効化
syntax on
set background=dark
colorscheme molokai 

" set filetype to scss
au BufNewFile,BufRead *.scss setf scss

" backup
set backupdir=~/work/vim/backup

" swap
set directory=~/work/vim/swap

" insertモードを抜けるとIMEオフ
set noimdisable
set iminsert=0 imsearch=0
set noimcmdline
inoremap <silent> <ESC> <ESC>:set iminsert=0<CR>

" 行番号
set nu

" 勝手にcd
au BufEnter * execute ":lcd ".expand("%:p:h")

" ツールバーを削除
set guioptions-=T

" ファイル名、文字エンコード、改行形式をステータスラインに表示
set statusline=%<%f\ %m%r%h%w%{'['.(&fenc!=''?&fenc:&enc).':'.&ff.']'}%=%l,%c%V(%L)%8P 
set laststatus=2

" 対応する括弧をハイライト表示
set showmatch

" 一行前のインデントにあわせてインデントする
set noautoindent

" 全角スペースを表示する
highlight ZenkakuSpace cterm=underline ctermfg=lightblue guibg=white

" 前回終了時の位置を記憶
if has("autocmd")
    autocmd BufReadPost *
    \ if line("'\"") > 0 && line ("'\"") <= line("$") |
    \   exe "normal! g'\"" |
    \ endif
endif

" 自殺コマンド
command! Suicide call system('kill -KILL' . getpid()) 

" --------------------------------------------------
" キーバインド
" --------------------------------------------------
" <leader>割り当て
let mapleader = "¥"

" おれは<ESC>をやめるぞ、jj------!
inoremap <expr> j getline('.')[col('.')-2] ==# 'j' ? "\<BS>\<ESC>" : 'j' 

" key remap
imap { {}<LEFT>
imap [ []<LEFT>
imap ( ()<LEFT>

" 入力モードで削除
inoremap <C-d> <Del>
inoremap <C-h> <BS>

" --------------------------------------------------
" 前回終了時の画面を保存
" http://d.hatena.ne.jp/akihito_s/20120122
" --------------------------------------------------

" --------------------------------------------------
" インデント
" --------------------------------------------------
" タブを何文字分として表示するか
set tabstop=8
" インデントのスペース設定
set shiftwidth=4
" タブを使わない
set expandtab

" --------------------------------------------------
" 検索	
" --------------------------------------------------
" 検索のとき、大文字小文字を無視する
set ignorecase

" 検索のとき、大文字がある場合は大文字小文字を区別する
set smartcase

" 検索のときに最後までいったらもう一度最初に戻る
set wrapscan

" 検索文字を打ち込むと即検索する
set incsearch

" --------------------------------------------------
" カーソル
" --------------------------------------------------
" カーソル行に下線を表示
set cursorline

" カーソルの点滅をやめる
set guicursor=a:blinkon0

" --------------------------------------------------
" BackSpace
" --------------------------------------------------
" おまじない
" http://koexuka.blogspot.jp/2011/02/vim.html
noremap <BS> 
noremap! <BS> 
noremap 
noremap! 

" BackSpaceの挙動変更
" http://fantatchi.info/archives/201
set backspace=indent,eol,start

" --------------------------------------------------
" NeoBundle
" --------------------------------------------------
set nocompatible
filetype off

if has('vim_starting')
  set rtp+=~/.vim/bundle/neobundle.vim
  call neobundle#rc(expand('~/.vim/bundle'))
endif

NeoBundle 'Shougo/neobundle.vim'
NeoBundle 'Shougo/vimproc'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'thinca/vim-quickrun'
NeoBundle 'Shougo/neocomplcache'
NeoBundle 'Shougo/neocomplcache-snippets-complete'
NeoBundle 'vim-scripts/YankRing.vim'
NeoBundle 'sjl/gundo.vim'
NeoBundle 'nathanaelkane/vim-indent-guides'
NeoBundle 'Lokaltog/vim-powerline'
NeoBundle 'vim-scripts/renamer.vim'
NeoBundle 'othree/html5.vim'
NeoBundle 'hail2u/vim-css3-syntax'
NeoBundle 'cakebaker/scss-syntax.vim'
NeoBundle 'teramako/jscomplete-vim'
NeoBundle 'JavaScript-Indent'
NeoBundle 'plasticboy/vim-markdown'
NeoBundle 'mattn/zencoding-vim'
NeoBundle 'tpope/vim-surround'
NeoBundle 'scrooloose/syntastic'
NeoBundle 'AtsushiM/jasmine-helper.vim'

filetype plugin on
filetype indent on

" --------------------------------------------------
" NeoCompcache
" --------------------------------------------------
" 補完ウィンドウの設定
set completeopt=menuone
 
" 起動時に有効化
let g:neocomplcache_enable_at_startup = 1
 
" 大文字が入力されるまで大文字小文字の区別を無視する
let g:neocomplcache_enable_smart_case = 1
 
" _(アンダースコア)区切りの補完を有効化
let g:neocomplcache_enable_underbar_completion = 1
 
let g:neocomplcache_enable_camel_case_completion  =  1
 
" ポップアップメニューで表示される候補の数
let g:neocomplcache_max_list = 20
 
" シンタックスをキャッシュするときの最小文字長
let g:neocomplcache_min_syntax_length = 3
 
" ディクショナリ定義
let g:neocomplcache_dictionary_filetype_lists = {
    \ 'default' : '',
    \ 'php' : $HOME . '/.vim/dict/php.dict',
    \ }

" SCSSを　CSSのオムニ補完にする
if !exists('g:neocomplcache_omni_patterns')
  let g:neocomplcache_omni_patterns = {}
endif
let g:neocomplcache_omni_patterns.scss = '^\s\+\w\+\|\w\+[):;]\?\s\+\|[@!]'

if !exists('g:neocomplcache_keyword_patterns')
    let g:neocomplcache_keyword_patterns = {}
endif
let g:neocomplcache_keyword_patterns['default'] = '\h\w*'

" --------------------------------------------------
" Unite.vim
" http://d.hatena.ne.jp/ruedap/20110117/vim_unite_plugin_1_week
" --------------------------------------------------
" 起動時にインサートモードで開始
let g:unite_enable_start_insert = 1

" バッファ一覧
nnoremap <silent> ,ub :<C-u>Unite buffer<CR>
" ファイル一覧
nnoremap <silent> ,uf :<C-u>UniteWithBufferDir -buffer-name=files file file/new<CR>
" レジスタ一覧
nnoremap <silent> ,ur :<C-u>Unite -buffer-name=register register<CR>
" 最近使用したファイル一覧
nnoremap <silent> ,um :<C-u>Unite file_mru<CR>
" 全部乗せ
nnoremap <silent> ,ua :<C-u>UniteWithBufferDir -buffer-name=files buffer file_mru bookmark file file/new<CR>

" unite.vim上でのキーマッピング
autocmd FileType unite call s:unite_my_settings()
function! s:unite_my_settings()
  " 単語単位からパス単位で削除するように変更
  imap <buffer> <C-w> <Plug>(unite_delete_backward_path)
  " ESCキーを2回押すと終了する
  nmap <silent><buffer> <ESC><ESC> q
  imap <silent><buffer> <ESC><ESC> <ESC>q
endfunction

" --------------------------------------------------
" Powerline
" --------------------------------------------------
call Pl#Hi#Allocate({
  \ 'black'          : 16,
  \ 'white'          : 231,
  \
  \ 'darkestgreen'   : 22,
  \ 'darkgreen'      : 28,
  \
  \ 'darkestcyan'    : 23,
  \ 'mediumcyan'     : 117,
  \
  \ 'darkestblue'    : 24,
  \ 'darkblue'       : 31,
  \
  \ 'darkestred'     : 52,
  \ 'darkred'        : 88,
  \ 'mediumred'      : 124,
  \ 'brightred'      : 160,
  \ 'brightestred'   : 196,
  \
  \
  \ 'darkestyellow'  : 59,
  \ 'darkyellow'     : 100,
  \ 'darkestpurple'  : 55,
  \ 'mediumpurple'   : 98,
  \ 'brightpurple'   : 189,
  \
  \ 'brightorange'   : 208,
  \ 'brightestorange': 214,
  \
  \ 'gray0'          : 233,
  \ 'gray1'          : 235,
  \ 'gray2'          : 236,
  \ 'gray3'          : 239,
  \ 'gray4'          : 240,
  \ 'gray5'          : 241,
  \ 'gray6'          : 244,
  \ 'gray7'          : 245,
  \ 'gray8'          : 247,
  \ 'gray9'          : 250,
  \ 'gray10'         : 252,
  \ })
" 'n': normal mode
" 'i': insert mode
" 'v': visual mode
" 'r': replace mode
" 'N': not active
let g:Powerline#Colorschemes#my#colorscheme = Pl#Colorscheme#Init([
  \ Pl#Hi#Segments(['SPLIT'], {
    \ 'n': ['white', 'gray2'],
    \ 'N': ['gray0', 'gray0'],
    \ }),
  \
  \ Pl#Hi#Segments(['mode_indicator'], {
    \ 'i': ['darkestgreen', 'white', ['bold']],
    \ 'n': ['darkestcyan', 'white', ['bold']],
    \ 'v': ['darkestpurple', 'white', ['bold']],
    \ 'r': ['mediumred', 'white', ['bold']],
    \ 's': ['white', 'gray5', ['bold']],
    \ }),
  \
  \ Pl#Hi#Segments(['fileinfo', 'filename'], {
    \ 'i': ['white', 'darkgreen', ['bold']],
    \ 'n': ['white', 'darkblue', ['bold']],
    \ 'v': ['white', 'mediumpurple', ['bold']],
    \ 'r': ['white', 'brightred', ['bold']],
    \ 'N': ['gray0', 'gray2', ['bold']],
    \ }),
  \
  \ Pl#Hi#Segments(['branch', 'scrollpercent', 'raw', 'filesize'], {
    \ 'n': ['gray2', 'gray7'],
    \ 'N': ['gray0', 'gray2'],
    \ }),
  \
  \ Pl#Hi#Segments(['fileinfo.filepath', 'status'], {
    \ 'n': ['gray10'],
    \ 'N': ['gray5'],
    \ }),
  \
  \ Pl#Hi#Segments(['static_str'], {
    \ 'n': ['white', 'gray4'],
    \ 'N': ['gray1', 'gray1'],
    \ }),
  \
  \ Pl#Hi#Segments(['fileinfo.flags'], {
    \ 'n': ['white'],
    \ 'N': ['gray4'],
    \ }),
  \
  \ Pl#Hi#Segments(['currenttag', 'fileformat', 'fileencoding', 'pwd', 'filetype', 'rvm:string', 'rvm:statusline', 'virtualenv:statusline', 'charcode', 'currhigroup'], {
    \ 'n': ['gray9', 'gray4'],
    \ }),
  \
  \ Pl#Hi#Segments(['lineinfo'], {
    \ 'n': ['gray2', 'gray10'],
    \ 'N': ['gray2', 'gray4'],
    \ }),
  \
  \ Pl#Hi#Segments(['errors'], {
    \ 'n': ['brightestorange', 'gray2', ['bold']],
    \ }),
  \
  \ Pl#Hi#Segments(['lineinfo.line.tot'], {
    \ 'n': ['gray2'],
    \ 'N': ['gray2'],
    \ }),
  \
  \ Pl#Hi#Segments(['paste_indicator', 'ws_marker'], {
    \ 'n': ['white', 'brightred', ['bold']],
    \ }),
  \
  \ Pl#Hi#Segments(['gundo:static_str.name', 'command_t:static_str.name'], {
    \ 'n': ['white', 'mediumred', ['bold']],
    \ 'N': ['brightred', 'darkestred', ['bold']],
    \ }),
  \
  \ Pl#Hi#Segments(['gundo:static_str.buffer', 'command_t:raw.line'], {
    \ 'n': ['white', 'darkred'],
    \ 'N': ['brightred', 'darkestred'],
    \ }),
  \
  \ Pl#Hi#Segments(['gundo:SPLIT', 'command_t:SPLIT'], {
    \ 'n': ['white', 'darkred'],
    \ 'N': ['white', 'darkestred'],
    \ }),
  \
  \ Pl#Hi#Segments(['ctrlp:focus', 'ctrlp:byfname'], {
    \ 'n': ['brightpurple', 'darkestpurple'],
    \ }),
  \
  \ Pl#Hi#Segments(['ctrlp:prev', 'ctrlp:next', 'ctrlp:pwd'], {
    \ 'n': ['white', 'mediumpurple'],
    \ }),
  \
  \ Pl#Hi#Segments(['ctrlp:item'], {
    \ 'n': ['darkestpurple', 'white', ['bold']],
    \ }),
  \
  \ Pl#Hi#Segments(['ctrlp:marked'], {
    \ 'n': ['brightestred', 'darkestpurple', ['bold']],
    \ }),
  \
  \ Pl#Hi#Segments(['ctrlp:count'], {
    \ 'n': ['darkestpurple', 'white'],
    \ }),
  \
  \ Pl#Hi#Segments(['ctrlp:SPLIT'], {
    \ 'n': ['white', 'darkestpurple'],
    \ }),
  \ ])
let g:Powerline_colorscheme='my' 

" --------------------------------------------------
" vim-indent-guides
" --------------------------------------------------
let g:indent_guides_enable_on_vim_startup=1
let g:indent_guides_start_level=2
let g:indent_guides_guide_size=1

" --------------------------------------------------
" jscomplete-vim
" --------------------------------------------------
autocmd FileType javascript
  \ :setl omnifunc=jscomplete#CompleteJS

" --------------------------------------------------
" Syntastic
" --------------------------------------------------
let g:syntastic_mode_map = { 'mode': 'passive',
                           \ 'active_filetypes': ['ruby', 'javascript'],
                           \ 'passive_filetypes': [] }

" デフォルト設定
let g:syntastic_javascript_jslint_conf = "--white --undef --nomen --regexp --plusplus --bitwise --newcap --sloppy --vars"
