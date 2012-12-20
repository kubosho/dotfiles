" 色の有効化
syntax on
set background=light
" set background=dark
" colorscheme molokai
colorscheme jellybeans
" colorscheme solarized
" colorscheme newspaper

" set filetype to scss
au BufNewFile,BufRead *.scss setf scss

" 行番号
set nu

" ファイルを保存しなくても他バッファを表示できるようにする
set hidden

" コマンドライン補完を便利にする
set wildmenu

" insertモードを抜けるとIMEオフ
set noimdisable
set iminsert=0 imsearch=0
set noimcmdline
inoremap <silent> <ESC> <ESC>:set iminsert=0<CR>
runtime macros/editexisting.vim

" 勝手にcd
au BufEnter * execute ":lcd ".expand("%:p:h")

" ファイル名、文字エンコード、改行形式をステータスラインに表示
set statusline=%<%f\ %m%r%h%w%{'['.(&fenc!=''?&fenc:&enc).':'.&ff.']'}%=%l,%c%V(%L)%8P
set laststatus=2

" 対応する括弧をハイライト表示
set showmatch

" 一行前のインデントにあわせてインデントする
set noautoindent

" 前回終了時の位置を記憶
if has("autocmd")
    autocmd BufReadPost *
    \ if line("'\"") > 0 && line ("'\"") <= line("$") |
    \   exe "normal! g'\"" |
    \ endif
endif

" 自殺コマンド
command! Suicide call system('kill -KILL' . getpid())

" 行末の空白を保存時に削除
autocmd BufWritePre * :%s/\s\+$//e

" --------------------------------------------------
" キーバインド
" --------------------------------------------------
" <leader>割り当て
let mapleader = "\"

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

"  --------------------------------------------------
"  非表示
"  --------------------------------------------------
" ツールバーを削除
set guioptions-=T
set guioptions-=l
set guioptions-=r

" --------------------------------------------------
" 可視化
" --------------------------------------------------
" 全角スペースを表示
hi FullWidthSpace term=underline ctermbg=blue guibg=darkgray
autocmd BufNew,BufRead * match FullWidthSpace /　/

" ハードタブと行末のスペースを表示
hi SpecialKey ctermfg=blue guifg=#555555
set listchars=tab:>\ ,trail:_
set list

" --------------------------------------------------
" 連番
" --------------------------------------------------
nnoremap <silent> co :ContinuousNumber <C-a><CR>
vnoremap <silent> co :ContinuousNumber <C-a><CR>
command! -count -nargs=1 ContinuousNumber let c = col('.')|for n in range(1, <count>?<count>-line('.'):1)|exec 'normal! j' . n . <q-args>|call cursor('.', c)|endfor

" --------------------------------------------------
" ディレクトリ指定
" --------------------------------------------------
" backup
set backupdir=~/work/vim/backup

" swap
set directory=~/work/vim/swap

" --------------------------------------------------
" インデント
" --------------------------------------------------
" タブを何文字分として表示するか
set tabstop=2
" インデントのスペース設定
set shiftwidth=2
" タブを使わない
set expandtab

" --------------------------------------------------
" テンプレート
" --------------------------------------------------
autocmd BufNewFile * silent! 0r $VIMHOME/templates/%:e.tpl

" --------------------------------------------------
" 辞書
" --------------------------------------------------
autocmd FileType javascript :set dictionary=javascript.dict<CR>

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
" 起動時に実行
" --------------------------------------------------
if has('vim_starting')
endif

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
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/neocomplcache'
NeoBundle 'Shougo/neocomplcache-snippets-complete'
NeoBundle 'thinca/vim-quickrun'
NeoBundle 'Shougo/vimproc'
NeoBundle "osyo-manga/shabadou.vim"
NeoBundle "osyo-manga/vim-watchdogs"
NeoBundle "jceb/vim-hier"

NeoBundle 'Lokaltog/vim-powerline'
NeoBundle 'tpope/vim-surround'
NeoBundle 'vim-scripts/renamer.vim'
NeoBundle 'sjl/gundo.vim'
NeoBundle 'AtsushiM/oop-js.vim'

NeoBundle 'nathanaelkane/vim-indent-guides'
NeoBundle 'othree/html5.vim'
NeoBundle 'hail2u/vim-css3-syntax'
NeoBundle 'cakebaker/scss-syntax.vim'
NeoBundle 'pangloss/vim-javascript'
NeoBundle 'teramako/jscomplete-vim'
NeoBundle 'walm/jshint.vim'
NeoBundle 'kchmck/vim-coffee-script'
NeoBundle 'plasticboy/vim-markdown'
NeoBundle 'mattn/zencoding-vim'

NeoBundle 'tell-k/vim-browsereload-mac'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'thinca/vim-template'

NeoBundle 'ujihisa/unite-colorscheme'
NeoBundle 'tomasr/molokai'
NeoBundle 'nanotech/jellybeans.vim'
NeoBundle 'altercation/solarized'
NeoBundle 'vim-scripts/newspaper.vim'

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

" SCSSをCSSのオムニ補完にする
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
" colorscheme
nnoremap <silent> ,uc :<C-u>Unite colorscheme -auto-preview<CR>

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
" oop-js
" --------------------------------------------------
let g:oopjs_autocheck = 1 "自動実行有り
let g:oopjs_ignorecheckfile = ['min\.js', 'combine\.js', 'lib\/.\+\.js']
" 1ファイルの限界行数
let g:oopjs_linelimitnum = 200
" 1行で使える.(ドット)の数
let g:oopjs_dotlimitnum = 5
" 1ファイル内の無名関数の限界数
let g:oopjs_anonymousfunctionlimitnum = 20
" 1ファイル内のvar宣言の限界数
let g:oopjs_varlimitnum = 20

" ----------------------------------------
" quickrun
" ----------------------------------------
let g:quickrun_config = {
\   "syntax_checker/ruby" : {
\       "command" : "ruby",
\       "exec"    : "%c %s:p %o",
\       "cmdopt"  : "-c",
\   },
\
\   "syntax_checker/jshint" : {
\       "command" : "jshint",
\       "exec"    : "%c %s:p",
\       "quickfix/errorformat" : "%f: line %l\\,\ col %c\\, %m",
\   },
\}
let g:quickrun_config._ = {'runner' : 'vimproc'}

" --------------------------------------------------
" watchdogs
" --------------------------------------------------
" 書き込み後にシンタックスチェックを行う
let g:watchdogs_check_BufWritePost_enable = 1

call watchdogs#setup(g:quickrun_config)

" ----------------------------------------
" vim-browsereload-mac
" ----------------------------------------
" アプリに戻らない設定
" let g:returnAppFlag = 0

" リロード後に戻ってくるアプリ
let g:returnApp = "MacVim"

nmap <Space>bc :ChromeReloadStart<CR>
nmap <Space>bC :ChromeReloadStop<CR>
nmap <Space>bf :FirefoxReloadStart<CR>
nmap <Space>bF :FirefoxReloadStop<CR>
nmap <Space>bs :SafariReloadStart<CR>
nmap <Space>bS :SafariReloadStop<CR>
nmap <Space>bo :OperaReloadStart<CR>
nmap <Space>bO :OperaReloadStop<CR>
nmap <Space>ba :AllBrowserReloadStart<CR>
nmap <Space>bA :AllBrowserReloadStop<CR>

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
