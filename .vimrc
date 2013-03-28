" -------------------------------------------
" NeoBundle
" -------------------------------------------
if has('vim_starting')
  set runtimepath+=~/.vim/bundle/neobundle.vim/
  call neobundle#rc(expand('~/.vim/bundle/'))
endif

" プラグイン管理
NeoBundle 'Shougo/neobundle.vim'

" ファイラー
NeoBundle 'Shougo/unite.vim'
NeoBundle 'scrooloose/nerdtree'

" 補完
NeoBundle 'Shougo/neocomplcache'
NeoBundle 'Shougo/neosnippet'
NeoBundle 'teramako/jscomplete-vim'

" check syntax
NeoBundle "osyo-manga/vim-watchdogs"
NeoBundle "jceb/vim-hier"

" フロントエンド
NeoBundle 'othree/html5.vim'
NeoBundle 'hail2u/vim-css3-syntax'
NeoBundle 'cakebaker/scss-syntax.vim'
NeoBundle 'pangloss/vim-javascript'
NeoBundle 'kchmck/vim-coffee-script'
NeoBundle 'mattn/zencoding-vim'

" quickrun関連
NeoBundle 'thinca/vim-quickrun'
NeoBundle 'Shougo/vimproc'
NeoBundle 'osyo-manga/unite-quickfix'
NeoBundle 'osyo-manga/shabadou.vim'

" いろいろ便利なもの
NeoBundle 'tpope/vim-surround'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'plasticboy/vim-markdown'
NeoBundle 'tell-k/vim-browsereload-mac'

" 外観
NeoBundle 'Lokaltog/vim-powerline'

" colorscheme
NeoBundle 'ujihisa/unite-colorscheme'
NeoBundle 'nanotech/jellybeans.vim'
NeoBundle 'chriskempson/tomorrow-theme'
NeoBundle 'w0ng/vim-hybrid'
NeoBundle 'vim-scripts/twilight'
NeoBundle 'jonathanfilip/vim-lucius'
NeoBundle 'tomasr/molokai'
NeoBundle 'vim-scripts/rdark'

" ファイル形式別プラグインのロードを有効化
filetype plugin on
filetype indent on
 
" -------------------------------------------
" 設定ファイル読み込み
" -------------------------------------------
if isdirectory(expand('~/.vim/config/'))
  source ~/.vim/config/base.vim
  source ~/.vim/config/status.vim
  source ~/.vim/config/plugin.vim
endif

" -------------------------------------------
" キーバインド
" -------------------------------------------
" おれは<ESC>をやめるぞ、jj------!
inoremap <expr> j getline('.')[col('.')-2] ==# 'j' ? "\<BS>\<ESC>" : 'j'

" key remap
imap { {}<LEFT>
imap [ []<LEFT>
imap ( ()<LEFT>

" 入力モードで削除
inoremap <C-d> <Del>
inoremap <C-h> <BS>
