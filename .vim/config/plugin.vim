" -------------------------------------------
" NeoCompleCache
" -------------------------------------------
" 補完ウィンドウの設定
set completeopt=menuone

" 起動時に有効化
let g:neocomplcache_enable_at_startup = 1

" SCSSをCSSのオムニ補完にする
if !exists('g:neocomplcache_omni_patterns')
  let g:neocomplcache_omni_patterns = {}
endif
let g:neocomplcache_omni_patterns.scss = '^\s\+\w\+\|\w\+[):;]\?\s\+\|[@!]'

if !exists('g:neocomplcache_keyword_patterns')
    let g:neocomplcache_keyword_patterns = {}
endif
let g:neocomplcache_keyword_patterns['default'] = '\h\w*'

" -------------------------------------------
" NERDTree
" http://blog.livedoor.jp/kumonopanya/archives/51048805.html
" -------------------------------------------
" <C-e>でNERDTreeをオンオフ。いつでもどこでも。
nmap <silent> <C-e>      :NERDTreeToggle<CR>
vmap <silent> <C-e> <Esc>:NERDTreeToggle<CR>
omap <silent> <C-e>      :NERDTreeToggle<CR>
imap <silent> <C-e> <Esc>:NERDTreeToggle<CR>
cmap <silent> <C-e> <C-u>:NERDTreeToggle<CR>

" -------------------------------------------
" vim-browserreload-mac
" -------------------------------------------
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

" -------------------------------------------
" quickrun
" http://d.hatena.ne.jp/osyo-manga/20120919/1348054752
" -------------------------------------------
let g:quickrun_config = {
\   "_" : {
\       "outputter/buffer/split" : ":botright 8sp",
\       "runner" : "vimproc",
\       "runner/vimproc/updatetime" : 40,
\   },
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
\   }
\}

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
" watchdogs
" --------------------------------------------------
" この関数に g:quickrun_config を渡す
" この関数で g:quickrun_config にシンタックスチェックを行うための設定を追加する
" 関数を呼び出すタイミングはユーザの g:quickrun_config 設定後
call watchdogs#setup(g:quickrun_config)

" 書き込み後にシンタックスチェックを行う
let g:watchdogs_check_BufWritePost_enable = 1

" -------------------------------------------
" zen-coding
" -------------------------------------------
" 日本語対応
let g:user_zen_settings = {
  \  'lang' : 'ja',
  \  'html' : {
  \    'indentation' : '  ',
  \    'snippets' : {
  \      'html:5': "<!DOCTYPE html>\n<html lang=\"ja\">\n<head>\n<meta charset=\"UTF-8\">\n<title></title>\n<meta name=\"description\" content=\"\">\n<meta name=\"keywords\" content=\"\">\n<meta name=\"viewport\" content=\"width=980\">\n<meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge,chrome=1\">\n<meta name=\"format-detection\" content=\"telephone=no\">\n<link rel=\"stylesheet\" href=\"css/lib/normalize.css\">\n<link rel=\"stylesheet\" href=\"css/style.css\">\n<!--[if lt IE 9]>\n<script src=\"js/lib/html5shiv-printshiv.js\"></script>\n<![endif]-->\n</head>\n\n<body>\n\n</body>\n</html>",
  \    },
  \  },
  \  'css' : {
  \    'filters' : 'fc',
  \  },
  \}
}

