" -------------------------------------------
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
" watchdogs
" --------------------------------------------------
" この関数に g:quickrun_config を渡す
" この関数で g:quickrun_config にシンタックスチェックを行うための設定を追加する
" 関数を呼び出すタイミングはユーザの g:quickrun_config 設定後
call watchdogs#setup(g:quickrun_config)

" 書き込み後にシンタックスチェックを行う
let g:watchdogs_check_BufWritePost_enable = 1
