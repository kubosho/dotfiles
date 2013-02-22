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
" 書き込み後にシンタックスチェックを行う
let g:watchdogs_check_BufWritePost_enable = 1
call watchdogs#setup(g:quickrun_config)
