set background=light
try
	colorscheme candy
catch
endtry

" encoding
if has("multi_byte")
    if &termencoding == ""
        let &termencoding = &encoding
    endif
    set encoding=utf-8                     " better default than latin1
    setglobal fileencoding=utf-8           " change default file encoding when writing new files
endif
" basics.
set number                              "line numbers on the left
" set relativenumber                      "relative line numbers on the left
set hlsearch                            "highlight search
set incsearch                           "incremental search
set tabstop=2                           "'\t' == 2 spaces while reading
set softtabstop=2                       "'\t' == 2 spaces while writing
set expandtab                           "put ' 's in place of '\t'
set shiftwidth=2                        "auto indenting == 2 spaces 
set autoindent                          "follow previous line
filetype plugin indent on               "file type based indentation. should never work along with smart/cindent
syntax on
set showmatch
set statusline+=%F                      "statusline
set laststatus=2
set tags=./tags;                        "ctags

" cursor after splits
set splitbelow
set splitright

" split navigation.
"imap wj <Esc><c-w>w
"nmap wj <c-w>wa
"imap wm <Esc><c-w>wa
"nmap wm <c-w>w
"nnoremap <c-j> <c-w>j
"nnoremap <c-k> <c-w>k
"nnoremap <c-h> <c-w>h
"nnoremap <c-l> <c-w>l

set colorcolumn=81,161,241,321,401,481,561,641,721,801

"augroup pylint
"  autocmd!
"  "autocmd FileType python compiler pylint
"  autocmd FileType python set makeprg=pylint\ --reports=n\ --msg-template=\"{path}:{line}:\ {msg_id}\ {symbol},\ {obj}\ {msg}\"\ %:p
"  autocmd FileType python set errorformat=%f:%l:\ %m
"augroup END
"augroup PylintWrite
"  autocmd FileType python
"      \ autocmd! PylintWrite BufWritePost * :make
"augroup END
"augroup OpenQuickfixWIndowAfterMake
"  autocmd QuickFixCmdPost [^l]* nested cwindow
"  autocmd QuickFixCmdPost    l* nested lwindow
"augroup END
"
"map - ddp
"map _ ddkP
