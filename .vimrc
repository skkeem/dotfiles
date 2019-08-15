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
set nu                                  "line numbers on the left
set hlsearch                            "highlight search
set incsearch                           "incremental search
set tabstop=2                           "'\t' == 4 spaces
set softtabstop=2                       "tab = 4 spaces
set expandtab                           "tab => spaces
set shiftwidth=2                        "indenting = 4 spaces 
set autoindent                          "follow previous line
filetype plugin indent on               "file type based indentation. should never work along with smart/cindent
syntax on
set tags=./tags;                        "ctags
set statusline+=%F                      "statusline
set laststatus=2

" cursor after splits
set splitbelow
set splitright

" split navigation.
imap wj <Esc><c-w>w
nmap wj <c-w>wa
imap wm <Esc><c-w>wa
nmap wm <c-w>w
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l

autocmd FileType python compiler pylint
