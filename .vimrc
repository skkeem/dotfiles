"call plug#begin('~/.vim/plugged')
"Plug 'altercation/vim-colors-solarized'
"Plug 'bronson/vim-trailing-whitespace'
"Plug 'airblade/vim-gitgutter'
"Plug 'tpope/vim-fugitive'
"Plug 'junegunn/vim-easy-align'
"call plug#end()

set background=dark
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
set tabstop=4                           "'\t' == 4 spaces
set softtabstop=4                       "tab = 4 spaces
set expandtab                           "tab => spaces
set shiftwidth=4                        "indenting = 4 spaces 
set autoindent                          "follow previous line
filetype plugin indent on               "file type based indentation. should never work along with smart/cindent
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

" ConqueTerm. 
let g:ConqueTerm_CloseOnEnd = 1         "Close Buffers when finished
let g:ConqueTerm_ReadUnfocused = 1      "Update unfocused buffers
" ConqueGdb.
nnoremap <silent> <Leader>Y :ConqueGdbCommand y<CR>
nnoremap <silent> <Leader>N :ConqueGdbCommand n<CR>

" Slimv
" disable eletric return(no extra newline)
"let g:paredit_electric_return=0
" matching parens have same color
let g:lisp_rainbow=1
" horizontal split below
let g:slimv_repl_split=2
" work in tmux
let g:slimv_swank_cmd = '! tmux new-window -d -n REPL-SBCL "sbcl --load ~/.vim/slime/start-swank.lisp"'

""""""""""""""""""""""""""""""""
" webkit specific
nnoremap <silent> <Leader>g :ConqueGdb /home/skkeem/forgdb/WebKitBuild/Debug/bin/jsc<CR>
set tags+=/home/skkeem/webkitgtk-2.12.0/Source/JavaScriptCore/tags,/home/skkeem/webkitgtk-2.12.0/Source/WTF/wtf/tags
cs add /home/skkeem/webkitgtk-2.12.0/Source/cscope.out
