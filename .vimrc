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
set tabstop=4                           "'\t' == 4 spaces
set softtabstop=4                       "tab = 4 spaces
set expandtab                           "tab => spaces
set shiftwidth=4                        "indenting = 4 spaces 
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

" ## added by OPAM user-setup for vim / base ## 93ee63e278bdfc07d1139a748ed3fff2 ## you can edit, but keep this line
let s:opam_share_dir = system("opam config var share")
let s:opam_share_dir = substitute(s:opam_share_dir, '[\r\n]*$', '', '')

let s:opam_configuration = {}

function! OpamConfOcpIndent()
  execute "set rtp^=" . s:opam_share_dir . "/ocp-indent/vim"
endfunction
let s:opam_configuration['ocp-indent'] = function('OpamConfOcpIndent')

function! OpamConfOcpIndex()
  execute "set rtp+=" . s:opam_share_dir . "/ocp-index/vim"
endfunction
let s:opam_configuration['ocp-index'] = function('OpamConfOcpIndex')

function! OpamConfMerlin()
  let l:dir = s:opam_share_dir . "/merlin/vim"
  execute "set rtp+=" . l:dir
endfunction
let s:opam_configuration['merlin'] = function('OpamConfMerlin')

let s:opam_packages = ["ocp-indent", "ocp-index", "merlin"]
let s:opam_check_cmdline = ["opam list --installed --short --safe --color=never"] + s:opam_packages
let s:opam_available_tools = split(system(join(s:opam_check_cmdline)))
for tool in s:opam_packages
  " Respect package order (merlin should be after ocp-index)
  if count(s:opam_available_tools, tool) > 0
    call s:opam_configuration[tool]()
  endif
endfor
" ## end of OPAM user-setup addition for vim / base ## keep this line

" ## Merlin, manual
let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
execute "set rtp+=" . g:opamshare . "/merlin/vim"

" ## Syntastic
execute pathogen#infect()
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_ocaml_checkers = ['merlin']

" ## Ocp-indent
"set rtp^="/home/skkeem/.opam/4.05.0/share/ocp-indent/vim"
set rtp^="/home/skkeem/.vim/ocp-indent-vim"

" ## vim-slime
let g:slime_target = "tmux"
"let g:slime_default_config = {"socket_name": split($TMUX, ",")[0], "target_pane": ":.2"}
let g:slime_default_config = {"socket_name": "default", "target_pane": "2"}
