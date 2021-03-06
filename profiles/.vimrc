" backup setting
set dir=~/.vim/swp
set backup
set backupdir=~/.vim/backup/
set backupext=.bak

set autoindent        " always set autoindenting on
set smartindent       " set smart indent
set smarttab          " use tabs at the start of a line, spaces elsewhere
set expandtab         " replace tab to whitespace
set tabstop=4 softtabstop=4 shiftwidth=4
set linespace=2
set backspace=2

set smartcase         " set smart case
set ignorecase        " easier to ignore case for searching
set incsearch         " do incremental searching
set hlsearch          " highlighting the last used search pattern. or :noh
"set list             " show chars on end of line, whitespace, etc
"set nowrap           " do not wrap lines

"set autochdir         " auto change work dir

set nocompatible      " explicitly get out of vi-compatible mode
set noexrc            " don't use local version of .(g)vimrc, .exrc
set wildmenu          " command-line completion operates in an enhanced mode
set history=50        " keep 50 lines of command line history
set mouse=a           " set mouse functions
set ruler             " show current positions along the bottom
set paste             " paste context with format

"don't make noise
set noerrorbells
set novisualbell
set visualbell t_vb=

filetype plugin on    " load plugin
filetype indent on
filetype on           " detect the type of file
syntax on             " syntax highlighting on
set fileformat=unix
set fileformats=unix,dos,mac
set textwidth=80

set encoding=utf-8
set fileencoding=utf-8
set fileencodings=utf-8,ucs-bom,gb18030,gbk,gb2312,cp936
set ambiwidth=double

if has("gui_running")
    set lines=25
    set columns=80
    set lazyredraw
    set guioptions-=m
    set guioptions-=T
    "set guifont=Consolas\ 12
    "set guifont=YaHei\ Consolas\ Hybrid\ 11
    "set guifont=Menlo\ Regular:h18
    set guifont=Monaco:h16
endif

" ==== keys mapping ===
map <F9> :TlistToggle<cr>
map <F4> :bdelete<cr>
map <F5> %
map <F6> :Explore<cr>
map <c-q> :bdelete<cr>
map <c-c> "+y<cr>
imap <c-s> <c-o>:up<cr>
nmap <c-s> :up<cr>
imap <c-v> <c-o>"+gP
imap <c-z> <c-o>u
imap <c-y> <c-o><c-r>
imap <c-f> function () {}<Left>
" Normal Mode, Visual Mode, and Select Mode,
" use <Tab> and <Shift-Tab> to indent
nmap <tab> v>
nmap <s-tab> v<
vmap <tab> >gv
vmap <s-tab> <gv
"nmap <F2> :GundoToggle<cr>

" ==== omni ====
imap <c-l> <c-x><c-o>

" ==== autocmd ===
au Filetype java setlocal omnifunc=javacomplete#Complete
au Bufenter,Bufnewfile *.clj setl complete+=k~/.clj_completions
au BufRead,BufNewFile *.json setfiletype json
au BufRead,BufNewFile *.ejs setfiletype html

"minibufexplorer
let g:miniBufExplMapWindowNavVim = 1
let g:miniBufExplMapWindowNavArrows = 1
let g:miniBufExplMapCTabSwitchBufs = 1
let g:miniBufExplModSelTarget = 1

" Remove trailing whitespace when writing a buffer, but not for diff files.
" From: Vigil
function RemoveTrailingWhitespace()
    if &ft != "diff"
        let b:curcol = col(".")
        let b:curline = line(".")
        silent! %s/\s\+$//
        silent! %s/\(\s*\n\)\+\%$//
        call cursor(b:curline, b:curcol)
    endif
endfunction
autocmd BufWritePre * call RemoveTrailingWhitespace()

"   map : all
"   nmap: normal map
"   vmap: visual map
"   imap: insert map
"
"   b:name        " buffer
"   w:name        " window
"   g:name        " global
"   v:name        " vim
"   a:name        " function param
"
"   set fdm=marker
"   zc: close
"   zo: open
"   zf: create
"   zd: delete
"   use jQuery syntax: set syntax=jquery
"   :help modeline
"   /* vim: set tabstop=4 shiftwidth=4 expandtab: */
