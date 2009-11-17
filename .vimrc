syntax on
set shiftwidth=4
set softtabstop=4
set expandtab
filetype on
filetype plugin indent on
set modeline
set visualbell
set scrolloff=4
set backspace=indent,eol,start

" http://vim.wikia.com/wiki/Omni_completion
set omnifunc=syntaxcomplete#Complete

set encoding=utf-8
set fileencoding=utf-8

set wildignore=*.beam

set background=light
if $TERMBGC == "dark"
    set background=dark
endif

if has('gui')
    colorscheme herald
    set background=dark
    set guioptions=acegirLt
    set guifont="DejaVu Sans Mono 8:antialias=true"
    set guifontwide="DejaVu Sans Mono 8:antialias=true"
endif

inoremap <C-\> <C-p>

map <C-f> l
map <C-b> h
map <C-e> $
map <C-a> ^
inoremap <C-f> <C-o>l
inoremap <C-b> <C-o>h
inoremap <C-e> <C-o>$
inoremap <C-a> <C-o>^
inoremap <C-p> <C-o>k
inoremap <C-n> <C-o>j
imap <C-d> <C-o>ma
map <C-d> ma
imap <C-k> <ESC><Leader>jhu_adeci
map <C-k> <Leader>jhu_adec
"map <C-k> <Leader>adec
nmap <F2> :s/(\%([^ \t)]\)\@=/( /ge<CR>:s/\%([^ \t(]\)\@<=)/ )/ge<CR>:s/)\%([^ \t;]\)\@=/) /ge<CR>
vmap <F2> <ESC>:'<,'>s/(\%([^ \t)]\)\@=/( /ge<CR>:'<,'>s/\%([^ \t(]\)\@<=)/ )/ge<CR>:'<,'>s/)\%([^ \t;]\)\@=/) /ge<CR>
imap <F2> <ESC>:s/(\%([^ \t)]\)\@=/( /ge<CR>:s/\%([^ \t(]\)\@<=)/ )/ge<CR>:s/)\%([^ \t;]\)\@=/) /ge<CR>i

call arpeggio#load()
Arpeggionmap tn :tabnext<CR>
Arpeggionmap tp :tabprevious<CR>

let clj_highlight_builtins = 1
let clj_paren_rainbow = 1

autocmd BufRead,BufNewFile *.php5 setfiletype php
autocmd FileType text setlocal textwidth=78
autocmd FileType c,cpp,h,hpp :set cindent
set autoindent
set wrap
let html_use_css=1
let use_xhtml=1

let vimclojure#NailgunClient = "/home/jhujhiti/dotfiles/clojure/ng-SunOS-i86pc"
let clj_want_gorilla = 1
