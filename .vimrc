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
    set guioptions=acegit
    set guifont="DejaVu Sans Mono 8:antialias=true"
    set guifontwide="DejaVu Sans Mono 8:antialias=true"
endif

:au BufWinEnter * let w:m2=matchadd('ErrorMsg', '\%>80v.\+', -1)

call arpeggio#load()
Arpeggionmap tn :tabnext<CR>
Arpeggionmap tp :tabprevious<CR>
Arpeggionmap t1 1gt
Arpeggionmap t2 2gt
Arpeggionmap t3 3gt
Arpeggionmap t4 4gt
Arpeggionmap t5 5gt
Arpeggionmap t6 6gt
Arpeggionmap t7 7gt
Arpeggionmap t8 8gt
Arpeggionmap t9 9gt

set smartindent
set autoindent
set wrap
let html_use_css=1
let use_xhtml=1
