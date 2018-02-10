" Key binding quick reference
" mapleader is ,
" <F1> - toggle search highlight
" <F2> - clear search
" <F3> - tabularize on whitespace
" <F4> - toggle relative line numbers
" <F5> - toggle paste
" ,lc - toggle listchars
" ,sw - soywiki
" :w!! - write it with sudo tee
" visual <tab> - easyalign
" ga - easyalign
" Tabs:
" <C-Space>X - go to tab X
" <C-Space><C-Space> - go to next tab
" Windows:
" gr - resize this split to fit the visual selection
" gss - split this selection
" gsa - split this selection above
" gsb - split this selection below
" braceless (python text objects):
" motion P selects this object
" Miscellaneous:
" ,cal - open Calendar
" ,nt - open NERDTree

set nocompatible
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
set showcmd
set exrc
set secure

let g:pathogen_disabled = []
if v:version < '704' || !has('python')
    call add(g:pathogen_disabled, 'vim-pandoc')
    call add(g:pathogen_disabled, 'vim-pandoc-syntax')
endif
if v:version < '800'
    call add(g:pathogen_disabled, 'ale')
endif
let g:ale_sign_column_always = 1
let g:ale_haskell_ghc_options = '-fno-code -v0 -dynamic'
call pathogen#infect()

autocmd BufRead,BufNewFile {README,INSTALL} setfiletype text
autocmd FileType {mail,gitcommit,text} setlocal spell spelllang=en_us tw=78
autocmd FileType {help} setlocal nospell

set formatoptions-=o
set diffopt+=vertical

cmap w!! w !sudo tee > /dev/null %

set hls
" toggle hls
nnoremap <F1> :set hlsearch! hlsearch?<CR>
" remove search term
nnoremap <F2> :let @/ = ""<CR>

vnoremap <F3> :Tabularize /[^ \t]\+<CR>

" relative line numbering
if version >= 703
    noremap <F4> :set relativenumber! relativenumber?<CR>:set number! number?<CR>
endif

noremap <F5> :set paste! paste?<CR>

" tab navigation
nnoremap <C-Space>1 1gt
nnoremap <C-Space>2 2gt
nnoremap <C-Space>3 3gt
nnoremap <C-Space>4 4gt
nnoremap <C-Space>5 5gt
nnoremap <C-Space>6 6gt
nnoremap <C-Space>7 7gt
nnoremap <C-Space>8 8gt
nnoremap <C-Space>9 9gt
nnoremap <C-Space><C-Space> gt


" reserve F6 and up for buffer-specific

let mapleader = ","

" cscope
if has("cscope")
    set cscopetag cscopeverbose
    cnoreabbrev csa cs add
    cnoreabbrev csf cs find
    cnoreabbrev csk cs kill
    cnoreabbrev csr cs reset
    cnoreabbrev css cs show
    cnoreabbrev csh cs help
endif

" make tabs and endlines visible in list mode
if has("win32")
    set listchars=tab:>.,eol:$
else
    set listchars=tab:⇥.,eol:$
endif
nnoremap <Leader>lc :set list!<CR>

nnoremap <Leader>nt :NERDTreeToggle<CR>

" http://vim.wikia.com/wiki/Omni_completion
set omnifunc=syntaxcomplete#Complete

set encoding=utf-8
set fileencoding=utf-8

set wildignore=*.beam

set background=dark

if has('gui_running') || &t_Co == 256
    colorscheme base16-eighties
endif

if has('gui_running')
    set guioptions=acegit
    set guifont=DejaVu\ Sans\ Mono\ 9
    if has('unix')
        let s:uname = system('uname -s')
        if s:uname == "Darwin"
            " macvim doesn't like this and i'm not sure i care enough to debug
            set guifontwide=DejaVu\ Sans\ Mono\ 9
        endif
    endif
    set lines=44
    set columns=132
    let g:calendar_google_calendar=1
    nnoremap <Leader>cal :Calendar<CR>
    if has('X11')
        set clipboard=unnamedplus " use the X11 CLIPBOARD
    endif
endif

set autoindent
set wrap
let html_use_css=1
let use_xhtml=1


au BufRead,BufNewFile *.pp setfiletype puppet
au BufRead,BufNewFile *.cf set ft=cf3


set laststatus=2
let g:airline_left_sep=''
let g:airline_right_sep=''

" vim-easy-align
vmap <tab> <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" only show quickscope when keys are pressed
let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']
