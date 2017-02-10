" Key binding quick reference
" mapleader is ,
" <F1> - toggle search highlight
" <F2> - clear search
" <F3> - tabularize on whitespace
" <F4> - toggle relative line numbers
" ,lc - toggle listchars
" ,sw - soywiki
" :w!! - write it with sudo tee
" visual <tab> - easyalign
" ga - easyalign
" Arpeggio:
" tn - next tab
" tp - previous tab
" tX - tab X
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

let g:pathogen_disabled = []
if v:version < '704' || !has('python')
    call add(g:pathogen_disabled, 'vim-pandoc')
    call add(g:pathogen_disabled, 'vim-pandoc-syntax')
endif
call pathogen#infect()

autocmd BufRead,BufNewFile {README,INSTALL} setfiletype text
autocmd FileType {mail,gitcommit,text} setlocal spell spelllang=en_us tw=78
autocmd FileType {help} setlocal nospell
autocmd FileType python BracelessEnable +indent +highlight +fold

set formatoptions-=o

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


" reserve F5 and up for buffer-specific

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

" Soywiki
nnoremap <Leader>sw :lcd ~/soywiki<CR>:Soywiki<CR>

nnoremap <Leader>nt :NERDTreeToggle<CR>

" http://vim.wikia.com/wiki/Omni_completion
set omnifunc=syntaxcomplete#Complete

set encoding=utf-8
set fileencoding=utf-8

set wildignore=*.beam

set background=dark

if has('gui_running')
    set guioptions=acegit
    colorscheme base16-eighties
    set guifont=DejaVu\ Sans\ Mono\ 9
    set guifontwide=DejaVu\ Sans\ Mono\ 9
    set lines=44
    set columns=132
    let g:calendar_google_calendar=1
    nnoremap <Leader>cal :Calendar<CR>
    if has('X11')
        set clipboard=unnamedplus " use the X11 CLIPBOARD
    endif
endif

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


au BufRead,BufNewFile *.pp setfiletype puppet
au BufRead,BufNewFile *.cf set ft=cf3


set laststatus=2
" smart statusline
" (originally from http://www.reddit.com/r/vim/comments/gexi6/a_smarter_statusline_code_in_comments/)
hi StatColor guibg=#95e454 guifg=black ctermbg=lightgreen ctermfg=black
hi Modified guibg=orange guifg=black ctermbg=lightred ctermfg=black

function! MyStatusLine(mode)
    let statusline=""
    if a:mode == 'Enter'
        let statusline.="%#StatColor#"
    endif
    let statusline.="\(%n\)\ %f\ "
    if a:mode == 'Enter'
        let statusline.="%*"
    endif
    let statusline.="%#Modified#%m"
    if a:mode == 'Leave'
        let statusline.="%*%r"
    elseif a:mode == 'Enter'
        let statusline.="%r%*"
    endif
    let statusline .= "\ (%l/%L,\ %c)\ %P%=%h%w\ %y\ [%{&encoding}:%{&fileformat}]\ \ "
    return statusline
endfunction

au WinEnter * setlocal statusline=%!MyStatusLine('Enter')
au WinLeave * setlocal statusline=%!MyStatusLine('Leave')
set statusline=%!MyStatusLine('Enter')

function! InsertStatuslineColor(mode)
    if a:mode == 'i'
        hi StatColor guibg=orange ctermbg=lightred
    elseif a:mode == 'r'
        hi StatColor guibg=#e454ba ctermbg=magenta
    elseif a:mode == 'v'
        hi StatColor guibg=#e454ba ctermbg=magenta
    else
        hi StatColor guibg=red ctermbg=red
    endif
endfunction 

au InsertEnter * call InsertStatuslineColor(v:insertmode)
au InsertLeave * hi StatColor guibg=#95e454 guifg=black ctermbg=lightgreen ctermfg=black

" ShowMotion
"*** Highlights both big and small motions
nmap w <Plug>(show-motion-both-w)
nmap W <Plug>(show-motion-both-W)
nmap b <Plug>(show-motion-both-b)
nmap B <Plug>(show-motion-both-B)
nmap e <Plug>(show-motion-both-e)
nmap E <Plug>(show-motion-both-E)

"*** Only highlights motions corresponding to the one you typed
"nmap w <Plug>(show-motion-w)
"nmap W <Plug>(show-motion-W)
"nmap b <Plug>(show-motion-b)
"nmap B <Plug>(show-motion-B)
"nmap e <Plug>(show-motion-e)
"nmap E <Plug>(show-motion-E)

"Show motion for chars:  
nmap f <Plug>(show-motion-f)
nmap t <Plug>(show-motion-t)
nmap F <Plug>(show-motion-F)
nmap T <Plug>(show-motion-T)
nmap ; <Plug>(show-motion-;)
nmap , <Plug>(show-motion-,) 

highlight SM_SmallMotionGroup cterm=italic                ctermbg=53 gui=italic                guibg=#5f005f
highlight SM_BigMotionGroup   cterm=italic,bold,underline ctermbg=54 gui=italic,bold,underline guibg=#5f0087
highlight SM_CharSearchGroup  cterm=italic,bold           ctermbg=4  gui=italic,bold           guibg=#3f6691

" vim-easy-align
vmap <tab> <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)
