if !hasmapto('<Plug>WrapperStart')
    map <unique> <SID>WS   <Plug>AlignMapsWrapperStart
endif
nmap <silent> <script> <Plug>AlignMapsWrapperStart  :set lz<CR>:call AlignMaps#WrapperStart(0)<CR>
vmap <silent> <script> <Plug>AlignMapsWrapperStart  :<c-u>set lz<CR>:call AlignMaps#WrapperStart(1)<CR>
if !hasmapto('<Plug>WrapperEnd')
    nmap <unique> <SID>WE  <Plug>AlignMapsWrapperEnd
endif
nmap <silent> <script> <Plug>AlignMapsWrapperEnd    :call AlignMaps#WrapperEnd()<CR>:set nolz<CR>
if !hasmapto('<Plug>jhu_adec') |map <unique> <Leader>jhu_adec    <Plug>jhu_adec|endif
map <script> <Plug>jhu_adec     <SID>WS:'a,'zs/\([^ \t/(]\)\([*&]\)/\1 \2/e<CR>:'y,'zv/^\//s/\([^ \t]\)\s\+/\1 /ge<CR>:'y,'zv/^\s*[*/]/s/\([^/][*&]\)\s\+/\1/ge<CR>:'y,'zv/^\s*[*/]/s/^\(\s*\%(\K\k*\s\+\%([a-zA-Z_*(&]\)\@=\)\+\)\([*(&]*\)\s*\([a-zA-Z0-9_()]\+\)\s*\(\(\[.\{-}]\)*\)\s*\(=\)\=\s*\(.\{-}\)\=\s*;/\1@\2#@\3\4@\6@\7;@/e<CR>:'y,'zv/^\s*[*/]/s/\*\/\s*$/@*\//e<CR>:'y,'zv/^\s*[*/]/s/^\s\+\*/@@@@@* /e<CR>:'y,'zv/^\s*[*/]/s/^@@@@@\*\(.*[^*/]\)$/&@*/e<CR>'yjma'zk:AlignCtrl v ^\s*[*/#]<CR>:call AlignMaps#StdAlign(1)<cr>:'y,'zv/^\s*[*/]/s/@ //ge<CR>:'y,'zv/^\s*[*/]/s/\(\s*\);/;\1/e<CR>:'y,'zv/^#/s/#//e<CR>:'y,'zv/^\s\+[*/#]/s/\([^/*]\)\(\*\+\) \( *\)/\1\3\2 /e<CR>:'y,'zv/^\s\+[*/#]/s/\((\+\)\( \+\)\*/\2\1*/e<CR>:'y,'zv/^\s\+[*/#]/s/^\(\s\+\) \*/\1*/e<CR>:'y,'zv/^\s\+[*/#]/s/[ \t@]*$//e<CR>:'y,'zs/^[*]/ */e<CR><SID>WE
