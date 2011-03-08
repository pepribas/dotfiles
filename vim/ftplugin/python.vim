setlocal tabstop=4
setlocal softtabstop=4
setlocal shiftwidth=4
setlocal textwidth=80
setlocal smarttab
setlocal expandtab
setlocal smartindent

" Python dict
if has("autocmd")
    autocmd FileType python set complete+=k~/.vim/syntax/python.vim isk+=.,(
endif
" automcomplete
set complete+=k~/.vim/pydiction-0.5/pydiction isk+=.,(
" Turn on completion:
set omnifunc=pythoncomplete#Complete
set completeopt-=preview
" " Map it to <Ctrl> + Space:
inoremap <Nul> <C-x><C-o>
