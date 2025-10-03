set number
set relativenumber

filetype plugin indent on
set expandtab
set shiftwidth=4
set softtabstop=4
set tabstop=4
set smartindent 

syntax on
set laststatus=2

command! -nargs=1 Run call RunToQuickfix(<q-args>)

function! RunToQuickfix(cmd)
  let output = system(a:cmd)
  " You can tweak errorformat to parse the output properly
  cexpr split(output, "\n")
  copen
endfunction

