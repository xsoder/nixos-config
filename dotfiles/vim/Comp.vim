" Comp.vim - A simple Emacs-style compilation mode for Vim

" Command: :Comp <command>
" Runs a shell command and sends its output to the quickfix list
command! -nargs=+ Comp call s:RunToQuickfix(<q-args>)

function! s:RunToQuickfix(...) abort
  let l:cmd = join(a:000, ' ')
  " Save current errorformat so we don't mess with user's settings
  let l:old_efm = &errorformat
  try
    " Set a generic errorformat that matches 'file:line:col: message'
    " You can customize this per command if needed
    set errorformat=%f:%l:%c:\ %m,%f:%l:\ %m

    " Run the command and capture output
    let l:output = system(l:cmd)

    " If the command failed, still populate quickfix
    if v:shell_error != 0
      echom "Command failed with exit code " . v:shell_error
    endif

    " Send to quickfix, parsed via errorformat
    cexpr l:output
    copen
  finally
    let &errorformat = l:old_efm
  endtry
endfunction

" Command: :CompVim <vimcmd>
" Runs a Vim command and sends any output to the quickfix list
command! -nargs=+ CompVim call s:RunVimToQuickfix(<q-args>)

function! s:RunVimToQuickfix(...) abort
  let l:cmd = join(a:000, ' ')
  redir => l:output
  silent execute l:cmd
  redir END

  if empty(l:output)
    echom "No output from command: " . l:cmd
    return
  endif

  " Dump into quickfix as raw lines — not navigable unless matches errorformat
  call setqflist(map(split(l:output, "\n"), '{ "text": v:val }'))
  copen
endfunction

