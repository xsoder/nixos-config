" ============================================================================
" Emacs Compilation Mode for Vim
" Full recreation of Emacs compilation-mode functionality
" ============================================================================

if exists('g:loaded_compilation_mode')
  finish
endif
let g:loaded_compilation_mode = 1

" Configuration variables
let g:compilation_auto_jump = get(g:, 'compilation_auto_jump', 1)
let g:compilation_scroll_output = get(g:, 'compilation_scroll_output', 1)
let g:compilation_window_height = get(g:, 'compilation_window_height', 15)

" State tracking
let s:compilation_buffer = -1
let s:compilation_process = -1
let s:compilation_directory = ''
let s:compilation_command = ''
let s:compilation_start_time = 0

" ============================================================================
" Core Compilation Functions
" ============================================================================

function! s:CreateCompilationBuffer()
  " Create or reuse compilation buffer
  if bufexists(s:compilation_buffer) && bufnr(s:compilation_buffer) != -1
    let winnr = bufwinnr(s:compilation_buffer)
    if winnr != -1
      execute winnr . 'wincmd w'
      %delete _
    else
      execute 'silent! botright ' . g:compilation_window_height . 'split'
      execute 'buffer ' . s:compilation_buffer
      %delete _
    endif
  else
    execute 'silent! botright ' . g:compilation_window_height . 'new'
    setlocal buftype=nofile
    setlocal bufhidden=hide
    setlocal noswapfile
    setlocal nowrap
    setlocal nonumber
    setlocal norelativenumber
    setlocal foldcolumn=0
    setlocal signcolumn=no
    file [Compilation]
    let s:compilation_buffer = bufnr('%')
  endif
  
  " Set compilation-specific settings
  setlocal filetype=compilation
  setlocal errorformat=%f:%l:%c:\ %m,%f:%l:\ %m
  
  " Set up key mappings
  nnoremap <buffer> <CR> :call <SID>JumpToError()<CR>
  nnoremap <buffer> <C-c><C-c> :call <SID>KillCompilation()<CR>
  nnoremap <buffer> g :call <SID>Recompile()<CR>
  nnoremap <buffer> n :call <SID>NextError()<CR>
  nnoremap <buffer> p :call <SID>PreviousError()<CR>
  nnoremap <buffer> q :call <SID>CloseCompilation()<CR>
  nnoremap <buffer> h :call <SID>ShowHelp()<CR>
endfunction

function! s:RunCompilation(cmd, use_emacs)
  " Removed s:SaveBuffers()

  let s:compilation_command = a:cmd
  let s:compilation_directory = getcwd()
  let s:compilation_start_time = localtime()
  
  call s:CreateCompilationBuffer()
  
  " Print header
  call append(line('$'), '-*- mode: compilation; default-directory: "' . s:compilation_directory . '" -*-')
  call append(line('$'), 'Compilation started at ' . strftime('%c'))
  call append(line('$'), '')
  call append(line('$'), 'Command: ' . a:cmd)
  call append(line('$'), repeat('=', 80))
  call append(line('$'), '')
  
  " Determine command to run
  let cmd = a:cmd
  if a:use_emacs
    " Emacs-style command processing
    let cmd = s:ProcessEmacsCommand(a:cmd)
  endif
  
  " Run command asynchronously if available
  if has('job') && has('channel')
    call s:RunAsync(cmd)
  else
    call s:RunSync(cmd)
  endif
endfunction

function! s:ProcessEmacsCommand(cmd)
  " Process Emacs Lisp-style commands
  " Convert common Emacs commands to shell equivalents
  let cmd = a:cmd
  
  " (compile "make") -> make
  if cmd =~# '^(compile\s\+".\{-}")'
    let cmd = matchstr(cmd, 'compile\s\+"\zs.\{-}\ze"')
  endif
  
  " (shell-command "...") -> ...
  if cmd =~# '^(shell-command\s\+".\{-}")'
    let cmd = matchstr(cmd, 'shell-command\s\+"\zs.\{-}\ze"')
  endif
  
  return cmd
endfunction

function! s:RunAsync(cmd)
  let opts = {
        \ 'out_cb': function('s:OnOutput'),
        \ 'err_cb': function('s:OnOutput'),
        \ 'exit_cb': function('s:OnExit'),
        \ 'out_mode': 'nl',
        \ 'err_mode': 'nl'
        \ }
  
  let s:compilation_process = job_start([&shell, &shellcmdflag, a:cmd], opts)
  
  if job_status(s:compilation_process) ==# 'fail'
    call s:AppendOutput('Failed to start compilation process')
    call s:FinishCompilation(1)
  endif
endfunction

function! s:RunSync(cmd)
  let output = systemlist(a:cmd)
  let exit_code = v:shell_error
  
  for line in output
    call s:AppendOutput(line)
  endfor
  
  call s:FinishCompilation(exit_code)
endfunction

function! s:OnOutput(channel, msg)
  call s:AppendOutput(a:msg)
endfunction

function! s:OnExit(job, status)
  call s:FinishCompilation(a:status)
endfunction

function! s:AppendOutput(text)
  if !bufexists(s:compilation_buffer)
    return
  endif
  
  let current_win = winnr()
  let comp_win = bufwinnr(s:compilation_buffer)
  
  if comp_win != -1
    execute comp_win . 'wincmd w'
    call append(line('$'), a:text)
    
    " Auto-scroll if enabled
    if g:compilation_scroll_output
      normal! G
    endif
    
    execute current_win . 'wincmd w'
  else
    call setbufline(s:compilation_buffer, line('$') + 1, a:text)
  endif
endfunction

function! s:FinishCompilation(exit_code)
  let elapsed = localtime() - s:compilation_start_time
  
  call s:AppendOutput('')
  call s:AppendOutput(repeat('=', 80))
  
  if a:exit_code == 0
    call s:AppendOutput('Compilation finished successfully')
  else
    call s:AppendOutput('Compilation exited abnormally with code ' . a:exit_code)
  endif
  
  call s:AppendOutput('Time elapsed: ' . elapsed . ' seconds')
  call s:AppendOutput('Finished at ' . strftime('%c'))
  
  " Parse errors and populate quickfix
  call s:ParseErrors()
  
  " Auto-jump to first error if enabled
  if g:compilation_auto_jump && !empty(getqflist())
    cfirst
  endif
endfunction

function! s:ParseErrors()
  if !bufexists(s:compilation_buffer)
    return
  endif
  
  " Get all buffer lines
  let lines = getbufline(s:compilation_buffer, 1, '$')
  
  " Build error list
  let errors = []
  
  for line in lines
    " Skip header/footer lines
    if line =~# '^-\*-' || line =~# '^=\+$' || line =~# '^Compilation' || line =~# '^Command:' || line =~# '^Time elapsed:' || line =~# '^Finished at'
      continue
    endif
    
    " Match various error formats
    " Format: filename:line:col: message
    let match = matchlist(line, '\v^([^:]+):(\d+):(\d+):\s*(.*)$')
    if !empty(match) && len(match) >= 5
      call add(errors, {
            \ 'filename': match[1],
            \ 'lnum': str2nr(match[2]),
            \ 'col': str2nr(match[3]),
            \ 'text': match[4],
            \ 'bufnr': 0,
            \ 'pattern': ''
            \ })
      continue
    endif
    
    " Format: filename:line: message
    let match = matchlist(line, '\v^([^:]+):(\d+):\s*(.*)$')
    if !empty(match) && len(match) >= 4
      call add(errors, {
            \ 'filename': match[1],
            \ 'lnum': str2nr(match[2]),
            \ 'col': 0,
            \ 'text': match[3],
            \ 'bufnr': 0,
            \ 'pattern': ''
            \ })
      continue
    endif
  endfor
  
  " Set quickfix list
  call setqflist(errors, 'r')
  
  if !empty(errors)
    echo 'Found ' . len(errors) . ' error(s)/warning(s)'
  endif
endfunction

" ============================================================================
" Navigation Functions
" ============================================================================

function! s:JumpToError()
  let line = getline('.')
  
  " Try to extract file:line:col
  let match = matchlist(line, '\v^([^:]+):(\d+):(\d+):')
  if empty(match)
    let match = matchlist(line, '\v^([^:]+):(\d+):')
  endif
  
  if !empty(match) && len(match) >= 3
    let fname = match[1]
    let line_nr = str2nr(match[2])
    let col_nr = 1
    if len(match) >= 4 && !empty(match[3])
      let col_nr = str2nr(match[3])
    endif
    
    " Resolve relative paths
    if fname !~# '^/'
      let fname = s:compilation_directory . '/' . fname
    endif
    
    if filereadable(fname)
      " Jump to the file
      wincmd p
      execute 'edit ' . fnameescape(fname)
      execute line_nr
      execute 'normal! ' . col_nr . '|'
      normal! zz
    else
      echo 'File not found: ' . fname
    endif
  endif
endfunction

function! s:NextError()
  try
    cnext
  catch
    echo 'No more errors'
  endtry
endfunction

function! s:PreviousError()
  try
    cprevious
  catch
    echo 'No previous errors'
  endtry
endfunction

" ============================================================================
" Control Functions
" ============================================================================

function! s:KillCompilation()
  if s:compilation_process != -1 && job_status(s:compilation_process) ==# 'run'
    call job_stop(s:compilation_process, 'term')
    call s:AppendOutput('')
    call s:AppendOutput('*** Compilation killed ***')
    let s:compilation_process = -1
  else
    echo 'No compilation process running'
  endif
endfunction

function! s:Recompile()
  if !empty(s:compilation_command)
    call s:RunCompilation(s:compilation_command, 0)
  else
    echo 'No previous compilation command'
  endif
endfunction

function! s:CloseCompilation()
  let winnr = bufwinnr(s:compilation_buffer)
  if winnr != -1
    execute winnr . 'wincmd w'
    close
  endif
endfunction

function! s:ShowHelp()
  echo "Compilation Mode Keys:\n"
  echo "  <CR>      - Jump to error at cursor\n"
  echo "  n         - Next error\n"
  echo "  p         - Previous error\n"
  echo "  g         - Recompile\n"
  echo "  C-c C-c   - Kill compilation\n"
  echo "  q         - Close compilation window\n"
  echo "  h         - Show this help"
endfunction

" ============================================================================
" Public Commands
" ============================================================================

function! Compile(cmd)
  call s:RunCompilation(a:cmd, 0)
endfunction

function! EmacsCompile(cmd)
  call s:RunCompilation(a:cmd, 1)
endfunction

" Command definitions
command! -nargs=+ -complete=shellcmd Compile call Compile(<q-args>)
command! -nargs=+ EmacsCompile call EmacsCompile(<q-args>)
command! Recompile call s:Recompile()
command! KillCompilation call s:KillCompilation()
command! NextError call s:NextError()
command! PreviousError call s:PreviousError()

" Default key mappings (can be disabled with g:compilation_mode_no_mappings)
if !exists('g:compilation_mode_no_mappings') || !g:compilation_mode_no_mappings
  nnoremap <leader>cc :Compile<Space>
  nnoremap <leader>cr :Recompile<CR>
  nnoremap <leader>ck :KillCompilation<CR>
  nnoremap <leader>cn :NextError<CR>
  nnoremap <leader>cp :PreviousError<CR>
endif

" Syntax highlighting for compilation buffer
augroup CompilationMode
  autocmd!
  autocmd FileType compilation call s:SetupCompilationHighlight()
augroup END

function! s:SetupCompilationHighlight()
  syntax match CompilationError /\v^[^:]+:\d+:(\d+:)?\s*(error|ERROR).*/
  syntax match CompilationWarning /\v^[^:]+:\d+:(\d+:)?\s*(warning|WARNING).*/
  syntax match CompilationInfo /\v^[^:]+:\d+:(\d+:)?\s*(note|info|INFO).*/
  syntax match CompilationFile /\v^[^:]+:\d+/
  syntax match CompilationHeader /^-\*-.*-\*-$/
  syntax match CompilationSeparator /^=\+$/
  
  highlight link CompilationError ErrorMsg
  highlight link CompilationWarning WarningMsg
  highlight link CompilationInfo Comment
  highlight link CompilationFile Identifier
  highlight link CompilationHeader Title
  highlight link CompilationSeparator Comment
endfunction
