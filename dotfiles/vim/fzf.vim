" FZF keymaps (requires Plug 'junegunn/fzf.vim')
nnoremap <leader>pf :Files<CR>
nnoremap <leader>pg :Git<CR>
nnoremap <leader>fo :History<CR>
nnoremap <leader>fb :Buffers<CR>
nnoremap <leader>fq :copen<CR>
nnoremap <leader>fh :Helptags<CR>

nnoremap <leader>fs :Rg <C-r><C-w><CR>

nnoremap <leader>ps :Rg<Space>

nnoremap <leader>fc :execute 'Rg ' . expand('%:t:r')<CR>

nnoremap <leader>fi :Files ~/nixos-config/dotfiles/vimrc<CR>
