colorscheme desert

syntax on
filetype plugin indent on

set whichwrap=<,>,[,],b,
set backspace=indent,eol,start
set ruler
set background=dark
set autoread
" Turn backup off, since most stuff is in SVN, git et.c anyway...
set nobackup
set nowb
set noswapfile

" But also have C-like languages use C spacing
" Thanks much jdhore
set ai
au BufRead,BufNewFile *.c,*.h,*.cpp,*.cxx,*.hpp,*.cc,*.c++,*.hh,*.hxx,*.ipp,*.moc,*.tcc,*.inl set cindent
au BufRead,BufNewFile *.c,*.h,*.cpp,*.cxx,*.hpp,*.cc,*.c++,*.hh,*.hxx,*.ipp,*.moc,*.tcc,*.inl set tabstop=8
au BufRead,BufNewFile *.c,*.h,*.cpp,*.cxx,*.hpp,*.cc,*.c++,*.hh,*.hxx,*.ipp,*.moc,*.tcc,*.inl set shiftwidth=8
set cinoptions=>s,e0,n0,f0,{0,}0,^0,=s,ps,t0,c3,+s,(2s,us,)20,*30,gs,hs

" Python spacing in python
autocmd Filetype python setlocal expandtab tabstop=4 shiftwidth=4

au Filetype lolcode setl et ts=4 sw=4

au BufRead APKBUILD setl noexpandtab softtabstop=0 tabstop=4 shiftwidth=4 nosmarttab

" status line
set ls=2
set statusline=%F%m%r%h%w\ >\ FORMAT=%{&ff}\ >\ TYPE=%Y\ >\ BUF=\#%n\ <\ POS=%04l,%04v\ <\ %p%%\ <\ LEN=%L

" wildmenu
set wildmenu
set wildignore=*.o,*~,*.pyc

" Delete extra spaces 4 at a time
:highlight ExtraWhitespace ctermbg=red guibg=red
:match ExtraWhitespace /\s\+$/
:match ExtraWhitespace /\s\+$\| \+\ze\t/

set pastetoggle=<F9>
map <F3> gg=G:w<cr>

" Lvimrc
" if .lvimrc exists in parent directory of loaded file, load it as config
let lvimrc_path = expand('%:p:h') . '/.lvimrc'
if filereadable(lvimrc_path)
        execute 'so' lvimrc_path
endif

" Color column definition
let &colorcolumn="80,100, 110"
highlight ColorColumn ctermbg=52

" Delete trailing white space on save, useful for Python and CoffeeScript ;)
func! DeleteTrailingWS()
  exe "normal mz"
  %s/\s\+$//ge
  exe "normal `z"
endfunc
autocmd BufWrite *.py :call DeleteTrailingWS()
" escape keychord
imap fd <esc>
" Spellchecking
map <leader>ss :setlocal spell!<cr>
" Spacemacs like functionality
nnoremap <SPACE> <Nop>
let mapleader = " "
nmap <LEADER>bb :buffers<CR>
nmap <LEADER>bd :bdelete<CR>
nmap <LEADER>bn :bn<CR>
nmap <LEADER>bp :bp<CR>
nmap <LEADER>fed :e ~/.vimrc<CR>
nmap <LEADER>feR :source ~/.vimrc<CR>
nmap <LEADER>ff :e
nmap <LEADER>fr :CtrlPMRU<CR>
nmap <LEADER>fs :w<CR>
nmap <LEADER>fS :wa<CR>
nmap <LEADER>gs :!git status<CR>
nmap <LEADER>qq :qa<CR>
nmap <Leader>qQ :qa!<CR>
nmap <Leader>qs :xa<CR>
nmap <Leader>sc :noh<CR>>
nmap <LEADER>sp :Ag<SPACE>
nmap <LEADER>tn :set number!<CR>
nmap <Leader>Td :GitGutterToggle<CR>
nmap <LEADER>w- :sp<CR>
nmap <LEADER>w/ :vsp<CR>
nmap <LEADER>w= <C-W>=
nmap <LEADER>wc :q<CR>
nmap <LEADER>wh <C-W>h
nmap <LEADER>wj <C-W>j
nmap <LEADER>wk <C-W>k
nmap <LEADER>wl <C-W>l
nmap <LEADER>ws <C-W>s
nmap <LEADER>wv <C-W>v
nmap <LEADER>wm :MaximizerToggle<CR>
nmap <LEADER>ww <C-W><C-W>
