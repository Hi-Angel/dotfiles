" WRN: make sure C-r is disabled at vimacs plugin

set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" Keep Plugin commands between vundle#begin/end.
" plugin on GitHub repo
Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'
Plugin 'L9'
Plugin 'FuzzyFinder'
Plugin 'AutoComplPop'
Plugin 'chrisbra/vim-diff-enhanced'

" cool grepping for vim
Plugin 'mileszs/ack.vim'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line

set nobackup
set number
set smartindent
set tabstop=4
set shiftwidth=4
set incsearch
imap <C-BS> <C-W>
nmap o <END>a<CR>i<BS><ESC>a
nmap D "_d
nmap <C-k> d$
set guioptions-=T "remove tool bar

syntax on
noremap j gj
noremap k gk
noremap $ g$
noremap ^ g^
map <C-x><C-f> :FufFileWithCurrentBufferDir<CR>
imap <C-x><C-f> <Esc>:FufFileWithCurrentBufferDir<CR>
map <C-x>b :FufBuffer<CR>
imap <C-x>b <Esc>:FufBuffer<CR>
map <C-x>k :bdelete
map <C-x><C-s> :w<CR>
imap <C-\> <C-^>
nmap <C-\> a<C-^><Esc>
imap <C-y> <Esc>"+pa
imap <M-n> <Down>
imap <M-p> <Up>

"adequate search'n'replce for visual mode
vnoremap : :<BS><BS><BS><BS><BS>s/\%V

"comfortable split usage
nmap <S-Left> <C-w>h
nmap <S-Right> <C-w>l
nmap <S-Up> <C-w>k
nmap <S-Down> <C-w>j
imap <S-Left> <Esc><C-w>ha
imap <S-Right> <Esc><C-w>la
imap <S-Up> <Esc><C-w>ka
imap <S-Down> <Esc><C-w>ja
nmap <C-x>1 <C-w><C-o>
nmap <C-x>2 :sp<CR>
nmap <C-x>3 :vsp<CR>
nmap <C-x>0 <C-w>q

nmap o A<CR>

" russian language
set keymap=russian-jcukenwin
set iminsert=0
set imsearch=0
highlight lCursor guifg=NONE guibg=Cyan

" set guifont=unifont\ 10
set guifont=Ubuntu\ Mono
autocmd FilterWritePre * if &diff | setlocal wrap< | endif

" russian spellcheck alias
cnoreabbrev rus setlocal spell spelllang=ru_RU

" changes to open really big files
let g:LargeFile = 1024 * 1024 * 10
augroup LargeFile
 autocmd BufReadPre * let f=getfsize(expand("<afile>")) | if f > g:LargeFile || f == -2 | call LargeFile() | endif
augroup END

function LargeFile()
 " no syntax highlighting etc
 set eventignore+=FileType
 " save memory when other file is viewed
 setlocal bufhidden=unload
 " is read-only (write with :w new_filename)
 setlocal buftype=nowrite
 " no undo possible
 setlocal undolevels=-1
 " display message
 autocmd VimEnter *  echo "The file is larger than " . (g:LargeFile / 1024 / 1024) . " MB, so some options are changed (see .vimrc for details)."
endfunction

" tcpdump highlight
call pathogen#infect()


:set relativenumber
:set norelativenumber  " turn relative line numbers off
:set relativenumber!   " toggle relative line numbers
:set hlsearch " highlighting all search occurences
:set numberwidth=1 " to make vim to not align number to right very often

" ---------
" by default vim has bad aliasing functional, so use this instead
:source ~/.vim/cmdalias.vim
:Alias ack Ack
noremap gs "jyiw:Ack <C-r>j .<CR>
