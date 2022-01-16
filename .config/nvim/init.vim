" This configuration was originally taken from a gist by mendeza. His
" config is clean and well organized, so it's a good starting point
" for me.
"
" (N)Vim Configuration File
" vim  : place in $HOME/.vimrc
" nvim : place in $HOME/.config/nvim/init.vim
" $ ln -s $HOME/.config/nvim/init.vim $HOME/.vimrc
" General settings
" https://learnvimscriptthehardway.stevelosh.com/
" ---------------------------------------------------------------------------
" drop vi support - kept for vim compatibility but not needed for nvim
" Probably not needed with Vim 8+
"set nocompatible


    " https://vim-bootstrap.com/
    let g:vim_bootstrap_langs = "c,go,javascript,lua,python,ruby"
    let g:vim_bootstrap_editor = "neovim"				" nvim or vim
    let g:vim_bootstrap_theme = "default"
    let g:vim_bootstrap_frams = ""

" Search recursively downward from CWD; provides TAB completion for filenames
" e.g., `:find vim* <TAB>`
set path+=**

" number of lines at the beginning and end of files checked for file-specific vars
set modelines=8

" reload files changed outside of Vim not currently modified in Vim (needs below)
set autoread

" http://stackoverflow.com/questions/2490227/how-does-vims-autoread-work#20418591
au FocusGained,BufEnter * :silent! !

" use Unicode
set encoding=utf-8

" errors flash screen rather than emit beep
set visualbell

" make Backspace work like Delete
set backspace=indent,eol,start

" don't create `filename~` backups
set nobackup

" don't create temp files
set noswapfile

" line numbers and distances
"set relativenumber 
set number 
set ruler

" number of lines offset when jumping
set scrolloff=4

" Tab key enters 4 spaces
" To enter a TAB character when `expandtab` is in effect,
" CTRL-v-TAB
set expandtab tabstop=4 shiftwidth=4 softtabstop=4 

" Indent new line the same as the preceding line
set autoindent

" statusline indicates insert or normal mode
set showmode showcmd

" make scrolling and painting fast
" ttyfast kept for vim compatibility but not needed for nvim
"set ttyfast lazyredraw

" highlight matching parens, braces, brackets, etc
set showmatch

" http://vim.wikia.com/wiki/Searching
set hlsearch incsearch ignorecase smartcase

" As opposed to `wrap`
set nowrap

" http://vim.wikia.com/wiki/Set_working_directory_to_the_current_file
set autochdir

" open new buffers without saving current modifications (buffer remains open)
set hidden

" http://stackoverflow.com/questions/9511253/how-to-effectively-use-vim-wildmenu
set wildmenu wildmode=list:longest,full

" non standard leader
let mapleader=','

" StatusLine always visible, display full path
" http://learnvimscriptthehardway.stevelosh.com/chapters/17.html
set laststatus=2 statusline=%F

" Use system clipboard
" http://vim.wikia.com/wiki/Accessing_the_system_clipboard
" for linux
set clipboard=unnamedplus
" for macOS
"set clipboard=unnamed

" Folding
" https://vim.fandom.com/wiki/Folding
" https://vim.fandom.com/wiki/All_folds_open_when_opening_a_file
" https://stackoverflow.com/questions/8316139/how-to-set-the-default-to-unfolded-when-you-open-a-file
set foldmethod=indent
set foldnestmax=1
set foldlevelstart=1

" netrw and vim-vinegar
let g:netrw_browse_split = 3

    " Plugins, syntax, and colors
    " ---------------------------------------------------------------------------
    " vim-plug
    " https://github.com/junegunn/vim-plug
    " Specify a directory for plugins
    " - For Neovim: ~/.local/share/nvim/plugged
    " - Avoid using standard Vim directory names like 'plugin'
    call plug#begin('~/.local/share/nvim/plugged')

    " Make sure to use single quotes
    " Install with `:PlugInstall`

    " merged from vim-bootstrap.com
    "
    " Plug 'jistr/vim-nerdtree-tabs'
    Plug 'tpope/vim-commentary'
    Plug 'tpope/vim-fugitive'
    Plug 'airblade/vim-gitgutter'
    Plug 'vim-scripts/grep.vim'
    Plug 'vim-scripts/CSApprox'
    Plug 'Raimondi/delimitMate'
    Plug 'majutsushi/tagbar'
    Plug 'dense-analysis/ale'
    Plug 'Yggdroot/indentLine'
    Plug 'editor-bootstrap/vim-bootstrap-updater'
    Plug 'tpope/vim-rhubarb' " required by fugitive to :Gbrowse

    Plug 'junegunn/fzf'
    Plug 'junegunn/fzf.vim'

    Plug 'xolox/vim-misc'
    Plug 'xolox/vim-session'

    " Plug 'ayu-theme/ayu-vim'
    Plug 'RRethy/nvim-base16'
    Plug 'fcpg/vim-fahrenheit'

    "
    " https://github.com/itchyny/lightline.vim
    Plug 'itchyny/lightline.vim'

    " https://github.com/tpope/vim-surround
    Plug 'tpope/vim-surround'

    " *** languages ***

    " c
    Plug 'vim-scripts/c.vim', {'for': ['c', 'cpp']}
    Plug 'ludwig/split-manpage.vim'

    " go
    "" Go Lang Bundle
    Plug 'fatih/vim-go', {'do': ':GoInstallBinaries'}

    " javascript
    "" Javascript Bundle
    Plug 'jelera/vim-javascript-syntax'

    " lua
    "" Lua Bundle
    Plug 'xolox/vim-lua-ftplugin'
    Plug 'xolox/vim-lua-inspect'

    " python
    "" Python Bundle
    Plug 'davidhalter/jedi-vim'
    Plug 'raimon49/requirements.txt.vim', {'for': 'requirements'}

    " ruby
    "Plug 'tpope/vim-rails'
    "Plug 'tpope/vim-rake'
    "Plug 'tpope/vim-projectionist'
    "Plug 'thoughtbot/vim-rspec'
    "Plug 'ecomba/vim-ruby-refactoring', {'tag': 'main'}

    " fortran
    "Plug 'rudrab/vimf90'
    " Initialize plugin system
    call plug#end()

syntax enable
" Neovim only
set termguicolors 

" Light scheme
"colorscheme night_owl_light

" Dark scheme
" colorscheme falcon
" colorscheme default
set termguicolors
"colorscheme base16-greenscreen
colorscheme fahrenheit
set background=dark

" Show character column
set colorcolumn=80

" https://medium.com/geekculture/neovim-configuration-for-beginners-b2116dbbde84
set splitright
set splitbelow

    " lightline config - add file 'absolutepath'
    " Delete colorscheme line below if using Dark scheme
    "      \ 'colorscheme': 'PaperColor_light',

    let g:lightline = {
                \ 'colorscheme': 'molokai',
                \ 'active': {
                    \   'left': [ [ 'mode', 'paste' ],
                    \             [ 'readonly', 'absolutepath', 'modified' ] ]
                    \ }
                    \ }

    let g:blamer_enabled = 1
    " %a is the day of week, in case it's needed
    let g:blamer_date_format = '%e %b %Y'
    highlight Blamer guifg=darkorange

" more changes pulled from vim-generate.com

set fileformats=unix,dos,mac

if exists('$SHELL')
    set shell=$SHELL
else
    set shell=/bin/sh
endif

" session management
let g:session_directory = "~/.config/nvim/session"
let g:session_autoload = "no"
let g:session_autosave = "no"
let g:session_command_aliases = 1

" mouse support
set mouse=a
set mousemodel=popup

" indentline
let g:indentLine_enabled = 1
let g:indentLine_concealCursor = 0
let g:indentLine_char = '|'
let g:indentLIne_faaster = 1

" vimf90
"let g:fortran_linter = 2
