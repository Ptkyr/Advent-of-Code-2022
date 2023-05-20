filetype plugin indent on
execute pathogen#infect()

set clipboard=unnamed

set autoindent    "Automatic indentation

"Automatic close brace on enter
inoremap {<CR> {<CR>}<Esc>O
"Automatic closing parenthesis
inoremap ( ()<Left>

"Skip over closing parenthesis/brace
inoremap <expr> ) getline('.')[col('.') - 1] == ")" ? "\<Right>" : ")"

set incsearch     "Jump to next match while searching
set hlsearch      "Highlight all occurrences on search
set scrolloff=10  "Buffer around cursor
set tabstop=4     "Show existing tab with 4 spaces width
set shiftwidth=4  "When indenting with '>', use 4 spaces width
set expandtab     "On pressing tab, insert 4 spaces
set softtabstop=4 "Number of columns for a TAB
set number        "Show line numbers

set termguicolors "Only needed in terminals
set bg=dark
colorscheme gruvbox

set t_Co=256
"syntax on
"highlight Comment guifg=#0070a8
highlight Search guifg=#6998b3
