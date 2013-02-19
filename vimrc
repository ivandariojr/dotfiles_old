"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""                                                                 ""
""  General stuff                                                  ""
""                                                                 ""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" don't emulate vi bugs!
set nocompatible

" do backups
set backup
set backupdir=~/.vim/backup//

" do undo files
if exists("undofile")
    set undofile 
    set undodir=~/.vim/undo//
endif

" do swap files
set dir=~/.vim/swap//,/var/tmp//,/tmp//,.

" load libraries
execute pathogen#infect()
filetype plugin indent on

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""                                                                 ""
""  Appearance                                                     ""
""                                                                 ""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" do syntax highlighting
syntax enable

" always show at least three lines before and after the cursor
set scrolloff=3

" use a visual bell instead of beeping
set visualbell

" show line numbers to the left - 3 characters, relative offset
set number " show line numbers to the left
set numberwidth=3
if exists("+relativenumber")
    set relativenumber
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""                                                                 ""
""  Behavior                                                       ""
""                                                                 ""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" do indentation
set autoindent
set smartindent

" can background buffers without saving them
set hidden 

" better tab-completion in commands
set wildmenu 
set wildmode=list:longest " complete using the longest common string in the cmd

" use four spaces for tabs
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab

" backspace goes over indents, EOLs, and starts.
set backspace=indent,eol,start 

" tell vim that we're using a really fast terminal
set ttyfast 

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""                                                                 ""
""  Keybindings                                                    ""
""                                                                 ""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" go by visible line rather than file line
nmap j gj
nmap k gk
