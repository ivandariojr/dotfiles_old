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
set backupdir=~/.vim/backup

" do swap files
set directory=~/.vim/tmp

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

" show "insert", "visual", or "overwrite" in status line
set showmode

" show partial command in status line
set showcmd 

" use a visual bell instead of beeping
set visualbell

" show buffer position in status line
set ruler 

" always show status line
set laststatus=2 

" show line numbers to the left - 4 characters, relative offset
set number " show line numbers to the left
set numberwidth=4
set relativenumber

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

" store undo information between sessions
set undofile 
