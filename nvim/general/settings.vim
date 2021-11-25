syntax enable
set encoding=utf8                " Set utf8 as standard encoding and 
                                 " en_US as the standard language.
set t_Co=256
set hidden
set number
set ruler
set smarttab
set shiftwidth=2
set conceallevel=0
set mouse=a
set nobackup
set cursorline
set expandtab                   " To enter spaces when tab is pressed.
set smarttab                    " To use smart tabs.
set autoindent                  " To copy indentation from current line 
                                " when starting a new line.
set si                          " To switch on smart indentation.

set showmatch                   " To show matching brackets when text indicator 
                                " is over them.

colorscheme onehalflight

nmap <C-f> :NERDTreeToggle<CR>

autocmd BufEnter * call ncm2#enable_for_buffer()    " To enable ncm2 for all buffers.
set completeopt=noinsert,menuone,noselect           " :help Ncm2PopupOpen for more
                                                    " information.
