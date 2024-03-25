(load-file "/home/arekkas/configs/emacs/helper.el")

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
   (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq
   split-width-threshold 0
   split-height-threshold nil)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(setq visible-bell t)

(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)
(global-hl-line-mode 1)


(setq-default fill-column 80)

;; Font settings
(set-face-attribute 'default nil :font"Code New Roman Nerd Font" :height 120)


;; Initialize package sources
(require 'package)
(setq package-archives '(
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package
(unless (package-installed-p 'use-package)
 (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ------------------------------------------------------------------------------------------------
;;                                               ivy
;; ------------------------------------------------------------------------------------------------
(use-package swiper)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; ------------------------------------------------------------------------------------------------
;;                                             Themes
;; ------------------------------------------------------------------------------------------------
(use-package all-the-icons
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :init
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package doom-themes)

;; (load-theme 'doom-tomorrow-day t)
(add-to-list 'default-frame-alist '(foreground-color . "#000000"))
(add-to-list 'default-frame-alist '(background-color . "#FFFFFF"))


;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

;; Parentheses delimiter
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish
  :config
  (setq which-key-idle-delay 0.3))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind
  (("M-x" . counsel-M-x)
   ("C-x b" . counsel-ibuffer)
   ("C-x C-f" . counsel-find-file)
   :map minibuffer-local-map
   ("C-r" . 'counsel-minibuffer-history)))
 

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package general)

;; Evil mode
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))


;; ------------------------------------------------------------------------------------------------
;;                                               org-mode
;; ------------------------------------------------------------------------------------------------
(use-package org
  :config
  (setq org-ellipsis " ⤵"
	org-hide-emphasis-markers t)
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-files
	'("~/Documents/todo.org")))

(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :weight 'regular :height (cdr face)))


(use-package org-bullets
  :ensure t
  :init
  (setq org-bullets-bullet-list
  '("◎" "○" "◉" "⚫" "►" "◇"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Set theme



(setq display-buffer-alist
      '(("*R Dired"
	 (display-buffer-reuse-window display-buffer-at-bottom)
	 (window-width . 0.5)
	 (window-height . 0.25)
	 (reusable-frames . nil))
	("*R"
	 (display-buffer-reuse-window display-buffer-in-side-window)
	 (side . right)
	 (slot . -1)
	 (window-width . 0.5)
	 (reusable-frames . nil))
	("*Help"
	 (display-buffer-reuse-window display-buffer-in-side-window)
	 (side . right)
	 (slot . 1)
	 (window-width . 0.5)
	 (reusable-frames . nil))))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

; Parentheses
(use-package highlight-parentheses
  :ensure t
  :config
  (progn
    (highlight-parentheses-mode)
    (global-highlight-parentheses-mode))
  )

(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)

(use-package highlight-indent-guides
  :ensure t
  :config (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))
(setq highlight-indent-guides-method 'character)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.qmd\\'" . markdown-mode)
         ("\\.rmd\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (setq markdown-fontify-code-blocks-natively t))


(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))


;; ------------------------------------------------------------------------------------------------
;;                                                projectile
;; ------------------------------------------------------------------------------------------------
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Documents/projects")
    (setq projectile-project-search-path '("~/Documents/projects/")))
  (setq projectile-switch-project-action #'projectile-dired))

;; ------------------------------------------------------------------------------------------------
;;                                                lsp-mode
;; ------------------------------------------------------------------------------------------------
;; if spinner cannot install need to clone from github (https://github.com/Malabarba/spinner.el)
;; and install manually M-x package-install-file and navigate to cloned .el file
;; For R language server, we need install.packages("languageserver")
;; (use-package lsp-mode
;;   :ensure t
;;   :commands (lsp lsp-deferred)
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")
;;   :config
;;   (lsp-enable-which-key-integration t)
;;   :hook ((ess-r-mode . lsp)
;;      (lsp-mode . lsp-enable-which-key-integration))
;;   )

;; ------------------------------------------------------------------------------------------------
;;                                               python-mode
;; ------------------------------------------------------------------------------------------------
;; Will probably need to install pyright as from snap install --classic pyright
(use-package python-mode
  :ensure t)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))  ; or lsp-deferred


;; ------------------------------------------------------------------------------------------------
;;                                             Keybindings
;; ------------------------------------------------------------------------------------------------
;; Window resizing
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

;; Custom shortcuts
(global-set-key (kbd "C-c i q") 'surround-quote)
(global-set-key (kbd "C-c i b") 'surround-bracket)
(global-set-key (kbd "C-c b s") 'bookmark-set)
(global-set-key (kbd "C-c b j") 'bookmark-jump)
(global-set-key (kbd "C-c b d") 'bookmark-delete)
(global-set-key (kbd "C-c t w") 'whitespace-mode)
(global-set-key (kbd "C-c t c") 'company-mode)
(global-set-key (kbd "C-c t a") 'auto-fill-mode)
(global-set-key (kbd "C-c r q") 'ar-quarto-render)

;;; init.el ends here
