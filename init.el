(defun init-message (msg)
  (message "(init.el) %s" msg))

(defun init-install-packages ()
  (init-message "Initializing packages")
  
  (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
  (unless (require 'el-get nil 'noerror)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
      (goto-char (point-max))
      (eval-print-last-sexp)))

  (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user")
  (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/haskell")
  (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/coq")

  (require 'el-get-elpa)
  ;; Build elpa recipes if they don't exist
  (unless (file-directory-p el-get-recipe-path-elpa)
    (el-get-elpa-build-local-recipes))

  ;; Fix PATH when on OSX GUI. This must be done before
  ;; installing other packages to ensure the brew versions of GNU
  ;; tools are used as sometimes they are when necessary.
  (when (eq system-type 'darwin)
    (el-get 'sync 'exec-path-from-shell)
    (require 'exec-path-from-shell)
    (when (memq window-system '(mac ns))
      (exec-path-from-shell-initialize)))  

  (setq
   my:el-get-packages
   '(el-get                  ; el-get is self-hosting
     solarized-emacs
     idle-highlight-mode
     esup
     haskell-mode
     ProofGeneral4.3))
  (el-get 'sync my:el-get-packages))

(defun init-customize-basic-settings ()
  (init-message "Customizing basic settings")
  
  ;; Emacs/System clipboard integration
  (setq x-select-enable-clipboard t)
  
  (setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
	backup-directory-alist `((".*" . ,temporary-file-directory))))

(defun init-customize-display ()
  (init-message "Customizing display")
  
  (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1)))

  ;; Window size
  (add-to-list 'default-frame-alist (cons 'width 90))
  (add-to-list 'default-frame-alist (cons 'height 50))
  (setq inhibit-startup-screen t)
  (setq scroll-step            1
	scroll-conservatively  10000)
  (setq initial-scratch-message "")

  ;; Fix font when on Linux GUI
  (when (eq system-type 'gnu/linux) 
    (set-default-font "Monospace-9")
    (require 'iso-transl)))

(defun init-customize-bindings ()
  (init-message "Customizing bindings")
  
  (cua-mode)
  (global-set-key (kbd "C-,") 'switch-to-buffer)
  (global-set-key (kbd "C-.") 'delete-other-windows)  

  (defun comment ()
    (interactive)
    (let ((start (line-beginning-position))
	  (end (line-end-position)))
      (when (region-active-p)
	(setq start (save-excursion
		      (goto-char (region-beginning))
		      (beginning-of-line)
		      (point))
	      end (save-excursion
		    (goto-char (region-end))
		    (end-of-line)
		    (point))))
      (comment-or-uncomment-region start end)))
  (global-set-key (kbd "M-;") 'comment)
  
  (global-set-key (kbd "C-x C-b") 'ibuffer))

(defun init-customize-visual ()
  (init-message "Customizing visual")
  
  (ido-mode)
  (global-visual-line-mode t)
  (global-hl-line-mode)
  (global-linenum-mode 0)  
  (show-paren-mode)
  (blink-cursor-mode 0)
  (setq-default cursor-type 'bar)

  (define-globalized-minor-mode global-idle-highlight-mode
    idle-highlight-mode idle-highlight-mode)
  (global-idle-highlight-mode t)
  
  (setq idle-highlight-idle-time 0)
  (load-theme 'solarized-light t)

  ;; Region selection with less constrast
  (set-face-attribute 'region nil :background "#d3d3d3" :foreground "#000000")

  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(idle-highlight ((t (:background "light gray"))))))  

(defun init-customize-git ()
  (init-message "Customizing git")
  
  ;; Disable git-commit flyspell
  (eval-after-load "git-commit-mode"
    '(cond
      ((boundp 'git-commit-mode-hook) ; old
       (remove-hook 'git-commit-mode-hook 'flyspell-mode))
      ((boundp 'git-commit-setup-hook) ; new
       (remove-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell))))

  ;; Disable "Loading vc-git"
  (setq vc-handled-backends nil))

(defun init-customize-haskell ()
  (init-message "Customizing Haskell")
  
  ;; Haskell mode is defined by the recipe in ~/.emacs.d/el-get-user/haskell.

  ;; Setup interactive mode
  (setq haskell-process-path-cabal "~/.cabal/bin/cabal")
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

(defun init-customize-coq ()
  (init-message "Customizing Coq")
  
  (setq proof-electric-terminator-enable 1))

(init-message "Start")
(init-install-packages)
(init-customize-basic-settings)
(init-customize-display)
(init-customize-bindings)
(init-customize-visual)
(init-customize-git)
(init-customize-haskell)
(init-customize-coq)
(init-message "End")
