(defun init-message (msg)
  (message "(init.el) %s" msg))

(defun init-customize-basic-settings ()
  (init-message "Customizing basic settings")
  
  ;; Emacs/System clipboard integration
  (setq x-select-enable-clipboard t)
  
  (setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
	backup-directory-alist `((".*" . ,temporary-file-directory)))
  
  ;; Fix PATH when on OSX GUI
  (when (eq system-type 'darwin)
    (el-get-install 'exec-path-from-shell)
    (when (memq window-system '(mac ns))
      (exec-path-from-shell-initialize))))

(defun init-customize-el-get ()
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
  
  (setq
   my:el-get-packages
   '(el-get                  ; el-get is self-hosting
     solarized-emacs
     esup
     haskell-mode
     ProofGeneral4.3))
  (el-get 'sync my:el-get-packages))

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
  (ido-mode)
  (global-visual-line-mode t)
  (global-linum-mode 1)
  (global-hl-line-mode)
  (load-theme 'solarized-light t))  

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
  ;; Haskell mode is defined by the recipes in ~/.emacs.d/el-get-user/haskell,
  ;; which are cloned from https://github.com/dysinger/el-get/tree/haskell/recipes.

  ;; Setup interactive mode
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

(init-message "Start")
(init-customize-basic-settings)
(init-customize-el-get)
(init-customize-display)
(init-customize-bindings)
(init-customize-visual)
(init-customize-git)
(init-customize-haskell)
(init-message "End")