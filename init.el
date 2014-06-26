;; This block is from: 
;; http://tim.dysinger.net/posts/2014-02-18-haskell-with-emacs.html
;; See comments in parentheses
;; NO FRILLS (remove toolbar)
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))
(setq inhibit-startup-screen t)
;; NO JUNK (remove local backups)
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `((".*" . ,temporary-file-directory)))
;; EL-GET (setup el-get)
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(defun el-get-sync-recipes (overlay)
  (let* ((recipe-glob (locate-user-emacs-file (concat overlay "/recipes/*.rcp")))
         (recipe-files (file-expand-wildcards recipe-glob))
         (recipes (mapcar 'el-get-read-recipe-file recipe-files)))
    (mapcar (lambda (r) (add-to-list 'el-get-sources r)) recipes)
    (el-get 'sync (mapcar 'el-get-source-name recipes))))
(setq el-get-user-package-directory user-emacs-directory)
;; EL-GET SYNC OVERLAYS (local el-get-haskell overlays)
(el-get-sync-recipes "el-get-haskell")
(el-get-sync-recipes "el-get-user")
;; CUSTOM FILE
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

;; el-get and elpa integration
(require 'el-get-elpa)

;; Theme
(el-get-install 'solarized-theme)
(load-theme 'solarized-light t)

;; Fix font when on Linux GUI
(if (eq system-type 'gnu/linux)
    (set-default-font "Monospace-9"))

;; Fix PATH when on OSX GUI
(if (eq system-type 'darwin)
    (el-get-install 'exec-path-from-shell)
    (when (memq window-system '(mac ns))
        (exec-path-from-shell-initialize)))
