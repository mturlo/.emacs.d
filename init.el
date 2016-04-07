;; global variables
(setq
 inhibit-startup-screen t
 create-lockfiles nil
 make-backup-files nil
 column-number-mode t
 scroll-error-top-bottom t
 show-paren-delay 0.5
 use-package-always-ensure t
 sentence-end-double-space nil)

;; buffer local variables
(setq-default
 indent-tabs-mode nil
 tab-width 4
 c-basic-offset 4)

;; modes
(electric-indent-mode 1)
(delete-selection-mode 1)

;; the package manager

; list the packages you want
(setq package-list
		'(use-package magit ensime scala-mode2 play-routes-mode yaml-mode helm spaceline idea-darkula-theme
			projectile))

; list the repositories containing them
(setq
 use-package-always-ensure t
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))       
    
; Ensime
(use-package ensime
  :commands ensime ensime-mode)

(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(setq exec-path (append exec-path '("/usr/local/bin")))

;; custom options

(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(set-default-font "Menlo 14")

;; helm
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode 1)
(setq helm-mode-fuzzy-match t)
(helm-autoresize-mode 1)

(load-theme 'idea-darkula t)

(require 'spaceline-config)
(spaceline-emacs-theme)
(spaceline-helm-mode)

; keys
(define-key global-map (kbd "RET") 'newline-and-indent)

; fun
(defun test-only ()
  "Run test with current file."
  (interactive)
  (sbt-command (concat "testOnly " (find-spec-name))))

(defun find-spec-name ()
  "Find spec name of current buffer."
  (concat "*." (file-name-sans-extension (file-name-nondirectory (buffer-name)))))

(defun compile-sbt-project ()
  "Compile the sbt project."
  (sbt-command "test:compile")
  )

(provide 'prelude-scala-sbt)

; projectile
(use-package projectile
  :demand
  ;; nice to have it on the modeline
  :init
  (setq projectile-use-git-grep t)
  :config
  (projectile-global-mode)
  (add-hook 'projectile-grep-finished-hook
            ;; not going to the first hit?
            (lambda () (pop-to-buffer next-error-last-buffer)))
  :bind
  (("s-f" . projectile-find-file)
   ("s-F" . projectile-grep)))