;;
;; init.el
;;

(require 'server)
(unless (server-running-p)
  (server-start))

;; package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;;(add-hook 'js2-mode-hook js2-mode)
(add-hook 'after-init-hook (lambda () (load "~/.emacs.d/after-init.el")))

(require 'cl)
(defvar installing-package-list
  '(
    ;; package list
    helm
    auto-complete
    ;; ac-js2
    ;; ac-helm
    js2-mode
    ;; solarized-theme
    zenburn-theme
    anzu
    ;; yascroll
    volatile-highlights
    ace-jump-mode
    undo-tree
    tabbar
    highlight-symbol
    diminish
    web-mode
    jade-mode
    expand-region
    ;; hlinum
    ;; smooth-scrolling
    gitignore-mode
    projectile
    helm-projectile
    ))
(let ((not-installed (loop for x in installing-package-list
                             when (not (package-installed-p x))
                             collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
      (package-install pkg))))
