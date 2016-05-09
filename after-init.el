;; キーバインド
(define-key global-map "\C-h" 'delete-backward-char) ; 削除
(define-key global-map "\M-?" 'help-for-help)        ; ヘルプ
(define-key global-map "\C-z" 'undo)                 ; undo
(define-key global-map "\C-\\" nil) ; \C-\の日本語入力の設定を無効にする
(define-key global-map (kbd "C-M-n") 'next-multiframe-window)     ; 次のウィンドウへ移動
(define-key global-map (kbd "C-M-p") 'previous-multiframe-window) ; 前のウィンドウへ移動

;; 行頭と非空白文字の先頭を行ったり来たり
(defun move-beginning-alt()
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))
(define-key global-map "\C-a" 'move-beginning-alt)

;; ウィンドウサイズ
(setq initial-frame-alist
      '((width . 120) (height . 45)))

;; ツールバー非表示
(tool-bar-mode 0)

;; スクロールバー
(set-scroll-bar-mode nil)

;; カーソル位置表示
(column-number-mode t)
(line-number-mode t)

;; 行の先頭でC-kを一回押すだけで行全体を消去する
(setq kill-whole-line t)

;; カーソル位置保存
(require 'saveplace)
(setq-default save-place t)
(setq inhibit-startup-message t)

;; バッファ自動再読み込み
(global-auto-revert-mode 1)

;; バックアップファイルを作らない
;; (setq backup-inhibited t)
;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)

;; 行番号表示
(custom-set-variables '(global-linum-mode t))

;; 現在行のハイライト
(defface hlline-face
  '((((class color)
      (background dark))
     (:background "gray30"))
    (((class color)
      (background light))
     (:background "#98FB98"))
    (t
     ()))
  "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)
(global-hl-line-mode)

;; カッコ強調表示
(show-paren-mode)
(setq show-paren-delay 0)
(setq show-paren-style 'mixed)

;; カッコ自動挿入
(electric-pair-mode)

;; オートインデントでスペースを使う
(setq-default indent-tabs-mode nil)

;; 空白や長すぎる行を視覚化する。
(require 'whitespace)
;; 1行が80桁を超えたら長すぎると判断する。
(setq whitespace-line-column 110)
(setq whitespace-style '(face              ; faceを使って視覚化する。
                         trailing          ; 行末の空白を対象とする。
                         lines-tail        ; 長すぎる行のうち
                                           ; whitespace-line-column以降のみを
                                           ; 対象とする。
                         tabs              ; タブ文字
                         tab-mark          ; タブ文字
                         space-mark        ; 全角スペース
                         space-before-tab  ; タブの前にあるスペースを対象とする。
                         space-after-tab)) ; タブの後にあるスペースを対象とする。
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '(;;(space-mark ?\x3000 [?\□])
        (tab-mark   ?\t     [?\xBB ?\t])
        ))

;; デフォルトで視覚化を有効にする。
(global-whitespace-mode 1)

;; ファイル末尾に点線を表示する
(setq-default indicate-empty-lines t)
;; ファイル末尾右に記号を表示する(┏：末尾、┛：改行が入力されている)
(setq-default indicate-buffer-boundaries 'right)

;; 行末の空白を削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; 最後に改行を入れる
(setq require-final-newline t)

;; color theme
;;(load-theme 'solarized-dark t)
(load-theme 'zenburn t)

;; helm
(helm-mode 1)

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; ac-helm
;(require 'ac-helm)
;(global-set-key (kbd "C-:") 'ac-complete-with-helm)
;(define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-helm)
;(ac-complete-with-helm)

;; js2-mode
(custom-set-variables
  '(js2-allow-rhino-new-expr-initializer nil)
  '(js2-include-browser-externs nil)
  '(js2-include-node-externs t)
  '(js2-indent-switch-body t)
  '(js2-strict-trailing-comma-warning nil)
  )

;; anzu
(require 'anzu)
(global-anzu-mode +1)
(custom-set-variables
  '(anzu-mode-lighter "")
  '(anzu-deactivate-region t)
  '(anzu-search-threshold 1000))

;; yascroll
(require 'yascroll)
(global-yascroll-bar-mode 1)

;; volatile-highlights
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; ace-jump-mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; undo-tree
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)

;; tabbar
(require 'tabbar)
(tabbar-mode 1)

;; タブ上でマウスホイール操作無効
;;(tabbar-mwheel-mode -1)

;; グループ化しない
(setq tabbar-buffer-groups-function nil)

;; 左に表示されるボタンを無効化
(dolist (btn '(tabbar-buffer-home-button
               tabbar-scroll-left-button
               tabbar-scroll-right-button))
  (set btn (cons (cons "" nil)
                 (cons "" nil))))

;; タブ同士の間隔
(setq tabbar-separator '(0.8))

;; 外観変更
(set-face-attribute
  'tabbar-default nil
  :family (face-attribute 'default :family)
  :background (face-attribute 'mode-line-inactive :background)
  :height 0.9)
(set-face-attribute
  'tabbar-unselected nil
  :background (face-attribute 'mode-line-inactive :background)
  :foreground (face-attribute 'mode-line-inactive :foreground)
  :box nil)
(set-face-attribute
  'tabbar-selected nil
  :background (face-attribute 'mode-line :background)
  :foreground (face-attribute 'mode-line :foreground)
  :box nil)

;; タブに表示させるバッファの設定
(defvar my-tabbar-displayed-buffers
  '("*scratch*" "*Messages*")
  "*Regexps matches buffer names always included tabs.")

(defun my-tabbar-buffer-list ()
  "Return the list of buffers to show in tabs.
Exclude buffers whose name starts with a space or an asterisk.
The current buffer and buffers matches `my-tabbar-displayed-buffers'
are always included."
  (let* ((hides (list ?\  ?\*))
         (re (regexp-opt my-tabbar-displayed-buffers))
         (cur-buf (current-buffer))
         (tabs (delq nil
                     (mapcar (lambda (buf)
                               (let ((name (buffer-name buf)))
                                 (when (or (string-match re name)
                                           (not (memq (aref name 0) hides)))
                                   buf)))
                             (buffer-list)))))
    ;; Always include the current buffer.
    (if (memq cur-buf tabs)
        tabs
      (cons cur-buf tabs))))

(setq tabbar-buffer-list-function 'my-tabbar-buffer-list)

;; タブ上をマウス中クリックで kill-buffer
(defun my-tabbar-buffer-help-on-tab (tab)
  "Return the help string shown when mouse is onto TAB."
  (if tabbar--buffer-show-groups
      (let* ((tabset (tabbar-tab-tabset tab))
             (tab (tabbar-selected-tab tabset)))
        (format "mouse-1: switch to buffer %S in group [%s]"
                (buffer-name (tabbar-tab-value tab)) tabset))
    (format "\
mouse-1: switch to buffer %S\n\
mouse-2: kill this buffer\n\
mouse-3: delete other windows"
            (buffer-name (tabbar-tab-value tab)))))

(defun my-tabbar-buffer-select-tab (event tab)
  "On mouse EVENT, select TAB."
  (let ((mouse-button (event-basic-type event))
        (buffer (tabbar-tab-value tab)))
    (cond
     ((eq mouse-button 'mouse-2)
      (with-current-buffer buffer
        (kill-buffer)))
     ((eq mouse-button 'mouse-3)
      (delete-other-windows))
     (t
      (switch-to-buffer buffer)))
    ;; Don't show groups.
    (tabbar-buffer-show-groups nil)))

(setq tabbar-help-on-tab-function 'my-tabbar-buffer-help-on-tab)
(setq tabbar-select-tab-function 'my-tabbar-buffer-select-tab)

(global-set-key (kbd "<C-next>") 'tabbar-forward-tab)
(global-set-key (kbd "<C-prior>") 'tabbar-backward-tab)

;; highlight-symbol
;;(require 'auto-highlight-symbol-config)
(require 'highlight-symbol)
(setq highlight-symbol-colors '("red4"
                                "DarkOrange4"
                                "gold4"
                                "chartreuse4"
                                "turquoise4"
                                "DarkOrchid4"
                                "PaleVioletRed4"
                                ))
(global-set-key (kbd "<f3>") 'highlight-symbol-at-point)
(global-set-key (kbd "M-<f3>") 'highlight-symbol-remove-all)

;; diminish
(require 'diminish)
(diminish 'undo-tree-mode)
(diminish 'auto-complete-mode)
(diminish 'helm-mode)
(diminish 'volatile-highlights-mode)
(diminish 'global-whitespace-mode)

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; jade-mode
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.jade" . jade-mode))

;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "M-=") 'er/contract-region)

;; hlinum
;(require 'hlinum)

;; smooth-scroll
;(require 'smooth-scroll)
;(smooth-scroll-mode t)

;; projectile
(projectile-global-mode)

;; helm-projectile
(global-set-key (kbd "C-c h") 'helm-projectile)

;; 個別設定
(condition-case err
    (load "~/.emacs.d/local")
  (error))
