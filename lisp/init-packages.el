;; 初始化软件包管理器
(require 'package)
;;个别时候会出现签名检验失败
(setq package-check-signature nil
      load-prefer-newer t)
;;清华镜像源
(setq package-archives
      '(("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("org" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; 刷新软件源索引
(unless package-archive-contents
  (package-refresh-contents))

;; settings for use-package package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; configure use-package prior to loading it
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-always-defer t
        use-package-expand-minimally t)
  (require 'use-package))

;;第三方插件
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dashboard
  :ensure t
  :init (add-hook 'after-init-hook #'(lambda () (dashboard-open)))
  :config
  (setq dashboard-banner-logo-title "Emacs")
  (setq dashboard-startup-banner 'official)
  (setq dashboard-center-content t)
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-items '((recents  . 10)
                          (bookmarks . 5)
                          (projects . 5)
                          ;;(agenda . 5)
                          ;;(registers . 5)
			  ))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator nil)
  (dashboard-setup-startup-hook)
  )

(use-package gnu-elpa-keyring-update)
(use-package diminish)
(use-package delight)


(use-package all-the-icons)
(defvar all-the-icons-font-installation-path
  (expand-file-name "fonts/" user-emacs-directory)
  "The path to install the all-the-icons fonts to.")

(unless (file-directory-p all-the-icons-font-installation-path)
  (make-directory all-the-icons-font-installation-path t)
  (all-the-icons-install-fonts))


;;自动补全
(use-package company
  :diminish
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-show-quick-access t))

;; 提供有用功能
(use-package crux)

;; 搜索优化
(use-package ctrlf
  :hook (after-init . ctrlf-mode))

;; 读取环境变量
(use-package exec-path-from-shell
  :defer nil
  :if (memq window-system '(mac ns x))
  :init (exec-path-from-shell-initialize))

;; Ctrl-C f 格式化代码
(use-package format-all
  :diminish
  :hook (prog-mode . format-all-ensure-formatter)
  :bind ("C-c f" . #'format-all-buffer))

;; 删除光标前后空白
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode))


;; 使用 M-<up>/<down>移动行或者文字
(use-package move-text
  :hook (after-init . move-text-default-bindings))

;; 为28以下版本启动项目管理插件，快捷键Ctrl-c P
(use-package projectile
  :when (< emacs-major-version 28)
  :diminish " Proj."
  :init (add-hook 'after-init-hook 'projectile-mode)
  :config (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; 彩色括号并高亮
(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
(use-package highlight-parentheses
  :init (add-hook 'prog-mode-hook 'highlight-parentheses-mode))

;; 快捷键建议
(use-package which-key
  :defer nil
  :diminish
  :init (which-key-mode))

;; 代码片段补全
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))
(use-package yasnippet-snippets)


;; (defun my-neotree-find ()
;;   "Open NeoTree window if it's not open, otherwise refresh it."
;;   (interactive)
;;   (let ((neotree-buffer (get-buffer "*NeoTree*")))
;;     (if (and neotree-buffer (get-buffer-window neotree-buffer))
;;         (neotree-find)
;;       (neotree-toggle))))


(use-package neotree
  :ensure t
  ;;:init (add-hook 'dashboard-after-initialize-hook #'(lambda () (neotree-toggle)))
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t)
  (setq neo-window-fixed-size nil)
  (setq neo-window-width 35)
  ;; (add-hook 'find-file-hook #'my-neotree-find)
  )


;;内置插件
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 自动扩展缩写词
(setq-default abbrev-mode t)

;; 切换到其他应用、切换缓冲区时自动保存，高亮当前行
;; `save-some-buffers' is provided by files.el (builtin)
;; `pulse-momentary-highlight-one-line' is provided by pulse.el (builtin)
(use-package pulse-and-save
  :ensure nil
  :init
  (defun pulse-save-buffers (&rest args)
    (save-some-buffers t)
    (pulse-momentary-highlight-one-line (point)))
  ;; auto save when frame lose focus, Alt-Tab
  (add-function :after after-focus-change-function #'pulse-save-buffers)
  ;; auto save when buffer changed
  (dolist (command '(other-window
                     switch-to-buffer
                     next-buffer
                     previous-buffer))
    (advice-add command :after #'pulse-save-buffers)))

;; auto revert
;; `global-auto-revert-mode' is provided by autorevert.el (builtin)
(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

;; 保存文件时自动删除末尾空白，选择文本时自动删除
;; (add-hook 'before-save-hook #'delete-trailing-whitespace)
;; (add-hook 'after-init-hook 'delete-selection-mode)

;; 自动缩进，自动补全括号，自动对齐
(add-hook 'after-init-hook 'electric-indent-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'electric-layout-mode)

;; 代码语法检查
(add-hook 'prog-mode-hook 'flymake-mode)

;; 代码折叠
(use-package hideshow
  :init (add-hook 'hs-minor-mode-hook (lambda () (diminish 'hs-minor-mode)))
  :hook (prog-mode . hs-minor-mode))

;; 缓冲区管理
(use-package ibuffer
  :init (defalias 'list-buffers 'ibuffer))

;; Ido ( instead of ivy & counsel & swiper)
(setq-default ido-auto-merge-work-directories-length -1
	      ido-enable-flex-matching t
	      isearch-lazy-count t
	      lazy-count-prefix-format "%s/%s: ")
(setq completion-ignored-extensions '(".o" ".elc" "~" ".bin" ".bak" ".obj" ".map" ".a" ".ln" ".class"))
(fido-mode t)
(fido-vertical-mode 1)

(use-package tab-bar
  :ensure nil
  :init
  (setq tab-bar-new-tab-choice "*scratch*") ;; buffer to show in new tabs
  (setq tab-bar-close-button-show t)      ;; hide tab close / X button
  (setq tab-bar-new-tab-to 'rightmost)
  (setq tab-bar-show 1)                     ;; hide bar if <= 1 tabs open
  (setq tab-bar-format '(tab-bar-format-menu-bar tab-bar-format-tabs tab-bar-separator))
  (setq tab-bar-tab-hints t)

  :config
  (tab-bar-mode 1)                           ;; enable tab bar
  (custom-set-faces
   '(tab-bar ((t (:inherit mode-line :box nil))))  ;; 设置标签栏背景色
   '(tab-bar-tab ((t (:inherit mode-line :foreground "gainsboro" :background "gray22" :box nil))))  ;; 设置活动标签的前景色和背景色
   '(tab-bar-tab-inactive ((t (:foreground "gray0")))))
  )

(defun my-neotree-open-in-new-tab ()
  "Open the given FILENAME in a new tab."
  (interactive)
  (let* ((file (neo-buffer--get-filename-current-line)))
    (message "Opening %s..." file)
    (let ((buffer (find-file file)))
      (switch-to-buffer-other-tab buffer)
      (neotree-toggle)
      )
    )
  )

(advice-add 'neotree-enter :around #'my-neotree-open-in-new-tab)


;; Org Mode
(setq org-hide-leading-stars t
      org-startup-indented t)

;; 括号高亮
(use-package paren
  :ensure nil
  :config (setq-default show-paren-style 'mixed
                        show-paren-when-point-inside-paren t
                        show-paren-when-point-in-periphery t)
  :hook (prog-mode . show-paren-mode))

;; 最近打开的文件
(add-hook 'after-init-hook (lambda ()
			     (recentf-mode 1)
			     (add-to-list 'recentf-exclude '("~\/.emacs.d\/elpa\/"))))
(setq-default recentf-max-menu-items 20
	      recentf-max-saved-items 20)

;; 记住emacs关闭前的光标位置
(save-place-mode 1)

;; 用空格缩进而不是tab键，一个tab=4个字符
;;(setq-default indent-tabs-mode nil)
;;(setq-default tab-width 4)

;; Diminish Builtins
(dolist (elem '(abbrev-mode eldoc-mode))
  (diminish elem))
(add-hook 'hs-minor-mode-hook (lambda () (diminish 'hs-minor-mode)))

(provide 'init-packages)
