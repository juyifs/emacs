;;启动垃圾回收，优化启动速度
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda ()
			     (setq gc-cons-threshold 800000)))

;;包初始化
(setq package-enable-at-startup nil)
(setq load-prefer-newer noninteractive)

;;不再调整窗口大小
(setq frame-inhibit-implied-resize t)

;;设置编码格式
(set-language-environment 'utf-8)

;;加载主题
(load-theme 'tango-dark t)

;;yes_or_no->y_or_n
(defalias 'yes-or-no-p 'y-or-n-p)

;;显示行号
(global-display-line-numbers-mode t)

;;隐藏滚动条
;;(scroll-bar-mode -1)

;;禁用开机画面
;;(setq inhibit-startup-screen t)
