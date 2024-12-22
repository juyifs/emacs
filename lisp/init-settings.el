;;判断操作系统
(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-windows* (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))

;;;;;;;;;;;;
;;系统设置;;;;
;;;;;;;;;;;;
(prefer-coding-system 'utf-8)

(setq auto-save-default nil	   ; disable auto save
      auto-window-vscroll nil
      delete-by-moving-to-trash t  ; disable delete directly
      fast-but-imprecise-scrolling t
      frame-title-format "%b"
      help-window-select t
      inhibit-startup-screen t	   ; disable the startup screen splash
      inhibit-default-init t
      inhibit-startup-message t
      initial-scratch-message ""
      inhibit-compacting-font-caches t
      initial-major-mode 'fundamental-mode
      make-backup-files nil             ; disable backup file
      ;; Mouse wheel scroll behavior
      ;; mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      next-line-add-newlines nil
      read-process-output-max (* 64 1024)
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position t
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      visible-bell nil
      )

;;; macOS
;; 删除时移动到回收站
(when (eq system-type 'darwin)
  (setq delete-by-moving-to-trash t))

;; <macOS> Command -> Meta, Option -> Super
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
	mac-option-modifier 'super))

;;; Windows
;; spcial coding settings for Windows
(unless (memq system-type '(cygwin windows-nt ms-dos))
  (setq selection-coding-system 'utf-8))


;;;;;;;;;;;;;
;;;;画面设置;;;
;;;;;;;;;;;;;

;;窗口最大化
(toggle-frame-maximized)

;; adjust the fonts
(defun available-font (font-list)
  "Get the first available font from FONT-LIST."
  (catch 'font
    (dolist (font font-list)
      (if (member font (font-family-list))
	  (throw 'font font)))))

;;;###autoload
(defun cabins/setup-font ()
  "Font setup."

  (interactive)
  (let* ((efl '("Cascadia Code" "Source Code Pro" "JetBrains Mono" "Courier New" "Monaco" "Ubuntu Mono"))
	 (cfl '("楷体" "黑体" "STHeiti" "STKaiti"))
	 (cf (available-font cfl))
	 (ef (available-font efl)))
    (when ef
      (dolist (face '(default fixed-pitch fixed-pitch-serif variable-pitch))
	(set-face-attribute face nil :family ef)))
    (when cf
      (dolist (charset '(kana han cjk-misc bopomofo))
	(set-fontset-font t charset cf))
      (setq face-font-rescale-alist
	    (mapcar (lambda (item) (cons item 1.2)) cfl)))))

;; settings for daemon mode
(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(with-selected-frame frame
		  (cabins/setup-font))))
  (add-hook 'after-init-hook #'cabins/setup-font))

(set-face-attribute 'default nil :height 180)

;;高亮当前行
(global-hl-line-mode t)
(custom-set-faces
 '(hl-line ((t (:inherit highlight :extend t :background "dark gray"))))
 )

;;光标设置为竖线，并设置成红色
(setq-default cursor-type 'bar)
(set-cursor-color "#FF0000")

(setq split-height-threshold 1)
(setq split-width-threshold nil)


(provide 'init-settings)
