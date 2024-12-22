;;;默认目录
(setq default-directory "~/")

;;加载lisp文件
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "lisp")))

;;设置原生编译，提供加载性能
(when (and (fboundp 'native-comp-available-p)
	   (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors nil
	comp-deferred-compilation t
	package-native-compile t)
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache" user-emacs-directory)))


(require 'init-settings)
(require 'init-packages)
(require 'init-ide)
(require 'init-key)

;;加载自定义设置
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)



