(global-set-key (kbd "M-n") #'flymake-goto-next-error)
(global-set-key (kbd "M-p") #'flymake-goto-prev-error)
(global-set-key [f2] 'neotree-toggle)
(global-set-key (kbd "S-<mouse-1>") 'xref-find-references)
(global-unset-key (kbd "C-<down-mouse-1>"))
(global-set-key (kbd "C-<mouse-1>") 'xref-find-definitions)
(global-set-key (kbd "s-<mouse-1>") 'mouse-buffer-menu)
(global-set-key (kbd "s-<mouse-3>") 'kill-current-buffer)
(global-set-key (kbd "C-<escape>") 'xref-go-back)



(provide 'init-key)
