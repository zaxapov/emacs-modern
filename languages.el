;;; REVERSE INPUT FOR RUSSIAN LANGUAGE
(use-package reverse-im
  :ensure t
  :custom
  (reverse-im-input-methods '("russian-computer"))
  :config
  (reverse-im-mode t))

;;; NASM CODING MODE
(use-package nasm-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.[n]*\\(asm\\|s\\)\\'" . nasm-mode)))

;;; MARKDOWN SETUP
(use-package markdown-mode)
(use-package markdown-toc)
