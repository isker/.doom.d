;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ian Kerins"
      user-mail-address "ianskerins@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font "PragmataPro")
(setq doom-unicode-font "PragmataPro")

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox-light)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(after! evil-snipe
  (setq evil-snipe-scope 'buffer))

(use-package! prettier
  :defer t
  :init
  (setq prettier-inline-errors-flag t
        prettier-pre-warm 'none)
  (dir-locals-set-class-variables 'prettier-js
                                  '((web-mode . ((eval . (prettier-mode t))))
                                    (js-mode . ((eval . (prettier-mode t))))
                                    (js2-mode . ((eval . (prettier-mode t))))
                                    (typescript-mode . ((eval . (prettier-mode t))))
                                    (graphql-mode . ((eval . (prettier-mode t))))
                                    (css-mode . ((eval . (prettier-mode t))))
                                    (json-mode . ((eval . (prettier-mode t))))))
  (dir-locals-set-directory-class "/path/to/my/pretty/project/" 'prettier-js))

(after! vterm
  (set-popup-rule! "^\\*doom:vterm-popup:" :size 0.4 :vslot -4 :select t :quit nil :ttl 0 :side 'right))
(set-popup-rule! "^\\*doom:eshell-popup:" :size 0.4 :vslot -4 :select t :quit nil :ttl 0 :side 'right)

;; vterm: "insertion-state" is really emacs-state; only do evil stuff when in
;; copy-mode. There's just too much jank when trying to edit using evil-mode.
(add-hook! vterm-mode 'evil-emacs-state)
(add-hook! vterm-copy-mode
  (defun meliache/evil-normal-in-vterm-copy-mode ()
    (if (bound-and-true-p vterm-copy-mode)
        (evil-normal-state)
      (evil-emacs-state))))
(map!
 :mode vterm-mode
 :e "C-g" 'vterm-copy-mode
 :e "<deletechar>" 'vterm-send-delete
 :mode vterm-copy-mode
 :n "i" 'vterm-copy-mode)

(after! evil-markdown
  (map!
   :map evil-markdown-mode-map
   ;; Too addicted to emacs navigation in insert mode; go back a word instead of bolding.
   :i "M-b" nil))

(setq +lsp-company-backends 'company-capf) ; default is '(company-capf company-yasnippet)
(after! lsp-ui
  (setq lsp-ui-sideline-diagnostic-max-lines 10))

(add-hook! compilation-mode
  (add-to-list 'compilation-error-regexp-alist 'flow)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(flow
                 ;; Error ---------------------------------------- index.js:5:12
                 "^\\(Error\\)[[:space:]-]*\\(\\([^:]+\\):\\([[:digit:]]+\\)\\):?\\([[:digit:]]+\\)?"
                 3 ; file
                 4 ; line
                 5 ; column
                 2 ; type - not a subgroup!  type 2 is "error"
                 2 ; file:line
                 (1 compilation-error-face))))

(setq git-commit-summary-max-length 68)

(use-package! graphql-mode)
(advice-add 'risky-local-variable-p :override #'ignore)

(use-package! nix-mode
  :mode "\\.nix\\'")

(setq kill-buffer-query-functions
      (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
