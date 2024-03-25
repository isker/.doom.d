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
(setq doom-font "PragmataPro Liga")
(setq doom-unicode-font "PragmataPro Liga")

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

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

(defun vterm-kill-line ()
  "Send `C-k' to libvterm."
  (interactive)
  (kill-ring-save (point) (vterm-end-of-line))
  (vterm-send-C-k))

(map!
 :mode vterm-mode
 :e "C-g" 'vterm-copy-mode
 :e "<escape>" 'vterm-copy-mode
 :e "<deletechar>" 'vterm-send-delete
 :e "C-k" 'vterm-kill-line
 :mode vterm-copy-mode
 :n "i" 'vterm-copy-mode
 :n "a" 'vterm-copy-mode)

(after! vterm
  ;; https://github.com/akermu/emacs-libvterm/issues/313#issuecomment-1183650463
  (advice-add #'vterm--redraw :around (lambda (fun &rest args)
                                        (let ((cursor-type cursor-type))
                                          (apply fun args)))))

(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override "main"))

(after! evil-markdown
  (map!
   :map evil-markdown-mode-map
   ;; Too addicted to emacs navigation in insert mode; go back a word instead of bolding.
   :i "M-b" nil))

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)
(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(after! lsp-ui
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-enable nil))
(after! lsp-mode
  (setq lsp-clients-typescript-log-verbosity "off"
        lsp-completion-show-detail nil))
(after! typescript-mode
  (setq typescript-indent-level 2))

(after! corfu
  (setq corfu-auto nil
        corfu-preview-current nil))

(setq git-commit-summary-max-length 68)

(use-package! graphql-mode)
(use-package! jsonnet-mode)
(advice-add 'risky-local-variable-p :override #'ignore)

(setq kill-buffer-query-functions
      (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

(setq evil-kill-on-visual-paste nil)

;; Until there is a release of this:
;; https://bitbucket.org/mituharu/emacs-mac/commits/5f6c306095c825eb01708e336f9d03c15271dfe9
(add-hook 'doom-after-init-hook (lambda ()
                                  (tool-bar-mode 1)
                                  (tool-bar-mode 0)))
(use-package! caddyfile-mode
  :mode (("Caddyfile\\'" . caddyfile-mode)
         ("caddy\\.conf\\'" . caddyfile-mode)))

;; yaml-mode derives from text-mode, so it gets spell-fu, which is nonsense.
;; Why would you think deriving yaml-mode from text-mode is a good idea?
(add-hook 'yaml-mode-hook (lambda () (spell-fu-mode -1)))
