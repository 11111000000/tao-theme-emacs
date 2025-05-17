;;; tao-theme.el --- Tao of EMACS perceptually uniform grayscale color theme -*- lexical-binding: t; -*-

;; Author: Peter 11111000000@email.com
;; Keywords: faces, theme, grayscale, perceptual, uniform
;; Contributors: Jasonm23 <jasonm23@gmail.com>, Steve Purcell (purcell), Jonas Bernoulli (tarsius), Guilherme G. (semente), Tanner Hobson (player1537), Syohei YOSHIDA (syohex), Thibault (thblt), Terje Larsen (terlar), Tang Xinfa (tangxinfa)
;; Version: 2025.05

;;; Commentary:


;; Tao theme: perceptually uniform grayscale with DRY faces and complete coverage of all legacy and modern Emacs faces and popular third-party packages.
;; Palette: 16-level gamma-corrected uniform grayscale.
;; Yin (Dark) Palette:
;; :color-0: #000000
;; :color-1: #080808
;; :color-10: #646464
;; :color-11: #737373
;; :color-12: #848484
;; :color-13: #999999
;; :color-14: #B5B5B5
;; :color-15: #FFFFFF
;; :color-2: #101010
;; :color-3: #191919
;; :color-4: #222222
;; :color-5: #2B2B2B
;; :color-6: #353535
;; :color-7: #3F3F3F
;; :color-8: #4B4B4B
;; :color-9: #575757
;; 
;; Yang (Light) Palette:
;; :color-0: #FFFFFF
;; :color-1: #F7F7F7
;; :color-10: #9B9B9B
;; :color-11: #8C8C8C
;; :color-12: #7B7B7B
;; :color-13: #666666
;; :color-14: #4A4A4A
;; :color-15: #000000
;; :color-2: #EFEFEF
;; :color-3: #E6E6E6
;; :color-4: #DDDDDD
;; :color-5: #D4D4D4
;; :color-6: #CACACA
;; :color-7: #C0C0C0
;; :color-8: #B4B4B4
;; :color-9: #A8A8A8

;; ALL faces are proxies for small tao-* base faces.

;;; Code:

(defun tao-theme-perceptual-scale ()
  "Generate a perceptually uniform grayscale gradient (gamma-corrected, 16 numbers)."
  (let ((n 16)
        (gamma 2.2)
        (result nil))
    (dotimes (k n (sort result '>))
      (let* ((L (/ (float k) (1- n)))
             (v (round (* 255 (expt L (/ 1.0 gamma))))))
        (push v result)))))

(defun tao-theme--taiji-scale (scale)
  "Invert a grayscale scale."
  (mapcar (lambda (it) (- #xFF it)) scale))

(defun tao-theme-yang-palette ()
  "Generate a palette for the light theme (yang): gradient from light to dark."
  (tao-theme--make-palette
   (funcall tao-theme-scale-fn)))

(defun tao-theme-yin-palette ()
  "Generate a palette for the dark theme (yin): gradient from dark to light."
  (tao-theme--make-palette
   (tao-theme--taiji-scale (funcall tao-theme-scale-fn)))
  )

(defun tao-theme--make-palette (&optional scale)
  "Return alist of (:color-0 ... :color-15), from darkest (black) to lightest (white).
If optional SCALE is given, use it instead of (funcall tao-theme-scale-fn)."
  (let* ((actual-scale (or scale (funcall tao-theme-scale-fn)))
         (palette
          (cl-loop for i from 0 to 15
                   for v in actual-scale
                   collect (cons (intern (format ":color-%d" i))
                                 (format "#%02X%02X%02X" v v v)))))
    palette))

(defcustom tao-theme-scale-fn
  #'tao-theme-perceptual-scale
  "Function to generate 16 intensity values for the grayscale scale (perceptual by default)."
  :type 'function :group 'tao-theme)

(defgroup tao-theme nil "Tao Theme Groups" :group 'faces)

(defmacro tao (key)
  `(cdr (assoc ,key p)))

;; ---- Copied from tao-theme-legacy.el (for compatibility) ----
(defmacro tao-with-color-variables (palette &rest body)
  "Bind color variables for the given PALETTE and execute BODY."
  (declare (indent 1))
  `(let ((p ,palette))
    ,@body))

(defun tao-apply-theme-faces (theme-name palette)
  "Apply the main Tao faces to THEME-NAME using PALETTE (alist)."
  (let* ((class '((class color)
                  (min-colors 89)))
         (p palette))
    (custom-theme-set-faces
     theme-name
     ;; Base DRY faces
     `(tao-bg         ((,class (:background ,(tao :color-0)))))
     `(tao-bg-alt     ((,class (:background ,(tao :color-1)))))
     `(tao-base       ((,class (:foreground ,(tao :color-11)))))
     `(tao-muted      ((,class (:foreground ,(tao :color-5)))))
     `(tao-active     ((,class (:foreground ,(tao :color-12)))))
     `(tao-accent     ((,class (:foreground ,(tao :color-14)))))
     `(tao-strong     ((,class (:foreground ,(tao :color-13)) :weight bold)))
     `(tao-faint      ((,class (:foreground ,(tao :color-3)))))
     `(tao-error      ((,class (:foreground ,(tao :color-15)) :background ,(tao :color-6) :weight bold)))
     `(tao-warning    ((,class (:foreground ,(tao :color-14)) :weight bold)))
     `(tao-success    ((,class (:foreground ,(tao :color-10)) :weight bold)))
     `(tao-link       ((,class (:foreground ,(tao :color-13)) :underline t)))
     ;; Core faces, editor chrome
     `(default ((,class (:background ,(tao :color-1) :foreground ,(tao :color-12)))))
     `(fringe                      ((,class (:inherit tao-bg-alt))))
     `(shadow                      ((,class (:inherit tao-faint))))
     `(minibuffer-prompt           ((,class (:inherit tao-accent :weight bold))))
     `(region ((,class (:background ,(tao :color-3)))))
     `(secondary-selection ((,class (:background ,(tao :color-4)))))
     `(highlight ((,class (:background ,(tao :color-4)))))
     `(vertical-border ((,class (:foreground ,(tao :color-6)))))
     `(cursor ((,class (:background ,(tao :color-12)))))
     `(line-number                 ((,class (:inherit tao-muted))))
     `(line-number-current-line    ((,class (:inherit tao-active :weight bold))))
     `(linum ((,class (:inherit tao-faint))))
     `(hl-paren-face ((,class (:inherit tao-accent :background ,(tao :color-4)))))
     `(paren-face-match ((,class (:inherit tao-strong))))
     `(paren-face-mismatch               ((,class (:inherit tao-error :underline t))))
     `(paren-face-no-match ((,class (:inherit tao-faint))))
     ;; Syntax/font-lock
     `(font-lock-builtin-face         ((,class (:inherit tao-active))))
     `(font-lock-comment-face         ((,class (:inherit tao-muted :slant italic))))
     `(font-lock-comment-delimiter-face ((,class (:inherit font-lock-comment-face))))
     `(font-lock-constant-face        ((,class (:inherit tao-accent))))
     `(font-lock-doc-face             ((,class (:inherit tao-faint :slant italic))))
     `(font-lock-function-name-face   ((,class (:inherit tao-base :weight bold))))
     `(font-lock-keyword-face         ((,class (:inherit tao-strong))))
     `(font-lock-string-face ((,class (:inherit tao-success))))
     `(font-lock-type-face            ((,class (:inherit tao-strong))))
     `(font-lock-variable-name-face   ((,class (:inherit tao-base))))
     `(font-lock-warning-face ((,class (:inherit tao-warning))))
     ;; Modeline, header
     `(mode-line               ((,class (:background ,(tao :color-2) :foreground ,(tao :color-11) :box (:line-width -1 :color ,(tao :color-7)) :height 1.0))))
     `(mode-line-inactive      ((,class (:background ,(tao :color-1) :foreground ,(tao :color-6) :box (:line-width -1 :color ,(tao :color-3)) :height 1.0))))
     `(mode-line-buffer-id     ((,class (:inherit tao-accent :weight bold))))
     `(header-line             ((,class (:inherit mode-line :box nil :overline t))))
     ;; Buttons, highlights, link
     `(link                         ((,class (:inherit tao-link))))
     `(link-visited                 ((,class (:inherit tao-link :slant italic))))
     `(button                       ((,class (:inherit link))))
     `(help-key-binding             ((,class (:inherit tao-accent))))
     `(trailing-whitespace          ((,class (:background ,(tao :color-3)))))
     ;; Show-paren
     `(show-paren-match ((,class (:inherit tao-strong :background ,(tao :color-5)))))
     `(show-paren-mismatch     ((,class (:inherit tao-error))))
     ;; ------------------------------------------
     ;; Diff, Compilation, Search, Error
     ;; ------------------------------------------
     `(error                       ((,class (:inherit tao-error))))
     `(warning                     ((,class (:inherit tao-warning))))
     `(success                     ((,class (:inherit tao-success))))
     `(compilation-error-face      ((,class (:inherit error))))
     `(compilation-warning-face    ((,class (:inherit warning))))
     `(compilation-info-face       ((,class (:inherit tao-active))))
     `(compilation-line-number     ((,class (:inherit tao-muted))))
     `(compilation-mode-line-exit  ((,class (:inherit tao-success))))
     `(compilation-mode-line-fail  ((,class (:inherit tao-error))))
     `(compilation-face ((,class (:inherit tao-base))))
     `(compilation-column-face ((,class (:inherit tao-muted))))
     `(compilation-enter-directory-face ((,class (:inherit tao-success))))
     `(compilation-message-face ((,class (:inherit tao-link))))
     `(compilation-mode-line-run ((,class (:inherit tao-active))))
     `(compilation-leave-directory-face ((,class (:inherit tao-muted))))
     `(compilation-line-face ((,class (:inherit tao-link))))
     `(compilation-info ((,class (:inherit tao-link))))
     `(compilation-mode-line-exit ((,class (:inherit tao-success))))
     `(compilation-mode-line-fail ((,class (:inherit tao-error))))
     `(diff-added ((,class (:foreground ,(tao :color-10)))))
     `(diff-removed ((,class (:foreground ,(tao :color-15)))))
     `(diff-changed                ((,class (:inherit tao-accent))))
     `(diff-file-header            ((,class (:background ,(tao :color-4) :weight bold))))
     `(diff-header ((,class (:background ,(tao :color-3)))))
     `(diff-context                ((,class (:inherit tao-muted))))
     ;; diff-hl
     `(diff-hl-insert ((,class (:background ,(tao :color-10)))))
     `(diff-hl-delete ((,class (:background ,(tao :color-15)))))
     `(diff-hl-change ((,class (:background ,(tao :color-13)))))
     ;; smerge
     `(smerge-upper ((,class (:background ,(tao :color-8)))))
     `(smerge-lower ((,class (:background ,(tao :color-9)))))
     `(smerge-markers ((,class (:background ,(tao :color-3)))))
     ;; DIRED/Dired+
     `(dired-directory         ((,class (:inherit tao-accent
                                         :weight bold))))
     `(dired-flagged           ((,class (:inherit tao-error))))
     `(dired-header            ((,class (:inherit tao-strong))))
     `(dired-mark              ((,class (:inherit tao-success))))
     `(dired-marked            ((,class (:inherit tao-warning))))
     `(dired-perm-write        ((,class (:inherit tao-success))))
     `(dired-symlink           ((,class (:inherit link))))
     `(dired-warning           ((,class (:inherit tao-warning))))
     ;; Dired+
     `(diredp-dir-priv         ((,class (:inherit dired-directory))))
     `(diredp-dir-name         ((,class (:inherit tao-base
                                         :weight bold))))
     `(diredp-file-name        ((,class (:inherit tao-base))))
     `(diredp-symlink          ((,class (:inherit dired-symlink))))
     `(diredp-exec-priv        ((,class (:inherit tao-active))))
     `(diredp-flag-mark        ((,class (:inherit dired-mark))))
     `(diredp-flag-mark-line   ((,class (:inherit dired-marked))))
     `(diredp-read-priv        ((,class (:inherit tao-success))))
     `(diredp-write-priv       ((,class (:inherit tao-strong))))
     `(diredp-no-priv          ((,class (:inherit tao-muted))))
     ;; Company, Ivy, Corfu, Vertico, Orderless, Completions
     `(company-tooltip ((,class (:inherit tao-bg-alt :foreground ,(tao :color-11)))))
     `(company-tooltip-selection ((,class (:inherit tao-strong))))
     `(company-tooltip-mouse ((,class (:inherit company-tooltip-selection))))
     `(company-scrollbar-bg ((,class (:inherit tao-bg-alt))))
     `(company-scrollbar-fg ((,class (:inherit tao-bg))))
     `(company-preview ((,class (:inherit tao-faint))))
     `(company-tooltip-annotation ((,class (:inherit tao-muted))))
     `(company-tooltip-common ((,class (:inherit tao-accent))))
     `(company-tooltip-common-selection            ((,class (:inherit tao-accent :weight bold))))
     `(corfu-current ((,class (:inherit company-tooltip-selection))))
     `(corfu-candidate-overlay-face ((,class (:inherit tao-accent))))
     `(vertico-current ((,class (:inherit tao-strong :background ,(tao :color-4)))))
     `(orderless-match-face-0 ((,class (:inherit tao-accent))))
     `(orderless-match-face-1 ((,class (:inherit tao-success))))
     `(orderless-match-face-2 ((,class (:inherit tao-active))))
     `(orderless-match-face-3 ((,class (:inherit tao-warning))))
     `(completions-common-part                     ((,class (:inherit tao-strong :weight bold))))
     ;; Ivy
     `(ivy-current-match ((,class (:inherit tao-link :background ,(tao :color-7)))))
     `(ivy-minibuffer-match-face-1 ((,class (:inherit tao-active))))
     `(ivy-minibuffer-match-face-2 ((,class (:inherit tao-accent :weight bold))))
     `(ivy-minibuffer-match-face-3 ((,class (:inherit tao-strong))))
     `(ivy-minibuffer-match-face-4 ((,class (:inherit tao-success :weight bold))))
     ;; Magit
     `(magit-section-title ((,class (:inherit tao-strong))))
     `(magit-branch ((,class (:inherit tao-accent))))
     `(magit-log-author                           ((,class (:inherit tao-faint :slant italic))))
     `(magit-tag ((,class (:inherit tao-accent))))
     `(magit-item-highlight ((,class (:background ,(tao :color-7)))))
     ;; Diff-hl, Ediff
     `(ediff-current-diff-A ((,class (:background ,(tao :color-6)))))
     `(ediff-current-diff-B ((,class (:background ,(tao :color-7)))))
     `(ediff-current-diff-C ((,class (:background ,(tao :color-8)))))
     `(ediff-even-diff-A ((,class (:background ,(tao :color-3)))))
     `(ediff-odd-diff-A ((,class (:background ,(tao :color-4)))))
     `(ediff-fine-diff-A ((,class (:background ,(tao :color-11)))))
     `(ediff-fine-diff-B ((,class (:background ,(tao :color-11)))))
     `(ediff-fine-diff-C ((,class (:background ,(tao :color-11)))))
     ;; Gnus/email
     `(gnus-group-mail-1-empty ((,class (:inherit tao-base))))
     `(gnus-group-mail-2-empty ((,class (:inherit tao-muted))))
     `(gnus-group-mail-3-empty ((,class (:inherit tao-faint))))
     `(gnus-group-news-1-empty ((,class (:inherit tao-base
                                         :weight bold))))
     `(gnus-summary-high-read  ((,class (:inherit tao-success))))
     `(gnus-summary-high-unread ((,class (:inherit tao-accent))))
     `(gnus-header-content     ((,class (:inherit tao-muted))))
     `(gnus-header-from        ((,class (:inherit tao-base))))
     `(gnus-header-subject     ((,class (:inherit tao-strong))))
     `(gnus-signature          ((,class (:inherit tao-faint
                                         :slant italic))))
     `(message-header-name     ((,class (:inherit tao-base))))
     `(message-header-other    ((,class (:inherit tao-muted))))
     `(message-header-to       ((,class (:inherit tao-accent))))
     `(message-header-from     ((,class (:inherit tao-accent))))
     `(message-header-cc       ((,class (:inherit tao-muted))))
     `(message-header-subject  ((,class (:inherit tao-strong))))
     `(message-mml             ((,class (:inherit tao-muted
                                         :slant italic))))
     `(message-separator       ((,class (:inherit tao-faint))))
     `(message-cited-text      ((,class (:inherit tao-muted
                                         :slant italic))))
     ;; Flycheck, Flymake, Flyspell
     `(flycheck-error ((,class (:inherit tao-error :underline (:style wave :color ,(tao :color-15))))))
     `(flycheck-warning ((,class (:inherit tao-warning :underline (:style wave :color ,(tao :color-14))))))
     `(flycheck-info ((,class (:inherit tao-active :underline (:style wave :color ,(tao :color-10))))))
     `(flyspell-incorrect ((,class (:inherit tao-error :underline (:style wave :color ,(tao :color-15))))))
     `(flyspell-duplicate ((,class (:inherit tao-warning :underline (:style wave :color ,(tao :color-14))))))
     `(flymake-error ((,class (:inherit tao-error :underline (:style wave :color ,(tao :color-15))))))
     `(flymake-warning ((,class (:inherit tao-warning :underline (:style wave :color ,(tao :color-14))))))
     `(flymake-note ((,class (:inherit tao-active :underline (:style wave :color ,(tao :color-10))))))
     ;; LSP, eglot, lsp-ui
     `(lsp-face-highlight-read ((,class (:background ,(tao :color-6)))))
     `(lsp-headerline-breadcrumb-path-error-face ((,class (:underline (:style wave :color ,(tao :color-15))))))
     `(lsp-headerline-breadcrumb-path-warning-face ((,class (:underline (:style wave :color ,(tao :color-14))))))
     `(lsp-headerline-breadcrumb-path-info-face ((,class (:underline (:style wave :color ,(tao :color-10))))))
     `(lsp-headerline-breadcrumb-symbols-error-face ((,class (:underline (:style wave :color ,(tao :color-15))))))
     `(lsp-headerline-breadcrumb-symbols-warning-face ((,class (:underline (:style wave :color ,(tao :color-14))))))
     `(lsp-headerline-breadcrumb-symbols-info-face ((,class (:underline (:style wave :color ,(tao :color-10))))))
     `(lsp-ui-sideline-symbol               ((,class (:inherit tao-accent :slant italic))))
     `(lsp-ui-sideline-current-symbol ((,class (:background ,(tao :color-8)))))
     `(lsp-ui-sideline-code-action          ((,class (:inherit tao-accent :box t))))
     `(eglot-highlight-symbol-face ((,class (:inherit tao-link))))
     `(eglot-inlay-hint-face                ((,class (:inherit tao-faint :italic t))))
     ;; Org, outline, markdown
     `(outline-1                   ((,class (:inherit tao-strong :height 1.3))))
     `(outline-2                   ((,class (:inherit tao-strong :height 1.2))))
     `(outline-3                   ((,class (:inherit tao-base))))
     `(outline-4                   ((,class (:inherit tao-muted))))
     `(outline-6                     ((,class (:inherit tao-faint))))
     `(outline-7                     ((,class (:inherit tao-faint))))
     `(outline-8                     ((,class (:inherit tao-faint))))
     `(org-level-1                 ((,class (:inherit tao-strong :height 1.15))))
     `(org-level-2                 ((,class (:inherit tao-base :height 1.12))))
     `(org-level-3                 ((,class (:inherit tao-muted))))
     `(org-level-4                 ((,class (:inherit tao-faint))))
     `(org-block                   ((,class (:inherit tao-bg-alt))))
     `(org-link                    ((,class (:inherit tao-link))))
     `(org-done                    ((,class (:inherit tao-success :weight bold))))
     `(org-todo                    ((,class (:inherit tao-warning :weight bold))))
     `(org-table                   ((,class (:inherit tao-muted))))
     `(org-warning                 ((,class (:inherit tao-warning))))
     `(org-scheduled               ((,class (:inherit tao-success))))
     `(org-headline-done           ((,class (:inherit tao-faint :strike-through t))))
     `(org-document-title          ((,class (:inherit tao-strong :height 1.35))))
     `(org-document-info           ((,class (:inherit tao-muted))))
     ;; Markdown
     `(markdown-header-face-1      ((,class (:inherit org-level-1))))
     `(markdown-header-face-2      ((,class (:inherit org-level-2))))
     `(markdown-header-face-3      ((,class (:inherit org-level-3))))
     `(markdown-header-face-4      ((,class (:inherit org-level-4))))
     `(markdown-inline-code-face   ((,class (:inherit tao-faint))))
     `(markdown-blockquote-face    ((,class (:inherit tao-muted :slant italic))))
     `(markdown-bold-face          ((,class (:weight bold))))
     `(markdown-italic-face        ((,class (:slant italic))))
     `(markdown-header-face-5        ((,class (:inherit outline-5))))
     `(markdown-header-face-6        ((,class (:inherit outline-6))))
     `(markdown-list-face            ((,class (:inherit tao-base))))
     `(markdown-markup-face          ((,class (:inherit tao-faint))))
     `(markdown-html-attr-name-face  ((,class (:inherit tao-muted))))
     `(markdown-html-attr-value-face ((,class (:inherit tao-faint))))
     `(markdown-html-tag-delimiter-face ((,class (:inherit tao-muted))))
     `(markdown-html-tag-name-face   ((,class (:inherit tao-base))))
     `(markdown-table-face           ((,class (:inherit tao-faint))))
     `(markdown-url-face             ((,class (:inherit tao-link))))
     `(markdown-pre-face             ((,class (:inherit tao-bg-alt))))
     `(markdown-language-keyword-face ((,class (:inherit tao-faint :height 0.8))))
     ;; Term/ANSI
     `(term-color-black   ((,class (:foreground ,(tao :color-0) :background ,(tao :color-0)))))
     `(term-color-white
       ((,class (:foreground ,(tao :color-15) :background ,(tao :color-15)))))
     `(term-color-red     ((,class (:foreground ,(tao :color-15) :background ,(tao :color-6)))))
     `(term-color-green   ((,class (:foreground ,(tao :color-10) :background ,(tao :color-3)))))
     `(term-color-yellow  ((,class (:foreground ,(tao :color-13) :background ,(tao :color-6)))))
     `(term-color-blue    ((,class (:foreground ,(tao :color-11) :background ,(tao :color-7)))))
     `(term-color-cyan ((,class (:foreground ,(tao :color-12) :background ,(tao :color-10)))))
     `(term-color-magenta ((,class (:foreground ,(tao :color-14) :background ,(tao :color-12)))))
     `(term-default-fg-color             ((,class (:inherit term-color-white))))
     `(term-default-bg-color             ((,class (:inherit term-color-black))))
     `(ansi-color-names-vector [, (tao :color-0)
                                  , (tao :color-10)
                                  , (tao :color-9)
                                  , (tao :color-13)
                                  , (tao :color-11)
                                  , (tao :color-10)
                                  , (tao :color-12)
                                  , (tao :color-13)])
     ;; Misc/proxies (suite)
     `(widget-field
       ((,class (:background ,(tao :color-3)))))
     `(hl-line
       ((,class (:background ,(tao :color-2)))))
     `(highlight-symbol-face ((,class (:background ,(tao :color-5)))))
     `(error-face                    ((,class (:inherit tao-error))))
     `(success-face                  ((,class (:inherit tao-success))))
     `(warning-face                  ((,class (:inherit tao-warning))))
     ;; Whitespace/folding/etc
     `(whitespace-tab
       ((,class (:background ,(tao :color-7)))))
     `(whitespace-space
       ((,class (:background ,(tao :color-4)))))
     `(whitespace-newline
       ((,class (:foreground ,(tao :color-6)))))
     `(whitespace-trailing
       ((,class (:background ,(tao :color-5)))))
     ;; Rainbow delimiters (for lisps etc)
     `(rainbow-delimiters-depth-1-face ((,class
         (:foreground
          ,(tao
            :color-7)))))
     `(rainbow-delimiters-depth-2-face ((,class
         (:foreground
          ,(tao
            :color-8)))))
     `(rainbow-delimiters-depth-3-face ((,class (:foreground ,(tao :color-9)))))
     `(rainbow-delimiters-depth-4-face ((,class (:foreground ,(tao :color-10)))))
     `(rainbow-delimiters-depth-5-face ((,class (:foreground ,(tao :color-11)))))
     `(rainbow-delimiters-depth-6-face ((,class (:foreground ,(tao :color-12)))))
     `(rainbow-delimiters-depth-7-face ((,class (:foreground ,(tao :color-13)))))
     `(rainbow-delimiters-depth-8-face ((,class (:foreground ,(tao :color-14)))))
     `(rainbow-delimiters-depth-9-face ((,class (:foreground ,(tao :color-15)))))
     ;; Explicit proxies for legacy/third-party
     `(ace-jump-face-background ((,class (:inherit tao-faint))))
     `(ace-jump-face-foreground ((,class (:inherit tao-accent :weight bold))))
     `(avy-lead-face            ((,class (:inherit tao-accent :weight bold))))
     `(avy-background-face      ((,class (:inherit tao-faint))))
     `(avy-lead-face-0                   ((,class (:inherit tao-accent :weight bold))))
     `(avy-lead-face-1                   ((,class (:inherit tao-accent :weight bold))))
     `(avy-lead-face-2                   ((,class (:inherit tao-accent :weight bold))))
     `(ace-window-path-face              ((,class (:inherit tao-accent :weight bold))))
     `(ace-window-lead-face              ((,class (:inherit tao-accent :weight bold))))
     `(ace-window-selection-face ((,class (:inherit tao-accent))))
     `(company-echo-common      ((,class (:inherit tao-accent))))
     `(egg-diff-add             ((,class (:inherit diff-added))))
     `(egg-diff-del             ((,class (:inherit diff-removed))))
     `(egg-diff-file-header     ((,class (:inherit diff-hl-removed))))
     `(egg-section-title        ((,class (:inherit tao-accent :weight bold))))
     `(egg-stash-mono           ((,class (:inherit tao-faint))))
     `(helm-selection           ((,class (:inherit tao-strong))))
     `(helm-header              ((,class (:inherit tao-mutd))))
     `(magit-section-heading    ((,class (:inherit tao-strong))))
     `(macrostep-expansion-highlight-face ((,class (:inherit highlight))))
     `(macrostep-gensym-1
       ((,class (:inherit tao-accent))))
     `(macrostep-gensym-2
       ((,class (:inherit tao-accent))))
     `(macrostep-gensym-3 ((,class (:inherit tao-accent))))
     `(macrostep-gensym-4 ((,class (:inherit tao-accent))))
     `(macrostep-gensym-5 ((,class (:inherit tao-accent))))
     `(macrostep-macro-face             ((,class (:inherit tao-link))))
     `(powerline-active1        ((,class (:inherit mode-line))))
     `(powerline-inactive2 ((,class (:inherit mode-line-inactive))))
     `(tab-bar                  ((,class (:background ,(tao :color-1)))))
     `(tab-bar-tab              ((,class (:background ,(tao :color-4)) :foreground ,(tao :color-10) :height 1.0)))
     `(tab-bar-tab-inactive     ((,class (:background ,(tao :color-7)) :foreground ,(tao :color-9))))
     `(tab-line                 ((,class (:background ,(tao :color-3)))))
     `(tab-line-tab             ((,class (:background ,(tao :color-4)) :foreground ,(tao :color-13) :weight normal)))
     `(tab-line-tab-current     ((,class (:background ,(tao :color-4)) :foreground ,(tao :color-13))))
     `(tab-line-tab-inactive    ((,class (:background ,(tao :color-5)) :foreground ,(tao :color-8))))
     `(tabbar-default ((,class (:inherit tao-bg-alt))))
     `(tabbar-button ((,class (:inherit tao-link))))
     `(tabbar-selected                   ((,class (:inherit tao-link :weight bold))))
     `(tabbar-unselected ((,class (:inherit tao-faint))))
     `(centaur-tabs-default ((,class (:inherit tao-bg-alt))))
     `(centaur-tabs-selected ((,class (:inherit tao-strong))))
     `(centaur-tabs-unselected ((,class (:inherit tao-faint))))
     `(bm-face ((,class (:inherit tao-warning :background ,(tao :color-5)))))
     `(bm-fringe-face ((,class (:inherit tao-warning))))
     `(bm-fringe-persistent-face ((,class (:inherit tao-warning :foreground ,(tao :color-2)))))
     `(bm-persistent-face ((,class (:inherit tao-warning :background ,(tao :color-3)))))
     `(eldoc-highlight-function-argument ((,class (:inherit tao-base :weight bold))))
     `(eval-sexp-fu-flash ((,class (:inherit tao-warning :background ,(tao :color-5)))))
     `(eval-sexp-fu-flash-error ((,class (:inherit tao-error :background ,(tao :color-6)))))
     ;; Icomplete/Ido
     `(icomplete-first-match             ((,class (:inherit tao-accent :weight bold))))
     `(icomplete-selected ((,class (:inherit tao-strong))))
     `(ido-first-match ((,class (:inherit tao-accent))))
     `(ido-only-match                    ((,class (:inherit tao-accent :weight bold))))
     `(ido-subdir ((,class (:inherit tao-success))))
     `(ido-indicator ((,class (:inherit tao-accent :background ,(tao :color-7)))))
     ;; Magit/Section
     `(magit-section-heading-selection ((,class (:inherit tao-strong :foreground ,(tao :color-10)))))
     `(magit-section-highlight
       ((,class (:inherit tao-base :background ,(tao :color-6)))))
     `(magit-item-highlight ((,class (:inherit tao-bg-alt))))
     `(magit-diff-file-heading-highlight ((,class (:inherit tao-bg-alt))))
     ;; whitespace (extras)
     `(whitespace-hspace ((,class (:inherit tao-faint))))
     `(whitespace-line ((,class (:inherit tao-bg-alt))))
     `(whitespace-space-before-tab ((,class (:inherit tao-faint))))
     `(whitespace-indentation ((,class (:inherit tao-faint))))
     `(whitespace-empty ((,class (:inherit tao-faint))))
     `(whitespace-space-after-tab ((,class (:inherit tao-faint))))
     ;; yascroll, neotree, tuareg, typescript, geiser, eglot, etc.
     `(yascroll:thumb-text-area ((,class (:inherit tao-bg-alt))))
     `(yascroll:thumb-fringe ((,class (:inherit tao-bg-alt))))
     `(neotree-dir-link-face ((,class (:inherit tao-strong))))
     `(neotree-expand-btn-face          ((,class (:inherit tao-link :weight bold))))
     `(neotree-file-link-face ((,class (:inherit tao-faint))))
     `(neotree-root-dir-face ((,class (:inherit tao-muted :background ,(tao :color-2)))))
     `(prolog-face ((,class (:inherit tao-accent))))
     `(tuareg-font-lock-operator-face   ((,class (:inherit tao-base))))
     `(tuareg-font-lock-governing-face  ((,class (:inherit tao-link))))
     `(typescript-primitive-face        ((,class (:inherit tao-base :italic t))))
     `(typescript-this-face             ((,class (:inherit tao-base :weight bold :italic t))))
     `(geiser-font-lock-doc-link        ((,class (:inherit tao-link))))
     `(geiser-font-lock-error-link      ((,class (:inherit tao-error :underline t))))
     `(geiser-font-lock-autodoc-identifier ((,class (:inherit tao-strong))))
     `(eglot-diagnostic-tag-unnecessary-face ((,class (:inherit tao-faint :italic t :underline ,(tao :color-5))))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(defun tao/print-colors ()
  "Print all colors from both yin (dark) and yang (light) palettes."
  (interactive)
  (let ((yin-colors (tao-theme-yin-palette))
        (yang-colors (tao-theme-yang-palette)))
    (with-current-buffer (get-buffer-create "*Tao Colors*")
      (erase-buffer)
      (insert (propertize "Yin (Dark) Palette:\n" 'face 'bold))
      (dolist (color (sort (mapcar #'car yin-colors) #'string<))
        (insert (format "%s: %s\n" color (alist-get color yin-colors)))
        (add-text-properties (line-beginning-position) (line-end-position)
                             (list 'face `(:foreground ,(alist-get color yin-colors)))))
      (insert "\n")
      (insert (propertize "Yang (Light) Palette:\n" 'face 'bold))
      (dolist (color (sort (mapcar #'car yang-colors) #'string<))
        (insert (format "%s: %s\n" color (alist-get color yang-colors)))
        (add-text-properties (line-beginning-position) (line-end-position)
                             (list 'face `(:foreground ,(alist-get color yang-colors)))))
      (display-buffer (current-buffer)))))

(provide 'tao-theme)

;;; tao-theme.el ends here
