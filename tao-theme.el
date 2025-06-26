;;; tao-theme.el --- Tao of EMACS perceptually uniform grayscale color theme -*- lexical-binding: t; -*-

;; Author: Peter 11111000000@email.com
;; Keywords: faces, theme, grayscale, perceptual, uniform
;; Contributors: Jasonm23 <jasonm23@gmail.com>, Steve Purcell (purcell), Jonas Bernoulli (tarsius), Guilherme G. (semente), Tanner Hobson (player1537), Syohei YOSHIDA (syohex), Thibault (thblt), Terje Larsen (terlar), Tang Xinfa (tangxinfa)
;; Version: 2025.05

;;; Commentary:

;; Colors blind people’s eyes;
;;         Lao Tzu - Tao Te Ching (Ch.  12, Sentence 1)

;; Tao theme: perceptually uniform grayscale with DRY faces and complete coverage of all legacy and modern Emacs faces and popular third-party packages.
;; Palette: 12-level gamma-corrected uniform grayscale.
;; ALL faces are proxies for small tao-* base faces.

;;; Code:

(defun tao-theme-perceptual-scale ()
  "Generate a perceptually uniform grayscale gradient *без* #000000 и #FFFFFF.
Возвращает список из 12 целых 0–255; самые крайние значения ≈ 12 и ≈ 243."
  (let* ((n 12)
         (gamma 1.8)          ; более пологая гамма → ровнее контраст
         (result nil))
    ;; Берём (k+1)/(n+1): тем самым пропускаем два крайних деления,
    ;; оставляя визуальный «запас» для реальных чёрного/белого.
    (dotimes (k n (sort result '<))
      (let* ((l (/ (float (1+ k)) (1+ n))) ; 1/(n+1)…n/(n+1)
             (v (round (* 255 (expt l (/ 1.0 gamma))))))
        (push v result)))))

(defun tao-theme-lab-scale ()
  "Generate a perceptually uniform grayscale gradient in CIE Lab
без предельных 0 / 100 L*.  Шкала остаётся из 12 значений."
  (require 'color)
  (let ((n 12)
        (result nil))
    (dotimes (k n (sort result '<))
      (pcase-let* ((l* (* 100.0 (/ (float (1+ k)) (1+ n)))) ; 100/(n+1)…n·100/(n+1)
                   (`(,x ,y ,z) (color-lab-to-xyz l* 0 0))
                   (`(,r ,g ,b) (color-xyz-to-srgb x y z)))
        ;; для серого r=g=b, достаточно любой компоненты
        (let ((v (round (* 255 (color-clamp r)))))
          (push v result))))))

(defun tao-theme--taiji-scale (scale)
  "Return the same SCALE but в обратном порядке (preserves spacing)."
  (reverse scale))

(defun tao-theme-yin-palette ()
  "Generate a palette for the dark theme (yin): gradient from dark to light."
  (tao-theme--make-palette
   (funcall tao-theme-scale-fn)))

(defun tao-theme-yang-palette ()
  "Generate a palette for the light theme (yang): gradient from lighs to dark."
  (tao-theme--make-palette
   (tao-theme--taiji-scale (funcall tao-theme-scale-fn))))

(require 'cl-macs)
(require 'color)

(defcustom tao-theme-scale-fn #'tao-theme-lab-scale
  "Function used to generate a list of 12 grayscale intensity values.

Pre-defined choices
 • `tao-theme-perceptual-scale` – gamma-corrected sRGB (fast, default);
 • `tao-theme-lab-scale`        – uniform L* steps in CIE Lab
   (more perceptually accurate).

You may also supply any custom function that returns a list of 12
integers in the range 0–255."
  :type '(choice (const :tag "Gamma-corrected sRGB" tao-theme-perceptual-scale)
                 (const :tag "CIELab L*"            tao-theme-lab-scale)
                 (function :tag "Custom function"))
  :group 'tao-theme)

(defun tao-theme--make-palette (&optional scale)
  "Return alist of (:color-0 ... :color-11).
from darkest (black) to lightest (white).
If optional SCALE is given, use it instead of (funcall tao-theme-scale-fn)."
  (let* ((actual-scale (or scale (funcall tao-theme-scale-fn)))
         (palette
          (cl-loop for i from 0 to 11
                   for v in actual-scale
                   collect (cons (intern (format ":color-%d" i))
                                 (format "#%02X%02X%02X" v v v)))))
    palette))


(defgroup tao-theme nil "Tao Theme Groups." :group 'faces)

(defmacro tao (key)
  "Return color value from palette `p` for KEY, or 'unspecified if missing."
  `(or (cdr (assoc ,key p)) 'unspecified))

;; ---- Copied from tao-theme-legacy.el (for compatibility) ----
(defmacro tao-with-color-variables (palette &rest body)
  "Bind color variables for the given PALETTE and execute BODY."
  (declare (indent 1))
  `(let ((p ,palette))
    ,@body))

;; Declare Tao base faces for documentation & customizability
(defface tao-active '((t :inherit default)) "Active foreground/emphasis face for Tao theme.")
(defface tao-bg '((t :inherit default)) "Base background face for Tao theme.")
(defface tao-bg-alt '((t :inherit default)) "Alternate background face for Tao theme.")
(defface tao-base '((t :inherit default)) "Base neutral foreground face for Tao theme.")
(defface tao-muted '((t :inherit default)) "Muted/low-contrast face for Tao theme.")
(defface tao-accent '((t :inherit default)) "Accent/highlight face for Tao theme.")
(defface tao-strong '((t :inherit default)) "Strong emphasis face for Tao theme.")
(defface tao-faint '((t :inherit default)) "Faint/lowest-contrast face for Tao theme.")
(defface tao-inverse '((t :inherit default)) "Faint/lowest-contrast face for Tao theme.")
(defface tao-error '((t :inherit default)) "Error face for Tao theme.")
(defface tao-warning '((t :inherit default)) "Warning face for Tao theme.")
(defface tao-success '((t :inherit default)) "Success/positive face for Tao theme.")
(defface tao-link '((t :inherit default)) "Link/URI face for Tao theme.")

(defun tao-apply-theme-faces (theme-name palette)
  "Apply the main Tao faces to THEME-NAME using PALETTE (alist)."
  (let* ((class '((class color)
                  (min-colors 89)))
         (p palette))
    (custom-theme-set-faces
     theme-name
     ;; Base DRY faces
     `(tao-bg                      ((t (:background ,(tao :color-1)))))
     `(tao-bg-alt                  ((t (:background ,(tao :color-2)))))
     `(tao-base                    ((t (:foreground ,(tao :color-9)))))
     `(tao-muted                   ((t (:foreground ,(tao :color-7)))))
     `(tao-faint                   ((t (:foreground ,(tao :color-6)))))
     `(tao-active                  ((t (:foreground ,(tao :color-10)))))
     `(tao-accent                  ((t (:foreground ,(tao :color-11)))))
     `(tao-strong                  ((t (:foreground ,(tao :color-10) :weight bold))))
     `(tao-inverse                 ((t (:foreground ,(tao :color-0)))))
     `(tao-error                   ((t (:foreground ,(tao :color-11) :background ,(tao :color-4) :weight bold))))
     `(tao-warning                 ((t (:foreground ,(tao :color-7) :weight bold))))
     `(tao-success                 ((t (:foreground ,(tao :color-9) :bold t))))
     `(tao-link                    ((t (:foreground ,(tao :color-9) :underline t))))
     ;; Core faces, editor chrome
     `(default                     ((t (:background ,(tao :color-1) :foreground ,(tao :color-10)))))
     `(fringe                      ((t (:inherit tao-bg))))
     `(shadow                      ((t (:inherit tao-faint))))
     `(minibuffer-prompt           ((t (:inherit tao-accent :weight bold))))
     `(region                      ((t (:background ,(tao :color-4)))))
     `(secondary-selection         ((t (:background ,(tao :color-4)))))
     `(highlight                   ((t (:background ,(tao :color-5)))))
     `(vertical-border             ((t (:foreground ,(tao :color-6)))))
     `(cursor                      ((t (:background ,(tao :color-8)))))
     `(line-number                 ((t (:inherit tao-inverse))))
     `(line-number-current-line    ((t (:inherit hl-line :foreground ,(tao :color-6)))))
     `(linum                       ((t (:inherit tao-faint))))
     `(hl-paren-face               ((t (:inherit tao-bg))))
     `(paren-face-match            ((t (:inherit tao-strong))))
     `(paren-face-mismatch         ((t (:inherit tao-error :underline t))))
     `(paren-face-no-match         ((t (:inherit tao-faint :weight bold))))
     ;; Syntax/font-lock
     `(font-lock-builtin-face           ((t (:inherit tao-active))))
     `(font-lock-comment-face           ((t (:inherit tao-muted :slant italic))))
     `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
     `(font-lock-constant-face          ((t (:inherit tao-accent))))
     `(font-lock-doc-face               ((t (:inherit tao-muted :slant italic))))
     `(font-lock-function-name-face     ((t (:inherit tao-accent))))
     `(font-lock-keyword-face           ((t (:inherit tao-strong))))
     `(font-lock-string-face            ((t (:inherit tao-success :slant italic))))
     `(font-lock-type-face              ((t (:inherit tao-faint))))
     `(font-lock-variable-name-face     ((t (:inherit tao-base))))
     `(font-lock-warning-face           ((t (:inherit tao-warning))))
     ;; Modeline, header
     `(mode-line               ((t (:background ,(tao :color-1) :foreground ,(tao :color-11) :box (:line-width -1 :color ,(tao :color-7)) :height 1.0))))
     `(mode-line-inactive      ((t (:background ,(tao :color-1) :foreground ,(tao :color-6) :box (:line-width -1 :color ,(tao :color-2)) :height 1.0))))
     `(mode-line-buffer-id     ((t (:inherit tao-accent :weight bold))))
     `(header-line             ((t (:inherit mode-line :box nil :overline t))))
     ;; Buttons, highlights, link
     `(link                         ((t (:inherit tao-link))))
     `(link-visited                 ((t (:inherit tao-link :slant italic))))
     `(button                       ((t (:inherit link))))
     `(help-key-binding             ((t (:inherit tao-accent))))
     `(trailing-whitespace          ((t (:background ,(tao :color-2)))))
     ;; Show-paren
     `(show-paren-match        ((t (:inherit tao-strong :background ,(tao :color-5)))))
     `(show-paren-mismatch     ((t (:inherit tao-error))))
     ;; ------------------------------------------
     ;; Diff, Compilation, Search, Error
     ;; ------------------------------------------
     `(error                       ((t (:inherit tao-error))))
     `(warning                     ((t (:inherit tao-warning))))
     `(success                     ((t (:inherit tao-success))))
     `(compilation-error-face      ((t (:inherit error))))
     `(compilation-warning-face    ((t (:inherit warning))))
     `(compilation-info-face       ((t (:inherit tao-active))))
     `(compilation-line-number     ((t (:inherit tao-muted))))
     `(compilation-mode-line-exit  ((t (:inherit tao-success))))
     `(compilation-mode-line-fail  ((t (:inherit tao-error))))
     `(compilation-face            ((t (:inherit tao-base))))
     `(compilation-column-face     ((t (:inherit tao-muted))))
     `(compilation-enter-directory-face ((t (:inherit tao-success))))
     `(compilation-message-face    ((t (:inherit tao-link))))
     `(compilation-mode-line-run   ((t (:inherit tao-active))))
     `(compilation-leave-directory-face ((t (:inherit tao-muted))))
     `(compilation-line-face       ((t (:inherit tao-link))))
     `(compilation-info            ((t (:inherit tao-link))))
     `(compilation-mode-line-exit  ((t (:inherit tao-success))))
     `(compilation-mode-line-fail  ((t (:inherit tao-error))))
     `(diff-added                  ((t (:foreground ,(tao :color-9)) ))
                                  )
     `(diff-removed                ((t (:foreground ,(tao :color-8)) ))
                                  )
     `(diff-changed                ((t (:inherit tao-accent :weight bold))))
     `(diff-file-header            ((t (:background ,(tao :color-5) :weight bold))))
     `(diff-header                 ((t (:background ,(tao :color-3))))
                                  )
     `(diff-context                ((t (:inherit tao-muted))))
     ;; diff-hl
     `(diff-hl-insert              ((t (:background ,(tao :color-9)))))
     `(diff-hl-delete              ((t (:background ,(tao :color-8)))))
     `(diff-hl-change              ((t (:background ,(tao :color-7)))))
     ;; smerge
     `(smerge-upper                ((t (:background ,(tao :color-7)))))
     `(smerge-lower                ((t (:background ,(tao :color-8)))))
     `(smerge-markers              ((t (:background ,(tao :color-3)))))
     ;; DIRED/Dired+
     `(ediff-current-diff-A ((t (:background ,(tao :color-6)))))
     `(ediff-current-diff-B ((t (:background ,(tao :color-7)))))
     `(ediff-current-diff-C ((t (:background ,(tao :color-8)))))
     `(ediff-even-diff-A    ((t (:background ,(tao :color-2)))))
     `(ediff-odd-diff-A     ((t (:background ,(tao :color-4)))))
     `(ediff-fine-diff-A    ((t (:background ,(tao :color-11)))))
     `(ediff-fine-diff-B    ((t (:background ,(tao :color-11)))))
     `(ediff-fine-diff-C    ((t (:background ,(tao :color-11)))))
     `(dired-directory         ((t (:inherit tao-accent :weight bold))))
     `(dired-flagged           ((t (:inherit tao-error))))
     `(dired-header            ((t (:inherit tao-strong))))
     `(dired-mark              ((t (:inherit tao-success))))
     `(dired-marked            ((t (:inherit tao-warning))))
     `(dired-perm-write        ((t (:inherit tao-success))))
     `(dired-symlink           ((t (:inherit link))))
     `(dired-warning           ((t (:inherit tao-warning))))
     ;; Dired+
     `(diredp-dir-priv         ((t (:inherit dired-directory))))
     `(diredp-dir-name         ((t (:inherit tao-base :weight bold))))
     `(diredp-file-name        ((t (:inherit tao-base))))
     `(diredp-symlink          ((t (:inherit dired-symlink))))
     `(diredp-exec-priv        ((t (:inherit tao-active))))
     `(diredp-flag-mark        ((t (:inherit dired-mark))))
     `(diredp-flag-mark-line   ((t (:inherit dired-marked))))
     `(diredp-read-priv        ((t (:inherit tao-success))))
     `(diredp-write-priv       ((t (:inherit tao-strong))))
     `(diredp-no-priv          ((t (:inherit tao-muted))))
     ;; Company, Ivy, Corfu, Vertico, Orderless, Completions
     `(company-tooltip                  ((t (:inherit tao-bg-alt :foreground ,(tao :color-11)))))
     `(company-tooltip-selection        ((t (:inherit tao-strong  :background ,(tao :color-7)))))
     `(company-tooltip-mouse            ((t (:inherit company-tooltip-selection))))
     `(company-scrollbar-bg             ((t (:inherit tao-bg-alt))))
     `(company-scrollbar-fg             ((t (:inherit tao-bg))))
     `(company-preview                  ((t (:inherit tao-faint))))
     `(company-tooltip-annotation       ((t (:inherit tao-muted))))
     `(company-tooltip-common           ((t (:inherit tao-accent))))
     `(company-tooltip-common-selection ((t (:inherit tao-accent :weight bold))))
     `(corfu-current                    ((t (:inherit company-tooltip-selection  :background ,(tao :color-5)))))
     `(corfu-candidate-overlay-face     ((t (:inherit tao-faint))))
     `(vertico-current                  ((t (:inherit tao-strong :background ,(tao :color-4)))))
     `(orderless-match-face-0           ((t (:inherit tao-accent))))
     `(orderless-match-face-1           ((t (:inherit tao-success))))
     `(orderless-match-face-2           ((t (:inherit tao-active))))
     `(orderless-match-face-3           ((t (:inherit tao-warning))))
     `(completions-common-part          ((t (:inherit tao-strong :weight bold))))
     ;; Ivy
     `(ivy-current-match                ((t (:inherit tao-link :background ,(tao :color-7)))))
     `(ivy-minibuffer-match-face-1      ((t (:inherit tao-active))))
     `(ivy-minibuffer-match-face-2      ((t (:inherit tao-accent :weight bold))))
     `(ivy-minibuffer-match-face-3      ((t (:inherit tao-strong))))
     `(ivy-minibuffer-match-face-4      ((t (:inherit tao-success :weight bold))))
     ;; Magit
     `(magit-section-title  ((t (:inherit tao-strong))))
     `(magit-branch         ((t (:inherit tao-accent))))
     `(magit-log-author     ((t (:inherit tao-faint :slant italic))))
     `(magit-tag            ((t (:inherit tao-accent))))
     `(magit-item-highlight ((t (:background ,(tao :color-7)))))
     ;; Diff-hl, Ediff
     `(ediff-current-diff-A ((t (:background ,(tao :color-6)))))
     `(ediff-current-diff-B ((t (:background ,(tao :color-7)))))
     `(ediff-current-diff-C ((t (:background ,(tao :color-8)))))
     `(ediff-even-diff-A    ((t (:background ,(tao :color-2)))))
     `(ediff-odd-diff-A     ((t (:background ,(tao :color-4)))))
     `(ediff-fine-diff-A    ((t (:background ,(tao :color-11)))))
     `(ediff-fine-diff-B    ((t (:background ,(tao :color-11)))))
     `(ediff-fine-diff-C    ((t (:background ,(tao :color-11)))))
     ;; Gnus/email
     `(gnus-group-mail-1-empty  ((t (:inherit tao-base))))
     `(gnus-group-mail-2-empty  ((t (:inherit tao-muted))))
     `(gnus-group-mail-3-empty  ((t (:inherit tao-faint))))
     `(gnus-group-news-1-empty  ((t (:inherit tao-base :weight bold))))
     `(gnus-summary-high-read   ((t (:inherit tao-success))))
     `(gnus-summary-high-unread ((t (:inherit tao-accent))))
     `(gnus-header-content     ((t (:inherit tao-muted))))
     `(gnus-header-from        ((t (:inherit tao-base))))
     `(gnus-header-subject     ((t (:inherit tao-strong))))
     `(gnus-signature          ((t (:inherit tao-faint :slant italic))))
     `(message-header-name     ((t (:inherit tao-base))))
     `(message-header-other    ((t (:inherit tao-muted))))
     `(message-header-to       ((t (:inherit tao-accent))))
     `(message-header-from     ((t (:inherit tao-accent))))
     `(message-header-cc       ((t (:inherit tao-muted))))
     `(message-header-subject  ((t (:inherit tao-strong))))
     `(message-mml             ((t (:inherit tao-muted :slant italic))))
     `(message-separator       ((t (:inherit tao-faint))))
     `(message-cited-text      ((t (:inherit tao-muted :slant italic))))
     ;; Flycheck, Flymake, Flyspell
     `(flycheck-error     ((t (:inherit tao-error :underline (:style wave :color ,(tao :color-11) :position t)))))
     `(flycheck-warning   ((t (:inherit tao-warning :underline (:style wave :color ,(tao :color-10) :position t)))))
     `(flycheck-info      ((t (:inherit tao-active :underline (:style wave :color ,(tao :color-9) :position t)))))
     `(flyspell-incorrect ((t (:inherit tao-error :underline (:style wave :color ,(tao :color-11))))))
     `(flyspell-duplicate ((t (:inherit tao-warning :underline (:style wave :color ,(tao :color-10))))))
     `(flymake-error      ((t (:inherit tao-error :underline (:style wave :color ,(tao :color-11))))))
     `(flymake-warning    ((t (:inherit tao-warning :underline (:style wave :color ,(tao :color-10))))))
     `(flymake-note       ((t (:inherit tao-active :underline (:style wave :color ,(tao :color-10))))))
     ;; LSP, eglot, lsp-ui
     `(lsp-face-highlight-read ((t (:background ,(tao :color-6)))))
     `(lsp-headerline-breadcrumb-path-error-face    ((t (:underline (:style wave :color ,(tao :color-11))))))
     `(lsp-headerline-breadcrumb-path-warning-face  ((t (:underline (:style wave :color ,(tao :color-10))))))
     `(lsp-headerline-breadcrumb-path-info-face     ((t (:underline (:style wave :color ,(tao :color-10))))))
     `(lsp-headerline-breadcrumb-symbols-error-face ((t (:underline (:style wave :color ,(tao :color-11))))))
     `(lsp-headerline-breadcrumb-symbols-warning-face ((t (:underline (:style wave :color ,(tao :color-10))))))
     `(lsp-headerline-breadcrumb-symbols-info-face    ((t (:underline (:style wave :color ,(tao :color-10))))))
     `(lsp-ui-sideline-symbol                         ((t (:inherit tao-accent :slant italic))))
     `(lsp-ui-sideline-current-symbol                 ((t (:background ,(tao :color-8)))))
     `(lsp-ui-sideline-code-action                    ((t (:inherit tao-accent :box t))))
     `(eglot-highlight-symbol-face                    ((t (:inherit tao-link))))
     `(eglot-inlay-hint-face                          ((t (:inherit tao-faint :italic t))))
     ;; Org, outline, markdown
     `(outline-1                   ((t (:inherit tao-strong :height 1.3))))
     `(outline-2                   ((t (:inherit tao-strong :height 1.2))))
     `(outline-3                   ((t (:inherit tao-base))))
     `(outline-4                   ((t (:inherit tao-muted))))
     `(outline-6                   ((t (:inherit tao-faint))))
     `(outline-7                   ((t (:inherit tao-faint))))
     `(outline-8                   ((t (:inherit tao-faint))))
     `(org-level-1                 ((t (:inherit tao-strong :height 1.15))))
     `(org-level-2                 ((t (:inherit tao-base :height 1.12))))
     `(org-level-3                 ((t (:inherit tao-muted))))
     `(org-level-4                 ((t (:inherit tao-faint))))
     `(org-block                   ((t (:inherit tao-bg-alt))))
     `(org-link                    ((t (:inherit tao-link))))
     `(org-done                    ((t (:inherit tao-success :weight bold))))
     `(org-todo                    ((t (:inherit tao-warning :weight bold))))
     `(org-table                   ((t (:inherit tao-muted :inherit 'fixed-pitch))))
     `(org-warning                 ((t (:inherit tao-warning))))
     `(org-scheduled               ((t (:inherit tao-success))))
     `(org-headline-done           ((t (:inherit tao-faint :strike-through t))))
     `(org-document-title          ((t (:inherit tao-strong :height 1.35))))
     `(org-document-info           ((t (:inherit tao-muted))))
     ;; Markdown
     `(markdown-header-face-1      ((t (:inherit org-level-1))))
     `(markdown-header-face-2      ((t (:inherit org-level-2))))
     `(markdown-header-face-3      ((t (:inherit org-level-3))))
     `(markdown-header-face-4      ((t (:inherit org-level-4))))
     `(markdown-inline-code-face   ((t (:inherit tao-faint))))
     `(markdown-blockquote-face    ((t (:inherit tao-muted :slant italic))))
     `(markdown-bold-face          ((t (:weight bold))))
     `(markdown-italic-face        ((t (:slant italic))))
     `(markdown-header-face-5        ((t (:inherit outline-5))))
     `(markdown-header-face-6        ((t (:inherit outline-6))))
     `(markdown-list-face            ((t (:inherit tao-base))))
     `(markdown-markup-face          ((t (:inherit tao-faint))))
     `(markdown-html-attr-name-face  ((t (:inherit tao-muted))))
     `(markdown-html-attr-value-face ((t (:inherit tao-faint))))
     `(markdown-html-tag-delimiter-face ((t (:inherit tao-muted))))
     `(markdown-html-tag-name-face   ((t (:inherit tao-base))))
     `(markdown-table-face           ((t (:inherit tao-faint))))
     `(markdown-url-face             ((t (:inherit tao-link))))
     `(markdown-pre-face             ((t (:inherit tao-bg-alt))))
     `(markdown-language-keyword-face ((t (:inherit tao-faint :height 0.8))))
     ;; Term/ANSI
     `(term-color-black   ((t (:foreground ,(tao :color-0) :background ,(tao :color-0)))))
     `(term-color-white   ((t (:foreground ,(tao :color-11) :background ,(tao :color-11)))))
     `(term-color-red     ((t (:foreground ,(tao :color-11) :background ,(tao :color-6)))))
     `(term-color-green   ((t (:foreground ,(tao :color-10) :background ,(tao :color-2)))))
     `(term-color-yellow  ((t (:foreground ,(tao :color-9) :background ,(tao :color-6)))))
     `(term-color-blue    ((t (:foreground ,(tao :color-11) :background ,(tao :color-7)))))
     `(term-color-cyan    ((t (:foreground ,(tao :color-8) :background ,(tao :color-10)))))
     `(term-color-magenta ((t (:foreground ,(tao :color-10) :background ,(tao :color-8)))))
     `(term-default-fg-color             ((t (:inherit term-color-white))))
     `(term-default-bg-color             ((t (:inherit term-color-black))))
     `(ansi-color-names-vector [, (tao :color-0)
                                  , (tao :color-10)
                                  , (tao :color-9)
                                  , (tao :color-9)
                                  , (tao :color-11)
                                  , (tao :color-10)
                                  , (tao :color-8)
                                  , (tao :color-9)])
     ;; Misc/proxies (suite)
     `(widget-field                  ((t (:background ,(tao :color-1)))))
     `(hl-line                       ((t (:background ,(tao :color-1) :extend t))))
     `(highlight-symbol-face         ((t (:background ,(tao :color-5)))))
     `(error-face                    ((t (:inherit tao-error))))
     `(success-face                  ((t (:inherit tao-success))))
     `(warning-face                  ((t (:inherit tao-warning))))
     ;; Whitespace/folding/etc
     `(whitespace-tab      ((t (:background ,(tao :color-7)))))
     `(whitespace-space    ((t (:background ,(tao :color-4)))))
     `(whitespace-newline  ((t (:foreground ,(tao :color-6)))))
     `(whitespace-trailing ((t (:background ,(tao :color-5)))))
     ;; Rainbow delimiters (for lisps etc)
     `(rainbow-delimiters-depth-1-face ((t (:foreground ,(tao :color-7)))))
     `(rainbow-delimiters-depth-2-face ((t (:foreground ,(tao :color-8)))))
     `(rainbow-delimiters-depth-3-face ((t (:foreground ,(tao :color-9)))))
     `(rainbow-delimiters-depth-4-face ((t (:foreground ,(tao :color-10)))))
     `(rainbow-delimiters-depth-5-face ((t (:foreground ,(tao :color-11)))))
     `(rainbow-delimiters-depth-6-face ((t (:foreground ,(tao :color-8)))))
     `(rainbow-delimiters-depth-7-face ((t (:foreground ,(tao :color-9)))))
     `(rainbow-delimiters-depth-8-face ((t (:foreground ,(tao :color-10)))))
     `(rainbow-delimiters-depth-9-face ((t (:foreground ,(tao :color-11)))))
     ;; Explicit proxies for legacy/third-party
     `(ace-jump-face-background ((t (:inherit tao-faint))))
     `(ace-jump-face-foreground ((t (:inherit tao-accent :weight bold))))
     `(avy-lead-face            ((t (:inherit tao-accent :weight bold))))
     `(avy-background-face      ((t (:inherit tao-faint))))
     `(avy-lead-face-0          ((t (:inherit tao-accent :weight bold))))
     `(avy-lead-face-1          ((t (:inherit tao-accent :weight bold))))
     `(avy-lead-face-2          ((t (:inherit tao-accent :weight bold))))
     `(ace-window-path-face     ((t (:inherit tao-accent :weight bold))))
     `(ace-window-lead-face     ((t (:inherit tao-accent :weight bold))))
     `(ace-window-selection-face ((t (:inherit tao-accent))))
     `(company-echo-common      ((t (:inherit tao-accent))))
     `(egg-diff-add             ((t (:inherit diff-added))))
     `(egg-diff-del             ((t (:inherit diff-removed))))
     `(egg-diff-file-header     ((t (:inherit diff-hl-removed))))
     `(egg-section-title        ((t (:inherit tao-accent :weight bold))))
     `(egg-stash-mono           ((t (:inherit tao-faint))))
     `(helm-selection           ((t (:inherit tao-strong))))
     `(helm-header              ((t (:inherit tao-mutd))))
     `(magit-section-heading    ((t (:inherit tao-strong))))
     `(macrostep-expansion-highlight-face ((t (:inherit highlight))))
     `(macrostep-gensym-1
       ((t (:inherit tao-accent))))
     `(macrostep-gensym-2
       ((t (:inherit tao-accent))))
     `(macrostep-gensym-3       ((t (:inherit tao-accent))))
     `(macrostep-gensym-4       ((t (:inherit tao-accent))))
     `(macrostep-gensym-5       ((t (:inherit tao-accent))))
     `(macrostep-macro-face     ((t (:inherit tao-link))))
     `(powerline-active1        ((t (:inherit mode-line))))
     `(powerline-inactive2      ((t (:inherit mode-line-inactive))))
     `(tab-bar                  ((t (:background ,(tao :color-0)))))
     `(tab-bar-tab              ((t (:background ,(tao :color-1) :foreground ,(tao :color-11) :height 1.0))))
     `(tab-bar-tab-inactive     ((t (:background ,(tao :color-0) :foreground ,(tao :color-5)))))
     `(tab-line                 ((t (:background ,(tao :color-1) :italic nil))))
     `(tab-line-tab             ((t (:background ,(tao :color-1) :foreground ,(tao :color-11) :weight normal))))
     `(tab-line-tab-current     ((t (:background ,(tao :color-4) :foreground ,(tao :color-9)))))
     `(tab-line-tab-inactive    ((t (:background ,(tao :color-2) :foreground ,(tao :color-9)))))
     `(tabbar-default           ((t (:inherit tao-bg-alt))))
     `(tabbar-button            ((t (:inherit tao-link))))
     `(tabbar-selected          ((t (:inherit tao-link :weight bold))))
     `(tabbar-unselected        ((t (:inherit tao-faint))))
     ;; Window divider (muted/faint)
     `(window-divider                ((t (:inherit tao-faint))))
     `(window-divider-first-pixel    ((t (:inherit tao-faint))))
     `(window-divider-last-pixel     ((t (:inherit tao-faint))))
     `(centaur-tabs-default     ((t (:inherit tao-bg-alt))))
     `(centaur-tabs-selected    ((t (:inherit tao-strong))))
     `(centaur-tabs-unselected  ((t (:inherit tao-faint))))
     `(bm-face                  ((t (:inherit tao-warning :background ,(tao :color-5)))))
     `(bm-fringe-face            ((t (:inherit tao-warning))))
     `(bm-fringe-persistent-face ((t (:inherit tao-warning :foreground ,(tao :color-1)))))
     `(bm-persistent-face        ((t (:inherit tao-warning :background ,(tao :color-2)))))
     `(eldoc-highlight-function-argument ((t (:inherit tao-base :weight bold))))
     `(eval-sexp-fu-flash                ((t (:inherit tao-warning :background ,(tao :color-5)))))
     `(eval-sexp-fu-flash-error          ((t (:inherit tao-error :background ,(tao :color-6)))))
     ;; Icomplete/Ido
     `(icomplete-first-match             ((t (:inherit tao-accent :weight bold))))
     `(icomplete-selected                ((t (:inherit tao-strong))))
     `(ido-first-match                   ((t (:inherit tao-accent))))
     `(ido-only-match                    ((t (:inherit tao-accent :weight bold))))
     `(ido-subdir                        ((t (:inherit tao-success))))
     `(ido-indicator                     ((t (:inherit tao-accent :background ,(tao :color-7)))))
     ;; Magit/Section
     `(magit-section-heading-selection   ((t (:inherit tao-strong :foreground ,(tao :color-10)))))
     `(magit-section-highlight           ((t (:inherit tao-base   :background ,(tao :color-6)))))
     `(magit-item-highlight              ((t (:inherit tao-bg-alt))))
     `(magit-diff-file-heading-highlight ((t (:inherit tao-bg-alt))))
     ;; whitespace (extras)
     `(whitespace-hspace                ((t (:inherit tao-faint))))
     `(whitespace-line                  ((t (:inherit tao-bg-alt))))
     `(whitespace-space-before-tab      ((t (:inherit tao-faint))))
     `(whitespace-indentation           ((t (:inherit tao-faint))))
     `(whitespace-empty                 ((t (:inherit tao-faint))))
     `(whitespace-space-after-tab       ((t (:inherit tao-faint))))
     ;; yascroll, neotree, tuareg, typescript, geiser, eglot, etc.
     `(yascroll:thumb-text-area         ((t (:inherit tao-bg-alt))))
     `(yascroll:thumb-fringe            ((t (:inherit tao-bg-alt))))
     `(neotree-dir-link-face            ((t (:inherit tao-strong))))
     `(neotree-expand-btn-face          ((t (:inherit tao-link :weight bold))))
     `(neotree-file-link-face           ((t (:inherit tao-faint))))
     `(neotree-root-dir-face            ((t (:inherit tao-muted :background ,(tao :color-1)))))
     `(prolog-face                      ((t (:inherit tao-accent))))
     `(tuareg-font-lock-operator-face   ((t (:inherit tao-base))))
     `(tuareg-font-lock-governing-face  ((t (:inherit tao-link))))
     `(typescript-primitive-face        ((t (:inherit tao-base :italic t))))
     `(typescript-this-face             ((t (:inherit tao-base :weight bold :italic t))))
     `(geiser-font-lock-doc-link        ((t (:inherit tao-link))))
     `(geiser-font-lock-error-link      ((t (:inherit tao-error :underline t))))
     `(geiser-font-lock-autodoc-identifier ((t (:inherit tao-strong))))
     `(eglot-diagnostic-tag-unnecessary-face ((t (:inherit tao-faint :italic t :underline ,(tao :color-5))))))))

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

(defun tao/print-faces ()
  "Print base tao-faces.
Buffer named *Tao Faces*."
  (interactive)
  (let ((faces '(tao-bg
                 tao-bg-alt
                 tao-base
                 tao-muted
                 tao-active
                 tao-accent
                 tao-strong
                 tao-faint
                 tao-error
                 tao-warning
                 tao-success
                 tao-link)))
    (with-current-buffer (get-buffer-create "*Tao Faces*")
      (erase-buffer)
      (insert (propertize "Tao Base Faces:\n" 'face 'bold))
      (dolist (face faces)
        (let ((line (format "%s\n" (symbol-name face))))
          (add-text-properties 0 (length line)
                               (list 'face face)
                               line)
          (insert line)))
      (display-buffer (current-buffer)))))

(provide 'tao-theme)

;;; tao-theme.el ends here
