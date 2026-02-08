;;; cv-export-init.el --- Async export init file for CV org files -*- lexical-binding: t; -*-

;; This file is loaded by the async Emacs process during org-export.
;; It must set up everything needed for the CV export to work.

(require 'org)
(require 'ox-latex)
(require 'ox-extra)

(ox-extras-activate '(ignore-headlines))

;; Add custom CV latex class
(add-to-list 'org-latex-classes
             '("custom" "\\documentclass{custom}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; Custom CV keywords
(dolist (opt '((:tagline "TAGLINE" nil nil t)
               (:summary "SUMMARY" nil nil t)))
  (add-to-list 'org-export-options-alist opt))

;; Hyperref template
(setq org-latex-hyperref-template
      "\\hypersetup{
 pdfauthor={%a},
 pdftitle={%t},
 pdfkeywords={%k},
 pdfsubject={%d},
 pdfcreator={%c},
 pdflang={%L}}\n")

;; Preamble filter for CV keywords
(defun cv-latex-insert-preamble (output backend info)
  "Insert CV-specific LaTeX commands into preamble."
  (if (org-export-derived-backend-p backend 'latex)
      (let ((tagline (plist-get info :tagline))
            (summary (plist-get info :summary))
            (commands ""))
        (when tagline
          (setq commands (concat commands (format "\\cvtagline{%s}\n" tagline))))
        (when summary
          (setq commands (concat commands (format "\\cvsummary{%s}\n" summary))))
        (replace-regexp-in-string
         "\\\\begin{document}"
         (concat commands "\\begin{document}")
         output t t))
    output))
(add-to-list 'org-export-filter-final-output-functions
             'cv-latex-insert-preamble)

;; Custom headline handler for positions
(defun cv-org-latex-headline (orig-fun headline contents info)
  "Around advice for org-latex-headline to handle CV positions."
  (let ((position (org-element-property :POSITION headline))
        (prior-position (org-element-property :PRIOR_POSITION headline))
        (prior-heading (org-element-property :PRIOR_HEADING headline))
        (highlight (org-element-property :HIGHLIGHT headline))
        (cv-location (org-element-property :CV_LOCATION headline))
        (location (org-element-property :LOCATION headline))
        (date (org-element-property :DATE headline))
        (title (org-export-data (org-element-property :title headline) info)))
    (cond
     ;; Header with contact info
     (cv-location
      (let ((phone (org-element-property :CV_PHONE headline))
            (phone-display (org-element-property :CV_PHONE_DISPLAY headline))
            (email (org-element-property :CV_EMAIL headline))
            (username (org-element-property :CV_USERNAME headline))
            (website (org-element-property :CV_WEBSITE headline)))
        (concat
         "\\heading\n"
         (format "\\contactinfo{%s}{%s}{%s}{%s}{%s}{%s}\n"
                 cv-location phone phone-display email username website)
         (or contents ""))))
     (position
      (concat
       (format "\\position{%s}{%s}{%s}{%s}\n\n"
               position title (or location "") (or date ""))
       (or contents "")))
     (prior-heading
      (concat "\\priorheading\n" (or contents "")))
     ;; Highlight entry (bold title with colon, then contents)
     (highlight
      (concat (format "\\textbf{%s}: %s" title (or contents ""))
              "\\\\[0.2\\baselineskip]\n"))
     (prior-position
      (concat
       (format "\\priorposition{%s}{%s}{%s}{%s}\n"
               prior-position title (or location "") (or date ""))
       (or contents "")))
     ;; Auto-insert \introduction for headlines with :OPENER: property
     ((org-element-property :OPENER headline)
      (funcall orig-fun headline
               (concat "\\introduction\n\n" (or contents ""))
               info))
     (t (funcall orig-fun headline contents info)))))
(advice-add 'org-latex-headline :around #'cv-org-latex-headline)

;; Other settings
(setq org-latex-prefer-user-labels t)
(setq org-latex-logfiles-extensions
      '("lof" "lot" "tex~" "aux" "idx" "log" "out"
        "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk"
        "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"
        "xmpi" "run.xml" "bcf"))

(provide 'cv-export-init)
;;; cv-export-init.el ends here
