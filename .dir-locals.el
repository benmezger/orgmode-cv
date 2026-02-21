;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((org-mode . ((eval . (progn
                        (let ((cv-dir (file-name-directory
                                       (or (buffer-file-name) default-directory))))
                          ;; Load shared export setup for interactive session
                          (load (expand-file-name "cv-export-init.el" cv-dir) nil t)

                          ;; Point async export to the same init file
                          (setq-local org-export-async-init-file
                                      (expand-file-name "cv-export-init.el" cv-dir))

                          ;; Define export function for easy calling
                          (defun benmezger/cv-export-pdfs ()
                            "Export CV org files to PDF."
                            (interactive)
                            (let ((name (file-name-nondirectory buffer-file-name))
                                  (dir (file-name-directory buffer-file-name)))
                              (cond
                               ((member name '("cv.org"))
                                (org-latex-export-to-pdf t))
                               ((string= name "basecv.org")
                                (dolist (f '("cv.org"))
                                  (with-current-buffer (find-file-noselect (expand-file-name f dir))
                                    (org-latex-export-to-pdf t)))))))

                          ;; Auto-export to PDF on save
                          (add-hook 'after-save-hook #'benmezger/cv-export-pdfs nil t))))))
 (latex-mode . ((eval . (progn
                          (let ((cv-dir (file-name-directory
                                         (or (buffer-file-name) default-directory))))
                            ;; Auto-export CVs when custom.cls changes
                            (add-hook 'after-save-hook
                                      (lambda ()
                                        (when (string= (file-name-nondirectory buffer-file-name) "custom.cls")
                                          (dolist (f '("cv.org"))
                                            (let ((org-file (expand-file-name f cv-dir)))
                                              (when (file-exists-p org-file)
                                                (with-current-buffer (find-file-noselect org-file)
                                                  (org-latex-export-to-pdf t)))))))
                                      nil t)))))))
