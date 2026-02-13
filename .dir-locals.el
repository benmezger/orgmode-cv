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

                          ;; Auto-export to PDF on save
                          (add-hook 'after-save-hook
                                    (lambda ()
                                      (let ((name (file-name-nondirectory buffer-file-name))
                                            (dir (file-name-directory buffer-file-name)))
                                        (cond
                                         ((member name '("cv.org"))
                                          (org-latex-export-to-pdf t))
                                         ((string= name "basecv.org")
                                          (dolist (f '("cv.org"))
                                            (with-current-buffer (find-file-noselect (expand-file-name f dir))
                                              (org-latex-export-to-pdf t)))))))
                                    nil t)))))))
