;;; early-init.el --- Early startup settings -*- lexical-binding: t; -*-

;; Older GCC drivers derive an invalid macOS target from Darwin 25 and later.
(when (and (eq system-type 'darwin)
           (>= (string-to-number operating-system-release) 25))
  (setenv "MACOSX_DEPLOYMENT_TARGET" "26.0")
  ;; GCC's configured SDK can be newer than the selected Apple linker.
  (with-temp-buffer
    (unless (eq 0 (call-process "/usr/bin/xcrun" nil '(t nil) nil
                                "--sdk" "macosx" "--show-sdk-path"))
      (error "Cannot locate the selected macOS SDK"))
    (goto-char (point-min))
    (setenv "SDKROOT"
            (buffer-substring-no-properties (point) (line-end-position))))
  ;; The app adds the CLT SDK library path again in native compiler subprocesses.
  (let ((sdk-library-path-form
         '(let* ((sdk-library-directory
                  (expand-file-name "usr/lib" (getenv "SDKROOT")))
                 (library-paths
                  (split-string (or (getenv "LIBRARY_PATH") "") path-separator t)))
            (setenv "LIBRARY_PATH"
                    (mapconcat
                     (lambda (path)
                       (if (equal path
                                  "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib")
                           sdk-library-directory
                         path))
                     library-paths path-separator)))))
    (eval sdk-library-path-form t)
    (setq native-comp-async-env-modifier-form
          `(progn
             ,(and (boundp 'native-comp-async-env-modifier-form)
                   native-comp-async-env-modifier-form)
             ,sdk-library-path-form))))

;;; early-init.el ends here
