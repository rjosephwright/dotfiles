;;; early-init.el --- Early startup settings -*- lexical-binding: t; -*-

;; Package activation is handled explicitly by init.el.
(setq package-enable-at-startup nil)

(setq inhibit-splash-screen t
      menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

(let* ((font-size
        (cond ((string= system-type "darwin") "12")
              ((string= system-type "gnu/linux") "13")
              ((string= system-type "windows-nt") "10")))
       (font (concat "Source Code Pro-" font-size)))
  (dolist (parameter `((font . ,font)
                       (menu-bar-lines . 0)
                       (tool-bar-lines . 0)
                       (horizontal-scroll-bars . nil)
                       (vertical-scroll-bars . nil)))
    (add-to-list 'default-frame-alist parameter)))

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
