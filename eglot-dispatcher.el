;;; eglot-strategies.el --- Declarative LSP strategy dispatcher for Eglot -*- lexical-binding: t; -*-

;; Author: Your Name <your@email.com>
;; Version: 0.1
;; Keywords: languages, tools
;; Package-Requires: ((emacs "29.1") (eglot "1.15"))
;; URL: https://github.com/yourname/eglot-strategies

;;; Commentary:

;; This package allows dynamic dispatch of multiple LSP servers per buffer using
;; project-aware strategy definitions. It supports fallback defaults, multiple modes,
;; and integration with LSP multiplexers like `lspx`.

;;; Example

;; (use-package eglot-strategies
;;   :load-path "~/path/to/your/package"
;;   :hook ((typescript-ts-mode . eglot-strategies-dispatch)
;;          (html-ts-mode . eglot-strategies-dispatch))
;;   :init
;;   (setq eglot-strategies-list
;;         '((:name "Angular + ESLint"
;;            :modes (typescript-ts-mode html-ts-mode)
;;            :project-root "angular.json"
;;            :command (lambda (root)
;;                       (list
;;                        `("npx" "ngserver"
;;                          "--stdio"
;;                          "--tsProbeLocations" ,(expand-file-name "node_modules" root)
;;                          "--ngProbeLocations" ,(expand-file-name "node_modules" root))
;;                        '("vscode-eslint-language-server" "--stdio"))))
;;           (:name "Default TS"
;;            :modes (typescript-ts-mode)
;;            :command (lambda (_root)
;;                        '(:command ("typescript-language-server" "--stdio")
;;                          :initializationOptions (:tsserver (:logVerbosity "verbose"))))))
;;  ))


;;; Code:

(require 'cl-lib)
(require 'project)
(require 'eglot)

(defgroup eglot-strategies nil
  "Declarative LSP dispatcher for Eglot."
  :prefix "eglot-strategies-"
  :group 'tools)

(defcustom eglot-strategies-list nil
  "List of LSP strategy plists."
  :type 'list
  :group 'eglot-strategies)

(defun eglot-dispatcher--wrap-lspx (commands)
  "Wrap multiple COMMANDS in a single `lspx` invocation."
  (apply #'append '("lspx")
         (cl-mapcan (lambda (cmd)
                      (list "--lsp" (mapconcat #'identity cmd " ")))
                    commands)))

;;;###autoload
(defun eglot-dispatcher-dispatch ()
  "Use the first matching LSP strategy from `eglot-strategies-list`."
  (unless (eglot-managed-p)
    (let ((current-mode major-mode)
          (matched nil))
      (dolist (strategy eglot-strategies-list)
        (let ((modes (plist-get strategy :modes)))
          (when (member current-mode modes)
            (let* ((rootfile (plist-get strategy :project-root))
                   (root (cond
                          (rootfile (locate-dominating-file default-directory rootfile))
                          ((project-current) (project-root (project-current)))
                          (t nil)))
                   (extra-pred (plist-get strategy :predicate)))
              (when (and root (or (null extra-pred)
                                  (funcall extra-pred root)))
                (let* ((cmd-fn (plist-get strategy :command))
                       (cmds (funcall cmd-fn root))
                       (wrapped-cmd (if (and (listp cmds)
                                             (listp (car cmds))) ;; multiple commands
                                        (eglot-dispacher--wrap-lspx cmds)
                                      cmds)))
                  (setq-local eglot-server-programs
                              `(((,current-mode) . ,(if (keywordp (car wrapped-cmd))
                                                        wrapped-cmd
                                                      `(:command ,wrapped-cmd)))))
                  (message "eglot-strategies: Using strategy: %s (%s)"
                           (plist-get strategy :name)
                           (if (listp (car cmds)) "lspx" "single LSP"))
                  (setq matched t)
                  (eglot-ensure))
                (cl-return))))))
      (unless matched
        (message "eglot-strategies: No strategy matched.")))))

(provide 'eglot-dispatcher)
;;; eglot-dispatcher=.el ends here
