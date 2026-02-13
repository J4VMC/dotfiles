;;; debug.el --- All necessary tools for debugging -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures `dap-mode` (Debug Adapter Protocol).
;;
;; What is DAP?
;; DAP is a standardized protocol that allows Emacs to communicate with
;; language-specific "debug adapters." This provides a fully-featured,
;; visual, step-by-step debugging experience identical to what you would
;; find in an IDE like VS Code or IntelliJ.
;;
;; ;;
;;
;; Configuration Overview:
;; 1. Core `dap-mode` setup and a unified `C-c d` keymap.
;; 2. Visual layout configuration using `dap-ui`.
;; 3. Language-specific adapter configurations (Python, Go, PHP, Rust, JS/TS).
;; 4. "Debug Templates" (launch configurations) for common project types.
;;
;;; Code:

;; =============================================================================
;; DAP MODE CORE
;; =============================================================================

(use-package dap-mode
  :ensure t
  ;; Ensure LSP (Language Server Protocol) loads first, as DAP often relies on it.
  :after lsp-mode
  :init
  ;; --- Keybinding Setup ---
  ;; Create a dedicated prefix map under `C-c d` for all debugging commands.
  ;; -> This centralizes debugger controls and avoids polluting the global namespace.

  (define-prefix-command 'my-dap-prefix-map)
  (global-set-key (kbd "C-c d") 'my-dap-prefix-map)

  ;; Essential debugger controls:
  (define-key my-dap-prefix-map (kbd "b") #'dap-breakpoint-toggle)   ; Toggle Breakpoint
  (define-key my-dap-prefix-map (kbd "B") #'dap-breakpoint-delete-all) ; Clear Breakpoints
  (define-key my-dap-prefix-map (kbd "d") #'dap-debug)                 ; Start Debugging (prompts for template)
  (define-key my-dap-prefix-map (kbd "n") #'dap-next)                  ; Step Over
  (define-key my-dap-prefix-map (kbd "i") #'dap-step-in)               ; Step Into
  (define-key my-dap-prefix-map (kbd "o") #'dap-step-out)              ; Step Out
  (define-key my-dap-prefix-map (kbd "c") #'dap-continue)              ; Continue (run to next breakpoint)
  (define-key my-dap-prefix-map (kbd "e") #'dap-debug-edit-template)   ; Edit Launch Configs
  (define-key my-dap-prefix-map (kbd "w") #'dap-switch-stack-frame)    ; Navigate Call Stack
  (define-key my-dap-prefix-map (kbd "l") #'dap-debug-last)            ; Rerun Last Session
  (define-key my-dap-prefix-map (kbd "r") #'dap-disconnect)            ; Stop/Disconnect
  (define-key my-dap-prefix-map (kbd "R") #'dap-debug-recent)          ; Select Recent Session

  :hook
  ;; Automatically enable debugging features in any buffer where LSP is active.
  ((lsp-mode . dap-mode)
   (lsp-mode . dap-ui-mode))

  :custom
  ;; Default Python configuration.
  ;; -> 'debugpy' is the modern, official Microsoft adapter for Python.
  (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)

  :config
  ;; --- Load Language Adapters ---
  ;; `require` the necessary adapters so `dap-mode` knows how to handle them.
  ;; We configure them in detail in separate blocks below.
  (require 'dap-python)   ; Python (debugpy)
  (require 'dap-php)      ; PHP (Xdebug)
  (require 'dap-dlv-go)   ; Go (Delve)
  (require 'dap-lldb)     ; Native C/C++/Rust (LLDB)
  (require 'dap-gdb-lldb) ; Native C/C++/Rust (GDB/LLDB bridge)
  (dap-gdb-lldb-setup)    ; Initialize the bridge
  (require 'dap-node)     ; JS/TS (Node.js)

  ;; --- Global Debug Templates ---
  ;; Templates act like `launch.json` in VS Code. They define how to start your app.

  ;; Python: Flask Web App
  (dap-register-debug-template "Python :: Debug (Flask)"
			       (list :type "python" :args "" :cwd nil
				     ;; Inject required environment variables before launch.
				     :env '(("FLASK_APP" . "app:app")
					    ("FLASK_ENV" . "development"))
				     :module "flask" :name "Python :: Debug (Flask)"))

  ;; Python: Django Web App
  (dap-register-debug-template "Python :: Debug (Django)"
			       (list :type "python" :args "manage.py runserver" :cwd nil
				     :name "Python :: Debug (Django)"))

  ;; Python: Pytest
  (dap-register-debug-template "Python :: Debug (Pytest)"
			       (list :type "python" :args "-m pytest -v" :cwd nil
				     :name "Python :: Debug (Pytest)"))

  ;; --- UX Enhancements ---
  ;; Automatically pop up a helpful keybinding cheat-sheet (`dap-hydra`)
  ;; every time the debugger pauses (e.g., hitting a breakpoint).
  (add-hook 'dap-stopped-hook (lambda (arg) (call-interactively #'dap-hydra))))

;; =============================================================================
;; DAP UI CONFIGURATION
;; =============================================================================

;; Configures the visual panels: Locals, Watches, Breakpoints, and Call Stack.
(use-package dap-ui
  :ensure nil ; Already installed as a dependency of `dap-mode`.
  :after dap-mode
  :config
  ;; Enable the UI elements and on-screen controls globally.
  (dap-ui-mode 1)
  (dap-ui-controls-mode 1)

  :custom
  ;; --- Window Layout ---
  ;; Defines exactly where the debugger windows should dock when a session starts.
  (dap-ui-buffer-configurations
   `((,(regexp-quote "*dap-ui-locals*")      . ((side . right) (slot . 1) (window-width . 0.20)))
     (,(regexp-quote "*dap-ui-expressions*") . ((side . right) (slot . 2) (window-width . 0.20)))
     (,(regexp-quote "*dap-ui-breakpoints*") . ((side . left)  (slot . 2) (window-width . 0.20)))
     (,(regexp-quote "*dap-ui-sessions*")    . ((side . left)  (slot . 3) (window-width . 0.20))))))

;; =============================================================================
;; LANGUAGE-SPECIFIC ADAPTER SETUPS
;; =============================================================================
;;
;; These blocks contain logic and templates specific to individual languages.
;; Note: `:ensure nil` is used because the adapters were `require`d in the main block.

;; --- Python ---
(use-package dap-python
  :ensure nil
  :after dap-mode
  :hook (python-ts-mode . dap-python-setup)
  :config
  ;; Smart Virtual Environment handling.
  (defun my/dap-python-get-interpreter ()
    "Return the active venv Python interpreter, falling back to system python3."
    (let ((venv (getenv "VIRTUAL_ENV")))
      (if (and venv (> (length venv) 0))
	  (concat venv "/bin/python3")
	"python3")))

  ;; Dynamically set the executable path based on the active environment.
  (setq dap-python-executable (my/dap-python-get-interpreter)))

;; --- PHP ---
(use-package dap-php
  :ensure nil
  :after dap-mode
  :hook (php-ts-mode . dap-php-setup)
  :config
  ;; Template: Listen for incoming Xdebug connections (e.g., from a browser).
  (dap-register-debug-template "PHP :: Listen for XDebug"
			       (list :type "php" :name "PHP :: Listen for XDebug" :port 9003))

  ;; Template: Debug a Laravel backend via `artisan serve`.
  (dap-register-debug-template "PHP :: Debug (Laravel)"
			       (list :type "php" :name "PHP :: Debug (Laravel)" :port 9003
				     :runtimeExecutable "php" :runtimeArgs '("artisan" "serve")
				     :program "${workspaceFolder}")))

;; --- Go ---
(use-package dap-dlv-go
  :ensure nil
  :after dap-mode
  :hook (go-ts-mode . dap-go-setup)
  :config
  ;; Template: Compile and debug the currently active Go file.
  (dap-register-debug-template "Go :: Debug (Current File)"
			       (list :type "go" :name "Go :: Debug (Current File)" :mode "debug"
				     :request "launch" :program "${file}")))

;; --- JavaScript / TypeScript ---
(use-package dap-node
  :ensure nil
  :after dap-mode
  :hook ((js-ts-mode typescript-ts-mode tsx-ts-mode) . dap-node-setup)
  :config
  ;; Template: Run the current JS/TS file.
  (dap-register-debug-template "JS/TS :: Debug (Current File)"
			       (list :type "node" :name "JS/TS :: Debug (Current File)" :request "launch"
				     :program "${file}" :cwd "${fileDirname}"
				     ;; Dynamically add `ts-node` if executing a TypeScript file.
				     :runtimeArgs (when (string-match-p "\\.ts$" (or (buffer-file-name) ""))
						    '("-r" "ts-node/register"))
				     ;; Ignore internal Node.js code when stepping through functions.
				     :skipFiles '("<node_internals>/**"))))

;; --- Rust (via LLDB) ---
(use-package dap-lldb
  :ensure nil
  :after dap-mode
  :hook ((rust-ts-mode) . dap-lldb-setup)
  :config
  ;; Template: Debug a compiled Rust binary.
  ;; -> Assumes a standard `cargo build` directory structure.
  (dap-register-debug-template "Rust :: Debug (Binary)"
			       (list :type "lldb" :name "Rust :: Debug (Binary)" :request "launch"
				     :program "${workspaceFolder}/target/debug/${workspaceFolderBasename}"
				     :cwd "${workspaceFolder}")))

;; =============================================================================
;; UTILITIES FOR DEBUGGING DAP ITSELF
;; =============================================================================

(defun dap-enable-logging ()
  "Enable logging of JSON messages between Emacs and the Debug Adapter.
Useful for troubleshooting adapter connection issues."
  (interactive)
  (setq dap-print-io t))

(defun dap-disable-logging ()
  "Disable DAP JSON logging."
  (interactive)
  (setq dap-print-io nil))

;; =============================================================================
;; FINALIZE
;; =============================================================================

(provide 'debug)

;;; debug.el ends here
