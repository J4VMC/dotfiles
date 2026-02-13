;;; terminal.el --- Terminal emulation and shell configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures the terminal experience inside Emacs.
;;
;; We primarily use `vterm`, which is a "full" terminal emulator based on a 
;; compiled C library (libvterm). Unlike built-in options like `shell` or 
;; `eshell`, `vterm` is fast enough to run intensive CLI apps like `htop`, 
;; `vim`, or complex shell themes without lag.
;;
;; Key features:
;; 1. Fish shell integration for modern syntax highlighting.
;; 2. "Quake-style" pop-up terminal via `vterm-toggle`.
;; 3. Intelligent project-root detection for automatic `cd` on launch.
;; 4. Force-kill functionality to bypass "Process is running" prompts.
;;
;;; Code:

;; =============================================================================
;; SHELL SUPPORT (FISH)
;; =============================================================================

;; Provides syntax highlighting and indentation for .fish script files.
;; -> Useful if you use Fish as your interactive shell (configured below).
(use-package fish-mode
  :ensure t)

;; =============================================================================
;; VTERM (THE TERMINAL EMULATOR)
;; =============================================================================

(use-package vterm
  :ensure t
  :init
  ;; Clipboard behavior:
  ;; -> Set to `nil` to include the shell prompt when copying text.
  (setq vterm-copy-exclude-prompt nil)

  ;; **CRITICAL PERFORMANCE**:
  ;; -> Vterm uses a compiled C "module" for speed. This setting ensures Emacs
  ;;    automatically compiles that module if it's missing or outdated.
  (setq vterm-always-compile-module t)

  ;; Buffer limits and responsiveness:
  (setq vterm-max-scrollback 20000) ; Lines of history to remember.
  (setq vterm-timer-delay 0.01)     ; Refresh rate (lower = snappier, higher CPU).

  ;; Remote Support (TRAMP):
  ;; -> Defines which shells to attempt when SSHing into remote machines.
  (setq vterm-tramp-shells
        '("/usr/bin/bash" "/bin/bash" "/bin/zsh" "docker" "/bin/sh"))

  :bind (:map vterm-mode-map
              ;; **PASTE OVERRIDE**:
              ;; -> Standard Emacs `C-y` (yank) often behaves oddly in terminals.
              ;; -> Mapping it to `vterm-yank` ensures it pastes from your system
              ;;    clipboard as expected in a modern terminal environment.
              ("C-y" . vterm-yank))

  :config
  ;; Set the default shell path.
  (setq vterm-shell (executable-find "fish"))

  ;; --- Directory & File Integration ---
  ;; This allows the shell to "talk" to Emacs. If the shell sends a specific 
  ;; escape sequence, Emacs will execute the corresponding function.
  (setq vterm-eval-cmds '(("find-file" find-file)
                          ("message" message)
                          ("vterm-clear-scrollback" vterm-clear-scrollback)
                          ("dired" dired)
                          ("ediff-files" ediff-files)))

  ;; Automatically close the Emacs window/buffer when you type `exit` in the shell.
  (setq vterm-kill-buffer-on-exit t)

  ;; ===========================================================================
  ;; CUSTOM "FORCE KILL" LOGIC
  ;; ===========================================================================
  ;;
  ;; Standard Emacs behavior prompts "Process is running; kill it?" every time 
  ;; you try to close a terminal. This `M-k` binding bypasses that prompt.

  (defun kill-buffer-and-its-windows (buffer)
    "Kill BUFFER and all windows displaying it without mercy."
    (interactive (list (read-buffer "Kill buffer: " (current-buffer) 'existing)))
    (setq buffer (get-buffer buffer))
    (if (buffer-live-p buffer)
        (let ((wins (get-buffer-window-list buffer nil t)))
          (when (and (buffer-modified-p buffer)
                     (fboundp '1on1-flash-ding-minibuffer-frame))
            (1on1-flash-ding-minibuffer-frame t))
          (when (kill-buffer buffer)
            (dolist (win wins)
              (when (window-live-p win)
                (condition-case nil
                    (delete-window win)
                  (error nil))))))
      (when (interactive-p)
        (error "Buffer `%s` is already dead." buffer))))

  ;; Bind `M-k` (Alt-k) to force-kill the terminal immediately.
  (define-key vterm-mode-map (kbd "M-k")
	      (lambda ()
		(interactive)
		(let ((buffer (current-buffer)))
		  ;; Silence the "Process is running" exit prompt.
		  (set-process-query-on-exit-flag (get-buffer-process buffer) nil)
		  (kill-buffer-and-its-windows buffer))))

  ;; Make URLs and file paths inside the terminal clickable.
  (add-hook 'vterm-mode-hook 'goto-address-mode))

;; =============================================================================
;; VTERM-TOGGLE (THE POP-UP DRAWER)
;; =============================================================================
;;
;; 
;;
;; This package provides a "Quake-style" terminal. Pressing `s-9` toggles 
;; a terminal window at the bottom of the screen.

(use-package vterm-toggle
  :after vterm
  :ensure t
  :config
  ;; Use a single, dedicated terminal buffer across all windows.
  (setq vterm-toggle-scope 'dedicated)

  ;; Automatically `cd` to the project root (Git/Projectile) when opening.
  (setq vterm-toggle-project-root t)
  (setq vterm-toggle-cd-auto-create-buffer nil)

  ;; UI Behavior:
  (setq vterm-toggle-reset-window-configuration-after-exit t)
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-hide-method 'delete-window)

  ;; --- Window Placement Rules ---
  ;; This tells Emacs: "If it's a vterm-toggle buffer, pop it up at the bottom
  ;; and make it take up exactly 30% of the screen height."
  (add-to-list
   'display-buffer-alist
   '((lambda (buffer-or-name _)
       (let ((buffer (get-buffer buffer-or-name)))
         (with-current-buffer buffer
           (or (equal major-mode 'vterm-mode)
               (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
     (display-buffer-reuse-window display-buffer-in-direction)
     (direction . bottom)
     (dedicated . t)
     (reusable-frames . visible)
     (window-height . 0.3)
     (window-width . 0.3)))

  ;; ===========================================================================
  ;; ROBUST PROJECT ROOT DETECTION (OVERRIDE)
  ;; ===========================================================================
  ;;
  ;; We override vterm-toggle's internal logic to use a more powerful 
  ;; "cascading" search for project roots.

  (defun my/get-project-root ()
    "Find the project root using Project.el, Projectile, or Git."
    (or
     (when (fboundp 'project-root)
       (when-let* ((project (project-current nil)))
         (project-root project)))
     (when (fboundp 'projectile-project-root)
       (projectile-project-root))
     (when-let ((git-dir (locate-dominating-file default-directory ".git")))
       (expand-file-name git-dir))
     default-directory))

  ;; Force vterm-toggle to use our smarter root-finding function.
  (defun vterm-toggle--new (&optional buffer-name)
    "Launch a new vterm using my/get-project-root."
    (let* ((buffer-name (or buffer-name vterm-buffer-name))
           (default-directory
            (if vterm-toggle-project-root
                (my/get-project-root)
              default-directory)))
      (if vterm-toggle-fullscreen-p
          (vterm buffer-name)
        (if (eq major-mode 'vterm-mode)
            (let ((display-buffer-alist nil))
              (vterm buffer-name))
          (vterm-other-window buffer-name)))))

  (defun vterm-toggle--project-root ()
    "Internal helper override for project root."
    (my/get-project-root)))

;; =============================================================================
;; GLOBAL CONTROLS
;; =============================================================================

;; Toggle the terminal drawer globally.
;; `s-9` = Cmd-9 (macOS) or Win-9 (Linux/Windows).
(global-set-key (kbd "s-9") 'vterm-toggle)

;; Multi-vterm: Simplifies managing and switching between multiple terminal sessions.
(use-package multi-vterm :ensure t)

;; =============================================================================
;; FINALIZE
;; =============================================================================

(provide 'terminal)

;;; terminal.el ends here
