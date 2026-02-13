;;; projects.el --- Project management and sidebar navigation -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file centralizes the configuration for "projects."
;; In Emacs, a project is defined as any directory containing a version control
;; marker (like a `.git` folder) or a project-specific file.
;;
;; This configuration utilizes two primary packages:
;;
;; 1. Projectile: The "Logic" Engine.
;;    A backend tool that understands project boundaries. It allows you to find
;;    files, switch between projects, and perform project-wide searches.
;;
;; 2. Treemacs: The "Visual" Sidebar.
;;    Provides a classic IDE-style file explorer sidebar. It allows you to browse
;;    the file tree visually using either the keyboard or mouse.
;;
;; We also unify these tools under a custom "Jump" keymap using the `s-p`
;; (Super-p) prefix for a streamlined developer experience.
;;
;;; Code:

;; =============================================================================
;; PROJECTILE (THE LOGIC ENGINE)
;; =============================================================================

(use-package projectile
  :ensure t
  :init
  ;; Disable Projectile's default "C-c p" keymap so we can use our custom setup.
  (setq projectile-enable-keymap nil)

  ;; Define the root directories where your code projects live.
  (setq projectile-project-search-path '("~/Projects")
        
        ;; PERFORMANCE: Use `rg` (ripgrep) for file indexing.
        ;; -> Significantly faster than the built-in Emacs `find` command.
        projectile-generic-command "rg -0 --files --color=never --hidden --glob !.git/ --max-filesize 1M"
        
        ;; PERFORMANCE: Use `rg` for project-wide searching (grep).
        projectile-grep-command "rg -n --with-filename --no-heading --max-columns=150 --ignore-case --max-filesize 1M --glob !.git/"
        
        ;; Enable caching for even faster subsequent file lookups.
        projectile-enable-caching t)
  :config
  ;; Activate Projectile globally.
  (projectile-mode 1))

;; =============================================================================
;; TREEMACS (THE VISUAL SIDEBAR)
;; =============================================================================
;;
;; 

(use-package treemacs
  :ensure t
  ;; PERFORMANCE: Defer loading until the first time a Treemacs command is used.
  :defer t
  :bind
  (("s-0" . treemacs-select-window)       ; Move cursor focus to the sidebar.
   ("C-c t d" . treemacs-select-directory)) ; Manually add a folder to the sidebar.
  :config
  ;; UX: Enable single-click to expand or collapse folders (default is double-click).
  (with-eval-after-load 'treemacs
    (define-key treemacs-mode-map [mouse-1] 'treemacs-single-click-expand-action))

  ;; --- Core Sidebar Settings ---
  (setq treemacs-collapse-dirs (if treemacs-python-executable 3 0)
        treemacs-display-in-side-window t
        treemacs-indentation 2         ; Folder indent spacing.
        treemacs-width 50               ; Default sidebar width in columns.
        
        ;; UI Syncing:
        treemacs-follow-after-init t   ; Highlight the current file on startup.
        treemacs-file-follow-delay 0.2 ; How quickly to sync the sidebar with the editor.
        
        ;; Persistence & Cleanup:
        treemacs-missing-project-action 'ask
        treemacs-no-delete-other-windows t ; Prevent sidebar from closing when splitting windows.
        treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        
        ;; Exclusion List: Hide "junk" directories to keep the tree clean.
        treemacs-litter-directories '("/node_modules" "/.venv" "/.cask" "/vendor")
        
        treemacs-position 'left
        treemacs-show-hidden-files t   ; Show dotfiles like .env or .gitignore.
        treemacs-workspace-switch-cleanup nil)

  ;; --- Treemacs Modes ---

  (treemacs-follow-mode t)     ; Automatically select the current file in the tree.
  (treemacs-filewatch-mode t)   ; Refresh the tree if files are changed externally (e.g., Git pull).
  (treemacs-fringe-indicator-mode 'always) ; Show expansion icons in the left margin.

  ;; Git Integration: Show file status (modified, new, ignored) via colors/icons.
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t) (treemacs-git-mode 'deferred))
    (`(t . _) (treemacs-git-mode 'simple))))

;; =============================================================================
;; SIDEBAR INTEGRATIONS
;; =============================================================================

;; Unified Icons: Share Treemacs icons with Dired for visual consistency.
(use-package treemacs-icons-dired
  :ensure t
  :hook (dired-mode . treemacs-icons-dired-enable-once))

;; Magit Integration: Better Git awareness within the tree.
(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;; Projectile Integration: The "Bridge" package.
;; -> Ensures Treemacs understands Projectile's project definitions.
(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

;; Workspace Integration: Restrict Treemacs view to the current tab.
(use-package treemacs-tab-bar
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

;; =============================================================================
;; UNIFIED "JUMP" KEYMAP (SUPER-P)
;; =============================================================================
;;
;; We group all project-related actions under the `s-p` prefix. This provides a
;; centralized "Command Palette" for project navigation.

;; 1. Initialize the prefix command.
(define-prefix-command 'my-jump-map)
;; 2. Bind it to Super-p.
(global-set-key (kbd "s-p") 'my-jump-map)

;; 3. Projectile Shortcuts (Project Logic)
(with-eval-after-load 'projectile
  (define-key my-jump-map (kbd "p") 'projectile-switch-project)   ; Open another project.
  (define-key my-jump-map (kbd "f") 'projectile-find-file)        ; Find file in project.
  (define-key my-jump-map (kbd "b") 'projectile-switch-to-buffer) ; Search project buffers.
  (define-key my-jump-map (kbd "d") 'projectile-dired)            ; Open file manager at root.
  (define-key my-jump-map (kbd "g") 'projectile-grep)             ; Search text in project.
  (define-key my-jump-map (kbd "c") 'projectile-compile-project)  ; Trigger build command.
  (define-key my-jump-map (kbd "r") 'projectile-run-vterm))       ; Launch terminal at root.

;; 4. Treemacs Shortcuts (UI Controls)
(with-eval-after-load 'treemacs
  (define-key my-jump-map (kbd "0") 'treemacs-select-window)      ; Focus the sidebar.
  (define-key my-jump-map (kbd "t") 'treemacs-add-and-display-current-project-exclusively))

;; 5. Terminal Integration
(with-eval-after-load 'vterm-toggle
  (define-key my-jump-map (kbd "v") 'vterm-toggle))               ; Toggle pop-up terminal.

;; =============================================================================
;; FINALIZE
;; =============================================================================

(provide 'projects)

;;; projects.el ends here
