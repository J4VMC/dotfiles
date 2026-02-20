# =============================================================================
# fish_config --- Main configuration for the Fish Shell
# =============================================================================
#
# Commentary:
# This is the entry point for the Fish shell. It handles path setup, 
# environment variables, and interactive features like prompts and aliases.
#
# The file is split into two main parts:
# 1. Universal Setup: Always runs (important for GUI apps like Emacs).
# 2. Interactive Setup: Runs only when you are actually typing in a terminal.
#
# =============================================================================
# 1. CORE ENVIRONMENT (PATH & VARIABLES)
# =============================================================================

# Disable the default Fish "Welcome" message for a cleaner startup.
set -U fish_greeting

# --- Homebrew Initialization ---
# Automatically detects the Homebrew installation path based on the OS/Architecture.
if test -x /opt/homebrew/bin/brew # Apple Silicon macOS
    eval (/opt/homebrew/bin/brew shellenv)
else if test -x /usr/local/bin/brew # Intel macOS
    eval (/usr/local/bin/brew shellenv)
else if test -x /home/linuxbrew/.linuxbrew/bin/brew # Linux
    eval (/home/linuxbrew/.linuxbrew/bin/brew shellenv)
end

# --- Homebrew Completions ---
# Force Fish to look at Homebrew's completion directories
if test -d "$HOMEBREW_PREFIX/share/fish/completions"
    set -p fish_complete_path "$HOMEBREW_PREFIX/share/fish/completions"
end
if test -d "$HOMEBREW_PREFIX/share/fish/vendor_completions.d"
    set -p fish_complete_path "$HOMEBREW_PREFIX/share/fish/vendor_completions.d"
end

# --- Custom PATH Additions ---
# Adds specific directories to the system PATH so their binaries can be executed.
# - Emacs bin: For Emacs-specific CLI tools.
# - GNU Grep: Replaces the default BSD grep with the faster GNU version.
# - Cargo: For Rust binaries.
# - Coursier: For Scala/Java binaries.
# - .local/bin: For user-installed Python/pip scripts.
fish_add_path -g \
    $HOME/.emacs.d/bin \
    $HOMEBREW_PREFIX/opt/grep/libexec/gnubin \
    $HOME/.cargo/bin \
    "$HOME/Library/Application Support/Coursier/bin" \
    $HOME/.local/bin

# --- Environment Variables ---
set -gx EDITOR emacs # Set Emacs as the default editor for git, etc.
set -gx GPG_TTY (tty) # Required for GPG signing and encryption.

# =============================================================================
# 2. INTERACTIVE-ONLY SETUP
# =============================================================================
#
# This section only executes if you are using the shell in a terminal window.

if status is-interactive

    # --- System Info ---
    if command -q fastfetch
        fastfetch
    end

    # --- Starship Prompt ---
    # 
    # A modern, fast, and customizable shell prompt.
    if command -q starship
        function starship_transient_prompt_func
            starship module character
        end
        starship init fish | source
        enable_transience # Keeps the scrollback clean by simplifying old prompts.
    end

    # --- Zoxide (Smarter `cd`) ---
    # Remembers your most visited folders so you can jump to them instantly.
    if command -q zoxide
        zoxide init fish --cmd cd | source
    end

    # --- McFly (Smarter `Ctrl-R`) ---
    # Replaces the default shell history search with a neural-network-powered UI.
    if command -q mcfly
        mcfly init fish | source
    end

    # --- CLI Tool Completion (Interactive Only) ---
    if type -q symfony
        symfony completion fish | source
    end

    if command -q gh
        gh completion -s fish | source
    end

    if command -q fzf
        fzf --fish | source
    end

    # -------------------------------------------------------------------------
    # BACKGROUND UPDATES
    # -------------------------------------------------------------------------
    # Checks once per day if Homebrew needs an update.
    # Runs in the background (nohup) so it doesn't slow down shell startup.
    set -l last_update (cat ~/.brew_last_update 2>/dev/null)
    set -l today (date +%Y-%m-%d)

    if test "$last_update" != "$today"
        nohup fish -c brew_daily_update >/dev/null 2>&1 &
    end

    # -------------------------------------------------------------------------
    # ALIASES & ABBREVIATIONS
    # -------------------------------------------------------------------------
    alias python=python3
    alias brewed="brew bundle dump --file=~/dotfiles/homebrew/.Brewfile --force" # Backup Homebrew setup
    alias brewup="brew update && brew upgrade && brew cleanup" # Update everything

    # eza: A modern replacement for 'ls' with icons and better colors.
    if command -q eza
        alias ls='eza --icons'
        alias ll='eza -l --icons'
        alias lt='eza -T --icons' # Tree view
    end

    # bat: A modern replacement for 'cat' with syntax highlighting.
    if command -q bat
        alias cat='bat --paging=never'
    end

    # -------------------------------------------------------------------------
    # PLUGIN CONFIGURATIONS
    # -------------------------------------------------------------------------
    set -g puffer_fish_color 8ec07c # Color for puffer-fish (syntax highlighting)
    set -g __fish_abbreviation_tips_enable_on_command_execution 1

    # -------------------------------------------------------------------------
    # GRUVBOX COLOR PALETTE
    # -------------------------------------------------------------------------
    # Applies the Gruvbox theme colors to the Fish UI.
    set -g fish_color_normal ebdbb2
    set -g fish_color_command b8bb26
    set -g fish_color_quote d79921
    set -g fish_color_redirection d3869b
    set -g fish_color_end 83a598
    set -g fish_color_error fb4934
    set -g fish_color_param 8ec07c
    set -g fish_color_comment 928374
end
