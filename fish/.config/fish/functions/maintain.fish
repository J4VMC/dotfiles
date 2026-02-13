# ~/dotfiles/fish/.config/fish/functions/maintain.fish

function maintain
    echo "🔄 1. Updating System Software..."
    brew update
    brew upgrade
    brew cleanup

    echo "🎣 2. Updating Fisher Plugins..."
    fisher update

    echo "📝 3. Updating Brewfile..."
    # Dumps current state to the symlinked .Brewfile in your repo
    brew bundle dump --global --describe --force

    # Update the timestamp so the background job doesn't run today
    date +%Y-%m-%d > ~/.brew_last_update

    echo "✅ System Synced!"
end