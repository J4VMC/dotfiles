function brew_daily_update
    set -l log_file ~/.brew-update.log
    echo (date) " - brew_daily_update function started." >>$log_file

    set -l update_marker ~/.brew_last_update
    set -l lock_file ~/.brew_update.lock
    set -l today (date +%Y-%m-%d)

    # --- 1. Check timestamp ---
    if test -f $update_marker
        if test (cat $update_marker) = "$today"
            echo (date) " - Already updated today. Exiting." >>$log_file
            return 0
        end
    end

    # --- 2. PID-based Lock (Self-Healing) ---
    if test -f $lock_file
        set -l lock_pid (cat $lock_file 2>/dev/null)

        # Check if the process is still running
        # 'kill -0' checks for existence without actually killing
        if test -n "$lock_pid" -a -n "$lock_pid" # Ensure valid string
            if kill -0 $lock_pid 2>/dev/null
                echo (date) " - Another update process (PID $lock_pid) is actively running. Exiting." >>$log_file
                return 0
            else
                # Process is dead, but file remains (Stale Lock)
                echo (date) " - Found stale lock file from dead PID $lock_pid. Removing." >>$log_file
                rm -f $lock_file
            end
        else
            # File exists but is empty or invalid
            rm -f $lock_file
        end
    end

    # Acquire Lock: Write the current Process ID ($fish_pid)
    echo $fish_pid >$lock_file
    echo (date) " - Lock acquired (PID $fish_pid)." >>$log_file

    # Set a Trap to ensure the file is removed when this script finishes (or crashes)
    trap 'rm -f $lock_file' EXIT

    # --- 3. Find Brew ---
    set -l brew_cmd
    if test -x /opt/homebrew/bin/brew
        set brew_cmd /opt/homebrew/bin/brew
    else if test -x /usr/local/bin/brew
        set brew_cmd /usr/local/bin/brew
    else
        echo (date) " - CRITICAL: Could not find brew command. Exiting." >>$log_file
        return 1
    end
    echo (date) " - Found brew at: $brew_cmd" >>$log_file

    # --- 4. Set Environment ---
    echo (date) " - Evaluating 'brew shellenv'..." >>$log_file
    eval ($brew_cmd shellenv) >>$log_file 2>&1
    if test $status -ne 0
        echo (date) " - CRITICAL: 'brew shellenv' failed. Exiting." >>$log_file
        return 1
    end
    echo (date) " - Environment set. PATH is now: $PATH" >>$log_file

    # --- 5. Run Update ---
    echo (date) " - Starting 'brew update'..." >>$log_file
    if $brew_cmd update >>$log_file 2>&1
        echo (date) " - 'brew update' complete. Starting 'brew upgrade'..." >>$log_file

        if $brew_cmd upgrade >>$log_file 2>&1
            echo (date) " - 'brew upgrade' complete. Starting 'brew cleanup'..." >>$log_file

            if $brew_cmd cleanup >>$log_file 2>&1
                echo (date) " - 'brew cleanup' complete." >>$log_file

                # SUCCESS
                echo $today >$update_marker
                echo (date) " - SUCCESS: Homebrew maintenance complete." >>$log_file
            else
                echo (date) " - FAILED: 'brew cleanup' failed." >>$log_file
            end
        else
            echo (date) " - FAILED: 'brew upgrade' failed." >>$log_file
        end
    else
        echo (date) " - FAILED: 'brew update' failed." >>$log_file
    end

    # 'trap' handles the lock removal automatically here.
end
