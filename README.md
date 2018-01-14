A Matrix client in CHICKEN Scheme
=================================

Only the most basic features have been implemented:

- password login
- message receiving
- message sending
- emotes
- basic notifications

The text user interface is very crude and has a few bugs.

If you want to support this project, consider donating [on liberapay](https://liberapay.com/Kooda/)!


How to use it
-------------

If you grabbed the source from git:
`git submodule init` followed by `git submodule update`

To build from sources (git or source tarball) run:
`make`

Run with:
`./ensemble`

It will prompt you for your server’s URL and credentials.

You can quit by typing /exit or /quit at the input prompt.

All configuration and session storage is kept in the $XDG_CONFIG_HOME/ensemble.dat file (usually ~/.config/ensemble.dat)


Available commands
------------------

- `/me text` sends an emote
- `/room regexp` (or `/r regexp`) room moves you to the room matching the regex
- `/exit` or `/quit` saves the session to config.scm and quits


Available key bindings
----------------------

- `backspace` removes the character before the cursor
- `C-d` removes the character at the cursor
- `left` or `C-b` moves the cursor back one character
- `right` or `C-f` moves the cursor forward one character
- `C-a` moves the cursor at the beginning of the line
- `C-f` moves the cursor at the end of the line
- `C-k` removes all characters starting at the cursor position
- `M-b` moves the cursor back one word
- `M-f` moves the cursor forward one word
- `M-d` removes the word at the right of the cursor
- `C-w` removes the word at the left of the cursor
- `Return` when the input line is empty moves the read marker to the last message
- `Page Up` and `Page Down` scroll the history of the current room
- `M-n` switches to the first room of the notification stack
- `Tab` to auto-complete nicknames (only works at the beginning of a message)

TUI Roadmap and ideas
---------------------

- [x] Room participation
- [x] Session storage
- [ ] Programmable key bindings and commands
- [ ] Configurable external commands for media handling
- [ ] Low bandwidth mode (for mobile connection)
- [ ] Configurable notifications support (external command? sounds? terminal beep?)
- [ ] End to end encryption
    - [ ] Flat file for room keys, device keys and trusted keys for easy import/export
- [ ] Optional state/timeline persistent storage
- [ ] Configurable typing notifications
- [ ] Media sending
- UI design
    - [ ] Login screen
    - [ ] Easy (and lazy) room history navigation
    - [ ] Rooms grouping
    - [ ] Tiny screen support: N900 has a 79x18 terminal (79x21 fullscreen)
    - [ ] Big screen support (st fullscreen is 239x67 here)
    - [ ] Good color support (8, 16, 256, 24 bits)
        - [ ] Nickname coloration
        - [ ] Images thumbnails
    - Message display
        - [ ] Partial MXID showing when multiple display names are identical
        - [ ] Message date
        - [ ] Formated messages viewing
        - [ ] Contrast check for colored messages
        - [ ] Non-intrusive receipt indicators
    - Message / command composition
        - [ ] Formated message composition
        - [ ] Multi-lines messages with intelligent behaviour when pasting text
        - [ ] Typing suggestions / completion (like [this](https://asciinema.org/a/37390))
        - [ ] Suggestion menu / command submenu
    - Encryption
        - [ ] Good E2E key verification interface
        - [ ] Encryption validity indicator
    - [ ] Last read message indicator
    - [ ] Screen indicators when viewing older messages, with a way to quickly go back to the present
    - [ ] Room participants screen with actions menu (private message, kick, ban, …)
    - [ ] Message/events navigation with contextual actions (quote, open/save media, …)
    - [ ] Status bar with typing notifications, encryption status, other rooms status…
    - [ ] Room directory screen with easy access to other servers’ directory
