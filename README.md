# spacetraders.el
Emacs Lisp client for the SpaceTraders API

Bless this mess

### Commands
`M-x`
Read/write to local state:
- `st-get`
- `st-set` (can trigger other actions).

Display certain into in the minibuffer:
- `st-query`


HTTP Methods across various endpoints:
- `st-fetch` (GET)
- `st-post`
- `st-put`


Display data in buffers:
- `st-display` (display entities as a table)
- `st-view-user` (show current user stats)
