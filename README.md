# spacetraders.el
Emacs Lisp client for the SpaceTraders API

Bless this mess

![image](https://user-images.githubusercontent.com/19170025/113608380-220f4000-9610-11eb-9d38-3e1853d8aae0.png)

### Requirements
- Emacs >= 27.1 (unsure about older versions)
- request.el
- deferred.el
- request-deferred.el

### Commands
`M-x`
Read/write to local state:
- `st-get`
- `st-set` (can trigger other actions).

Display certain data in the minibuffer:
- `st-query`


HTTP Methods across various endpoints:
- `st-fetch` (GET)
- `st-post`
- `st-put`


Display data in buffers:
- `st-display` (display entities as a table)
- `st-view-user` (show current user stats)
