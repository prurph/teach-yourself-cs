# Structure and Interpretation of Computer Programs (SICP)

[sicp.pdf](https://web.mit.edu/alexmv/6.037/sicp.pdf)

## Setup and Tricks

### Paredit

[paredit.vim](https://github.com/vim-scripts/paredit.vim) is a plugin to manage
editing of parentheses in Lisps.

Useful shortcuts (`:h paredit-keys`)

#### Normal Mode

- `<` and `>` "barf/slurp" parens left and right
  - Prefix with `<leader>` to use while in insert
- `(` and `)` find opening/closing of current list (repeat to move up/down levels)
- `[[` and `]]` move between top-level deffuns
- `<leader>W` wrap current (selection, motion, etc) in parens
  - This is a shrotcut for `<leader>w(`, and curlies, braces can also be used
