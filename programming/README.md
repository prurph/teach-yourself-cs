# Programming Module

This module is based on Structure and Interpretation of Computer Programs (SICP) and Berkeley's CS61A course.

## Structure and Interpretation of Computer Programs (SICP)

[sicp.pdf](https://web.mit.edu/alexmv/6.037/sicp.pdf)

[Exercise Solutions](http://community.schemewiki.org/?sicp-solutions)

## CS61A

[Lectures](https://archive.org/details/ucberkeley-webcast-PL3E89002AA9B9879E)

[Course Page](https://people.eecs.berkeley.edu/~bh/61a-pages/)

## Scheme

This module uses Scheme. I installed a Scheme interpreter and REPL through
[Racket](https://racket-lang.org/), and several useful Vim plugins as described
in [Configuring Vim for SICP](https://crash.net.nz/posts/2014/08/configuring-vim-for-sicp/)

### REPL

Run the Racket REPL

```shell
$ racket -i -l xrepl -p dyoo/simply-scheme -p neil/sicp

; See if nil (from sicp) is available
> nil
'()

; See if se ("sentence" from simply-scheme) is available
> (se 1 2 3)
'(1 2 3)
```

- `-i` interactively
- `-l` require the extended repl plackage (readline, tab completion, etc)
- `-p` load useful packages from Racket PLaneT repo
  - dyoo/simply-scheme: includes builtins used in CS61A
  - neil/sicp: includes inc, dec, nil, etc assumed by the book


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

#### Insert Mode

- `)` moves the cursor beyond the (auto-inserted) closing paren
  - Extremely useful to move to typing the next expression instead of going into normal mode, moving the cursor to the left, then going back into insert
