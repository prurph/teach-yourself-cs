# Programming Module

This module is based on Structure and Interpretation of Computer Programs (SICP) and Berkeley's CS61A course.

## Structure and Interpretation of Computer Programs (SICP)

[sicp.pdf](https://web.mit.edu/alexmv/6.037/sicp.pdf)

[Exercise Solutions](http://community.schemewiki.org/?sicp-solutions)

[Drew Hess' Soltuions](http://wiki.drewhess.com/wiki/Category:SICP_solutions)

- I generally find these superior to the community wiki

### SICP Picture Language

[Chapter 2.2.4](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_sec_2.2.4) implements a picture language

[sicp-pict docs](https://docs.racket-lang.org/sicp-manual/SICP_Picture_Language.html)

To run the exercises, use DrRacket. I prefer to use homebrew Racket in a REPL for everything else.

- Download and install Racket from the internet (folder to drop into /Applications)
- Inside DrRacket: File > Install Package > sicp
- Add the following lines to the files to run:

```scheme
#lang sicp

;; The sicp language has a different require syntax with `#%`
(#% require sicp-pict)

(paint einsein)
```



Chapter 2.4

## CS61A

[Lectures](https://archive.org/details/ucberkeley-webcast-PL3E89002AA9B9879E)

[Course Page](https://people.eecs.berkeley.edu/~bh/61a-pages/)

## Scheme

This module uses Scheme. I installed a Scheme interpreter and REPL through
[Racket](https://racket-lang.org/) via brew/pkg manager, and several useful Vim plugins as described
in [Configuring Vim for SICP](https://crash.net.nz/posts/2014/08/configuring-vim-for-sicp/)

### REPL

Install xrepl (and it's infinite dependencies)

```shell
$ raco pkg install xrepl
```

Run the Racket REPL

```shell
$ racket -i -l xrepl -p dyoo/simply-scheme

; See if se ("sentence" from simply-scheme) is available
> (se 1 2 3)
'(1 2 3)

$ racket -i -l xrepl -p neil/sicp

; See if nil (from sicp) is available
> nil
'()

$ racket -i -l xrepl -p dyoo/sicp-concurrency:1:2/sicp-concurrency -p neil/sicp
```

- `-i` interactively
- `-l` require the extended repl plackage (readline, tab completion, etc)
- `-p` load useful packages from Racket PLaneT repo
  - dyoo/simply-scheme: includes builtins used in CS61A
  - dyoo/sicp-concurrency:1:2/sicp-concurrency: includes `parallel-execute` used in Section 3.4
  - neil/sicp: includes inc, dec, nil, etc assumed by the book

#### Warning About Packages

I initially required both the simply-scheme and sicp packages, however ran into
issues because sicp uses an `mcons` for pairs (and takes over the `cons`
procedure when you require it last), whereas the former has `cons`. I have not
dug into this issue too much, rather have settled on just using one package in
the REPL, typically simply-scheme.

```scheme
> (reduce + (list 1 2 3))
; cdr: contract violation
;   expected: pair?
;   given: (mcons 1 (mcons 2 (mcons 3 '())))
; [,bt for context]
```

This is confusing because `(pair? (mcons 1 '())` etc. works. I haven't dug into
it, but easiest just to use the simply-scheme package, especially because `cons`
implements a much easier to read representation in the repl.

Unfortunately, because it uses `cons` and not the mutable `mcons`, simply-scheme
doesn't support `set-car!` and `set-cdr!`. Gross.

For Section 3.4: Concurrency, I used both dyoo/sicp-concurrency and neil/sicp,
requiring the latter second to make the list implementation `mcons` to enable
`set-car!` and `set-cdr!`.

#### Requiring Packages Inside a File

##### For Local Files/Code

- In the module, use `(provide my-identifier)` to export something
- To require it, use `(require path/to/my-file.scm)`

##### For PLaneT (Racket Package Manager)

As an alternative to using `-p <package-spec>` when invoking the REPL, require inside the REPL or any file

- [PLaneT packages at racket-lang.org](http://planet.racket-lang.org/)
- PLaneT is deprecated. New packages are at [pkgs.racket-lang.org](https://pkgs.racket-lang.org/package)

```rkt
(require (planet dyoo/sicp-concurrency:1:2/sicp-concurrency))
```

### Paredit

[paredit.vim](https://github.com/vim-scripts/paredit.vim) is a plugin to manage
editing of parentheses in Lisps.

Useful shortcuts (`:h paredit-keys`)

#### Normal Mode

- `<` and `>` "barf/slurp" parens left and right
  - Prefix with `<leader>` to use while in insert
- `(` and `)` find opening/closing of current list (repeat to move up/down levels)
- `[[` and `]]` move between top-level deffuns
- `<leader>W` wrap current symbol or selection in parens
  - This is a shrotcut for `<leader>w(`, and curlies, braces can also be used

#### Insert Mode

- `)` moves the cursor beyond the (auto-inserted) closing paren
  - Extremely useful to move to typing the next expression instead of going into normal mode, moving the cursor to the left, then going back into insert

