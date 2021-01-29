# Doom Emacs configuration

Private [Doom Emacs](https://github.com/hlissner/doom-emacs) configuration.

## Private modules

Adding private modules is as easy as adding them in the [modules](./modules)
folder, so this is my testing ground :
- before pushing PRs to Doom, or
- to easily toggle features

### Completion

#### Selectrum
Trying out selectrum from time to time to see if I prefer it to Ivy or Helm

### Config

#### Smartparens

This is the ~modules/config/default +smartparens~ part I copied in order to
control the changes in smartparens behaviour. I will probably deprecate it
soon.

### Editor

#### Format

A copy of the WIP format module rewrite using apheleia

### Lang

#### Beancount

Support for Beancount files. Basically just including the beacount major mode
from beancount source code (I changed the base mode be cause I target newer
emacs).

I am also trying to add `bean-format` as a formatter. This is a WIP.

#### Powershell (basic module example)

Support for powershell. Most basic module ever, if anyone wants to see how to
"package" a plugin and a simple config in a module.

### Misc.

#### Elcord (basic module example)

Rich presence in Discord (get the /playing Doom Emacs/ status)

#### C/C++ module

Inspired by maskRay, I keep my cc configuration apart. Mostly just a few
overrides of settings (for GDB / Projectile), and if `lsp-mode` is used (not
`eglot`), then the `ccls` configuration kicks in too, adding a few functions
wrote by maskRay for code navigation in the ccls-tree mode.

#### Transient (Spacemacs-like)

Use [Hercules.el](https://gitlab.com/jjzmajic/hercules.el) to get "transient
states" like Spacemacs advertises. Currently this is just a test module to see
how it could be integrated in Doom. I only use it for buffers and for windows
(and not even that often)

### Tools

#### Direnv

A copy of the WIP direnv module rewrite

#### FZF

A try to integrate [fzf.el](https://github.com/bling/fzf.el) directly in Doom,
its architecture (i.e. maintain the fzf process with a subjob in emacs) is
better for the filtering of large lists of candidates

#### Tree Sitter

A draft to include [Tree Sitter](https://tree-sitter.github.io/tree-sitter/)
into Doom. This currently works "well enough" for my setup, but this is the
kind of module which is hard to PR because of the width of the scope and
support issue it might generate

### UI

#### Telephone line

I use telephone line as my modeline. I mostly copied some modeline formats from
the default :ui modeline module, and made them work nicely with telephone-line.

The arbritrary choice of separators conviced me to go for it.

#### Workspaces

Building an Emacs 27+ version of `:ui workspace` module to get rid of the costly
(maintenance-wise) persp-mode dependency.

It uses the new tab-bar-mode to handle the workspaces, and uses bufler solely
for buffer grouping.

Hopefully, adding in burly.el and bookmark+ will help create per-project
persistence to return exactly where the user was before closing.
