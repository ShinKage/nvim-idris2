# EXPERIMENTAL: Idris2 IDE Mode for neovim in Idris2 (via Lua)

WARNING: Still in very experimental phase, the code is not packaged to be a
proper neovim plugin and thus requires to be run manually, follow the
instruction to get it working.

### Requirements
- [Neovim 0.5](https://github.com/neovim/neovim/releases) (currently nightly)
- [Idris2-Lua](https://github.com/Russoul/idris2-lua)

### Install
You can install with any plugin manager with the github url and a post-hook
```
Plug 'ShinKage/nvim-idris2', {'do': 'make build'}
```
