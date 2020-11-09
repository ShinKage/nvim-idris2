# EXPERIMENTAL: Idris2 IDE Mode for neovim in Idris2 (via Lua)

WARNING: Plugin is still in experimental phase and requires nightly releases.

### Requirements
- [Neovim 0.5](https://github.com/neovim/neovim/releases) (currently nightly)
- [Idris2-Lua](https://github.com/Russoul/idris2-lua)

### Install
You can install with any plugin manager with the repository url and a post-hook
```
Plug 'ShinKage/nvim-idris2', {'do': 'make build'}
```

### Configuration
Keybindings can be customized by setting global options:

| Option | Description | Default binding |
| ------ | ----------- | --------------- |
| `idris2_loadCurrent_key` | Reload the current file | `<LocalLeader>r` |
| `idris2_typeOf_key` | Type of the name under cursor | `<LocalLeader>t` |
| `idris2_docOverview` | Documentation overview of the name under cursor | `<LocalLeader>d` |
| `idris2_caseSplit_key` | Try case-split on argument under cursor | `<LocalLeader>c` |
| `idris2_exprSearch_key` | Try expression search on hole under cursor | `<LocalLeader>s` |
| `idris2_exprSearchNext_key` | Try next candidate expression search | `<LocalLeader>sn` |
| `idris2_addClause_key` | Adds a clause for the definition under cursor | `<LocalLeader>a` |
| `idris2_generateDef_key` | Adds a clause and search the definition under cursor | `<LocalLeader>g` |
| `idris2_generateDefNext_key` | Try next candidate definition search | `<LocalLeader>gn` |
| `idris2_makeLemma_key` | Make lemma function for hole under cursor | `<LocalLeader>l` |
| `idris2_makeCase_key` | Make case boilerplate for hole under cursor | `<LocalLeader>mc` |
| `idris2_makeWith_key` | Make with boilerplate for hole under cursor | `<LocalLeader>w` |
| `idris2_interpret_key` | Evaluate the selected expression | `<LocalLeader>e` |

If you want to use an external IDE server instead of the one started by the plugin you can set
`g:idris2_external_server` to true. The host and port for external server can be configured with
`g:idris2_external_host` and `g:idris2_external_port` respectively.
For the automatic server the port can be configured with `g:idris2_server_port`.

Automatic loading of opened Idris2 files can be disabled with setting to false the option `g:idris2_load_on_start`.
