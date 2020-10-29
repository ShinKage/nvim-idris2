# EXPERIMENTAL: Idris2 IDE Mode for neovim in Idris2 (via Lua)

WARNING: Still in very experimental phase, the code is not packaged to be a
proper neovim plugin and thus requires to be run manually, follow the
instruction to get it working.

### Requirements
- [Neovim 0.5](https://github.com/neovim/neovim/releases) (currently nightly)
- [Idris2-Lua](https://github.com/Russoul/idris2-lua)
- prepackaged dependencies in the `lua` directory may not work for your system,
  in that case, copy the dependencies installed in your system-wide lua
  installation (e.g. installed via luarocks) to the `lua` directory.

### Running instructions
1. `make build`
2. `cd lua` and open `plugin.lua`
3. `:luafile %` in neovim
4. use the keybindings defined in `Plugin.main` or manually write to the server
   socket with `:lua global_client:write(sexp)`
5. (Optional) if you want to connect to an already launched idris2 server
   uncomment the `connectIdris2` line in `Plugin.main` and comment
   `spawnAndConnectIdris2`
