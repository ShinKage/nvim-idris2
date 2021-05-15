.PHONY: build

build:
	eval $(luarocks path --lua-version=5.1) && LuaVersion=5.1 idris2-lua --build nvim.ipkg
	cp build/exec/idris.lua lua
