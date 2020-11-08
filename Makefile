.PHONY: build

build:
	LuaVersion=5.1 idris2-lua --build nvim.ipkg
	cp build/exec/plugin.lua lua
