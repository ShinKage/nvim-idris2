.PHONY: build

build:
	LuaVersion=5.1 idris2-lua --build nvim.ipkg
	cp build/exec/plugin.lua lua
	echo 'package.path = package.path .. ";./?.lua;./?/init.lua"' > lua/temp_plugin.lua
	cat lua/plugin.lua >> lua/temp_plugin.lua
	mv lua/temp_plugin.lua lua/plugin.lua
