if bufexists('idris-response')
    finish
endif

if exists('b:did_ftplugin')
    finish
endif

let b:did_ftplugin = 1
lua idris["Plugin.main"](idris.W)
