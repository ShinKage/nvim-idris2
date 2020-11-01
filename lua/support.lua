module = {}

function module.world(f, i)
    local perform
    perform = function(f, i)
        if i == 0 then
            return f("%MkWorld")
        else
            return function(x) return perform(f(x), i - 1) end
        end
    end
    return perform(f, i)
end

function module.fastLines(str)
    local splitted = vim.split(str, '\n')

    local list = {}
    list.tag = "0"

    for _, v in pairs(splitted) do
        if v ~= "" then
            local new_list = {}
            new_list.tag = "1"
            new_list.arg1 = v
            new_list.arg2 = list
            list = new_list
        end
    end

    return list
end

function module.spawn(cmd, opts, callback)
    return vim.loop.spawn(cmd, opts, vim.schedule_wrap(module.world(callback, 2)))
end

function module.readStart(stream, onok, onerr, onclose)
    stream:read_start(vim.schedule_wrap(function (err, chunk)
        if err then
            module.world(onerr, 1)(err)
        elseif chunk then
            module.world(onok, 1)(chunk)
        else
            module.world(onclose, 0)
        end
    end))
end

function module.connect(client, host, port, onok, onerr)
    client:connect(host, port, vim.schedule_wrap(function (err)
        if err then
            module.world(onerr, 1)(err)
        else
            module.world(onok, 0)
        end
    end))
end

return module
