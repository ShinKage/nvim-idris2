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

function module.getSelection()
    local s_start = vim.fn.getpos("'<")
    local s_end = vim.fn.getpos("'>")
    local n_lines = math.abs(s_end[2] - s_start[2]) + 1
    local lines = vim.api.nvim_buf_get_lines(0, s_start[2] - 1, s_end[2], false)
    lines[1] = string.sub(lines[1], s_start[3], -1)
    if n_lines == 1 then
        lines[n_lines] = string.sub(lines[n_lines], 1, s_end[3] - s_start[3] + 1)
    else
        lines[n_lines] = string.sub(lines[n_lines], 1, s_end[3])
    end
    return table.concat(lines, '\n')
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

module.cmd_history = {}
module.cmd_index = 1
module.cmd_last_search = nil

function module.putCmdInHistory(idx, cmd)
    module.cmd_history[idx] = cmd
end

function module.putLastSearch(s)
    module.cmd_last_search = s
end

function module.deleteCmdInHistory(idx)
    module.cmd_history[idx] = nil
end

function module.deleteLastSearch()
    module.cmd_last_search = nil
end

function module.genHistoryIndex()
    local idx = module.cmd_index
    module.cmd_index = module.cmd_index + 1
    return idx
end

function module.getCmdFromHistory(idx)
    local cmd = {}
    if module.cmd_history[idx] ~= nil then
        cmd.tag = '1'
        cmd.arg1 = module.cmd_history[idx]
    else
        cmd.tag = '0'
    end
    return cmd
end

function module.getLastSearch()
    local res = {}
    if module.cmd_last_search ~= nil then
        res.tag = '1'
        res.arg1 = module.cmd_last_search
    else
        res.tag = '0'
    end
    return res
end

function module.updateCmdInHistory(idx, cmd)
    if cmd.tag == '0' then
        module.cmd_history[idx] = nil
    else
        module.cmd_history[idx] = cmd
    end
end

return module
