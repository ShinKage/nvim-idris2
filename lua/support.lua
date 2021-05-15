module = {}

function module.world(f, i)
    local perform
    perform = function(f, i)
        if i == 0 then
            return f(idris.W)
        else
            return function(x) return perform(f(x), i - 1) end
        end
    end
    return perform(f, i)
end

-- Converts an Idris List into a Lua array
-- (Basically it flatterns the provided table)
function module.listToArray(list)
    local function rec(list, table, i)
        if list.tag == "0" then
            return table
        else
            table[i] = list.arg1
            return rec(list.arg2, table, i + 1)
        end
    end

    return rec(list, {}, 1)
end

-- The opposite process
function module.arrayToList(array)
    local function rec(list, array, i, size)
       if i == 0 then
         return list
       else
         return rec({tag = "1", arg1 = array[i], arg2 = list}, array, i - 1, size)
       end
    end
    local size = #array
    return rec({tag = "0"}, array, size, size)
end

function module.getSelection(_)
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

function module.spawn(cmd)
    return function(opts)
        return function(callback)
            return function(_)
                return vim.loop.spawn(cmd, opts, vim.schedule_wrap(module.world(callback, 2)))
            end
        end
    end
end

function module.readStart(stream)
    return function(onok)
       return function(onerr)
           return function(onclose)
               return function(_)
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
           end
       end
    end
end

function module.connect(client)
    return function(host)
        return function(port)
            return function(onok)
                return function(onerr)
                    return function(_)
                        client:connect(host, port, vim.schedule_wrap(function (err)
                            if err then
                                module.world(onerr, 1)(err)
                            else
                                module.world(onok, 0)
                            end
                        end))
                    end
                end
            end
        end
    end
end

module.cmd_history = {}
module.cmd_index = 1
module.cmd_last_search = nil

function module.putCmdInHistory(idx)
    return function(cmd)
        return function(_)
            module.cmd_history[idx] = cmd
        end
    end
end

function module.putLastSearch(s)
    return function(_)
        module.cmd_last_search = s
    end
end

function module.deleteCmdInHistory(idx)
    return function(_)
        module.cmd_history[idx] = nil
    end
end

function module.deleteLastSearch(_)
    module.cmd_last_search = nil
end

function module.genHistoryIndex(_)
    local idx = module.cmd_index
    module.cmd_index = module.cmd_index + 1
    return idx
end

function module.getCmdFromHistory(idx)
    return function(_)
        local cmd = {}
        if module.cmd_history[idx] ~= nil then
            cmd.tag = '1'
            cmd.arg1 = module.cmd_history[idx]
        else
            cmd.tag = '0'
        end
        return cmd
    end
end

function module.getLastSearch(_)
    local res = {}
    if module.cmd_last_search ~= nil then
        res.tag = '1'
        res.arg1 = module.cmd_last_search
    else
        res.tag = '0'
    end
    return res
end

function module.updateCmdInHistory(idx)
    return function(cmd)
        return function(_)
            if cmd.tag == '0' then
                module.cmd_history[idx] = nil
            else
                module.cmd_history[idx] = cmd
            end
        end
    end
end

function module.getGlobalStringVar(key)
    -- TODO: add typing check
    return function(_)
        local var = vim.g[key]
        local res = {}
        if var == nil then
            res.tag = '0'
        else
            res.tag = '1'
            res.arg1 = var
        end
        return res
    end
end

local function toIdrisBool(bool)
    if bool then
        return 1
    else
        return 0
    end
end

function module.isBufferModified(_)
    return toIdrisBool(vim.bo.modified)
end

function module.getGlobalBoolVar(key)
    -- TODO: add typing check
    return function(_)
        local var = vim.g[key]
        local res = {}
        if var == nil then
            res.tag = '0'
        else
            res.tag = '1'
            res.arg1 = toIdrisBool(var)
        end
        return res
    end
end

function module.getGlobalIntVar(key)
    -- TODO: add typing check
    return function(_)
        local var = vim.g[key]
        local res = {}
        if var == nil then
            res.tag = '0'
        else
            res.tag = '1'
            res.arg1 = var
        end
        return res
    end
end

function module.appendToBuffer(commented)
    return function(last)
        return function(line)
            return function(str)
                return function(_)
                    local ls = vim.split(str, '\n')
                    if commented == 1 then
                        ls = vim.tbl_map(function(s) return '-- ' .. s end, ls)
                    end
                    if last == 1 then
                        table.insert(ls, '')
                    end
                    vim.fn.append(line, ls)
                end
            end
        end
    end
end

-- Taken from https://github.com/junegunn/fzf/issues/1778#issuecomment-697208274
function module.fzfWindowBottom(name) -- Don't know what the `name` is useful for
    return function(source) -- A List (!) of string options to be shown to the user
        return function(callback) -- A unary callback function
                                  --   taking the selected string as an argument
            return function(_ --[[ %World --]])
                local fzf_run = vim.fn["fzf#run"]
                local fzf_wrap = vim.fn["fzf#wrap"]

                local wrapped = fzf_wrap(name, {
                    source = module.listToArray(source),
                    options = {},
                    down = "~30%" -- We probably should have a more general function
                                  -- with user-defined options as a table argument
                    -- don't set `sink` or `sink*` here
                })
                wrapped["sink*"] = nil   -- this line is required if you want to use `sink` only
                wrapped.sink = module.world(callback, 1)
                fzf_run(wrapped)
            end
        end
    end
end

function module.matchlist(str)
    return function(pattern)
        return module.arrayToList(vim.fn.matchlist(str, pattern))
    end
end

return module
