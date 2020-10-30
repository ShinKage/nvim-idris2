module = {}

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

return module
