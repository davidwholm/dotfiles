local config = {}
local telescope = require("telescope")

config.org_roam_directory = os.getenv("HOME") .. "/org-roam"

local function node_insert()
end

local function node_find()
  telescope.builtin.find_files({
    cwd = config.org_roam_directory
  })
end
