## Configuration


## Set Working directory
if (wd == "auto") {
  homedir <- getwd()
} else if (wd == "default") {
  homedir  <- normalizePath("~") %>% str_replace_all("\\\\", "/")
} else {
  stop("Wrong input wd, requires 'default' or 'auto'")
}

username <- unlist(strsplit(homedir,"/"))[3]

## Create subfolders
data_path <- file.path("data", prj_name)

dir.create("data"   , showWarnings = F)
dir.create(data_path, showWarnings = F)
