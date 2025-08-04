# Load packages ####
library(here)

# Set URL  #### 
url <- "https://microdata.worldbank.org/index.php/catalog/5908/download/62511"

# Set destination folder ####
path_project  <- here()
path_data_orig <- file.path(path_project, 'data', 'original')

# Set filenames ####
data_population_fn <- file.path(path_data_orig, "microdata.zip")

# Download and unzip files ####
tryCatch({
  # mode = "wb" for binary files
  download.file(url = url, 
                destfile = data_population_fn,
                mode = "wb",
                method = "auto")
  
  message("Download completed: ", data_population_fn)
  
  # extract zip file content
  unzip(zipfile = data_population_fn, exdir = path_data_orig)
  message("Files extracted in: ", path_data_orig)
  
  # list extracted files
  files <- list.files(path_data_orig, pattern = "\\.(dta|sav|csv|txt)$", full.names = TRUE)
  message("Available files:")
  print(basename(files))
  file.remove(data_population_fn)
  
}, error = function(e) {
  message("Download error: ", e$message)
  if(file.exists(data_population_fn)) file.remove(data_population_fn)
})
