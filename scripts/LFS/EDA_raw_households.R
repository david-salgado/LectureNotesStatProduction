# Load packages ####
library(haven)
library(data.table)
library(dlookr)
library(ggplot2)
library(GGally)
library(jsonlite)
library(here)
library(DDIwR)
library(xml2)

# Set relative paths ####
path_project  <- here()
path_data_raw <- file.path(path_project, 'data', 'raw')
path_metadata <- file.path(path_project, 'metadata')


# Explore metadata (DDI-C) ####
metadata_xml <- read_xml(file.path(path_metadata, "WLD_2023_SYNTH-CEN-EN_v01_M.xml"))
metadata_xml_nons <- xml_ns_strip(metadata_xml)

## Main root nodes ####
nodes_root <- xml_children(metadata_xml_nons)
xml_name(nodes_root)

## Node docDscr ####
nodes_docDscr <- xml_find_first(metadata_xml_nons, ".//docDscr")
xml_structure(nodes_docDscr)
cat("Title IDNo: ", xml_text(xml_find_first(nodes_docDscr, ".//titlStmt/IDNo")))
cat("Producer: ", xml_text(xml_find_first(nodes_docDscr, ".//prodStmt/producer")))
cat("Production Date: ", xml_text(xml_find_first(nodes_docDscr, ".//prodStmt/prodDate")))
cat("Software: ", xml_text(xml_find_first(nodes_docDscr, ".//prodStmt/software")))

## Node stdyDscr ####
nodes_stdyDscr <- xml_find_first(metadata_xml_nons, ".//stdyDscr")
xml_structure(nodes_stdyDscr)
cat("Study Title:", xml_text(xml_find_first(nodes_stdyDscr, ".//citation/titlStmt/titl")))
cat("Study Subtitle:", xml_text(xml_find_first(nodes_stdyDscr, ".//citation/titlStmt/subTitl")))
cat("Author Affiliation:", xml_text(xml_find_first(nodes_stdyDscr, ".//citation/rspStmt/AuthEnty")))
cat("Citation: ", xml_text(xml_find_first(nodes_stdyDscr, ".//citation/biblCit")))
cat("Abstract: ", xml_text(xml_find_first(nodes_stdyDscr, ".//stdyInfo/abstract")))
cat("Keywords: ", paste(xml_text(xml_find_all(nodes_stdyDscr, ".//stdyInfo/subject/keyword")), collapse = ", "))
cat("Collection Dates: ")
xml_attrs(xml_find_all(metadata_xml_nons, ".//stdyDscr/stdyInfo/sumDscr/collDate"))
cat("Geographical Cover: ", xml_text(xml_find_first(metadata_xml_nons, ".//stdyDscr/stdyInfo/sumDscr/geogCover")))
cat("Geographical Units: ", xml_text(xml_find_first(metadata_xml_nons, ".//stdyDscr/stdyInfo/sumDscr/geogUnit")))
cat("Analysis Units: ", xml_text(xml_find_first(metadata_xml_nons, ".//stdyDscr/stdyInfo/sumDscr/anlyUnit")))
cat("Finite Population: ", xml_text(xml_find_first(metadata_xml_nons, ".//stdyDscr/stdyInfo/sumDscr/universe")))
cat("Other Quality Statement: ", xml_text(xml_find_first(metadata_xml_nons, ".//stdyDscr/stdyInfo/qualityStatement/otherQualityStatement")))
cat("General Notes: ", xml_text(xml_find_first(metadata_xml_nons, ".//stdyDscr/stdyInfo/notes")))
cat("Data Collection Method: ", xml_text(xml_find_first(metadata_xml_nons, ".//stdyDscr/method/dataColl/resInstru")))
cat("Weights: ", xml_text(xml_find_first(metadata_xml_nons, ".//stdyDscr/method/dataColl/weight")))
cat("Editing: ", xml_text(xml_find_first(metadata_xml_nons, ".//stdyDscr/method/dataColl/cleanOps")))
cat("Methodological Notes: ", xml_text(xml_find_first(metadata_xml_nons, ".//stdyDscr/method/notes")))


## Nodes fileDscr ####
nodes_fileDscr <- xml_find_all(metadata_xml_nons, ".//fileDscr")
xml_structure(nodes_fileDscr)
cat("\n\n Available Data Files:\n")
for(file in nodes_fileDscr) {
  file_name <- xml_text(xml_find_first(file, "./fileTxt/fileName"))
  file_content <- xml_text(xml_find_first(file, "./fileTxt/fileCont"))
  cases <- xml_text(xml_find_first(file, "./fileTxt/dimensns/caseQnty"))
  vars <- xml_text(xml_find_first(file, "./fileTxt/dimensns/varQnty"))
  
  cat("\n- File Name:", file_name)
  cat("\n  Contents:", file_content)
  cat("\n  Population Size:", cases, "| No. Variables:", vars)
}

## Node dataDscr ####
variables <- xml_find_all(metadata_xml_nons, "//var")
cat("\n\nSummary of Variables:\n")
for(i in seq_along(variables)) {
  var_id <- xml_attr(variables[i], "ID")
  var_name <- xml_attr(variables[i], "name")
  var_label <- xml_text(xml_find_first(variables[i], "./labl"))
  var_type <- xml_attr(variables[i], "intrvl")
  var_quest <- xml_find_first(variables[i], "./qstn/qstnLit")
  var_quest <- ifelse(is.na(var_quest), "-", xml_text(var_quest))
  var_file <- xml_attr(variables[i], "files")
  
  cat("\n\nVariable ", var_id, ": ", var_name)
  cat("\n- Variable Label: ", var_label)
  cat("\n- Question: ", var_quest)
  cat("\n- In files: ", var_file)
  cat("\n- Variable Type: ", var_type)
  
  # Categories if variable is discrete
  if(var_type == "discrete") {
    categories <- xml_find_all(variables[i], "./catgry")
    if(length(categories) > 0) {
      cat("\n- No. Categories:", length(categories))
      cat("\n- Category Values: ", paste(xml_text(xml_find_all(categories, ".//catValu")), collapse = ", "))
      cat("\n- Category Names: ", paste(xml_text(xml_find_all(categories, ".//labl")), collapse = ", "))
    }
  }
  
  # Statistics if available
  stats <- xml_find_all(variables[i], "./sumStat")
  if(length(stats) > 0) {
    cat("\n- No. Available Statistics: ", length(stats))
    cat("\n- Available Statistics: ", paste(sapply(stats, xml_attrs), collapse = ", "))
  }
}



# Read data
data_household_raw.dt <- as.data.table(
  read_dta(file.path(path_data_raw, 'WLD_2023_SYNTH-CEN-HLD-EN_v01_M.dta'))
)

metadata_json <- fromJSON(file.path(path_metadata, "WLD_2023_SYNTH-CEN-EN_v01_M.json"))

stats_eda_univ.dt <- as.data.table(describe(data_household_raw.dt, -hid))
 

