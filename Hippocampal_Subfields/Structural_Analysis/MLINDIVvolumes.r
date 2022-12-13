# Author: Alina Tu
# Contact: alinat2@uci.edu
# Last Updated: 11/5/2022
# About: This script is used to organize the volume stats from ITK-SNAP to a large CSV file to be used in the  
#        structural analysis of the Maze Learning Individual Differences (MLINDIV) project.

# Before running analyses in the following sections, run everything in the RUN FIRST section.

# Note if using RStudio: I recommend collapsing/expanding sections using the small arrows on the left pane next to 
#                        the line numbers.

#### ------------------------------------------------ RUN FIRST ------------------------------------------------ ####
# Getting started [PACKAGES AND FUNCTIONS]
# install.packages("dplyr")
# install.packages("tidyr")

library(dplyr)
library(tidyr)

# Working directory
workdir <- "/mnt/chrastil/lab/users/alina/analysis/subfield_vol"
setwd(workdir)

#### ---------------------------------------------- RAW VOL TO CSV --------------------------------------------- ####
# Create an empty data frame to contain all subfield volumes
Lsub_vol_full <- tibble()
Rsub_vol_full <- tibble()

subfields <- c("CA1", "CA23", "DG", "ERC", "PHC", "PRC", "SUB")
Lsubfields <- gsub(" ", "", paste("L", subfields))
Rsubfields <- gsub(" ", "", paste("R", subfields))

# Create a list of files containing participant raw data
rawvol_filelist <- list.files(pattern = "*t.txt") # raw segmentation volumes

# Loop through each participant's raw data files
for (file in 1:length(rawvol_filelist)){
  current_file <- rawvol_filelist[file]
  
  sub_vol <- read.delim(file = current_file) %>% dplyr::select("Label.Name", "Volume..mm.3.")
  sub_num <- regmatches(current_file, regexpr("[0-9].*[0-9]", current_file))
  sub_vol$sub_id <- rep(sub_num, 8)
  sub_vol_wide <- sub_vol %>% pivot_wider(names_from = Label.Name, values_from = Volume..mm.3.)
  
  # Depending on hemisphere, create separate cbind'ed tables
  if (grepl("left", current_file, fixed=TRUE) == TRUE){
    colnames(sub_vol_wide) <- c("sub_id", "gray", Lsubfields)
    sub_vol_wide <- sub_vol_wide %>% dplyr::select("sub_id", Lsubfields)
    Lsub_vol_full <- rbind(Lsub_vol_full, sub_vol_wide)
  } else {
    colnames(sub_vol_wide) <- c("sub_id", "gray", Rsubfields)
    sub_vol_wide <- sub_vol_wide %>% dplyr::select("sub_id", Rsubfields)
    Rsub_vol_full <- rbind(Rsub_vol_full, sub_vol_wide)
  }
}

# Combine the left hemisphere subfield volumes with the right in a large table for raw data
col_order <- c("sub_id", "LCA1", "LCA23", "LDG", "LSUB", "LERC", "LPRC", "LPHC", 
               "RCA1", "RCA23", "RDG", "RSUB", "RERC", "RPRC", "RPHC")
raw_vol_full <- cbind(Lsub_vol_full, Rsub_vol_full[,-c(1)]) %>% 
  dplyr::select(col_order)

write.csv(raw_vol_full, "raw_vol_full.csv")

#### -------------------------------------------- HC BODY VOL TO CSV ------------------------------------------- ####
# Create an empty data frame to contain all subfield volumes
Lsub_vol_full <- tibble()
Rsub_vol_full <- tibble()

subfields <- c("CA1", "CA23", "DG", "PHC", "PRC", "SUB")
Lsubfields <- gsub(" ", "", paste("L", subfields))
Rsubfields <- gsub(" ", "", paste("R", subfields))

# Create a list of files containing participant processed-HC-body data
bodyvol_filelist <- list.files(pattern = "*body.txt") # body segmentation volumes

# Loop through each participant's processed-HC-body data files
for (file in 1:length(bodyvol_filelist)){
  current_file <- bodyvol_filelist[file]
  
  sub_vol <- read.delim(file = current_file) %>% dplyr::select("Label.Name", "Volume..mm.3.")
  sub_num <- regmatches(current_file, regexpr("[0-9].*[0-9]", current_file))
  sub_vol$sub_id <- rep(sub_num, 7)
  sub_vol_wide <- sub_vol %>% pivot_wider(names_from = Label.Name, values_from = Volume..mm.3.)
  
  # Depending on hemisphere, create separate cbind'ed tables
  if (grepl("left", current_file, fixed=TRUE) == TRUE){
    colnames(sub_vol_wide) <- c("sub_id", "gray", Lsubfields)
    sub_vol_wide <- sub_vol_wide %>% dplyr::select("sub_id", Lsubfields)
    Lsub_vol_full <- rbind(Lsub_vol_full, sub_vol_wide)
  } else {
    colnames(sub_vol_wide) <- c("sub_id", "gray", Rsubfields)
    sub_vol_wide <- sub_vol_wide %>% dplyr::select("sub_id", Rsubfields)
    Rsub_vol_full <- rbind(Rsub_vol_full, sub_vol_wide)
  }
}

# Combine the left hemisphere subfield volumes with the right in a large table for raw data
col_order <- c("sub_id", "LCA1", "LCA23", "LDG", "LSUB", "LPRC", "LPHC", 
               "RCA1", "RCA23", "RDG", "RSUB", "RPRC", "RPHC")
body_vol_full <- cbind(Lsub_vol_full, Rsub_vol_full[,-c(1)]) %>% 
  dplyr::select(col_order)

write.csv(body_vol_full, "body_vol_full.csv")
