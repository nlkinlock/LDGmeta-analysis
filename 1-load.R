# LOAD
##
#

library(googlesheets)
library(glmulti)
library(polycor)
library(car)
library(metafor)
library(ggplot2)

gs_ls() # load data sheet directly from Google Spreadsheet
LDG_data <- gs_title("Coding Datasheet")  # load the correct sheet within spreadsheet
dat.init <- gs_read_csv(LDG_data, ws = "IncludedStudies", col_names = TRUE)
dat.init <- as.data.frame(dat.init)  #coerce to data frame (ggplot only works with df)
str(dat.init)  # check type of data (numerical, integer, character) for each variable


