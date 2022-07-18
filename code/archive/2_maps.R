#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Data Summary
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 11/28/2020
#Purpose: Create leaflet map of available data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Helpful link: http://ryanpeek.org/2017-11-21-mapping-with-sf-part-3/

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup workspace -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear workspace
remove(list = ls())

#Load relevant packages
library(tidyverse)
library(readxl)
library(sf)
library(raster)
library(riverdist)
library(parallel)
library(leaflet)
library(xts)
library(htmlwidgets)

#create temp workspace
file.create("data/temp")