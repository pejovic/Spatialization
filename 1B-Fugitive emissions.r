#' ---
#' title: "Pollutant inventory spatialization"
#' author: GILAB team
#' date: "`r format(Sys.time(), '%d %B %Y')`"
#' output:   
#'    html_document:
#'      keep_md: true
#'      theme: "simplex"
#'      highlight: tango
#'      toc: true
#'      toc_depth: 5
#'      toc_float: true
#'      fig_caption: yes
#' ---
#' 
#' 
#' # 1A3 - Transport
#' This document provides the methodlogy and the main results regarding the spatialization of pollutation inventory.
#' 
#' 
#+ include = TRUE, echo = FALSE, results = 'hide', warning = FALSE, message = FALSE
library(XLConnect)
library(tidyverse)
library(sf)
library(readxl)
library(ggpubr)
library(ggfortify)
library(here)
library(knitr)
library(kableExtra)
library(DT)
#' 
#' 
#' ## Methodology: 
#' 
#' ### Auxiliary Data to be used:
#' - The road network, including 1A, 2A, 1B and 2B road category [^1], 
#' - Data of the Mean Daily Trafic for one Year (MDTY) obtained from ~400 automatic Vehicle Counting Devices (VCD), spreaded over the whole territory of Serbia, on three different road category (1A, 2A and 1B) [^1],
#' - Number of vehicles registered in each municipality of the Serbia for the year 2015 [^2].
#' - The map of Serbia with municipalities [^3]
#' - Road network within the urban areas [^4]
#' - Corine Land Cover for the territory of Serbia [^5]
#' 
#' [^1]: Source: Public Enterprise “Roads of Serbia”
#' [^2]: Source: Statistical Office of the Republic of Serbia.
#' [^3]: Source: GADM, the Database of Global Administrative Areas.
#' [^4]: Source: Open Street Maps
#' [^5]: Source: Corine Land Cover.
#' 
#' ### Methodology for road transport (1A3b)[^6] includes the following steps:
#' 
#' [^6]: Road transport that includes the urban/rural/highway inventory separately
#' 
#' 
#' #### Rural inventory
#' 1. Spatial classification of the whole Territory of Serbia into urban and rural areas based on Corine Land Cover data.
#' 2. Spatial interpolation of the MDTY values (for each vehicle category) from VCDs at the mid-points of the roads where VCD not exist. This will be done for each category of road, separately, by using the knn method (k-nearest neighbors). In doing so, the distances to the whole road section where AVC exist will be taken into account. Model will be trained based through the cross-validation procedure, by using the RMSE (Root Mean Squared Error) as a measure of performace. In this way, the vehicle activity for each road will be estimated.
#' 3. Based on these estimates, total rural inventory for each vehicle category will be spatially dissagregated into the 5x5km cells propotionally, by taking the estimated MDTY and the lengths of corresponding road sections that lie within the cell into account.
#' 
#'
#' #### Urban inventory 
#' 1. First, total inventory pollution will be spatially dissagregated based on the number of the vehicles registered in each municipality.
#' 2. Next, estimated urban emissions in each municipality will be further spatialized into 5x5km cells based on the the lenght of the roads that lie in each cell of urban areas.
#' 
#' #### Highway inventory  
#' 
#' - Total inventory for highway transport will be spatially dissagregated based on the MDTY values and the lenght of the corresponding highway section that lie in each cell. 
#'    
#' #### Total inventory
#' 
#' - Total air pollution from the road transport in each cell will be estimated as the sum of rural, urban and highway inventory for each cell proportionally.     
#'    
#'    
#+ include = FALSE 
# rmarkdown::render(here::here("/1A3-Transport.r"), output_dir = here::here("/Reports"))
 
