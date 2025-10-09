
source("UI_Fluid_Raw_Moïse.R")
source("fluid_page_jules.R")
library(shiny)
library(leaflet)
library(data.table)
library(dplyr)
library(sf)



ui <- navbarPage(
  "World Health Dashboard",
  
  # --- Landing page ---
  tabPanel("Apperçu des données",
           fluidPage(fluid_page_accueil)),
  
  # --- Comparaison des systèmes de santé ---
  tabPanel("Comparaison",
           fluidPage(wellPanel(FR_Condi),
                     FR_Map,
                     FR_C_Sim_barplot)),
  
  # --- Analyse temporelle ---
  tabPanel("Analyse temporelle",
           fluidPage(
             h2("Analyse temporelle des indicateurs de santé"),
             p("À construire...")
           )))







