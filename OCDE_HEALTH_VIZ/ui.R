#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)
library(data.table)
library(dplyr)
library(sf)

# # Define UI for application that draws a histogram
# fluidPage(
#   titlePanel("Carte interactive de similarité des systèmes de santé"),
#   
#   sidebarLayout(
#     sidebarPanel(
#       # --- Pays de référence ---
#       
#       selectInput("ref_country", "Pays de référence :", choices = dt$REF_AREA),
#       
#       # --- Groupe de variables ---
#       
#       selectInput("var_group", "Groupe de variables :", choices = unique(dict$var_group)),
#       
#       # --- Sélection des variables du groupe ---
#       
#       uiOutput("vars_ui"),
#       
#       # --- Mode de comparaison ---
#       selectInput(
#         inputId = "compare_mode",
#         label = "Comparer avec :",
#         choices = c("Moyenne", "Meilleur pays", "Pays spécifique")
#         
#       ),
#       
#       # --- Choix du pays spécifique  ---
#       uiOutput("specific_country_ui")
#     ),
#     
#     
#     mainPanel(
#       # --- Carte interactive ---
#       leafletOutput("map", height = 700),
#       br(),
#       
#       # --- détail de la contrib des variables à la similarité  ---
#       plotOutput("barplot_contrib", height = 400),
#       
#       # --- Graphique interactif comparatif ---
#       #plotlyOutput("comparison_plot", height = 400)
#       
#       
#     )
#   )
# )

ui <- navbarPage(
  "World Health Dashboard",
  
  # --- Landing page ---
  tabPanel("Accueil",
           fluidPage(
             h2("Bienvenue sur le tableau de bord des systèmes de santé"),
             p("Graphiques et visualisations à construire...")
           )),
  
  # --- Comparaison des systèmes de santé ---
  tabPanel("Comparaison",
           fluidPage(
             # 1) Panel de sélection des variables et pays en haut
             fluidRow(
               column(4,
                      selectInput("ref_country_S", "Pays de référence :", choices = dt$REF_AREA),
                      selectInput("var_group", "Groupe de variables :", choices = unique(dict$var_group)),
                      uiOutput("vars_ui"),
                      uiOutput("specific_country_ui")
               ),
               column(8,
                      h4("Carte interactive"),
                      leafletOutput("map", height = 400)
               )
             ),
             
             # 2) Graphique de comparaison à gauche de la carte
             fluidRow(
               column(12,
                      h4("Écarts par variable (pays cliqué vs pays de référence)"),
                      plotOutput("barplot_contrib", height = 400)
               )
             ),
             
             # 3) Séparation horizontale
             ################################################
             ################################################
             tags$hr(),
             ##############
             # 4) Graphique comparatif vs Moyenne / Meilleur / Pays spécifique
             fluidRow(
               column(12,
                      h2("Comparaison des pays par groupes de variables")
               )),
             #############
             tags$hr(),
             ############# 
             fluidRow(
               column(4,
                      selectInput("ref_country_C", "Pays de référence :", choices = dt$REF_AREA),
                      selectInput("vars_G", "Groupe de variables :", choices = names(vars_groupes_scores),
                      selectInput("compare_mode", "Mode de comparaison :", 
                                  choices = c("Moyenne", "Meilleur pays", "Pays spécifique"))
                      
               ),
               column(8,
                      h4("plot"),
                      plotOutput("comparison_plot", height = 400))
             )

           ))),
  
  # --- Analyse temporelle ---
  tabPanel("Analyse temporelle",
           fluidPage(
             h2("Analyse temporelle des indicateurs de santé"),
             p("À construire...")
           ))
)







