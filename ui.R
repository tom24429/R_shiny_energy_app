library(ggplot2)
library(shiny)
library(bslib)
library(DT)
library(data.table)
library(plotly)
library(officer)
source("utils.R")

# ================= UI =================
ui <- page_sidebar(
  title = "PSE Energia OZE - Analiza zaawansowana",
  
  sidebar = sidebar(
    h4("🔧 Podstawowe parametry"),
    dateRangeInput("okres", "Okres danych:", start="2024-06-14", end=Sys.Date()-1, max=Sys.Date()-1),
    checkboxGroupInput("szeregi", "Wybierz dane:",
                       choices=c("Zapotrzebowanie"="demand","PV"="pv","Wiatr"="wi","Magazyny"="jgm"),
                       selected=c("demand","pv","wi","jgm")),
    radioButtons("agregacja", "Widok danych:", choices=c("15 min"="15min","1D"="1d"), selected="15min", inline=TRUE),
    actionButton("pobierz","🌐 Pobierz z PSE API", class="btn-primary btn-lg mb-3"),
    
    h4("🔧 Filtrowanie danych"),
    checkboxGroupInput("filter_month","Wybierz miesiące:", choices=1:12, selected=1:12),
    checkboxGroupInput("filter_wday","Wybierz dni tygodnia:", choices=1:7, selected=1:7),
    checkboxGroupInput("filter_hour","Wybierz godziny:", choices=0:23, selected=0:23)
  ),
  
  tabsetPanel(
    tabPanel("📊 Tabela", DTOutput("tabela")),
    
    tabPanel("📈 Wykres",
             fluidRow(
               column(3,
                      checkboxGroupInput("wykresy_series","Pokaż serie:", choices=NULL, selected=character(0)),
                      radioButtons("wykres_layout","Układ wykresów:", choices=c("Ten sam układ"="single","Osobne układy"="separate"), selected="single")
               ),
               column(9, plotlyOutput("timeseries", height="600px"))
             )),
    
    tabPanel("📦 Boxplot",
             fluidRow(
               column(3,
                      checkboxGroupInput("boxplot_series","Pokaż serie:", choices=NULL, selected=character(0)),
                      selectInput("boxplot_type","Typ grupowania:",
                                  choices=c("Miesiąc"="month","Dzień tygodnia"="wday","Godzina"="hour"),
                                  selected="month")
               ),
               column(9, plotlyOutput("boxplot", height="600px"))
             )),
    
    tabPanel("📊 Statystyki", uiOutput("statystyki_ui")),
    
    tabPanel("💾 Eksport",
             downloadButton("download_csv","📄 CSV"),
             downloadButton("download_report","📑 Generuj raport (PPTX)"))
  )
)