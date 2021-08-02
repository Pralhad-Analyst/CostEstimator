options(shiny.jquery.version=1)
library(gentelellaShiny)
library(shiny)
library(plotly)
library(shinyjs)
library(shinyWidgets)
library(reactable)
library(dplyr)

format_big <- function(n) {
  case_when(
    n >= 1e12 ~ paste0(round(n/1e12, 2), "T"),
    n >= 1e9 ~ paste0(round(n/1e9, 2), "B"),
    n >= 1e6 ~ paste0(round(n/1e6, 2), "M"),
    n >= 1e3 ~ paste0(round(n/1e3, 2), "K"),
    TRUE ~ as.character(n)
  )  
}

mat_influence <- readRDS("/srv/shiny-server/ce/database/mat_influence.rds")


box_dashboard_UI <- function(id) {
  
  ns = NS(id)
  
  fluidRow(
    uiOutput(ns("t_period")),
    uiOutput(ns("t_pot")),
    uiOutput(ns("t_act")),
    uiOutput(ns("t_act_proj")),
    uiOutput(ns("t_parts"))
  )
  
}


keymaterial_dashboard_UI <- function(id) {
  ns = NS(id)
  
  fluidRow(box(
    width = 12,
    title = h3("Key Material Price Trend"),
    
    column(10, plotlyOutput(ns("t_keymat"))),
    column(
      2,
      
      uiOutput(ns("t_standard")),
      uiOutput(ns("t_country"))
      # uiOutput(ns("t_rates")),
      # uiOutput(ns("t_ratedates")),
      # actionButton(ns("t_pricesubmit"),
      #              "Submit")
    )
  ))
}

keymaterial_influence_UI <- function(id){
  ns = NS(id)
  
  fluidRow(box(
    width = 8,
    title = h3("Key Material Influence"),
    
    column(10, 
           
           h5("It shows the weightage of different material cost on commodity"),
           plotlyOutput(ns("k_keyinf"))),
    column(2,
           uiOutput(ns("k_com")))
  ))
}


cost_report_UI <- function(id) {
  
  ns = NS(id)
  
  fluidRow(
    
    box(collapsible = F,
        width = 12,
        height = 900,
        title = h3("Cost Engineering Report"),
        column(12,
               fluidRow(
                 column(2,
                        fileInput(ns("t_report"),
                                  "",
                                  multiple = F,
                                  accept = c("image/png", "image/jpeg"),
                                  placeholder = ".jpeg or .png format only",
                                  buttonLabel = "Upload ..")),
                 column(2,
                        uiOutput(ns("t_img")))
               )),
        column(12, 
               imageOutput(ns("report"))))
  )
}

dashboard_function <- function(input, output, session) {
  
  ns <- session$ns
  
  reactive_material <- reactiveFileReader(2000, session, "/srv/shiny-server/ce/database/material_price.RDS", readRDS)
  
  reactive_cost_tracker <- reactiveFileReader(2000, session, "/srv/shiny-server/ce/database/cost_tracker1.RDS", readRDS)
  
  
  image_name <- list.files(path = "/srv/shiny-server/ce/database/www/")
  
  output$t_period <- renderUI({
    column(12,
           fluidRow(column(
             2,
             selectInput(ns("t_year"),
                         "Year",
                         choices = reactive_cost_tracker()$Year)
           )))
  })
  
  output$t_standard <- renderUI({
    selectInput(ns("t_stand"), "Standard",
                choices = reactive_material()$Standard)
  })
  
  output$t_country <- renderUI({
    selectInput(ns("t_region"),
                "Region",
                choices = reactive_material()$Region)
  })
  
  # output$t_rates <- renderUI({
  #   numericInput(ns("t_partprice"),
  #                "Add Latest Rate",
  #                value = NULL)
  # })
  # 
  # output$t_ratedates <- renderUI({
  #   dateInput(ns("t_partdate"),
  #             "Entry Date",
  #             format = "dd-mm-yyyy")
  # })
  
  output$t_img <- renderUI({
    selectInput(ns("report_img"),
                "Report",
                choices = image_name)
  })
  
  output$k_com <- renderUI({
    selectInput(ns("k_commodity"),
                "Commodity",
                choices = mat_influence$Commodity)
  })
  
  observeEvent(input$t_year, {
    
    req(input$t_year)
    
    
    
    cost_trackerr <- reactive_cost_tracker()
    cost_trackerr$Potential_Project_Level <- as.numeric(cost_trackerr$Potential_Project_Level)
    cost_trackerr$Actual_Project_Level <- as.numeric(cost_trackerr$Actual_Project_Level)
    cost_trackerr$Project_Code <- as.numeric(cost_trackerr$Project_Code)
    cost_trackerr$No_of_Parts <- as.numeric(cost_trackerr$No_of_Parts)
    
    # reactive_cost_tracker()$Potential_Part_Level <- as.numeric(reactive_cost_tracker()$Potential_Part_Level)
    # reactive_cost_tracker()$Actual_Part_Level <- as.numeric(reactive_cost_tracker()$Actual_Part_Level)
    # reactive_cost_tracker()$Actual_Project_Level <- as.numeric(reactive_cost_tracker()$Actual_Project_Level)
    # reactive_cost_tracker()$No_of_Parts <- as.numeric(reactive_cost_tracker()$No_of_Parts)
    
    reactive_data <- reactive({ filter(cost_trackerr, Year == input$t_year)})
    reactive_pot <- reactive({ 
      a <- sum(reactive_data()$Potential_Project_Level, na.rm = T) 
      format_big(a)})
    reactive_act <- reactive({ 
      b <- sum(reactive_data()$Actual_Project_Level, na.rm = T)
      format_big(b)})
    reactive_proj <- reactive({ length(unique(reactive_data()$Project_Code))})
    reactive_parts <- reactive({ sum(reactive_data()$No_of_Parts, na.rm = T)})
    
    output$t_pot <- renderUI({
      
      valueBox(paste0("$ ", reactive_pot()),
               title = "Potential Savings",
               description = "approx. potential savings for the year",
               icon = icon("calculator"))
    })
    
    output$t_act <- renderUI({
      
      valueBox(paste0("$ ", reactive_act()),
               title = "Actual Savings",
               description = "approx. actual savings for the year",
               icon = icon("thumbs-o-up"))
    })
    
    output$t_act_proj <- renderUI({
      
      valueBox(reactive_act_proj(),
               title = "Projects",
               description = "projects worked by CE Team",
               icon = icon("tasks"))
    })
    
    output$t_parts <- renderUI({
      
      valueBox(reactive_parts(),
               title = "Parts Calculated",
               description = "total parts calculated by CE team",
               icon = icon("check"))
      
    })
  })
  
  
  
  output$t_keymat <- renderPlotly({
    
    req(input$t_stand, input$t_region)
    
    data_materiall <- reactive_material()
    data_materiall$Standard <- as.factor(data_materiall$Standard)
    data_materiall$Date <- as.Date(data_materiall$Date, format = "%d-%m-%Y")
    data_materiall$Rates <- as.numeric(data_materiall$Rates)
    data_materiall$Region <- as.factor(data_materiall$Region)
    
    reactive_com <- reactive({ data_materiall %>% arrange(Date) %>% filter(Standard == input$t_stand & Region == input$t_region) })
    reactive_min <- reactive({ min(reactive_com()$Rates, na.rm = T) - 0.1 })
    reactive_max <- reactive({ max(reactive_com()$Rates, na.rm = T) + 0.1 })
    
    plot_ly(
      reactive_com(),
      x = ~reactive_com()$Date,
      y = ~reactive_com()$Rate,
      type = "scatter",
      showlegend = F,
      mode = "lines+markers",
      line = list(color = 'RGB(150,202,89, 0.5)', width = 2),
      fill = 'tozeroy',
      fillcolor = 'rgba(242,249,235, 0.7)') %>%
      add_trace(
        x = ~reactive_com()$Date,
        y = ~reactive_com()$Rate,
        marker = list(
          color = "#EDEDED",
          size = 7,
          line = list(color = 'rgb(150, 202, 89)',
                      width = 2)
        )
      ) %>%
      
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "($/Kg) Material Rate",
                     range = c(reactive_min(), reactive_max()))
      )
    
  })
  
  
  output$k_keyinf <- renderPlotly({
    
    req(input$k_commodity)
    
    mat_influence$Raw.Materials <- as.factor(mat_influence$Raw.Materials)
    mat_influence$Percentage <- as.numeric(mat_influence$Percentage)
    
    reactive_data <- reactive({
      mat_influence %>% filter(Commodity == input$k_commodity)
    })
    
    plot_ly(reactive_data(), labels = ~Raw.Materials, values = ~Percentage, type = 'pie') 
    
  })
  
  observeEvent(input$t_report, {
    inFile <- input$t_report
    if (is.null(inFile))
      return()
    file.copy(inFile$datapath, file.path("/srv/shiny-server/ce/database/www/", inFile$name))
    
  })
  
  output$report <- renderImage({
    
    
    list(src = normalizePath(file.path("/srv/shiny-server/ce/database/www", paste0(input$report_img,  sep = ""))),
         
         height = 700,
         alt = "Report")
    
  }, deleteFile = F)
  
  
  # observeEvent(input$t_pricesubmit, {
  #   
  #   rates <- data.frame(Standard = as.factor(input$t_stand),
  #                       Date = input$t_partdate,
  #                       Region = as.factor(input$t_region),
  #                       Rates = as.numeric(input$t_partprice))
  #   
  #   new_data <- rbind(reactive_material(), rates)
  #   
  #   saveRDS(new_data, "/srv/shiny-server/ce/database/material_price.RDS")
  #   
  #   showNotification("New Rate Added ... ", type = "warning")
  #   
  # })
  
  output$tracksheets <- renderReactable(
    reactable(reactive_cost_tracker(), searchable = TRUE)
  )
  
  output$tracksheet_download <- downloadHandler(
    
    filename = function() {paste("tracksheet_dataset",".csv", sep = "")},
    
    content = function(file){write.csv(reactive_cost_tracker(), file, row.names = F)}
  )
  
}

table_tracksheet_UI <- function(id) {
  
  ns = NS(id)
  
  fluidRow(
    box(
      width = 12,
      title = "Tracksheet Table",
      downloadButton(ns("tracksheet_download"), "Download.."),
      reactableOutput(ns("tracksheets"))
    )
  )
}

# tracksheet_function <- function(input, output, session) {
#   
#   output$tracksheets <- renderReactable(
#     reactable(reactiv, searchable = TRUE)
#   )
#   
#   output$tracksheet_download <- downloadHandler(
#     
#     filename = function() {paste("tracksheet_dataset",".csv", sep = "")},
#     
#     content = function(file){write.csv(cost_tracksheet, file, row.names = F)}
#   )
#   
# }


ui <- gentelellaShiny::gentelellaPageCustom(
  title = "Cost Estimator!",
  
  navbar = gentelellaNavbar(
    navbarItems = notif(
      id = "notify",
      icon = shiny::HTML(paste(shiny::icon("sort-down"), tags$strong("Extra"))),
      status = "danger",
      notifItem(
        a(href = "https://hysteryale.sharepoint.com/sites/CP-SC-COSTENG/SitePages/HYG-WIKIpedia.aspx", icon("wordpress"), " Wiki Page",  target = "_blank"),
        a(href = "http://www.google.com", icon("exclamation"), " Google", target = "_blank")
      )
    )
  ),
  
  sidebar = gentelellaSidebar(
    site_title = shiny::HTML(paste(shiny::icon("gear"), "Cost Estimator !")),
    sidebarDate(),
    
    sidebarMenu(
      title = "Welcome !",
      sidebarItem("Dashboard", tabName = "dashboard", icon = tags$i(class = "fas fa-chart-pie")),
      sidebarItem("Tracksheet", tabName = "tracksheet", icon = tags$i(class = "fas fa-table")),
      sidebarItem("Calculators", tabName = "calc", icon = tags$i(class = "fas fa-calculator")),
      sidebarItem("Regression Models", tabName = "regression", icon = tags$i(class = "fas fa-chart-line"))
    ),
    
    footer = shiny::HTML(paste(shiny::icon("registered"), "Cost Analyst Group"))
  ),
  
  body = gentelellaBody(
    
    shinyjs::useShinyjs(),
    tabItems(
      
      tabItem(
        tabName = "dashboard",
        fluidRow(
          box_dashboard_UI("ds"),
          keymaterial_dashboard_UI("ds"),
          keymaterial_influence_UI("ds"),
          cost_report_UI("ds")
        )
      ),
      
      tabItem(
        tabName = "tracksheet",
        
        fluidRow(
          table_tracksheet_UI("ds")
        )
      ),
      
      tabItem(
        tabName = "calc",
        
        tabsetPanel(
          shiny::tabPanel("Wire Harness",
                          
                          # fluidRow(
                          #   tags$br(),
                          #   input_wireharness_UI("wh"),
                          #   connectorterminal_wireharness_UI("wh"),
                          #   costbreakup_wireharness_UI("wh"),
                          #   forecast_wireharness_UI("wh")
                          # )
          ),
          shiny::tabPanel("Frame Weldment"),
          
          navbarMenu(
            "Process Calculators",
            
            shiny::tabPanel("Brake Press Process")
          )
        )
      )
    )
  ),
  
  footer = gentelellaFooter(
    leftText = "© Hyster-Yale Materials Handling, Inc All rights reserved 5875 Landerbrook Drive, Suite 300 | Cleveland, Ohio 44124-4069",
    rightText = ""
  )
)

server <- function(input, output, session){
  
  callModule(dashboard_function, "ds")
  
  # callModule(tracksheet_function, "tk")
  
  # callModule(wireharness_function, "wh")
  # 
}

shinyApp(ui, server)

