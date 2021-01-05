options(shiny.jquery.version=1)
# remotes::install_github("MarkEdmondson1234/gentelellaShiny")
library(gentelellaShiny)
library(shiny)
library(rhandsontable)

data_tracksheet <- data.frame(
  GDMS = as.factor(""),
  PN_No = as.character(""),
  Rev = as.character(""),
  Descr1 = as.character(""),
  Descr2 = as.character(""),
  Project = as.character(""),
  Group = as.character(""),
  Commodity = as.character(""),
  Sub_Commodity = as.character(""),
  Requestor = as.character(""),
  Owner = as.character(""),
  Request_Date = as.character(""),
  Estimated_Delivery = as.character(""),
  Actual_Delivery = as.character(""),
  Lead_Time = as.numeric(""),
  Delays = as.numeric(""),
  GDMS_Ear = as.numeric(""),
  Calc_Ear = as.numeric(""),
  Plant = as.character(""),
  CA_No = as.character(""),
  Supplier_Region = as.character(""),
  Supplier_Exworks = as.numeric(""),
  Supplier_Currency = as.character(""),
  Supplier_Logistics = as.numeric(""),
  Supplier_LandedCost = as.numeric(""),
  Supplier_LandedCurrency = as.character(""),
  Supplier_Name = as.character(""),
  Calc_Region1 = as.character(""),
  Calc_Exworks1 = as.numeric(""),
  Calc_Currency1 = as.character(""),
  Calc_Logistics1 = as.numeric(""),
  Calc_LandedCost1 = as.numeric(""),
  Calc_LandedCurrency1 = as.character(""),
  Analysis_Type = as.character(""),
  Calc_Region2 = as.character(""),
  Calc_Exworks2 = as.numeric(""),
  Calc_Currency2 = as.character(""),
  Calc_Logistics2 = as.numeric(""),
  Calc_LandedCost2 = as.numeric(""),
  Calc_LandedCurrency2 = as.character(""),
  Calc_Region3 = as.character(""),
  Calc_Exworks3 = as.numeric(""),
  Calc_Currency3 = as.character(""),
  Calc_Logistics3 = as.numeric(""),
  Calc_LandedCost3 = as.numeric(""),
  Calc_LandedCurrency3 = as.character(""),
  Project_Comment = as.character(""),
  Project_status = as.character(""),
  Potential_Savings = as.numeric(""),
  Potential_Currency = as.character("")
)


# Tracksheet Input Layout (Format : parameter_commodity_UI)

input_tracksheet_UI <- function(id) {
  ns = NS(id)
  
  fluidRow(
    box(
      width = 12,
      title = "Tracksheet Fill-up Form",
      fluidRow(
        column(1, selectInput(ns("gdms"), "GDMS?", choices = c("Yes", "No"))),
        column(1, textInput(ns("project_pn"), "Part No")),
        column(1, textInput(ns("project_rev"), "Rev.")),
        column(1, textInput(ns("project_description1"), "Descr.1")),
        column(2, textInput(ns("project_description2"), "Descr.2")),
        column(1, textInput(ns("project"), "Project")),
        column(1, textInput(ns("project_group"), "Group Code")),
        column(1, textInput(ns("part_commodity"), "Commodity")),
        column(1, textInput(ns("part_subcommodity"), "Sub-Com.")),
        column(1, textInput(ns("project_requestor"), "Requestor")),
        column(1, textInput(ns("project_owner"), "Project Owner"))
      ),
      
      fluidRow(
        column(2, dateInput(ns("date_request"), "Request Date")),
        column(2, dateInput(ns("date_estimate"), "Estimated Delivery")),
        column(2, dateInput(ns("date_actual"), "Actual Delivery Date")),
        column(1, textInput(ns("leadtime"), "Lead Time")),
        column(1, textInput(ns("delays"), "Delays")),
        column(1, textInput(ns("gdms_ear"), "GDMS EAR")),
        column(1, textInput(ns("calc_ear"), "Calc Ear")),
        column(1, textInput(ns("project_plant"), "Plant")),
        column(1, textInput(ns("ca_no"), "CA No."))
      ),
      
      fluidRow(
        column(1, textInput(ns("supplier_region"), "Supplier Reg.")),
        column(1, textInput(ns("supplier_exworks"), "Supplier Exw.")),
        column(1, selectInput(ns("supplier_currency"), "Currency", choices = c("$", "₹", "€", "£", "¥", "Ұ"))),
        column(1, textInput(ns("supplier_logistics"), "Logistics")),
        column(2, textInput(ns("supplier_landedcost"), "Supplier Landed Cost")),
        column(1, selectInput(ns("supplier_landedcurrency"), "Currency", choices = c("$", "₹", "€", "£", "¥", "Ұ"))),
        column(2, textInput(ns("supplier_name"), "Supplier Name"))
      ),
      
      fluidRow(
        column(1, textInput(ns("region1"), "Calc Region1")),
        column(1, textInput(ns("exworks1"), "Exwork Cost1")),
        column(1, selectInput(ns("currency1"), "Currency1", choices = c("$", "₹", "€", "£", "¥", "Ұ"))),
        column(1, textInput(ns("logistics1"), "Logistic Cost1")),
        column(2, textInput(ns("landedcost1"), "Landed Cost1")),
        column(1, selectInput(ns("landedcurrency1"), "Currency1", choices = c("$", "₹", "€", "£", "¥", "Ұ"))),
        column(2, textInput(ns("analysis_type"), "Analysis Type"))
      ),
      
      fluidRow(
        column(1, textInput(ns("region2"), "Calc Region2")),
        column(1, textInput(ns("exworks2"), "Exwork Cost2")),
        column(1, selectInput(ns("currency2"), "Currency2", choices = c("$", "₹", "€", "£", "¥", "Ұ"))),
        column(1, textInput(ns("logistics2"), "Logistic Cost2")),
        column(2, textInput(ns("landedcost2"), "Landed Cost2")),
        column(1, selectInput(ns("landedcurrency2"), "Currency2", choices = c("$", "₹", "€", "£", "¥", "Ұ")))
      ),
      
      fluidRow(
        column(1, textInput(ns("region3"), "Calc Region3")),
        column(1, textInput(ns("exworks3"), "Exwork Cost3")),
        column(1, selectInput(ns("currency3"), "Currency3", choices = c("$", "₹", "€", "£", "¥", "Ұ"))),
        column(1, textInput(ns("logistics3"), "Logistic Cost3")),
        column(2, textInput(ns("landedcost3"), "Landed Cost3")),
        column(1, selectInput(ns("landedcurrency3"), "Currency3", choices = c("$", "₹", "€", "£", "¥", "Ұ"))),
        column(2, textAreaInput(ns("project_comment"), "Comments"))
      ),
      
      fluidRow(
        column(1, textInput(ns("project_status"), "Status")),
        column(2, textInput(ns("potential"), "Total Potential Savings")),
        column(2, selectInput(ns("potential_currency"), "Potential Currency", choices = c("$", "₹", "€", "£", "¥", "Ұ"))),
        column(2, textInput(ns("actual_saving"), "Actual Savings")),
        column(2, selectInput(ns("actual_currency"), "Actual Currency", choices = c("$", "₹", "€", "£", "¥", "Ұ")))
      ),
      
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(),
      
      fluidRow(
        
        column(1,
               actionButton(ns("click"), "Add new Part No.", style = "color: #fff; background-color: #007BFF; border-color: #2e6da4")
        ),
        column(1,
               offset = 4,
               actionButton(ns("project_save"), "Update Existing Part No.", style = "color: #fff; background-color: #26B99A; border-color: #26B99A")
        )
      )
    )
  )
}

table_tracksheet_UI <- function(id){
  
  ns = NS(id)
  
  fluidRow(
    box(
      width = 12,
      title = "Tracksheet Table",
      downloadButton(ns("tracksheet_download"), "Download.."),
      # rHandsontableOutput(ns("track"))
    )
  )
}

# Function Tracksheet

tracksheet_function <- function(input, output, session){
  
  ## Reactive GDMS data
  
  reactive_data <- reactive({
    
    req(input$project_pn, input$project_rev)
    
    filter(data_gdms, PartNumber %in% input$project_pn & Revision %in% input$project_rev)
  })
  
  
  output$track1 <- renderRHandsontable({
    rhandsontable(data_tracksheet, height = "100%") %>% hot_cols(colWidths = 150)
  })
  
  
  
}

ui <- gentelellaShiny::gentelellaPageCustom(
  title = "Cost Estimator!",
  
  navbar = gentelellaNavbar(
    navbarItems = notif(
      id = "notify",
      icon = shiny::HTML(paste(shiny::icon("sort-down"), tags$strong("Extra"))),
      status = "danger",
      notifItem(
        a(href = "http://hyghome/GP/Other%20Process%20Areas/CompanySharedResources/HYG%20Wiki/Pages/default.aspx", icon("wordpress"), " Wiki Page",  target = "_blank"),
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
        tabName = "dashboard"
      ),
      
      tabItem(
        tabName = "tracksheet",
        
        fluidRow(
          input_tracksheet_UI("tk"),
          table_tracksheet_UI("tk")
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
  
  callModule(tracksheet_function, "tk")
  
}

shinyApp(ui, server)