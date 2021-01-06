options(shiny.jquery.version=1)
# remotes::install_github("MarkEdmondson1234/gentelellaShiny")
library(gentelellaShiny)
library(shiny)

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
  Potential_Currency = as.character(""),
  Actual_Savings = as.numeric(""),
  Actual_Currency = as.character("")
)


data_gdms <- readRDS("D:/Pralhad/git_CostEstimator/gdms_data.RDS")

track_path <- file.path("D:", "Pralhad", "git_CostEstimator", "track_data", fsep = "/")

epochTime <- function() {
  as.integer(Sys.time())
}

loadData <- function(path){
  files <- list.files(file.path(path), full.names = T)
  d <- purrr::map_df(files, readRDS)
  
}

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

saveData <- function(data, path){
  fileName <- sprintf("%s_%s.RDS", 
                      humanTime(),
                      digest::digest(data))
  
  saveRDS(data, file = file.path(path, fileName), ascii = T)
}

fieldsAll <- c("gdms", "project_pn", "project_rev", "project_description1", "project_description2", "project", "project_group",
               "part_commodity", "part_subcommodity", "project_request", "project_owner", "date_request", "date_estimate", "date_actual",
               "leadtime", "delays", "gdms_ear", "calc_ear", "project_plant", "ca_no", "supplier_region", "supplier_exworks",
               "supplier_currency", "supplier_logistics", "supplier_landedcost", "supplier_landedcurrency", "supplier_name",
               "region1", "exworks1", "currency1", "logistics1", "landedcost1", "landedcurrency1", "analysis_type","region2",
               "exworks2", "currency2", "logistics2", "landedcost2", "landedcurrency2", "region3", "exworks3", "currency3", 
               "logistics3", "landedcost3", "landedcurrency3", "project_comment", "project_status", 
               "potential", "potential_currency", "actual_saving", "actual_currency")

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
        
      ),
      
      uiOutput(ns("MainBody_trich")),actionButton(inputId = ns("Updated_trich"),label = "Save")
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
      DT::dataTableOutput(ns("table_tracksheet"))
    )
  )
}

# Function Tracksheet

tracksheet_function <- function(input, output, session){
  
  ns <- session$ns
  
  ## Reactive GDMS data
  
  reactive_data <- reactive({
    
    req(input$project_pn, input$project_rev)
    
    dplyr::filter(data_gdms, PartNumber == input$project_pn & Revision == input$project_rev)
  })
  
  observeEvent(reactive_data(), {
    
    gdms <- reactive_data()
    
    descr1 <- as.character(gdms$DescriptionLine1[1])
    
    updateTextInput(session, "project_description1", value = descr1)
    
    descr2 <- as.character(gdms$DescriptionLine2[1])
    
    updateTextInput(session, "project_description2", value = descr2)
    
    proj <- as.character(gdms$ReleasingProjectNumber[1])
    
    updateTextInput(session, "project", value = proj)
    
    group <- as.character(gdms$PSI_GroupCode[1])
    
    updateTextInput(session, "project_group", value = group)
    
    com <- as.character(gdms$PSI_Commodity[1])
    
    updateTextInput(session, "part_commodity", value = com)
    
    subcom <- as.character(gdms$PSI_SubCommodity[1])
    
    updateTextInput(session, "part_subcommodity", value = subcom)
    
    exwork1 <- as.character(gdms$PSI_ShouldCost1ExWorks[1])
    
    updateTextInput(session, "exworks1", value = exwork1)
    
    curr1 <- as.character(gdms$PSI_ShouldCost1Currency[1])
    
    updateTextInput(session, "currency1", value = curr1)
    
    reg1 <- as.character(gdms$PSI_ShouldCost1Region[1])
    
    updateTextInput(session, "region1", value = reg1)
    
    exwork2 <- as.character(gdms$PSI_ShouldCost2ExWorks[1])
    
    updateTextInput(session, "exworks2", value = exwork2)
    
    curr2 <- as.character(gdms$PSI_ShouldCost2Currency[1])
    
    updateTextInput(session, "currency2", value = curr2)
    
    reg2 <- as.character(gdms$PSI_ShouldCost2Region[1])
    
    updateTextInput(session, "region2", value = reg2)
    
    exwork3 <- as.character(gdms$PSI_ShouldCost3ExWorks[1])
    
    updateTextInput(session, "exworks3", value = exwork3)
    
    curr3 <- as.character(gdms$PSI_ShouldCost3Currency[1])
    
    updateTextInput(session, "currency3", value = curr3)
    
    reg3 <- as.character(gdms$PSI_ShouldCost3Region[1])
    
    updateTextInput(session, "region3", value = reg3)
    
  })
  
  
  observeEvent(input$click, {
    
    tracksheet_neww <- data.frame(GDMS = as.character(input$gdms),
                                  Part.No = as.character(input$project_pn),
                                  Rev = as.character(input$project_rev),
                                  Descr1 = as.character(input$project_description1),
                                  Descr2 = as.character(input$project_description2),
                                  Project = as.character(input$project),
                                  Group = as.character(input$project_group),
                                  Commodity = as.character(input$part_commodity),
                                  Sub.Commodity = as.character(input$part_subcommodity),
                                  Requestor = as.character(input$project_requestor),
                                  Owner = as.character(input$project_owner),
                                  Request.Date = input$date_request,
                                  Estimated.Delivery = input$date_estimate,
                                  Actual.Delivery = input$date_actual,
                                  Lead.Time = as.character(input$leadtime),
                                  Delays = as.character(input$delays),
                                  GDMS.Ear = as.character(input$gdms_ear),
                                  Calc.Ear = as.character(input$calc_ear),
                                  Plant = as.character(input$project_plant),
                                  CA.No = as.character(input$ca_no),
                                  Supplier.Region = as.character(input$supplier_region),
                                  Supplier.Exworks = as.character(input$supplier_exworks),
                                  Supplier.Currency = as.character(input$supplier_currency),
                                  Supplier.Logistics = as.character(input$supplier_logistics),
                                  Supplier.Landed.Cost = as.character(input$supplier_landedcost),
                                  Supplier.Landed.Currency = as.character(input$supplier_landedcurrency),
                                  Supplier.Name = as.character(input$supplier_name),
                                  Calc.Region1 = as.character(input$region1),
                                  Calc.Exworks1 = as.character(input$exworks1),
                                  Calc.Currency1 = as.character(input$currency1),
                                  Calc.Logistics1 = as.character(input$logistics1),
                                  Calc.Landed.Cost1 = as.character(input$landedcost1),
                                  Calc.Landed.Currency1 = as.character(input$landedcurrency1),
                                  Analysis.Type = as.character(input$analysis_type),
                                  Calc.Region2 = as.character(input$region2),
                                  Calc.Exworks2 = as.character(input$exworks2),
                                  Calc.Currency2 = as.character(input$currency2),
                                  Calc.Logistics2 = as.character(input$logistics2),
                                  Calc.Landed.Cost2 = as.character(input$landedcost2),
                                  Calc.Landed.Currency2 = as.character(input$landedcurrency2),
                                  Calc.Region3 = as.character(input$region3),
                                  Calc.Exworks3 = as.character(input$exworks3),
                                  Calc.Currency3 = as.character(input$currency3),
                                  Calc.Logistics3 = as.character(input$logistics3),
                                  Calc.Landed.Cost3 = as.character(input$landedcost3),
                                  Calc.Landed.Currency3 = as.character(input$landedcurrency3),
                                  Project.Comment = as.character(input$project_comment),
                                  Project.Status = as.character(input$project_status),
                                  Potential.Savings = as.character(input$potential),
                                  Potential.Currency = as.character(input$potential_currency),
                                  Actual.Savings = as.character(input$actual_saving)
    )
    
    saveData(tracksheet_neww, track_path)
  })
                                  
  
  # formData <- reactive({
  #   data <- sapply(fieldsAll, function(x) input[[x]])
  #   data <- c(data, timestamp = epochTime())
  #   data <- t(data)
  #   data
  # })
  # 
  # observeEvent(input$click, {
  #   saveData(formData(), track_path )
  # })
  # 
  p <- loadData(track_path)
  
  output$table_tracksheet <- DT::renderDataTable(
    p,
    options = list(scrollX = TRUE),
    class = 'cell-border stripe', 
    editable = T
  )
  
  
  vals_trich<-reactiveValues()
  vals_trich$Data <- p
  
  output$MainBody_trich<-renderUI({
    fluidPage(
      hr(),
      column(12,dataTableOutput(ns("Main_table_trich")))
    ) 
  })
  
  
  output$Main_table_trich<-renderDataTable({
    DT=vals_trich$Data
    datatable(DT,editable = TRUE, selection = "none") 
  }, server = T )
  
  proxy = dataTableProxy('Main_table_trich')
  
  
  observeEvent(input$Main_table_trich_cell_edit, {
    
    info = input$Main_table_trich_cell_edit
    
    str(info) 
    i = info$row 
    j = info$col 
    v = info$value
    
    vals_trich$Data[i, j] <<- DT::coerceValue(v, vals_trich$Data[i, j]) 
    replaceData(proxy, vals_trich$Data, resetPaging = FALSE) # important
    
    
  })
  
  
  ### save to RDS part 
  observeEvent(input$Updated_trich,{
    saveRDS(vals_trich$Data, "note.rds")
    shinyalert(title = "Saved!", type = "success")
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