options(shiny.jquery.version=1)
devtools::install_github("MarkEdmondson1234/gentelellaShiny")
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
  Potential_Currency = as.character("")
)


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
  
  body = gentelellaBody(),
  
  footer = gentelellaFooter(
    leftText = "© Hyster-Yale Materials Handling, Inc All rights reserved 5875 Landerbrook Drive, Suite 300 | Cleveland, Ohio 44124-4069",
    rightText = ""
  )
)

server <- function(input, output, session){}

shinyApp(ui, server)