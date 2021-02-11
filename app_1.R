options(shiny.jquery.version=1)
library(gentelellaShiny)
library(shiny)
library(plotly)
library(shinyjs)


box_dashboard_UI <- function(id){
  
  ns = NS(id)
  
  fluidRow(
    valueBox(paste0("$ ", 1890),
             title = "Potential Savings",
             description = "approximate savings till date",
             icon = icon("calculator")),
    
    valueBox(paste0("$ ", 2879),
             title = "Actual Savings",
             description = "actual savings till date",
             icon = icon("thumbs-o-up")),
    
    valueBox(115,
             title = "Parts Calculated",
             description = "total parts calculated by CE team",
             icon = icon("check")),
    
    valueBox(18,
             title = "Projects",
             description = "No. of projects by CE Team",
             icon = icon("tasks"))
  )
}

keymaterial_dashboard_UI <- function(id) {
  ns = NS(id)
  
  fluidRow(
    box(width = 12,
      title = h3("Key Material Price Trend"),
      subtitle = "",
      column(10, plotlyOutput(ns("keymat"))),
      column(2, selectInput(ns("material"), "Select Material", choices = c("Aluminum", "Acrylonitrile Butadiene Styrene (ABS)", "Aluminum Alloy A-380", "Aluminum Scrap", "Bunker Fuel", "Carbon Black", "Coke ", "Copper", "Crude Oil - US Barrel", "Lead", "Natural Gas - US", "Nickel", "Oil - UK, Brent Blend 38º", "Palladium", "Platinum", "Polyamide 6 ", "Polyethylene", "Polypropylene", "Rhodium", "Rubber - Malaysian", "Rubber - TSR20", "SBQ - 1'' Round Alloy 4100 Bar", "SBQ - 1'' Round Carbon 1000 Bar", "Silver", "Steel - Hot Roll Sheet", "Steel Plate (carbon CTL)", "Steel Scrap No. 1 Busheling Chi.", "Steel Scrap No. 1 Busheling Det.", "Steel Scrap No. 1 Heavy Melt", "Steel Scrap No. 1 HM Composite", "Trucking Costs", "Vanadium", "Zinc")))
    )
  )
}

cost_report_UI <- function(id) {
  ns = NS(id)
  
  fluidRow(
    box(collapsible = F,
      width = 12,
      height = 900,
      title = h3("Report"),
      column(12, imageOutput(ns("report")))
      
    )
  )
}

cost_team_UI <- function(id) {
  ns = NS(id)
  
  fluidRow(
    box(
      width = 6,
      title = "Cost Engineering Team",
      userList(
        userListItem(
          user_img = "prachi.JPG",
          
          title = "Shete, Prachi <prachi.shete@hyster-yale.com>",
          subtitle = "Americas Department", "Electronics Commodity"
        ),
        userListItem(
          user_img = "harshit.JPG",
          
          title = "Singhai, Harshit <harshit.singhai@hyster-yale.com>",
          subtitle = "Europe Department", "Fabrication Commodity"
          
        )
      )
    )
  )
}



dashboard_function <- function(input, output, session) {
  
  output$keymat <- renderPlotly({
    
    reactive_com <- commodityDf(input$material) %>% arrange(CaptureDate)
    reactive_min <- min(reactive_com$MaterialPrice, na.rm = T) - 0.1
    reactive_max <- max(reactive_com$MaterialPrice, na.rm = T) + 0.1
    
    plot_ly(
      reactive_com,
      x = ~reactive_com$CaptureDate,
      y = ~reactive_com$MaterialPrice,
      type = "scatter",
      showlegend = F, 
      mode = "lines+markers",
      line = list(color = 'RGB(150,202,89, 0.5)', width = 2),
      fill = 'tozeroy',
      fillcolor = 'rgba(242,249,235, 0.7)') %>%
      add_trace(
        x = ~reactive_com$CaptureDate, 
        y = ~reactive_com$MaterialPrice,
        marker = list(
          color = "#EDEDED",
          size = 7,
          line = list(color = 'rgb(150, 202, 89)', 
                      width = 2)
        )
      ) %>%
      
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "($/lb) Material Price",
                     range = c(reactive_min, reactive_max))
      )
    
  })
  
  output$report <- renderImage({
    
   # filename <- normalizePath(file.path("D:/Pralhad/git_CostEstimator", "report.PNG"))
    
    list(src = normalizePath(file.path("D:/Pralhad/git_CostEstimator/CostEstimator/www/", "report.PNG")),
         contentType = 'image/png',
         height = 800,
         alt = "Report")
    
  }, deleteFile = F)
  
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
        tabName = "dashboard",
        fluidRow(
          box_dashboard_UI("ds"),
          keymaterial_dashboard_UI("ds"),
          cost_report_UI("ds"),
          
          cost_team_UI("ds")
        )
      ),
      
      tabItem(
        tabName = "tracksheet",
        
        # fluidRow(
        #   input_tracksheet_UI("tk"),
        #   table_tracksheet_UI("tk")
        # )
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
  
}

shinyApp(ui, server)