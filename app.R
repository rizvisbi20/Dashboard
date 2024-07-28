# setwd("C:/Users/rizvi/OneDrive - University of Saskatchewan/Dashboard/Dashboard_rizvi")


# 0.0: LIBRARIES ----
library(readr)

library(tidyverse)
library(reactable)

library(shiny)
library(shinyauthr)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)

library(future)
library(knitr)
library(kableExtra)

library(openxlsx)
library(shiny.i18n)

# 1.0: DATA FILES ----


user_base_tbl <- read_csv("credentials_20_July_2022.csv")


selectors_tbl <- read_csv("selectors_final_update-10May2023.csv")
selectors_tbl$Category <-trimws(selectors_tbl$Category, which = c("both"))

selectors_tbl$Variable <- iconv(selectors_tbl$Variable, from = 'UTF-8', to = 'ASCII//TRANSLIT')
selectors_tbl$Variable <-trimws(selectors_tbl$Variable, which = c("both"))

selectors_tbl_french <- read.csv("selectors_final_update_french-10May2023.csv",encoding = "latin1")
selectors_tbl_french$Category <-trimws(selectors_tbl_french$Category, which = c("both"))

selectors_tbl_french$Variable <- iconv(selectors_tbl$Variable, from = 'UTF-8', to = 'ASCII//TRANSLIT')
selectors_tbl_french$Variable <-trimws(selectors_tbl$Variable, which = c("both"))




selectors_tbl <- selectors_tbl %>% 
  filter (!is.na(Order)) 
translate <- Translator$new(translation_json_path = "translations/translation.json")

translate$set_translation_language("English")
# 2.0: SOURCE FILES ----


source(file = "00_scripts/00_organization_type.R")
organization_type()

source(file = "00_scripts/00_organization_type_french.R")
organization_type_french()
# data_frence <- read_csv("final_dataset_09Nov2022.csv") 
organization_df_french<-organization_df_french %>%
  filter(ORGANIZATION != "Arctic Co-op")%>%
  select (CaseId,Type, ORGANIZATION, Wave, Year) %>%
  mutate (Type = recode(Type,
                        `1` = "Coopératives de vente au détail",
                        `2` = "Coopératives multipartenaires",
                        `3` = "Coopératives de producteurs",
                        `4` = "Les caisses populaires",
                        `5` = "Deuxième niveau ",
                        `6` = "Coopérative de consommation",
                        `7`="Coopérative de santé",
                        `8`="Autre"),
          Wave=recode(Wave,
                      `1`="Vague-1",
                      `2`="Vague-2")) %>%
  rename(Organization = ORGANIZATION) %>% arrange(Organization)
questionnaire_tbl <- read.csv("questionnaire-eng-fra.csv",encoding = "latin1")

source(file = "00_scripts/01_Logo.R") 


source(file = "00_scripts/02_datasteps.R")
main_dataset()

# source(file = "00_scripts/02_datasteps_wave2.R")
# main_dataset_wave2()

source(file = "00_scripts/02_datasteps_french.R")
main_dataset_french()

source(file = "00_scripts/02_datasteps_questionnaire.R")
main_dataset_questionnaire()

source(file = "00_scripts/02_datasteps_questionnaire_french.R")
main_dataset_questionnaire_french()

ui <- fluidPage(
  
  # * Modal Alerts ----
  useSweetAlert(), #needed for the modal alerts. can stick anywhere in the ui!
  
  
  # * CSS ----
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = shinytheme("cerulean"))
    ,tags$link(rel = "stylesheet", type = "text/css", href = "my_styles.css")
  ),
  
  # * JS ----
  shinyjs::useShinyjs(), #allows the application to use ShinyJs 
  
  # * Favicon ----
  
  tags$head(tags$link(rel="shortcut icon", href="favicon.ico/JSGS symbol.png")),
  
  shiny.i18n::usei18n(translate),
  tags$div(
    class="btn btn-primary btn-md pull-right",
    style='float: right;font-size: 18px;',
    selectInput(
      inputId='selected_language',
      label=translate$t('Change language'),
      choices = translate$get_languages(),
      selected = translate$get_key_translation()
    )
  ),
  # * User Login ----
  shinyauthr::loginUI(
    id = "login",
    title = tagList(h2(class = "text-center",translate$t('Co-operative Governance Benchmarking Application')),
                    p(class= "text-center",translate$t("To reset user name or password please email: chasr@usask.ca"))),
    login_title = translate$t("Enter"),
    user_title =  translate$t("User Name"),
    pass_title =  translate$t("Password"),
    
    
  ),
  
  
  
  uiOutput(outputId = "website")
  
)  


# ~~~SERVER~~~ ----
server <- function(input, output, session) {
  
  observeEvent(input$selected_language, {
    update_lang(session, input$selected_language)
  })
  
  credentials <- callModule(
    module   = shinyauthr::login,
    id       = "login",
    data     = user_base_tbl,
    user_col = user,
    pwd_col  = password,
    log_out  = reactive(logout_init())
  ) 
  
  
  logout_init <- callModule(
    module = shinyauthr::logout,
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  
  reactive_values <- reactiveValues()
  
  observe({
    if(credentials()$user_auth){
      
      user_data_tbl <- credentials()$info
      
      
      reactive_values$user            <- user_data_tbl$user 
      reactive_values$password        <- user_data_tbl$password
      reactive_values$organization    <- user_data_tbl$organization 
      
      
    }
  })
  
  
  
  
  output$website <- renderUI({
    
    req(credentials()$user_auth)  
    
    # Nav Bar:   
    navbarPage(
      title = translate$t("Co-operative Governance Benchmarking Application"),
      inverse = FALSE,
      collapsible = TRUE,
      
      # Welcome Message
      header = div(
        class = "pull-right",
        style= "padding-right: 20px;",
        p(tags$b(translate$t("Welcome, "), reactive_values$organization, "!")),
      ),
      
      # 3.3.3: Sign Out ----
      a(href="https://syedrizvi05.shinyapps.io/DashboardApp/", class="btn btn-danger btn-md pull-right", translate$t("Sign Out")),
      
      # * Home Page ----
      tabPanel(
        title = translate$t("Home"),
        value = "page_0b",
        
        # 3.0: PEER GROUP SECTION ----
        sidebarLayout(
          sidebarPanel = sidebarPanel(
            width = 5,
            
            # Instructions For Selecting Peer Groups
            div(
              style = "color: #2e6da4;",
              
              tags$b(translate$t("Step 1: Creating a custom peer group allows you to benchmark your organization against the group you selected."),
                     style = "font-size: 18px;")
            ),
            
            div(
              style = "padding-top: 50px;",
              
              tags$b(translate$t("To create your peer group, please select organizations that you would like to include in the peer group:"),
                     style = "color: #000000; font-size: 18px;")
            ),
            div(
              
              style = "color: #FF0000; padding-top: 20px; padding-bottom: 20px;",
              
              tags$b(translate$t("A minimum selection of five organizations is required"))
            ),
            
            
            # 3.1: Select/Clear All ----
            conditionalPanel(
              condition = "input.selected_language == 'English'",
              tabsetPanel(
                id       = "tabset_0",
                
                tabPanel (title = translate$t("Peer Group Creator"),
                          div(
                            class = "container",
                            style = "padding-bottom: 10px; padding-top: 15px;"),
                          
                          
                          actionButton(inputId = "select_btn", label = translate$t("Select All"), class ="btn btn-primary btn-md"),
                          actionButton(inputId = "clear_btn", label  = translate$t("Clear Selection"), class ="btn btn-primary btn-md"),
                          # 3.3.1: View Peer Groups ----
                          actionButton(inputId = "view_group", label = translate$t("View Peer Groups"), icon = icon("fas fa-table"), class="btn btn-primary btn-md"),
                          
                          # 3.2: Peer Group Table ----
                          div(
                            class = "container",
                            style = "padding-bottom: 10px; padding-top: 10px;"),
                          reactableOutput(outputId = "peergroup_tbl")),
                tabPanel (title = translate$t("Dowload Your Report"),
                          # 3.3.0: Action Buttons ----
                          div(
                            class = "container",
                            style = "padding-bottom: 10px; padding-top: 10px;",
                            
                            # # 3.3.1: View Peer Groups ----
                            # actionButton(inputId = "view_group_1", label = "test", icon = icon("fas fa-table"), class="btn btn-primary btn-md"),
                            # 
                            # 3.3.2: Download Own Report ----
                            uiOutput("download0"),
                            # downloadButton("download0", label = translate$t("Download Your Report-Wave1"), class="btn btn-primary btn-md pull-left", style = "margin-right: 5px;"),
                            # 3.3.2: Download Own Report test ----
                            # actionButton("init_test", label = translate$t("Download Your Report-Wave1"), icon = icon("download")),
                            # downloadButton("downloadData_test", label = translate$t("Download Your Report-Wave1"), style = "visibility: hidden;"),
                            # downloadButton("download0_test", label = translate$t("Download Your Report-test"), class="btn btn-primary btn-md pull-left", style = "margin-right: 5px;"),
                            
                            # 3.3.3: Download Custom Report ----
                            uiOutput("download0_2022")
                          )
                          
                          
                          
                )
              )
            ),
            conditionalPanel(
              condition = "input.selected_language == 'French'",
              tabsetPanel(
                id       = "tabset_0_french",
                
                tabPanel (title = translate$t("Peer Group Creator"),
                          div(
                            class = "container",
                            style = "padding-bottom: 10px; padding-top: 15px;",
                            
                            
                            actionButton(inputId = "select_btn_french", label = translate$t("Select All"), class ="btn btn-primary btn-md"),
                            actionButton(inputId = "clear_btn_french", label  = translate$t("Clear Selection"), class ="btn btn-primary btn-md"),
                            # 3.3.1: View Peer Groups ----
                            actionButton(inputId = "view_group_french", label = translate$t("View Peer Groups"), icon = icon("fas fa-table"), class="btn btn-primary btn-md"),
                          ),
                          
                          # 3.2: Peer Group Table ----
                          div(
                            class = "container",
                            style = "padding-bottom: 10px; padding-top: 10px;"),
                          reactableOutput(outputId = "peergroup_tbl_french")
                ),
                tabPanel (title = translate$t("Download Your Report"),
                          # 3.3.0: Action Buttons ----
                          div(
                            class = "container",
                            style = "padding-bottom: 10px; padding-top: 10px;",
                            # 3.3.2: Download Own Report ----
                            uiOutput("download0_french"),
                            # downloadButton("download0_french", label = translate$t("Download Your Report-Wave1"), class="btn btn-primary btn-md pull-left", style = "margin-right: 5px;"),
                            # 3.3.2: Download Own Report ----
                            # downloadButton("download0_test_french", label = translate$t("Download Your Report-test"), class="btn btn-primary btn-md pull-left", style = "margin-right: 5px;"),
                            # 3.3.3: Download Custom Report ----
                            uiOutput("download0_2022_french")
                          )
                )
              )
            )
            
          ),
          
          # 4.0: SELECTORS SECTION ----
          mainPanel = mainPanel( width = 7,
                                 
                                 # Instructions For Selecting Comparison Selectors
                                 
                                 div(
                                   style = "color: #2e6da4; padding-top: 20px; padding-bottom: 20px;",
                                   tags$b(translate$t("Step 2: Select from list of the comparison selections to see your results and the averages of the peer group"),
                                          style = "font-size: 18px;")
                                 ),
                                 
                                 # 4.1: Select/Clear/Expand/Collapse -----
                                 conditionalPanel(
                                   condition = "input.selected_language == 'English'",
                                   tabsetPanel(
                                     id       = "tabset_1",
                                     
                                     
                                     tabPanel (title = translate$t("Comparison Selections"),
                                               div(
                                                 class = "container",
                                                 style = "padding-bottom: 30px; padding-top: 10px;",
                                                 actionButton(inputId = "select_btn1", label = translate$t("Select All"), class ="btn btn-primary btn-md"),
                                                 actionButton(inputId = "clear_btn1", label  = translate$t("Clear Selection"), class ="btn btn-primary btn-md"),
                                                 actionButton("expand_btn", translate$t("Expand Rows"), class ="btn btn-primary btn-md"),
                                                 actionButton("collapse_btn", translate$t("Collapse Rows"), class ="btn btn-primary btn-md")
                                               ),
                                               
                                               # 4.2: Selectors Table ----
                                               reactableOutput(outputId = "selections_tbl"),
                                               
                                               # 4.3.0: Actions Buttons ----
                                               div(
                                                 class = "container",
                                                 style = "padding-top: 10px;",
                                                 
                                                 # 4.3.1: View Selectors ----
                                                 actionButton(inputId = "view_comparison", label = translate$t("View Comparison Selections"), icon = icon("fas fa-table")
                                                              ,class="btn btn-primary btn-md pull-left", style = "margin-right: 5px;"),
                                                 
                                                 # 4.3.2: Download Custom Report ----
                                                 # uiOutput("downloadData")
                                                 actionButton("init_1", label = translate$t("Peer Group Analysis- pdf"), icon = icon("download"),class = "btn btn-primary btn-md"),
                                                 actionButton("init_2", label = translate$t("Peer Group Analysis- Excel"), icon = icon("download"),class = "btn btn-primary btn-md"),
                                                 downloadButton("download1", style = "visibility: hidden;"),
                                                 downloadButton("download2", style = "visibility: hidden;")
                                                 
                                                 # 
                                                 # downloadButton("download1", label = translate$t("Peer Group Analysis- pdf"), class = "btn btn-primary btn-md", icon = icon("far fa-file-pdf-o")),
                                                 # 
                                                 # downloadButton("download2", label = translate$t("Peer Group Analysis- Excel"), class = "btn btn-primary btn-md", icon = icon("far fa-file-excel")),
                                                 
                                                 
                                               )
                                               
                                               
                                     )
                                   )
                                 ),
                                 conditionalPanel(
                                   condition = "input.selected_language == 'French'",
                                   tabsetPanel(
                                     id       = "tabset_1_french",
                                     
                                     
                                     tabPanel (title = translate$t("Comparison Selections"),
                                               div(
                                                 class = "container",
                                                 style = "padding-bottom: 30px; padding-top: 10px;",
                                                 actionButton(inputId = "select_btn1_french", label = translate$t("Select All"), class ="btn btn-primary btn-md"),
                                                 actionButton(inputId = "clear_btn1_french", label  = translate$t("Clear Selection"), class ="btn btn-primary btn-md"),
                                                 actionButton("expand_btn_french", translate$t("Expand Rows"), class ="btn btn-primary btn-md"),
                                                 actionButton("collapse_btn_french", translate$t("Collapse Rows"), class ="btn btn-primary btn-md")
                                               ),
                                               
                                               # 4.2: Selectors Table ----
                                               reactableOutput(outputId = "selections_tbl_french"),
                                               
                                               # 4.3.0: Actions Buttons ----
                                               div(
                                                 class = "container",
                                                 style = "padding-top: 30px;",
                                                 
                                                 # 4.3.1: View Selectors ----
                                                 actionButton(inputId = "view_comparison_french", label = translate$t("View Comparison Selections"), icon = icon("fas fa-table")
                                                              ,class="btn btn-primary btn-md pull-left", style = "margin-right: 5px;"),
                                                 
                                                 # 4.3.2: Download Custom Report ----
                                                 actionButton("init_1_french", label = translate$t("Peer Group Analysis- pdf"), icon = icon("download"),class = "btn btn-primary btn-md"),
                                                 actionButton("init_2_french", label = translate$t("Peer Group Analysis- Excel"), icon = icon("download"),class = "btn btn-primary btn-md"),
                                                 downloadButton("download1_french", style = "visibility: hidden;"),
                                                 downloadButton("download2_french", style = "visibility: hidden;")
                                                 # uiOutput("downloadData_french")
                                                 
                                               )
                                               
                                               
                                     )
                                   )
                                 )
                                 
                                 
          )
          
          
        )
        
        
        
      ),
      # 3.3.3: Sign Out ----
      # a(href="https://coopstudies.shinyapps.io/Dashboard/", class="btn btn-danger btn-md pull-right", translate$t("Sign Out")),
      logo()   
      
    )  
    
  })
  
  
  
  
  #~~~PEER GROUPS~~~ ----
  reactiveDf_wave <- reactive({
    organization_df %>% 
      filter(Organization == reactive_values$organization) %>%
      select("Wave")
  })
  organization_df_wave <- reactive({
    organization_df %>%
      inner_join(reactiveDf_wave(), by = "Wave")
  })
  reactiveDf <- reactive({
    organization_df_wave() %>%
      filter(Organization != reactive_values$organization) %>%
      filter(!is.na(Type))
  })
  reactiveDf_wave_french <- reactive({
    organization_df_french %>% 
      filter(Organization == reactive_values$organization) %>%
      select("Wave")
  })
  organization_df_wave_french <- reactive({
    organization_df_french %>%
      inner_join(reactiveDf_wave_french(), by = "Wave")
  })
  reactiveDf_french <- reactive({
    organization_df_wave_french() %>% 
      filter(Organization != reactive_values$organization)%>%
      filter(!is.na(Type)) 
  })
  
  reactiveDf_2022 <- reactive({
    organization_df %>% 
      filter(Organization==reactive_values$organization & Wave=="Wave-2")
    # filter(!is.na(Type))
    # select(Organization)
  })
  reactiveDf_wave1 <- reactive({
    organization_df %>% 
      filter(Organization==reactive_values$organization & Wave=="Wave-1")
    # filter(!is.na(Type))
    # select(Organization)
  })
  # reactiveDf_test <- reactive({
  #   organization_df %>% 
  #     filter(Organization == reactive_values$organization)
  # })
  reactivedf_peer<-reactive({
    if (nrow(reactiveDf_2022())==1) {
      reactivedf_peer<-selectors_tbl
    } else{
      reactivedf_peer<-selectors_tbl %>%
        filter(is.na(Wave)==TRUE)

    }
  })
  reactivedf_peer_french<-reactive({
    if (nrow(reactiveDf_2022())==1) {
      reactivedf_peer_french<-selectors_tbl_french
    } else{
      reactivedf_peer_french<-selectors_tbl_french %>%
        filter(is.na(Wave)==TRUE)
      
    }
  })
  
  
  # (3.2) Peer Group Table ----    
  
  
  output$peergroup_tbl <- renderReactable({
    reactable(reactiveDf()[ , c(-1,-5)],
              selection = "multiple",
              groupBy = c("Wave","Type"),
              # selection = "multiple", # allows the user to select more than one row!
              # details = function(i) paste("Details for row", i),
              bordered = TRUE,
              striped = TRUE,
              highlight = TRUE,
              resizable = TRUE,
              wrap = TRUE,
              searchable = TRUE,
              pagination = FALSE, # not enough peer groups to have multiple pages
              compact = TRUE,
              defaultColDef = colDef(
                align = "left",
                maxWidth = 200
              ),
              
              columns = list(
                Type = colDef(name = translate$t("Type")),
                Organization = colDef(name = translate$t("Organization")),
                Year = colDef(name = translate$t("Year")),
                Wave = colDef(name = translate$t("Wave")),
                # Wave = colDef(name = translate$t("Wave")),
                .selection = colDef(
                  # width = 80,
                  # sticky = "left",
                  style = list(cursor = "pointer"),
                  # style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
                  # headerClass = "hide-checkbox",
                  # headerStyle = list(cursor = "pointer"),
                  headerStyle = list(pointerEvents = "none")
                  
                )
              ),
              language = reactableLang(
                searchPlaceholder = translate$t("Search...") #,
                # noData = "No entries found",
                # pageInfo = "{rowStart}\u2013{rowEnd} of {rows} entries",
                # pagePrevious = "\u276e",
                # pageNext = "\u276f",
                # 
                # # Accessible labels for assistive technology, such as screen readers
                # pagePreviousLabel = "Previous page",
                # pageNextLabel = "Next page"
              ),
              onClick = "select",
              # class="rt-td rt-td-select rt-align-right",
              theme = reactableTheme(
                headerStyle = list("& input[type='checkbox']" = list(display = "none"))
              ),
              rowStyle = JS("function(rowInfo) {
    if (rowInfo && rowInfo.selected) {
      return { backgroundColor: '#eee', boxShadow: 'inset 2px 0 0 0 #2fa4e7' }
    }
  }")
    )
  })
  output$peergroup_tbl_french <- renderReactable({
    reactable(reactiveDf_french()[ , c(-1,-5)],
              groupBy = c("Wave","Type"), #,"Wave","Year"
              selection = "multiple", # allows the user to select more than one row!
              bordered = TRUE,
              striped = TRUE,
              highlight = TRUE,
              resizable = TRUE,
              wrap = TRUE,
              searchable = TRUE,
              pagination = FALSE, # not enough peer groups to have multiple pages
              compact = TRUE,
              defaultColDef = colDef(
                align = "left",
                maxWidth = 200
              ),
              columns = list(
                Type = colDef(name = translate$t("Type")),
                Organization = colDef(name = translate$t("Organization")),
                Year = colDef(name = translate$t("Year")),
                Wave = colDef(name = translate$t("Wave")),
                .selection = colDef(
                  # width = 80,
                  # sticky = "left",
                  style = list(cursor = "pointer"),
                  # headerStyle = list(cursor = "pointer")
                  headerStyle = list(pointerEvents = "none")
                )
                
              ),
              language = reactableLang(
                searchPlaceholder = translate$t("Search...") #,
                # noData = "No entries found",
                # pageInfo = "{rowStart}\u2013{rowEnd} of {rows} entries",
                # pagePrevious = "\u276e",
                # pageNext = "\u276f",
                # 
                # # Accessible labels for assistive technology, such as screen readers
                # pagePreviousLabel = "Previous page",
                # pageNextLabel = "Next page"
              ),
              
              onClick = "select",
              theme = reactableTheme(
                headerStyle = list("& input[type='checkbox']" = list(display = "none"))
              ),
              rowStyle = JS("function(rowInfo) {
    if (rowInfo && rowInfo.selected) {
      return { backgroundColor: '#eee', boxShadow: 'inset 2px 0 0 0 #2fa4e7' }
    }
  }")
    )
  })
  
  
  # (3.1) Select/Clear All ----
  
  # SELECT ALL
  observeEvent(input$select_btn, {
    # Select rows
    updateReactable("peergroup_tbl", selected = c(1:nrow(reactiveDf())))
  })
  observeEvent(input$select_btn_french, {
    # Select rows
    updateReactable("peergroup_tbl_french", selected = c(1:nrow(reactiveDf_french())))
  })
  
  # CLEAR ALL
  observeEvent(input$clear_btn, {
    # Clear row selection
    updateReactable("peergroup_tbl", selected = NA)
  }) 
  observeEvent(input$clear_btn_french, {
    # Clear row selection
    updateReactable("peergroup_tbl_french", selected = NA)
  }) 
  
  
  # (3.3.1) Peer Groups Selected ----
  
  # *User Selections ----
  
  
  selected <- reactive(getReactableState("peergroup_tbl", "selected"))
  selected_french <- reactive(getReactableState("peergroup_tbl_french", "selected"))
  
  # **Testing ----
  # prints to the console
  # observe({
  #   # print(selected())
  #   # print(reactiveDf()[selected(), -1])  #prints the entire table
  #   print((organization_df[selected(), ])) #prints the row numbers
  # 
  # })
  
  # * Table- Selected Peer Groups ----  
  
  #Table of the peer groups the user selects
  selected_Wave1 <- reactive({
    df<-filter(reactiveDf()[selected(), -1],Wave=="Wave-1")
  })
  selected_Wave2 <- reactive({
    df<-filter(reactiveDf()[selected(), -1],Wave=="Wave-2")
  })
  selected_Wave1_french <- reactive({
    df<-filter(reactiveDf_french()[selected_french(), -1],Wave=="Vague-1")
  })
  selected_Wave2_french <- reactive({
    df<-filter(reactiveDf_french()[selected_french(), -1],Wave=="Vague-2")
  })
  # observe({
  #   # print(selected())
  #   # print(reactiveDf()[selected(), -1])  #prints the entire table
  #   print(length(reactiveDf_test)) #prints the row numbers
  # 
  # })
  output$peergroup_tbl1 <- renderReactable({
    reactable(reactiveDf()[selected(), -1], #subsets for the rows the user selected
              groupBy = c("Wave"),
              bordered = TRUE,
              striped = TRUE,
              highlight = TRUE,
              resizable = TRUE,
              wrap = TRUE,
              searchable = TRUE,
              pagination = FALSE, # not enough peer groups to have multiple pages
              defaultColDef = colDef(
                align = "left",
                maxWidth = 200
              ),
              language = reactableLang(
                searchPlaceholder = translate$t("Search...") #,
                # noData = "No entries found",
                # pageInfo = "{rowStart}\u2013{rowEnd} of {rows} entries",
                # pagePrevious = "\u276e",
                # pageNext = "\u276f",
                # 
                # # Accessible labels for assistive technology, such as screen readers
                # pagePreviousLabel = "Previous page",
                # pageNextLabel = "Next page"
              )
    )
  })
  
  
  output$peergroup_tbl1_french <- renderReactable({
    reactable(reactiveDf_french()[selected_french(), -1], #subsets for the rows the user selected
              groupBy = c("Wave"),
              bordered = TRUE,
              striped = TRUE,
              highlight = TRUE,
              resizable = TRUE,
              wrap = TRUE,
              searchable = TRUE,
              pagination = FALSE, # not enough peer groups to have multiple pages
              columns = list(
                Type = colDef(name = translate$t("Type")),
                Organization = colDef(name = translate$t("Organization")),
                Year = colDef(name = translate$t("Year")),
                Wave = colDef(name = translate$t("Wave"))),
              defaultColDef = colDef(
                align = "left",
                maxWidth = 200
              ),
              language = reactableLang(
                searchPlaceholder = translate$t("Search...") #,
                # noData = "No entries found",
                # pageInfo = "{rowStart}\u2013{rowEnd} of {rows} entries",
                # pagePrevious = "\u276e",
                # pageNext = "\u276f",
                # 
                # # Accessible labels for assistive technology, such as screen readers
                # pagePreviousLabel = "Previous page",
                # pageNextLabel = "Next page"
              )
    )
  })
  
  
  
  # * Modal: View Peer Groups ----    
  observeEvent(input$view_group, {
    
    if(length(selected())<5)   { #has to be more than 5 organizations selected
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = tags$b(translate$t("*Select a minimum of 5 organizations to proceed!"),
                      style = "color:#FF0000;"),
        type = "error"
      )
    }  else { 
      
      
      modalDialog(
        title = div(tags$b(translate$t("Organizations In This Analysis"),
                           style = "color:#2e6da4;")),
        
        size = "m",
        easyClose = TRUE,
        
        
        
        
        reactableOutput(outputId = "peergroup_tbl1"),
        
        footer = modalButton(div(tags$b(translate$t("Exit"),
                                        style = "color:#2e6da4;"))
        )
      ) %>% showModal()
      
    }
    
  })  
  observeEvent(input$view_group_french, {
    
    if(length(selected_french())<5) { #has to be more than 5 organizations selected
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = tags$b(translate$t("*Select a minimum of 5 organizations to proceed!"),
                      style = "color:#FF0000;"),
        type = "error"
      )
    }  else { 
      
      
      modalDialog(
        title = div(tags$b(translate$t("Organizations In This Analysis"),
                           style = "color:#2e6da4;")),
        
        size = "m",
        easyClose = TRUE,
        
        
        
        
        reactableOutput(outputId = "peergroup_tbl1_french"),
        
        footer = modalButton(div(tags$b(translate$t("Exit"),
                                        style = "color:#2e6da4;"))
        )
      ) %>% showModal()
      
    }
    
  })  
  
  # ~~~SELECTORS~~~ ----
  
  # (4.2) Selectors Table----
  
  # Full table of the selectors   
  output$selections_tbl <- renderReactable({
    reactable(reactivedf_peer()[, c(-1, -5,-6)],
              groupBy = c("Category",
                          "Second_level_disaggregation"),
              selection = "multiple", # allows the user to select more than one row!
              bordered = TRUE,
              striped = TRUE,
              highlight = TRUE,
              resizable = TRUE,
              wrap = TRUE,
              searchable = TRUE,
              pagination = TRUE,
              compact = TRUE,
              defaultColDef = colDef(
                align = "left",
                maxWidth = 175
              ),
              
              columns = list(
                Category = colDef(name = translate$t("Theme")),
                Second_level_disaggregation = colDef(name = translate$t("Category")),
                Third_level_disaggregation  = colDef(name = translate$t("Measure")),
                .selection = colDef(
                  #width = 80,
                  # sticky = "left",
                  style = list(cursor = "pointer"),
                  # headerStyle = list(cursor = "pointer")
                  headerStyle = list(pointerEvents = "none")
                )
              ),
              language = reactableLang(
                searchPlaceholder = translate$t("Search...") #,
                # noData = "No entries found",
                # pageInfo = "{rowStart}\u2013{rowEnd} of {rows} entries",
                # pagePrevious = "\u276e",
                # pageNext = "\u276f",
                # 
                # # Accessible labels for assistive technology, such as screen readers
                # pagePreviousLabel = "Previous page",
                # pageNextLabel = "Next page"
              ),
              
              onClick = "select",
              theme = reactableTheme(
                headerStyle = list("& input[type='checkbox']" = list(display = "none"))
              ),
              rowStyle = JS("function(rowInfo) {
    if (rowInfo && rowInfo.selected) {
      return { backgroundColor: '#eee', boxShadow: 'inset 2px 0 0 0 #2fa4e7' }
    }
  }")
    )
  })  
  output$selections_tbl_french <- renderReactable({
    reactable(reactivedf_peer_french()[, c(-1, -5,-6)],
              groupBy = c("Category",
                          "Second_level_disaggregation"),
              selection = "multiple", # allows the user to select more than one row!
              bordered = TRUE,
              striped = TRUE,
              highlight = TRUE,
              resizable = TRUE,
              wrap = TRUE,
              searchable = TRUE,
              pagination = TRUE,
              compact = TRUE,
              defaultColDef = colDef(
                align = "left",
                maxWidth = 175
              ),
              
              columns = list(
                Category = colDef(name = translate$t("Theme")),
                Second_level_disaggregation = colDef(name = translate$t("Category")),
                Third_level_disaggregation  = colDef(name = translate$t("Measure")),
                .selection = colDef(
                  #width = 80,
                  # sticky = "left",
                  style = list(cursor = "pointer"),
                  # headerStyle = list(cursor = "pointer")
                  headerStyle = list(pointerEvents = "none")
                )
              ),
              language = reactableLang(
                searchPlaceholder = translate$t("Search...") #,
                # noData = "No entries found",
                # pageInfo = "{rowStart}\u2013{rowEnd} of {rows} entries",
                # pagePrevious = "\u276e",
                # pageNext = "\u276f",
                # 
                # # Accessible labels for assistive technology, such as screen readers
                # pagePreviousLabel = "Previous page",
                # pageNextLabel = "Next page"
              ),
              
              onClick = "select",
              theme = reactableTheme(
                headerStyle = list("& input[type='checkbox']" = list(display = "none"))
              ),
              rowStyle = JS("function(rowInfo) {
    if (rowInfo && rowInfo.selected) {
      return { backgroundColor: '#eee', boxShadow: 'inset 2px 0 0 0 #2fa4e7' }
    }
  }")
    )
  })  
  # (4.1) Select/Clear/Expand/Collapse ----
  
  # SELECT ALL
  observeEvent(input$select_btn1, {
    # Select all of the rows (1: to nrow)
    updateReactable("selections_tbl", selected = c(1:nrow(selectors_tbl)))  
  })
  observeEvent(input$select_btn1_french, {
    # Select all of the rows (1: to nrow)
    updateReactable("selections_tbl_french", selected = c(1:nrow(selectors_tbl_french)))  
  })
  
  # CLEAR ALL
  observeEvent(input$clear_btn1, {
    # Clear row selection
    updateReactable("selections_tbl", selected = NA)
  }) 
  observeEvent(input$clear_btn1_french, {
    # Clear row selection
    updateReactable("selections_tbl_french", selected = NA)
  }) 
  
  
  # EXPAND 
  observeEvent(input$expand_btn, {
    # Expand all rows
    updateReactable("selections_tbl", expanded = TRUE)
  })
  observeEvent(input$expand_btn_french, {
    # Expand all rows
    updateReactable("selections_tbl_french", expanded = TRUE)
  })
  
  # COLLAPSE
  observeEvent(input$collapse_btn, {
    # Collapse all rows
    updateReactable("selections_tbl", expanded = FALSE)
  }) 
  observeEvent(input$collapse_btn_french, {
    # Collapse all rows
    updateReactable("selections_tbl_french", expanded = FALSE)
  }) 
  
  
  # (4.3.1) Selectors Selected ----
  
  # *User Selections ----
  
  
  selected1 <- reactive(getReactableState("selections_tbl", "selected"))  
  selected1_french <- reactive(getReactableState("selections_tbl_french", "selected"))  
  
  # * Testing ----    
  
  #prints to the console
  # observe({
  #   print(selectors_tbl[selected1(), ])
  #   print(nrow(selectors_tbl[selected1(), ]))
  # })  
  
  # * Table - Selected Selectors ----
  
  observeEvent(input$selections, {
    # Select rows
    updateReactable("selections_tbl", selected = c(selected1()))
  })
  observeEvent(input$selections_french, {
    # Select rows
    updateReactable("selections_tbl_french", selected = c(selected1_french()))
  })
  
  #Table of the comparison selections the user selects
  output$selectors_tbl1 <- renderReactable({
    reactable(selectors_tbl[ c(selected1()), c(-1, -5,-6)],
              #c(selected1(),select_themes()), c(-1, -5)],
              groupBy = c("Category",
                          "Second_level_disaggregation"),
              bordered = TRUE,
              striped = TRUE,
              highlight = TRUE,
              resizable = TRUE,
              wrap = TRUE,
              searchable = TRUE,
              pagination = TRUE,
              compact = TRUE,
              defaultColDef = colDef(
                align = "left",
                maxWidth = 200
              ),
              
              columns = list(
                Category = colDef(name = translate$t("Theme")),
                Second_level_disaggregation = colDef(name = translate$t("Category")),
                Third_level_disaggregation  = colDef(name = translate$t("Measure"))
              ),
              language = reactableLang(
                searchPlaceholder = translate$t("Search...") #,
                # noData = "No entries found",
                # pageInfo = "{rowStart}\u2013{rowEnd} of {rows} entries",
                # pagePrevious = "\u276e",
                # pageNext = "\u276f",
                # 
                # # Accessible labels for assistive technology, such as screen readers
                # pagePreviousLabel = "Previous page",
                # pageNextLabel = "Next page"
              )
    )
  })
  output$selectors_tbl1_french <- renderReactable({
    reactable(selectors_tbl_french[ c(selected1_french()), c(-1, -5,-6)],
              #c(selected1(),select_themes()), c(-1, -5)],
              groupBy = c("Category",
                          "Second_level_disaggregation"),
              bordered = TRUE,
              striped = TRUE,
              highlight = TRUE,
              resizable = TRUE,
              wrap = TRUE,
              searchable = TRUE,
              pagination = TRUE,
              compact = TRUE,
              defaultColDef = colDef(
                align = "left",
                maxWidth = 200
              ),
              
              columns = list(
                Category = colDef(name = translate$t("Theme")),
                Second_level_disaggregation = colDef(name = translate$t("Category")),
                Third_level_disaggregation  = colDef(name = translate$t("Measure"))
              ),
              language = reactableLang(
                searchPlaceholder = translate$t("Search...") #,
                # noData = "No entries found",
                # pageInfo = "{rowStart}\u2013{rowEnd} of {rows} entries",
                # pagePrevious = "\u276e",
                # pageNext = "\u276f",
                # 
                # # Accessible labels for assistive technology, such as screen readers
                # pagePreviousLabel = "Previous page",
                # pageNextLabel = "Next page"
              )
    )
  })
  
  
  # *Modal: View Selectors Selected ----   
  
  observeEvent(input$view_comparison, {
    
    
    if(length(selected1())<1) {
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = tags$b(translate$t("*Select a minimum of 1 comparison selector to proceed!"),
                      style = "color:#FF0000;"),
        type = "error"
      )
    }  else { 
      
      
      modalDialog(
        size = "l",
        easyClose = TRUE,
        
        title = div(tags$b(translate$t("Comparison Selectors In This Analysis"),
                           style = "color:#2e6da4;")),
        
        
        
        reactableOutput(outputId = "selectors_tbl1"),
        
        
        
        footer = modalButton(div(tags$b(translate$t("Exit"),
                                        style = "color:#2e6da4;"))
        )
        
      ) %>% showModal()
      
    }
  }) 
  observeEvent(input$view_comparison_french, {
    
    
    if(length(selected1_french())<1) {
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = tags$b(translate$t("*Select a minimum of 1 comparison selector to proceed!"),
                      style = "color:#FF0000;"),
        type = "error"
      )
    }  else { 
      
      
      modalDialog(
        size = "l",
        easyClose = TRUE,
        
        title = div(tags$b(translate$t("Comparison Selectors In This Analysis"),
                           style = "color:#2e6da4;")),
        
        
        
        reactableOutput(outputId = "selectors_tbl1_french"),
        
        
        
        footer = modalButton(div(tags$b(translate$t("Exit"),
                                        style = "color:#2e6da4;"))
        )
        
      ) %>% showModal()
      
    }
  })
  
  
  # ~~~OWN REPORT~~~ ----
  
  # (3.3.2) Download Own Report Wave1 ----
  
  output$download0_french <- renderUI({
    req(nrow(reactiveDf_wave1()) >= 1 ) #select at least 5 co-ops and 1 selector
    div(
      class = "container",
      style= "padding-bottom: 10px; padding-top: 10px;",
      downloadButton("download0_french_report", label = translate$t("Download Your Report-Wave1"), icon = icon("download"), class="btn btn-primary btn-md",style = "margin-right: 5px;")
    )
    
  })
  output$download0 <- renderUI({
    req(nrow(reactiveDf_wave1()) >= 1 ) #select at least 5 co-ops and 1 selector
    div(
      class="container",
      style= "padding-bottom: 10px; padding-top: 10px;",
      downloadButton("download0_report", label = translate$t("Download Your Report-Wave1"), icon = icon("download"), class="btn btn-primary btn-md",style = "margin-right: 5px;") 
    )
  })
  output$download0_report <- downloadHandler(
    # filename = function() {
    #   paste('Your-Results-', Sys.Date(), '.pdf', sep='')
    # },
    filename = "report.pdf",
    content = function(file) {
      withProgress(message = "Creating Report. Please wait...",{
        tempReport <- file.path(tempdir(), "questionnaire_english.Rmd")
        src1<-file.path(tempdir(), "USask_CHASR_Logo.png")
        file.copy("USask_CHASR_Logo.png", src1, overwrite = TRUE)
        src2<-file.path(tempdir(), "JSGS_CCSC_Logo_CMYK-highnobg.jpg") #"C:\Users\rizvi\OneDrive - University of Saskatchewan\Dashboard\Dashboard Code\JSGS_CCSC_Logo_CMYK-highnobg.jpg"
        file.copy("JSGS_CCSC_Logo_CMYK-highnobg.jpg", src2, overwrite = TRUE)
        src3<-file.path(tempdir(), "1200px-United_Farmers_of_Alberta_Logo.jpg") #"C:\Users\rizvi\OneDrive - University of Saskatchewan\Dashboard\Dashboard Code\1200px-United_Farmers_of_Alberta_Logo.jpg"
        file.copy("1200px-United_Farmers_of_Alberta_Logo.jpg", src3, overwrite = TRUE)
        src4<-file.path(tempdir(), "CCSC Dashboard - Organizational Finances.png") #"C:\Users\rizvi\OneDrive - University of Saskatchewan\Dashboard\Dashboard Code\1200px-United_Farmers_of_Alberta_Logo.jpg"
        file.copy("CCSC Dashboard - Organizational Finances.png", src4, overwrite = TRUE)
        file.copy("questionnaire_english.Rmd", tempReport, overwrite = TRUE)
        params <- list(survey_english = survey_data_wave1(),
                       orga=reactive_values$organization
                       
        )
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
        # When the downloadHandler function runs, increment rv1$download_flag
        rv1$download_flag <- rv1$download_flag + 1
        
        if(rv1$download_flag > 0){  # trigger event whenever the value of rv1$download_flag changes
          shinyjs::alert("File downloaded!")
        }
      })
      
    }
  )
  output$download0_french_report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      withProgress(message = "Creating Report. Please wait...",{
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "questionnaire_french.Rmd")
        src1<-file.path(tempdir(), "USask_CHASR_Logo.png")
        file.copy("USask_CHASR_Logo.png", src1, overwrite = TRUE)
        src2<-file.path(tempdir(), "JSGS_CCSC_Logo_CMYK-highnobg.jpg") #"C:\Users\rizvi\OneDrive - University of Saskatchewan\Dashboard\Dashboard Code\JSGS_CCSC_Logo_CMYK-highnobg.jpg"
        file.copy("JSGS_CCSC_Logo_CMYK-highnobg.jpg", src2, overwrite = TRUE)
        src3<-file.path(tempdir(), "1200px-United_Farmers_of_Alberta_Logo.jpg") #"C:\Users\rizvi\OneDrive - University of Saskatchewan\Dashboard\Dashboard Code\1200px-United_Farmers_of_Alberta_Logo.jpg"
        file.copy("1200px-United_Farmers_of_Alberta_Logo.jpg", src3, overwrite = TRUE)
        src4<-file.path(tempdir(), "CCSC Dashboard - Organizational Finances.png") #"C:\Users\rizvi\OneDrive - University of Saskatchewan\Dashboard\Dashboard Code\1200px-United_Farmers_of_Alberta_Logo.jpg"
        file.copy("CCSC Dashboard - Organizational Finances.png", src4, overwrite = TRUE)
        
        file.copy("questionnaire_french.Rmd", tempReport, overwrite = TRUE)
        params <- list(survey_french = survey_data_wave1_french(),
                       orga=reactive_values$organization
                       
        )
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
        # When the downloadHandler function runs, increment rv1$download_flag
        rv1$download_flag <- rv1$download_flag + 1
        
        if(rv1$download_flag > 0){  # trigger event whenever the value of rv1$download_flag changes
          shinyjs::alert("File downloaded!")
        }
      })
      
    }
  )
  
  # 3.3.3: Download own Report 2022----
  output$download0_2022 <- renderUI({
    req(nrow(reactiveDf_2022()) >= 1 ) #select at least 5 co-ops and 1 selector
    div(
      class="container",
      # style= "padding-bottom: 30px; padding-top: 10px;",
      downloadButton("download0_2022_report", label = translate$t("Download Your Report-Wave2"), icon = icon("download"), class="btn btn-primary btn-md",style = "margin-right: 5px;")
    ) 
    # downloadButton("download0_2022_report", label = translate$t("Download Your Report-Wave2"), icon = icon("fas fa-table"), class="btn btn-primary btn-md",style = "margin-right: 5px;")
  })
  output$download0_2022_french <- renderUI({
    req(nrow(reactiveDf_2022()) >= 1 ) #select at least 5 co-ops and 1 selector
    
    
    div(
      class = "container",
      # style= "padding-bottom: 30px; padding-top: 10px;",
      downloadButton("download0_2022_report_french", label = translate$t("Download Your Report-Wave2"), icon = icon("download"), class="btn btn-primary btn-md",style = "margin-right: 5px;")
    )
    
  })
  # (3.3.2) Download Own Report 2022 ----
  
  output$download0_2022_report <- downloadHandler(
    # filename = function() {
    #   paste('Your-Results-', Sys.Date(), '.pdf', sep='')
    # },
    filename = "report.pdf",
    content = function(file) {
      withProgress(message = "Creating Report. Please wait...",{
        tempReport <- file.path(tempdir(), "questionnaire_english.Rmd")
        src1<-file.path(tempdir(), "USask_CHASR_Logo.png")
        file.copy("USask_CHASR_Logo.png", src1, overwrite = TRUE)
        src2<-file.path(tempdir(), "JSGS_CCSC_Logo_CMYK-highnobg.jpg") #"C:\Users\rizvi\OneDrive - University of Saskatchewan\Dashboard\Dashboard Code\JSGS_CCSC_Logo_CMYK-highnobg.jpg"
        file.copy("JSGS_CCSC_Logo_CMYK-highnobg.jpg", src2, overwrite = TRUE)
        src3<-file.path(tempdir(), "1200px-United_Farmers_of_Alberta_Logo.jpg") #"C:\Users\rizvi\OneDrive - University of Saskatchewan\Dashboard\Dashboard Code\1200px-United_Farmers_of_Alberta_Logo.jpg"
        file.copy("1200px-United_Farmers_of_Alberta_Logo.jpg", src3, overwrite = TRUE)
        src4<-file.path(tempdir(), "CCSC Dashboard - Organizational Finances.png") #"C:\Users\rizvi\OneDrive - University of Saskatchewan\Dashboard\Dashboard Code\1200px-United_Farmers_of_Alberta_Logo.jpg"
        file.copy("CCSC Dashboard - Organizational Finances.png", src4, overwrite = TRUE)
        file.copy("questionnaire_english.Rmd", tempReport, overwrite = TRUE)
        params <- list(survey_english = survey_data_wave2(),
                       orga=reactive_values$organization
                       
        )
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
        # When the downloadHandler function runs, increment rv1$download_flag
        rv1$download_flag <- rv1$download_flag + 1
        
        if(rv1$download_flag > 0){  # trigger event whenever the value of rv1$download_flag changes
          shinyjs::alert("File downloaded!")
        }
      })
      
    }
  )
  output$download0_2022_report_french <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      withProgress(message = "Creating Report. Please wait...",{
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "questionnaire_french.Rmd")
        src1<-file.path(tempdir(), "USask_CHASR_Logo.png")
        file.copy("USask_CHASR_Logo.png", src1, overwrite = TRUE)
        src2<-file.path(tempdir(), "JSGS_CCSC_Logo_CMYK-highnobg.jpg") #"C:\Users\rizvi\OneDrive - University of Saskatchewan\Dashboard\Dashboard Code\JSGS_CCSC_Logo_CMYK-highnobg.jpg"
        file.copy("JSGS_CCSC_Logo_CMYK-highnobg.jpg", src2, overwrite = TRUE)
        src3<-file.path(tempdir(), "1200px-United_Farmers_of_Alberta_Logo.jpg") #"C:\Users\rizvi\OneDrive - University of Saskatchewan\Dashboard\Dashboard Code\1200px-United_Farmers_of_Alberta_Logo.jpg"
        file.copy("1200px-United_Farmers_of_Alberta_Logo.jpg", src3, overwrite = TRUE)
        src4<-file.path(tempdir(), "CCSC Dashboard - Organizational Finances.png") #"C:\Users\rizvi\OneDrive - University of Saskatchewan\Dashboard\Dashboard Code\1200px-United_Farmers_of_Alberta_Logo.jpg"
        file.copy("CCSC Dashboard - Organizational Finances.png", src4, overwrite = TRUE)
        
        file.copy("questionnaire_french.Rmd", tempReport, overwrite = TRUE)
        params <- list(survey_french = survey_data_wave2_french(),
                       orga=reactive_values$organization
                       
        )
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
        # When the downloadHandler function runs, increment rv1$download_flag
        rv1$download_flag <- rv1$download_flag + 1
        
        if(rv1$download_flag > 0){  # trigger event whenever the value of rv1$download_flag changes
          shinyjs::alert("File downloaded!")
        }
      })
      
    }
  )
  # ~~~CUSTOM REPORTS (PDF & EXCEL)~~~ ----
  
  # (4.3.2) Download Buttons ----
  
  # output$downloadData <- renderUI({
  #   req((nrow(selected_Wave1()) >= 5 & length(selected1())  >= 1 ) | (nrow(selected_Wave2()) >= 5 & length(selected1())  >= 1 )) #select at least 5 co-ops and 1 selector
  #   
  #   
  #   div(
  #     class = "container",
  #     
  #     downloadButton("download1", label = translate$t("Peer Group Analysis- pdf"), class = "btn btn-primary btn-md", icon = icon("far fa-file-pdf-o")),
  #     
  #     downloadButton("download2", label = translate$t("Peer Group Analysis- Excel"), class = "btn btn-primary btn-md", icon = icon("far fa-file-excel")),
  #   )
  #   
  # }) 
  # output$downloadData_french <- renderUI({
  #   req((nrow(selected_Wave1_french()) >= 5 & length(selected1_french())  >= 1 ) | (nrow(selected_Wave2_french()) >= 5 & length(selected1_french())  >= 1 )) #select at least 5 co-ops and 1 selector
  #   
  #   
  #   div(
  #     class = "container",
  #     
  #     downloadButton("download1_french", label = translate$t("Peer Group Analysis- pdf"), class = "btn btn-primary btn-md", icon = icon("far fa-file-pdf-o")),
  #     
  #     downloadButton("download2_french", label = translate$t("Peer Group Analysis- Excel"), class = "btn btn-primary btn-md", icon = icon("far fa-file-excel")),
  #   )
  #   
  # })
  
  
  #5.0: PDF ----
  observeEvent(input$init_1, {
    if(nrow(selected_Wave2())>=5 & length(selected1())>=1 & (nrow(selected_Wave1())==0 |  nrow(selected_Wave1())>=5)){
      shinyjs::runjs("document.getElementById('download1').click();")
    }
    else if (nrow(selected_Wave1())>=5 & length(selected1())>=1 & (nrow(selected_Wave2())==0 |  nrow(selected_Wave2())>=5)){
      shinyjs::runjs("document.getElementById('download1').click();")
    }
    else {
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = tags$b(translate$t("*Select a minimum of 5 organizations from each wave and a minimum of 1 comparison selector to proceed!"),
                      style = "color:#FF0000;"),
        type = "error"
      )
    }
    
  })
  rv1 <- reactiveValues(download_flag = 0) # for the shinyAlert     
  output$download1 <- downloadHandler(
    # filename = "report-1.doc",
    # filename = "report.pdf",
    # filename = "report-1.html",
    filename = "report_english.pdf",
    content = function(file) {
      withProgress(message = "Creating Report. Please wait...",{
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport1 <- file.path(tempdir(), "report_english.Rmd")
        src1<-file.path(tempdir(), "USask_CHASR_Logo.png")
        file.copy("USask_CHASR_Logo.png", src1, overwrite = TRUE)
        src2<-file.path(tempdir(), "JSGS_CCSC_Logo_CMYK-highnobg.jpg") #"C:\Users\rizvi\OneDrive - University of Saskatchewan\Dashboard\Dashboard Code\JSGS_CCSC_Logo_CMYK-highnobg.jpg"
        file.copy("JSGS_CCSC_Logo_CMYK-highnobg.jpg", src2, overwrite = TRUE)
        src3<-file.path(tempdir(), "1200px-United_Farmers_of_Alberta_Logo.jpg") #"C:\Users\rizvi\OneDrive - University of Saskatchewan\Dashboard\Dashboard Code\1200px-United_Farmers_of_Alberta_Logo.jpg"
        file.copy("1200px-United_Farmers_of_Alberta_Logo.jpg", src3, overwrite = TRUE)
        src4<-file.path(tempdir(), "CCSC Dashboard - Organizational Finances.png") #"C:\Users\rizvi\OneDrive - University of Saskatchewan\Dashboard\Dashboard Code\1200px-United_Farmers_of_Alberta_Logo.jpg"
        file.copy("CCSC Dashboard - Organizational Finances.png", src4, overwrite = TRUE)
        file.copy("report_english.Rmd", tempReport1, overwrite = TRUE)
        # Set up parameters to pass to Rmd document
        # data.frame()
        params <- list(peergroup_wave1 = peergroups_selected_Wave1(),
                       peergroup_wave2 = peergroups_selected_Wave2(),
                       orgprofile_wave1=org_profile_df1_wave1(),
                       orgprofile_wave2=org_profile_df1_wave2(),
                       wave1=peergroups_selected_Wave1(),
                       wave2=peergroups_selected_Wave2(),
                       ceo_wave1=ceo_df1_wave1(),
                       ceo_wave2=ceo_df1_wave2(),
                       boardcomp_wave1=boardcomp_df1_wave1(),
                       boardcomp_wave2=boardcomp_df1_wave2(),
                       bedi_wave1=bedi_df1_wave1(),
                       bedi_wave2=bedi_df1_wave2(),
                       cob_wave1=cob_df1_wave1(),
                       cob_wave2=cob_df1_wave2(),
                       bp_wave1=bp_df1_wave1(),
                       bp_wave2=bp_df1_wave2(),
                       compen_wave1=compen_df1_wave1(),
                       compen_wave2=compen_df1_wave2(),
                       tldel_wave1=tldel_df1_wave1(),
                       tldel_wave2=tldel_df1_wave2(),
                       del_wave1=del_df1_wave1(),
                       del_wave2=del_df1_wave2()
        )
        
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport1, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
        # When the downloadHandler function runs, increment rv1$download_flag
        rv1$download_flag <- rv1$download_flag + 1
        
        if(rv1$download_flag > 0){  # trigger event whenever the value of rv1$download_flag changes
          shinyjs::alert("File downloaded!")
        }
      })
      
    }
  )     
  
  #### french
  observeEvent(input$init_1_french, {
    if(nrow(selected_Wave2_french())>=5 & length(selected1_french())>=1 & (nrow(selected_Wave1_french())==0 |  nrow(selected_Wave1_french())>=5)){
      shinyjs::runjs("document.getElementById('download1_french').click();")
    }
    else if (nrow(selected_Wave1_french())>=5 & length(selected1_french())>=1 & (nrow(selected_Wave2_french())==0 |  nrow(selected_Wave2_french())>=5)){
      shinyjs::runjs("document.getElementById('download1_french').click();")
    }
    else {
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = tags$b(translate$t("*Select a minimum of 5 organizations from each wave and a minimum of 1 comparison selector to proceed!"),
                      style = "color:#FF0000;"),
        type = "error"
      )
    }
    
  })
  rv1_french <- reactiveValues(download_flag = 0) # for the shinyAlert     
  output$download1_french <- downloadHandler(
    # filename = "report-1.doc",
    # filename = "report.pdf",
    # filename = "report-1.html",
    filename = "report_french.pdf",
    content = function(file) {
      withProgress(message = translate$t("Creating Report. Please wait..."),{
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        # src2 <- normalizePath('JSGS_CCSC_Logo_CMYK-highnobg.jpg')
        # file.copy(src2, 'JSGS_CCSC_Logo_CMYK-highnobg.jpg', overwrite = TRUE)
        tempReport1_french <- file.path(tempdir(), "report_french.Rmd")
        src1_french<-file.path(tempdir(), "USask_CHASR_Logo.png")
        file.copy("USask_CHASR_Logo.png", src1_french, overwrite = TRUE)
        src2_french<-file.path(tempdir(), "JSGS_CCSC_Logo_CMYK-highnobg.jpg") #"C:\Users\rizvi\OneDrive - University of Saskatchewan\Dashboard\Dashboard Code\JSGS_CCSC_Logo_CMYK-highnobg.jpg"
        file.copy("JSGS_CCSC_Logo_CMYK-highnobg.jpg", src2_french, overwrite = TRUE)
        src3_french<-file.path(tempdir(), "1200px-United_Farmers_of_Alberta_Logo.jpg") #"C:\Users\rizvi\OneDrive - University of Saskatchewan\Dashboard\Dashboard Code\1200px-United_Farmers_of_Alberta_Logo.jpg"
        file.copy("1200px-United_Farmers_of_Alberta_Logo.jpg", src3_french, overwrite = TRUE)
        file.copy("report_french.Rmd", tempReport1_french, overwrite = TRUE)
        src4_french<-file.path(tempdir(), "CCSC Dashboard - Organizational Finances.png") #"C:\Users\rizvi\OneDrive - University of Saskatchewan\Dashboard\Dashboard Code\1200px-United_Farmers_of_Alberta_Logo.jpg"
        file.copy("CCSC Dashboard - Organizational Finances.png", src4_french, overwrite = TRUE)
        # Set up parameters to pass to Rmd document
        # data.frame()
        params <- list(peergroup_wave1_french = peergroups_selected_Wave1_french(),
                       peergroup_wave2_french = peergroups_selected_Wave2_french(),
                       orgprofile_wave1_french=org_profile_df1_wave1_french(),
                       orgprofile_wave2_french=org_profile_df1_wave2_french(),
                       wave1_french=peergroups_selected_Wave1_french(),
                       wave2_french=peergroups_selected_Wave2_french(),
                       ceo_wave1_french=ceo_df1_wave1_french(),
                       ceo_wave2_french=ceo_df1_wave2_french(),
                       boardcomp_wave1_french=boardcomp_df1_wave1_french(),
                       boardcomp_wave2_french=boardcomp_df1_wave2_french(),
                       bedi_wave1_french=bedi_df1_wave1_french(),
                       bedi_wave2_french=bedi_df1_wave2_french(),
                       cob_wave1_french=cob_df1_wave1_french(),
                       cob_wave2_french=cob_df1_wave2_french(),
                       bp_wave1_french=bp_df1_wave1_french(),
                       bp_wave2_french=bp_df1_wave2_french(),
                       compen_wave1_french=compen_df1_wave1_french(),
                       compen_wave2_french=compen_df1_wave2_french(),
                       tldel_wave1_french=tldel_df1_wave1_french(),
                       tldel_wave2_french=tldel_df1_wave2_french(),
                       del_wave1_french=del_df1_wave1_french(),
                       del_wave2_french=del_df1_wave2_french()
                       
        )
        
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport1_french, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
        # When the downloadHandler function runs, increment rv1$download_flag
        rv1_french$download_flag <- rv1_french$download_flag + 1
        
        if(rv1_french$download_flag > 0){  # trigger event whenever the value of rv1$download_flag changes
          shinyjs::alert(translate$t("File downloaded!"))
        }
      })
      
    }
  )     
  
  # 6.0: EXCEL---- 
  observeEvent(input$init_2, {
    if(nrow(selected_Wave2())>=5 & length(selected1())>=1 & (nrow(selected_Wave1())==0 |  nrow(selected_Wave1())>=5)){
      shinyjs::runjs("document.getElementById('download2').click();")
    }
    else if (nrow(selected_Wave1())>=5 & length(selected1())>=1 & (nrow(selected_Wave2())==0 |  nrow(selected_Wave2())>=5)){
      shinyjs::runjs("document.getElementById('download2').click();")
    }
    else {
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = tags$b(translate$t("*Select a minimum of 5 organizations from each wave and a minimum of 1 comparison selector to proceed!"),
                      style = "color:#FF0000;"),
        type = "error"
      )
    }
    
  })
  
  rv2 <- reactiveValues(download_flag = 0) # for the shinyAlert 
  
  output$download2 <- downloadHandler(
    
    filename = function() {
      "report.xlsx"
    }, 
    
    content = function(file) {
      
      #Loading message
      withProgress(message = "Creating Report. Please wait...",{      
        
        customstyle <- createStyle(wrapText = TRUE) #wraps text in a cell
        
        my_workbook <- createWorkbook()
        
        # *Home ----
        addWorksheet(wb = my_workbook, sheetName = "Home")
        
        writeData(
          my_workbook,
          sheet = "Home",
          c("Peer Group Analysis Report",
            paste0("Last compiled on ", format(Sys.time(), '%d %B, %Y'))),
          startRow = 14,
          startCol = 1
        ) 
        addStyle(
          my_workbook,
          sheet = "Home",
          rows = 14:15,
          cols = 1,
          style = createStyle(
            fontSize = "24",
            textDecoration = "bold"
          )
        )
        showGridLines(
          my_workbook,
          sheet = "Home",
          showGridLines = FALSE
        )
        
        insertImage(
          my_workbook,
          sheet = "Home",
          file = "JSGS_CCSC_Logo_CMYK-highnobg.jpg", 
          width = 3,
          height = 1.25,
          startRow = 2,
          startCol = 1,
          units = "in",
          dpi = 250
        )
        insertImage(
          my_workbook,
          sheet = "Home",
          file = "JSGS_CCSC_Logo_CMYK-highnobg.jpg", #"1200px-United_Farmers_of_Alberta_Logo.jpg"
          width = 3,
          height = 1.25,
          startRow = 2,
          startCol = 5,
          units = "in",
          dpi = 300
        )  
        insertImage(
          my_workbook,
          sheet = "Home",
          file = "USask_CHASR_Logo.png",
          width = 3,
          height = 1.25,
          startRow = 2,
          startCol = 9,
          units = "in",
          dpi = 300
        )     
        
        #~~~Peer Groups Wave-1~~~ ----   
        if (nrow(peergroups_selected_Wave1())>=1) {
          
          addWorksheet(wb = my_workbook, sheetName = "Peer Groups Wave-1", gridLines = TRUE)
          
          
          writeDataTable(
            my_workbook,
            sheet = "Peer Groups Wave-1",
            peergroups_selected_Wave1(), 
            startRow = 5,
            startCol = 1,
            tableStyle = "TableStylelight9",
            withFilter = openxlsx_getOp("withFilter", FALSE),
            headerStyle = createStyle(
              textDecoration = "Bold",
              halign = "center",
              border = "bottom",
              #fgFill = "#1a5bc4",
              #fontColour = "#ffffff",
              fontSize = 16
            )
          )
          
          mergeCells(my_workbook, "Peer Groups Wave-1", cols = 1:4, rows =4)
          writeData(
            my_workbook,
            sheet = "Peer Groups Wave-1",
            "Table 0: Peer Groups in Analysis Wave-1",
            startRow = 4,
            startCol = 1
          )
          addStyle(
            my_workbook,
            sheet = "Peer Groups Wave-1",
            rows = 4,
            cols = 1:4,
            style = createStyle(
              fontSize = "16",
              textDecoration = "bold",
              fontColour  = "#000000"
            )
          )
          if (nrow(peergroups_selected_Wave1())>0 & nrow(peergroups_selected_Wave1())<5) {
            mergeCells(my_workbook, "Peer Groups Wave-1", cols = 1:4, rows =nrow(peergroups_selected_Wave1())+6)
            writeData(
              my_workbook,
              sheet = "Peer Groups Wave-1",
              "Note: A minimum selection of five organizations is required for analysis",
              startRow = nrow(peergroups_selected_Wave1())+6,
              startCol = 1,
            )
            addStyle(
              my_workbook,
              sheet = "Peer Groups Wave-1",
              rows = nrow(peergroups_selected_Wave1())+6,
              cols = 1,
              style = createStyle(
                fontSize = "20",
                textDecoration = "bold",
                fontColour  = "#CC0000"
              )
            )
          }
          addStyle(my_workbook, sheet = "Peer Groups Wave-1", customstyle, rows = 1:100, cols = 1:100)
          setColWidths(my_workbook, sheet = "Peer Groups Wave-1", cols = 1:2, widths = "auto")
          
        }
        # ~~~ Full Table Wave-1~~~ ----  
        if (nrow(peergroups_selected_Wave1())>=5) {
          addWorksheet(wb = my_workbook, sheetName = "Full Table Wave-1", gridLines = TRUE)
          mergeCells(my_workbook, "Full Table Wave-1", cols = 1:4, rows =4)
          writeData(
            my_workbook,
            sheet = "Full Table Wave-1",
            "Table 1: Peer Group Analysis Report Wave-1",
            startRow = 4,
            startCol = 1
          ) 
          addStyle(
            my_workbook,
            sheet = "Full Table Wave-1",
            rows = 4,
            cols = 1:4,
            style = createStyle(
              fontSize = "16",
              textDecoration = "bold",
              fontColour  = "#000000"
              # numFmt = openxlsx_getOp("numFmt", "TEXT")
            )
          )
          writeDataTable(
            my_workbook,
            sheet = "Full Table Wave-1",
            full_table_wave1(), 
            startRow = 5,
            startCol = 1,
            tableStyle = "TableStylelight9",
            withFilter = openxlsx_getOp("withFilter", FALSE),
            headerStyle = createStyle(
              textDecoration = "Bold",
              halign = "center",
              border = "bottom",
              #fgFill = "#1a5bc4",
              #fontColour = "#ffffff",
              fontSize = 16
            )
          )
          
          setColWidths(my_workbook, sheet = "Full Table Wave-1", cols = 1, widths = 40)
          setColWidths(my_workbook, sheet = "Full Table Wave-1", cols = 3, widths = 40)
          setColWidths(my_workbook, sheet = "Full Table Wave-1", cols = c(2,4), widths = 80) 
        }
        
        #~~~Peer Groups Wave-2~~~ ----   
        if (nrow(peergroups_selected_Wave2())>=1) {
          addWorksheet(wb = my_workbook, sheetName = "Peer Groups Wave-2", gridLines = TRUE)
          
          mergeCells(my_workbook, "Peer Groups Wave-2", cols = 1:4, rows =4)
          writeData(
            my_workbook,
            sheet = "Peer Groups Wave-2",
            "Table 0: Peer Groups in Analysis Wave-2",
            startRow = 4,
            startCol = 1
          )
          addStyle(
            my_workbook,
            sheet = "Peer Groups Wave-2",
            rows = 4,
            cols = 1:4,
            style = createStyle(
              fontSize = "16",
              textDecoration = "bold",
              fontColour  = "#000000"
            )
          )
          writeDataTable(
            my_workbook,
            sheet = "Peer Groups Wave-2",
            peergroups_selected_Wave2(), 
            startRow = 5,
            startCol = 1,
            tableStyle = "TableStylelight11",
            withFilter = openxlsx_getOp("withFilter", FALSE),
            headerStyle = createStyle(
              textDecoration = "Bold",
              halign = "center",
              border = "bottom",
              #fgFill = "#1a5bc4",
              #fontColour = "#ffffff",
              fontSize = 16
            )
          )
          
          if (nrow(peergroups_selected_Wave2())>0 & nrow(peergroups_selected_Wave2())<5) {
            mergeCells(my_workbook, "Peer Groups Wave-2", cols = 1:4, rows =nrow(peergroups_selected_Wave2())+6)
            writeData(
              my_workbook,
              sheet = "Peer Groups Wave-2",
              "Note: A minimum selection of five organizations is required for analysis",
              startRow = nrow(peergroups_selected_Wave2())+6,
              startCol = 1,
            )
            addStyle(
              my_workbook,
              sheet = "Peer Groups Wave-2",
              rows = nrow(peergroups_selected_Wave2())+6,
              cols = 1,
              style = createStyle(
                fontSize = "20",
                textDecoration = "bold",
                fontColour  = "#CC0000"
              )
            )
          }
          addStyle(my_workbook, sheet = "Peer Groups Wave-2", customstyle, rows = 1:100, cols = 1:100)
          setColWidths(my_workbook, sheet = "Peer Groups Wave-2", cols = 1:2, widths = "auto")
          
        }
        # ~~~ Full Table Wave-2~~~ ----  
        if (nrow(peergroups_selected_Wave2())>=5) {
          addWorksheet(wb = my_workbook, sheetName = "Full Table Wave-2", gridLines = TRUE)
          mergeCells(my_workbook, "Full Table Wave-2", cols = 1:4, rows =4)
          writeData(
            my_workbook,
            sheet = "Full Table Wave-2",
            "Table 1: Peer Group Analysis Report Wave-2",
            startRow = 4,
            startCol = 1
          ) 
          addStyle(
            my_workbook,
            sheet = "Full Table Wave-2",
            rows = 4,
            cols = 1:4,
            style = createStyle(
              fontSize = "16",
              textDecoration = "bold",
              fontColour  = "#000000"
            )
          )
          writeDataTable(
            my_workbook,
            sheet = "Full Table Wave-2",
            full_table_wave2(), 
            startRow = 5,
            startCol = 1,
            tableStyle = "TableStylelight11",
            withFilter = openxlsx_getOp("withFilter", FALSE),
            headerStyle = createStyle(
              textDecoration = "Bold",
              halign = "center",
              border = "bottom",
              #fgFill = "#1a5bc4",
              #fontColour = "#ffffff",
              fontSize = 16
            )
          )
          
          setColWidths(my_workbook, sheet = "Full Table Wave-2", cols = 1, widths = 40)
          setColWidths(my_workbook, sheet = "Full Table Wave-2", cols = 3, widths = 40)
          setColWidths(my_workbook, sheet = "Full Table Wave-2", cols = c(2,4), widths = 80)
        }
        # ~~~
        
        saveWorkbook(my_workbook, file)
        
        
        # When the downloadHandler function runs, increment rv2$download_flag
        rv2$download_flag <- rv2$download_flag + 1
        
        if(rv2$download_flag > 0){  # trigger event whenever the value of rv2$download_flag changes
          shinyjs::alert("File downloaded!")
        }
        
      })      
      
    }
  )    
  
  
  # ~~~TABLE CALCULATIONS~~~ ----
  
  
  # Peer Groups Selected 
  peergroups_selected <- reactive({
    # req(length(selected()) >= 5)
    reactiveDf()[selected(),-1 ] # Remove CaseID
  })      
  peergroups_selected_Wave2<-reactive({
    peergroups_selected () %>%
      filter(Wave=="Wave-2")
  })
  peergroups_selected_Wave1<-reactive({
    peergroups_selected () %>%
      filter(Wave=="Wave-1")
  })
  # prints to the console
  # observe({
  #   print(peergroups_selected_1()$Wave)
  # })
  # *User Selections: Selectors ----
  variables_selected <- reactive({
    selectors_tbl[selected1(),1:2 ] # Variable, Category  
    
  })
  summarydata<-function(x,columns="MARKET",col="MARKET1") {
    t<-columns
    t1<-paste0("'",columns[1],"'")
    t2<-paste0(columns[1],"1")
    df <- x %>%
      select(columns[1])
    MARKET_df<-na.omit(df)
    # %>%
    #   filter(!is.na(columns[1]))
    # print((MARKET_df))
    if (nrow(MARKET_df)>0) {
      MARKET_df <- data.frame(table(MARKET_df))
      t2<-paste0(columns,"1")
      MARKET_df <- MARKET_df %>%
        mutate(Percent = paste0(round(100 * MARKET_df[,2]/sum(MARKET_df[,2]), 0), "%"))
      MARKET_df <- MARKET_df%>%
        mutate(t=paste0(MARKET_df[,1],":"," ",MARKET_df[,3]))
      fin<-paste(unique(MARKET_df[,4]), collapse = paste0(" ",";"," "))
      MARKET_df <- MARKET_df %>%
        mutate(percentage = fin) %>%
        filter(row_number()==1)
      
    } else {
      MARKET_df<-data.frame(percentage= 1)
      MARKET_df <- MARKET_df %>%
        mutate(percentage= NA) %>%
        filter(row_number()==1)
    } 
    # print(MARKET_df)
    return (MARKET_df)
    
  }
  summarydatasort<-function(x,columns="MARKET",col="MARKET1") {
    t<-columns
    t1<-paste0("'",columns[1],"'")
    t2<-paste0(columns[1],"1")
    df <- x %>%
      select(columns[1])
    MARKET_df<-na.omit(df)
    # %>%
    #   filter(!is.na(columns[1]))
    # print((MARKET_df))
    if (nrow(MARKET_df)>0) {
      MARKET_df <- data.frame(table(MARKET_df))
      MARKET_df<- MARKET_df[order(MARKET_df$Freq,decreasing=TRUE),]
      t2<-paste0(columns,"1")
      MARKET_df <- MARKET_df %>%
        mutate(Percent = paste0(round(100 * MARKET_df[,2]/sum(MARKET_df[,2]), 0), "%"))
      MARKET_df <- MARKET_df%>%
        mutate(t=paste0(MARKET_df[,1],":"," ",MARKET_df[,3]))
      fin<-paste(unique(MARKET_df[,4]), collapse = paste0(" ",";"," "))
      MARKET_df <- MARKET_df %>%
        mutate(percentage = fin) %>%
        filter(row_number()==1)
      
    } else {
      MARKET_df<-data.frame(percentage= 1)
      MARKET_df <- MARKET_df %>%
        mutate(percentage= NA) %>%
        filter(row_number()==1)
    } 
    # print(MARKET_df)
    return (MARKET_df)
    
  }
  meandata<-function(x,col, doller) {
    col<-enquo(col)
    df <- x %>%
      select(!!col)
    MARKET_df<-na.omit(df)
    if (nrow(MARKET_df)>0) {
      # t<-MARKET_df %>%
      #   summarise(mean=mean(!!col, na.rm=TRUE))
      # print(t)
      # print(mean(as.numeric(MARKET_df[,t1])))
      MARKET_df<-MARKET_df %>%
        summarise(Mean=mean(!!col, na.rm=TRUE),
                  # SD=sd(!!col, na.rm=TRUE),
                  Median=median(!!col, na.rm=TRUE),
                  Minimum=min(!!col, na.rm=TRUE),
                  Maximum=max(!!col, na.rm=TRUE),
        )
      MARKET_df<-MARKET_df%>%
        pivot_longer(cols=c(Mean,Median,Minimum,Maximum),
                     values_to='value')
      MARKET_df<-MARKET_df%>%
        mutate(value=round(value,2))
      # MARKET_df$value <- as.character(MARKET_df$value) 
      if (doller==1) {
        MARKET_df <- MARKET_df%>%
          mutate(t=sprintf("%s : $%0.2f", name, value))
        # mutate(t=paste0(MARKET_df$name,":"," $",MARKET_df$value))
        # MARKET_df$t<-noquote(MARKET_df$t)
      } else{
        MARKET_df <- MARKET_df%>%
          mutate(t=sprintf("%s : %0.2f", name, value))
        # mutate(t=paste0(MARKET_df$name,":"," ",MARKET_df$value))
        # MARKET_df$t<-noquote(MARKET_df$t)
      }
      fin<-paste0(unique(MARKET_df[,3]), collapse = paste0(" ",";"," "))
      # fin1<-paste(unique(MARKET_df[,3]), collapse = paste0(" ",";"," "))
      MARKET_df <- MARKET_df %>%
        mutate(percentage = fin)%>%
        filter(row_number()==1)
      MARKET_df$percentage<-gsub("\"", "", MARKET_df$percentage)
      MARKET_df$percentage<-gsub("c", "", MARKET_df$percentage)
      MARKET_df$percentage<-gsub("\\(", "", MARKET_df$percentage)
      MARKET_df$percentage<-gsub("\\)", "", MARKET_df$percentage)
      MARKET_df$percentage<-gsub("\\,", " ;", MARKET_df$percentage)
    } else {
      MARKET_df<-data.frame(percentage= 1)
      MARKET_df <- MARKET_df %>%
        mutate(percentage= NA) %>%
        filter(row_number()==1)
    }
    # print(MARKET_df)
    return (MARKET_df)
    
  }
  
  # ~~~ORGANIZATIONAL PROFILE~~~ ----   
  org_profile_list_wave1 <- selectors_tbl %>%
    filter(Category == "Organizational Profile") %>% 
    arrange(Order) %>%
    filter(is.na(Wave)==TRUE)
  org_profile_list_wave2 <- selectors_tbl %>%
    filter(Category == "Organizational Profile") %>% arrange(Order) 
  
  own_org_profile1_wave1 <- reactive({
    
    df <- data1 %>% 
      filter(Wave==1) %>%
      
      mutate(
        REVENUE         = scales::dollar(REVENUE),
        ASSETS          = scales::dollar(ASSETS),
        MEMBERS         = formatC(MEMBERS, format="f", big.mark=",", digits = 2),
        FTEMPLOYEES     = formatC(FTEMPLOYEES, format="f", big.mark=",", digits = 2),
        EMPLOYEES       = formatC(EMPLOYEES, format="f", big.mark=",", digits = 2),
        YEARSINBUSINESS = formatC(YEARSINBUSINESS, format="f", digits = 2)
        
      ) %>%  
      
      
      filter(Organization %in% reactive_values$organization) %>% 
      #mutate_all(~ifelse(is.nan(.), NA, .)) %>% replace(is.na(.), "-") %>% 
      select(starts_with(org_profile_list_wave1$Variable)) # (Step 0)
    
    # Transpose df from wide to long 
    
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_org_profile1_wave1<-cbind(df1,V1)
    }else {
      own_org_profile1_wave1 <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_org_profile1_wave1 <-  as_tibble(t(df), rownames = "Variable")  
    
  })    
  
  
  own_org_profile2_wave1 <- reactive({
    own_org_profile1_wave1() %>% inner_join(org_profile_list_wave1, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation") #ATTN:AHMAD MOBIN
  })
  
  
  
  org_profile_df_wave1 <- reactive({
    
    variables_selected() %>% filter(Category == "Organizational Profile") %>% 
      inner_join(own_org_profile2_wave1(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable") #ATTN:AHMAD MOBIN
    
  })
 
  
  peer_org_profile_wave1 <- reactive({  
    df <-   data1 %>% filter(Organization %in% peergroups_selected_Wave1()$Organization) %>%
      filter(Wave==1) %>%
      select(starts_with(org_profile_list_wave1$Variable))
    MARKET_df<-summarydatasort (df,"MARKET")
    PROVINCEC1_df<-summarydata (df,"PROVINCEC1")
    PROVINCEC2_df<-summarydata (df,"PROVINCEC2")
    PROVINCEC3_df<-summarydata (df,"PROVINCEC3")
    PROVINCEC4_df<-summarydata (df,"PROVINCEC4")
    PROVINCEC5_df<-summarydata (df,"PROVINCEC5")
    PROVINCEC6_df<-summarydata (df,"PROVINCEC6")
    PROVINCEC7_df<-summarydata (df,"PROVINCEC7")
    PROVINCEC8_df<-summarydata (df,"PROVINCEC8")
    PROVINCEC9_df<-summarydata (df,"PROVINCEC9")
    PROVINCEC10_df<-summarydata (df,"PROVINCEC10")
    PROVINCEC11_df<-summarydata (df,"PROVINCEC11")
    PROVINCEC12_df<-summarydata (df,"PROVINCEC12")
    PROVINCEC13_df<-summarydata (df,"PROVINCEC13")
    PROVINCEC14_df<-summarydata (df,"PROVINCEC14")
    SECTORSC1_df<-summarydata (df,"SECTORSC1")
    SECTORSC2_df<-summarydata (df,"SECTORSC2")
    SECTORSC3_df<-summarydata (df,"SECTORSC3")
    SECTORSC4_df<-summarydata (df,"SECTORSC4")
    SECTORSC5_df<-summarydata (df,"SECTORSC5")
    SECTORSC6_df<-summarydata (df,"SECTORSC6")
    SECTORSC7_df<-summarydata (df,"SECTORSC7")
    SECTORSC8_df<-summarydata (df,"SECTORSC8")
    SECTORSC9_df<-summarydata (df,"SECTORSC9")
    PRIMARYSECTOR_df<-summarydatasort (df,"PRIMARYSECTOR")
    TYPE_df<-summarydatasort (df,"TYPE")
    TIERTYPE_df<-summarydata (df,"TIERTYPE")
    ASSETS_df<-meandata (df,ASSETS,1)
    REVENUE_df<-meandata (df,REVENUE,1)
    MEMBERS_df<-meandata (df,MEMBERS,0)
    FTEMPLOYEES_df<-meandata (df,FTEMPLOYEES,0)
    EMPLOYEES_df<-meandata (df,EMPLOYEES,0)
    YEARSINBUSINESS_df<-meandata (df,YEARSINBUSINESS,0)
    peer_org_profile_wave1 <-  df %>% #filter(Organization %in% peergroups_selected()$Organization) %>%
      #select(starts_with(org_profile_list$Variable)) %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
                
                
                
                PROVINCEC14O = (paste(na.omit(unique(PROVINCEC14O)), collapse = ";")),
                SECTORSC9O    = (paste(na.omit(unique(SECTORSC9O)), collapse = ";"))
                
      ) %>%
      
      mutate(
        
        # REVENUE     = scales::dollar(REVENUE),
        # ASSETS      = ASSETS_df$percentage,
        # MEMBERS     = formatC(MEMBERS, format="f", big.mark=",", digits = 2),
        # FTEMPLOYEES = formatC(FTEMPLOYEES, format="f", big.mark=",", digits = 2),
        # EMPLOYEES   = formatC(EMPLOYEES, format="f", big.mark=",", digits = 2),
        # YEARSINBUSINESS = formatC(YEARSINBUSINESS, format="f", digits = 2),
        REVENUE     = REVENUE_df$percentage,
        ASSETS      = ASSETS_df$percentage,
        MEMBERS     = MEMBERS_df$percentage,
        FTEMPLOYEES = FTEMPLOYEES_df$percentage,
        EMPLOYEES   = EMPLOYEES_df$percentage,
        YEARSINBUSINESS = YEARSINBUSINESS_df$percentage,
        MARKET     = MARKET_df$percentage
        ,PROVINCEC1  = PROVINCEC1_df$percentage
        ,PROVINCEC2  = PROVINCEC2_df$percentage
        ,PROVINCEC3  = PROVINCEC3_df$percentage
        ,PROVINCEC4  = PROVINCEC4_df$percentage
        ,PROVINCEC5  = PROVINCEC5_df$percentage
        ,PROVINCEC6  = PROVINCEC6_df$percentage
        ,PROVINCEC7  = PROVINCEC7_df$percentage
        ,PROVINCEC8  = PROVINCEC8_df$percentage
        ,PROVINCEC9  = PROVINCEC9_df$percentage
        ,PROVINCEC10 = PROVINCEC10_df$percentage
        ,PROVINCEC11 = PROVINCEC11_df$percentage
        ,PROVINCEC12 = PROVINCEC12_df$percentage
        ,PROVINCEC13 = PROVINCEC13_df$percentage
        ,PROVINCEC14 = PROVINCEC14_df$percentage
        ,SECTORSC1 = SECTORSC1_df$percentage
        ,SECTORSC2 = SECTORSC2_df$percentage
        ,SECTORSC3 = SECTORSC3_df$percentage
        ,SECTORSC4 = SECTORSC4_df$percentage
        ,SECTORSC5 = SECTORSC5_df$percentage
        ,SECTORSC6 = SECTORSC6_df$percentage
        ,SECTORSC7 = SECTORSC7_df$percentage
        ,SECTORSC8 = SECTORSC8_df$percentage
        ,SECTORSC9 = SECTORSC9_df$percentage
        ,PRIMARYSECTOR = PRIMARYSECTOR_df$percentage
        ,TYPE = TYPE_df$percentage
        ,TIERTYPE = TIERTYPE_df$percentage
        
        
      )  %>% mutate_all(~ifelse(is.nan(.), NA, .)
      ) #%>% replace(is.na(.), "NA")
    
  })  
  peer_org_profile1_wave1<-reactive({
    if (nrow(peer_org_profile_wave1())==0) {

        df1<-as_tibble(t(peer_org_profile_wave1()), rownames = "Variable")
        V1<-"text"
        peer_org_profile1_wave1<-cbind(df1,V1)



    }else {
      peer_org_profile1_wave1 <-  as_tibble(t(peer_org_profile_wave1()), rownames = "Variable")
    }
  })

  # peer_org_profile1_wave1 <-  reactive({as_tibble(t(peer_org_profile_wave1()), rownames = "Variable")
  # })
  
  
  peer_org_profile2_wave1 <- reactive({
    peer_org_profile1_wave1() %>% inner_join(org_profile_list_wave1, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })   
  
  
  
  org_profile_df1_wave1 <- reactive({
    
    org_profile_df_wave1() %>% 
      inner_join(peer_org_profile2_wave1(), by = "Variable") %>%
      arrange(Theme) %>% 
      select("Theme","Measure", "Your Organization", "Peer Group") %>% 
      rename("Your Organization" = "Your Organization")
    
  })   
  own_org_profile1_wave2 <- reactive({
    
    df <- data1 %>% 
      filter(Wave==2) %>%
      
      mutate(
        REVENUE         = scales::dollar(REVENUE),
        ASSETS          = scales::dollar(ASSETS),
        MEMBERS         = formatC(MEMBERS, format="f", big.mark=",", digits = 2),
        FTEMPLOYEES     = formatC(FTEMPLOYEES, format="f", big.mark=",", digits = 2),
        EMPLOYEES       = formatC(EMPLOYEES, format="f", big.mark=",", digits = 2),
        YEARSINBUSINESS = formatC(YEARSINBUSINESS, format="f", digits = 2)
        
      ) %>%  
      
      
      filter(Organization %in% reactive_values$organization) %>% 
      #mutate_all(~ifelse(is.nan(.), NA, .)) %>% replace(is.na(.), "-") %>% 
      select(starts_with(org_profile_list_wave2$Variable)) # (Step 0)
    
    # Transpose df from wide to long 
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_org_profile1_wave2<-cbind(df1,V1)
    }else {
      own_org_profile1_wave2 <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_org_profile1_wave2 <-  as_tibble(t(df), rownames = "Variable")  
    
  })    
  
  
  own_org_profile2_wave2 <- reactive({
    own_org_profile1_wave2() %>% inner_join(org_profile_list_wave2, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation") #ATTN:AHMAD MOBIN
  })
  
  
  
  org_profile_df_wave2 <- reactive({
    
    variables_selected() %>% filter(Category == "Organizational Profile") %>% 
      inner_join(own_org_profile2_wave2(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable") #ATTN:AHMAD MOBIN
    
  })
  
  
  peer_org_profile_wave2 <- reactive({  
    df <-   data1 %>% filter(Organization %in% peergroups_selected_Wave2()$Organization) %>%
      filter(Wave==2) %>%
      select(starts_with(org_profile_list_wave2$Variable))
    MARKET_df<-summarydatasort (df,"MARKET")
    PROVINCEC1_df<-summarydata (df,"PROVINCEC1")
    PROVINCEC2_df<-summarydata (df,"PROVINCEC2")
    PROVINCEC3_df<-summarydata (df,"PROVINCEC3")
    PROVINCEC4_df<-summarydata (df,"PROVINCEC4")
    PROVINCEC5_df<-summarydata (df,"PROVINCEC5")
    PROVINCEC6_df<-summarydata (df,"PROVINCEC6")
    PROVINCEC7_df<-summarydata (df,"PROVINCEC7")
    PROVINCEC8_df<-summarydata (df,"PROVINCEC8")
    PROVINCEC9_df<-summarydata (df,"PROVINCEC9")
    PROVINCEC10_df<-summarydata (df,"PROVINCEC10")
    PROVINCEC11_df<-summarydata (df,"PROVINCEC11")
    PROVINCEC12_df<-summarydata (df,"PROVINCEC12")
    PROVINCEC13_df<-summarydata (df,"PROVINCEC13")
    PROVINCEC14_df<-summarydata (df,"PROVINCEC14")
    SECTORSC1_df<-summarydata (df,"SECTORSC1")
    SECTORSC2_df<-summarydata (df,"SECTORSC2")
    SECTORSC3_df<-summarydata (df,"SECTORSC3")
    SECTORSC4_df<-summarydata (df,"SECTORSC4")
    SECTORSC5_df<-summarydata (df,"SECTORSC5")
    SECTORSC6_df<-summarydata (df,"SECTORSC6")
    SECTORSC7_df<-summarydata (df,"SECTORSC7")
    SECTORSC8_df<-summarydata (df,"SECTORSC8")
    SECTORSC9_df<-summarydata (df,"SECTORSC9")
    PRIMARYSECTOR_df<-summarydatasort (df,"PRIMARYSECTOR")
    TYPE_df<-summarydatasort (df,"TYPE")
    TIERTYPE_df<-summarydatasort (df,"TIERTYPE")
    COOPLOCATION_df<-summarydata (df,"COOPLOCATION")
    ASSETS_df<-meandata (df,ASSETS,1)
    REVENUE_df<-meandata (df,REVENUE,1)
    MEMBERS_df<-meandata (df,MEMBERS,0)
    FTEMPLOYEES_df<-meandata (df,FTEMPLOYEES,0)
    EMPLOYEES_df<-meandata (df,EMPLOYEES,0)
    YEARSINBUSINESS_df<-meandata (df,YEARSINBUSINESS,0)
    peer_org_profile_wave2 <-  df %>% #filter(Organization %in% peergroups_selected()$Organization) %>%
      #select(starts_with(org_profile_list$Variable)) %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
                
                
                
                PROVINCEC14O = (paste(na.omit(unique(PROVINCEC14O)), collapse = ";")),
                SECTORSC9O    = (paste(na.omit(unique(SECTORSC9O)), collapse = ";"))
                
      ) %>%
      
      mutate(
        
        REVENUE     = REVENUE_df$percentage,
        ASSETS      = ASSETS_df$percentage,
        MEMBERS     = MEMBERS_df$percentage,
        FTEMPLOYEES = FTEMPLOYEES_df$percentage,
        EMPLOYEES   = EMPLOYEES_df$percentage,
        YEARSINBUSINESS = YEARSINBUSINESS_df$percentage,
        MARKET     = MARKET_df$percentage
        ,PROVINCEC1  = PROVINCEC1_df$percentage
        ,PROVINCEC2  = PROVINCEC2_df$percentage
        ,PROVINCEC3  = PROVINCEC3_df$percentage
        ,PROVINCEC4  = PROVINCEC4_df$percentage
        ,PROVINCEC5  = PROVINCEC5_df$percentage
        ,PROVINCEC6  = PROVINCEC6_df$percentage
        ,PROVINCEC7  = PROVINCEC7_df$percentage
        ,PROVINCEC8  = PROVINCEC8_df$percentage
        ,PROVINCEC9  = PROVINCEC9_df$percentage
        ,PROVINCEC10 = PROVINCEC10_df$percentage
        ,PROVINCEC11 = PROVINCEC11_df$percentage
        ,PROVINCEC12 = PROVINCEC12_df$percentage
        ,PROVINCEC13 = PROVINCEC13_df$percentage
        ,PROVINCEC14 = PROVINCEC14_df$percentage
        ,SECTORSC1 = SECTORSC1_df$percentage
        ,SECTORSC2 = SECTORSC2_df$percentage
        ,SECTORSC3 = SECTORSC3_df$percentage
        ,SECTORSC4 = SECTORSC4_df$percentage
        ,SECTORSC5 = SECTORSC5_df$percentage
        ,SECTORSC6 = SECTORSC6_df$percentage
        ,SECTORSC7 = SECTORSC7_df$percentage
        ,SECTORSC8 = SECTORSC8_df$percentage
        ,SECTORSC9 = SECTORSC9_df$percentage
        ,PRIMARYSECTOR = PRIMARYSECTOR_df$percentage
        ,TYPE = TYPE_df$percentage
        ,TIERTYPE = TIERTYPE_df$percentage
        ,COOPLOCATION = COOPLOCATION_df$percentage
        
        
      )  %>% mutate_all(~ifelse(is.nan(.), NA, .)
      ) #%>% replace(is.na(.), "NA")
    
  })  
  
  peer_org_profile1_wave2<-reactive({
    if (nrow(peer_org_profile_wave2())==0) {
      
      df1<-as_tibble(t(peer_org_profile_wave2()), rownames = "Variable")
      V1<-"text"
      peer_org_profile1_wave2<-cbind(df1,V1)
      
      
      
    }else {
      peer_org_profile1_wave2 <-  as_tibble(t(peer_org_profile_wave2()), rownames = "Variable")
    }
  })
  # peer_org_profile1_wave2 <-  reactive({as_tibble(t(peer_org_profile_wave2()), rownames = "Variable")
  # })
  
  
  peer_org_profile2_wave2 <- reactive({
    peer_org_profile1_wave2() %>% inner_join(org_profile_list_wave2, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })   
  
  
  
  org_profile_df1_wave2 <- reactive({
    
    org_profile_df_wave2() %>% 
      inner_join(peer_org_profile2_wave2(), by = "Variable") %>%
      arrange(Theme) %>% 
      select("Theme","Measure", "Your Organization", "Peer Group") %>% 
      rename("Your Organization" = "Your Organization")
    
  })   
  
  
  
  
  # # ~~~ORGANIZATIONAL PROFILE~~~ ----   
  # org_profile_list <- selectors_tbl %>%
  #   filter(Category == "Organizational Profile") %>% arrange(Order)
  # org_profile_varlist <- reactive({
  #   df<-selectors_tbl[selected1(),1:2 ] # Variable, Category
  #   org_profile_varlist <-df%>%
  #     filter(Category == "Organizational Profile")
  # })
  # summarydata<-function(x,columns="MARKET",col="MARKET1") {
  #   library(dplyr)
  #   t<-columns
  #   # for(j in 1:length(columns)){
  #   t1<-paste0("'",columns[1],"'")
  #   t2<-paste0(columns[1],"1")
  #   MARKET_df <- x %>%
  #     select(columns[1]) %>%
  #     filter(!is.na(columns[1]))
  #   if (sum(is.na(MARKET_df))!=nrow(MARKET_df)) {
  #     MARKET_df <- data.frame(table(MARKET_df))
  #     # }
  #     t2<-paste0(columns,"1")
  #     MARKET_df <- MARKET_df %>%
  #       mutate(Percent = paste0(round(100 * MARKET_df[,2]/sum(MARKET_df[,2]), 0), "%"))
  #     MARKET_df <- MARKET_df%>%
  #       mutate(t=paste0(MARKET_df[,1],":",MARKET_df[,3]))
  #     fin<-paste(unique(MARKET_df[,4]), collapse = ";")
  #     MARKET_df <- MARKET_df %>%
  #       mutate(percentage = fin) %>%
  #       filter(row_number()==1)
  #   } else{
  #     MARKET_df <- MARKET_df %>%
  #       mutate(percentage = NA)
  #       filter(row_number()==1)
  #   }
  #   
  #   return (MARKET_df)
  # }

  # own_org_profile1<-function(x=data1,wave=1,text="data not available",orglist=org_profile_list){
  #   own_org_profile1 <- reactive({
  #     df <- x %>%
  #       filter(Wave==wave)%>%
  # 
  #       mutate(
  #         REVENUE         = scales::dollar(REVENUE),
  #         ASSETS          = scales::dollar(ASSETS),
  #         MEMBERS         = formatC(MEMBERS, format="f", big.mark=",", digits = 2),
  #         FTEMPLOYEES     = formatC(FTEMPLOYEES, format="f", big.mark=",", digits = 2),
  #         EMPLOYEES       = formatC(EMPLOYEES, format="f", big.mark=",", digits = 2),
  #         YEARSINBUSINESS = formatC(YEARSINBUSINESS, format="f", digits = 2)
  # 
  #       ) %>%
  # 
  # 
  #       filter(Organization %in% reactive_values$organization) %>%
  #       #mutate_all(~ifelse(is.nan(.), NA, .)) %>% replace(is.na(.), "-") %>%
  #       select(starts_with(orglist$Variable)) # (Step 0)
  # 
  #     # Transpose df from wide to long
  #     # own_org_profile1 <-  as_tibble(t(df), rownames = "Variable")
  #     if (nrow(df)==0) {
  #       own_org_profile1 <-  as_tibble(t(df), rownames = "Variable")
  #       own_org_profile1$V1<-text
  #     }else {
  #       own_org_profile1 <-  as_tibble(t(df), rownames = "Variable")
  #     }
  #     return(own_org_profile1)
  # 
  #   })
  # 
  # 
  # }
  # # # own_org_profile1<-own_org_profile1(x=x,wave=wave)
  # own_org_profile2<-function(ownorg=own_org_profile1(),orglist=org_profile_list, varlist=org_profile_varlist()){
  #   own_org_profile2 <- reactive({
  #     ownorg %>% inner_join(orglist, by = "Variable") %>% arrange(Order) %>%
  #       select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>%
  #       rename( "Your Organization"  = "V1",
  #               "Theme"              = "Second_level_disaggregation",
  #               "Measure"            =  "Third_level_disaggregation") #ATTN:AHMAD MOBIN
  #   })
  #   org_profile_df <- reactive({
  #     varlist %>% #filter(Category == "Organizational Profile") %>%
  #       inner_join(own_org_profile2(), by = "Variable") %>%
  #       select("Theme","Measure", "Your Organization", "Variable") #ATTN:AHMAD MOBIN
  # 
  #   })
  #   return(org_profile_df)
  # }
  # peer_org_profile<-function(x=data1, tb=peergroups_selected_Wave1(), orglist=org_profile_list){
  #   peer_org_profile <- reactive({
  #     df <-   x %>% filter(Organization %in% tb$Organization) %>%
  #       select(starts_with(orglist$Variable))
  #     MARKET_df<-summarydata (df,"MARKET")
  #     PROVINCEC1_df<-summarydata (df,"PROVINCEC1")
  #     PROVINCEC2_df<-summarydata (df,"PROVINCEC2")
  #     PROVINCEC3_df<-summarydata (df,"PROVINCEC3")
  #     PROVINCEC4_df<-summarydata (df,"PROVINCEC4")
  #     PROVINCEC5_df<-summarydata (df,"PROVINCEC5")
  #     PROVINCEC6_df<-summarydata (df,"PROVINCEC6")
  #     PROVINCEC7_df<-summarydata (df,"PROVINCEC7")
  #     PROVINCEC8_df<-summarydata (df,"PROVINCEC8")
  #     PROVINCEC9_df<-summarydata (df,"PROVINCEC9")
  #     PROVINCEC10_df<-summarydata (df,"PROVINCEC10")
  #     PROVINCEC11_df<-summarydata (df,"PROVINCEC11")
  #     PROVINCEC12_df<-summarydata (df,"PROVINCEC12")
  #     PROVINCEC13_df<-summarydata (df,"PROVINCEC13")
  #     PROVINCEC14_df<-summarydata (df,"PROVINCEC14")
  #     SECTORSC1_df<-summarydata (df,"SECTORSC1")
  #     SECTORSC2_df<-summarydata (df,"SECTORSC2")
  #     SECTORSC3_df<-summarydata (df,"SECTORSC3")
  #     SECTORSC4_df<-summarydata (df,"SECTORSC4")
  #     SECTORSC5_df<-summarydata (df,"SECTORSC5")
  #     SECTORSC6_df<-summarydata (df,"SECTORSC6")
  #     SECTORSC7_df<-summarydata (df,"SECTORSC7")
  #     SECTORSC8_df<-summarydata (df,"SECTORSC8")
  #     SECTORSC9_df<-summarydata (df,"SECTORSC9")
  #     PRIMARYSECTOR_df<-summarydata (df,"PRIMARYSECTOR")
  #     TYPE_df<-summarydata (df,"TYPE")
  #     TIERTYPE_df<-summarydata (df,"TIERTYPE")
  #     peer_org_profile <-  df %>% #filter(Organization %in% peergroups_selected()$Organization) %>%
  #       #select(starts_with(org_profile_list$Variable)) %>%
  #       summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
  #                 PROVINCEC14O = (paste(na.omit(unique(PROVINCEC14O)), collapse = ";")),
  #                 SECTORSC9O    = (paste(na.omit(unique(SECTORSC9O)), collapse = ";"))
  # 
  #       ) %>%
  # 
  #       mutate(
  # 
  #         REVENUE     = scales::dollar(REVENUE),
  #         ASSETS      = scales::dollar(ASSETS),
  #         MEMBERS     = formatC(MEMBERS, format="f", big.mark=",", digits = 2),
  #         FTEMPLOYEES = formatC(FTEMPLOYEES, format="f", big.mark=",", digits = 2),
  #         EMPLOYEES   = formatC(EMPLOYEES, format="f", big.mark=",", digits = 2),
  #         YEARSINBUSINESS = formatC(YEARSINBUSINESS, format="f", digits = 2),
  #         MARKET     = MARKET_df$percentage
  #         ,PROVINCEC1  = PROVINCEC1_df$percentage
  #         ,PROVINCEC2  = PROVINCEC2_df$percentage
  #         ,PROVINCEC3  = PROVINCEC3_df$percentage
  #         ,PROVINCEC4  = PROVINCEC4_df$percentage
  #         ,PROVINCEC5  = PROVINCEC5_df$percentage
  #         ,PROVINCEC6  = PROVINCEC6_df$percentage
  #         ,PROVINCEC7  = PROVINCEC7_df$percentage
  #         ,PROVINCEC8  = PROVINCEC8_df$percentage
  #         ,PROVINCEC9  = PROVINCEC9_df$percentage
  #         ,PROVINCEC10 = PROVINCEC10_df$percentage
  #         ,PROVINCEC11 = PROVINCEC11_df$percentage
  #         ,PROVINCEC12 = PROVINCEC12_df$percentage
  #         ,PROVINCEC13 = PROVINCEC13_df$percentage
  #         ,PROVINCEC14 = PROVINCEC14_df$percentage
  #         ,SECTORSC1 = SECTORSC1_df$percentage
  #         ,SECTORSC2 = SECTORSC2_df$percentage
  #         ,SECTORSC3 = SECTORSC3_df$percentage
  #         ,SECTORSC4 = SECTORSC4_df$percentage
  #         ,SECTORSC5 = SECTORSC5_df$percentage
  #         ,SECTORSC6 = SECTORSC6_df$percentage
  #         ,SECTORSC7 = SECTORSC7_df$percentage
  #         ,SECTORSC8 = SECTORSC8_df$percentage
  #         ,SECTORSC9 = SECTORSC9_df$percentage
  #         ,PRIMARYSECTOR = PRIMARYSECTOR_df$percentage
  #         ,TYPE = TYPE_df$percentage
  #         ,TIERTYPE = TIERTYPE_df$percentage
  # 
  # 
  #       )  %>% mutate_all(~ifelse(is.nan(.), NA, .)
  #       ) #%>% replace(is.na(.), "NA")
  # 
  #   })
  #   peer_org_profile1 <-  reactive({as_tibble(t(peer_org_profile()), rownames = "Variable")
  #   })
  #   peer_org_profile2 <- reactive({
  #     peer_org_profile1() %>% inner_join(orglist, by = "Variable") %>%
  #       select("Variable",
  #              #"Label",
  #              "V1") %>% rename( "Peer Group" = "V1"
  #                                #,"Measure"             = "Label"
  #              )
  #   })
  #   return(peer_org_profile2)
  # }
  # org_profile_df1<-function(data1=org_profile_df(), data2=peer_org_profile2()){
  #   org_profile_df1 <- reactive({
  # 
  #     data1 %>%
  #       inner_join(data2, by = "Variable") %>%
  #       arrange(Theme) %>%
  #       select("Theme","Measure", "Your Organization", "Peer Group") %>%
  #       rename("Your Organization" = "Your Organization")
  # 
  #   })
  #   return(org_profile_df1)
  # 
  # }
  # # own_org_profile1_wave1<-own_org_profile1(data1,1,text="data not available",orglist=org_profile_list)
  # # org_profile_df_wave1<-own_org_profile2(ownorg = own_org_profile1_wave1(),orglist=org_profile_list,varlist=org_profile_varlist())
  # # peer_org_profile2_wave1<-peer_org_profile(x=data1,tb=peergroups_selected_Wave1(),orglist=org_profile_list)
  # # org_profile_df1_wave1<-org_profile_df1(data1=org_profile_df_wave1(), data2=peer_org_profile2_wave1())
  # own_org_profile1_wave2<-own_org_profile1(data1,2,text="data not available",orglist=org_profile_list)
  # org_profile_df_wave2<-own_org_profile2(ownorg = own_org_profile1_wave2(),orglist=org_profile_list,varlist=org_profile_varlist())
  # peer_org_profile2_wave2<-peer_org_profile(x=data1,tb=peergroups_selected_Wave2(),orglist=org_profile_list)
  # org_profile_df1_wave2<-org_profile_df1(data1=org_profile_df_wave2(), data2=peer_org_profile2_wave2())
  # ~~~CHEIF EXECUTIVE OFFICER~~~ ----
  ceo_list_wave1 <- selectors_tbl %>%
    filter(Category == "Chief Executive Officer") %>% 
    arrange(Order)  %>% 
    filter(is.na(Wave)==TRUE)
  ceo_list_wave2 <- selectors_tbl %>%
    filter(Category == "Chief Executive Officer") %>% arrange(Order)
  own_ceo1_wave1 <- reactive({
    df <- data1 %>% 
      filter(Wave==1) %>%
      mutate(
        CEONUMBER         = formatC(CEONUMBER, format="f", big.mark=",", digits = 2),
        HIREDOUTSIDE     = formatC(HIREDOUTSIDE, format="f", big.mark=",", digits = 2),
        PERFORMANCEBASED       = formatC(PERFORMANCEBASED, format="f", big.mark=",", digits = 2)
        
      ) %>%  
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(ceo_list_wave1$Variable))
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_ceo1_wave1<-cbind(df1,V1)
    }else {
      own_ceo1_wave1 <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_ceo1_wave1 <-  as_tibble(t(df), rownames = "Variable")
    
  }) 
  own_ceo2_wave1 <- reactive({
    own_ceo1_wave1() %>% inner_join(ceo_list_wave1, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation") #ATTN:AHMAD MOBIN
  })  
  ceo_df_wave1 <- reactive({
    variables_selected() %>% filter(Category == "Chief Executive Officer") %>% 
      inner_join(own_ceo2_wave1(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
  })  
  
  peer_ceo_wave1 <- reactive({
    
    df <-   data1 %>% filter(Organization %in% peergroups_selected_Wave1()$Organization) %>%
      filter(Wave==1) %>%
      select(starts_with(ceo_list_wave1$Variable))
    SUCCESSION_df<-summarydata (df,"SUCCESSION")
    WRITTENCONTRACT_df<-summarydata (df,"WRITTENCONTRACT")
    QUANTMEASURES_df<-summarydata (df,"QUANTMEASURES")
    COMPBENCHMARKS_df<-summarydata (df,"COMPBENCHMARKS")
    CEONUMBER_df<-meandata (df,CEONUMBER,0)
    HIREDOUTSIDE_df<-meandata (df,HIREDOUTSIDE,0)
    PERFORMANCEBASED_df<-meandata (df,PERFORMANCEBASED,0)
    peer_ceo_wave1 <-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
      ) %>%
     mutate(
        # CEONUMBER         = formatC(CEONUMBER, format="f", big.mark=",", digits = 2),
        # HIREDOUTSIDE     = formatC(HIREDOUTSIDE, format="f", big.mark=",", digits = 2),
        # PERFORMANCEBASED       = formatC(PERFORMANCEBASED, format="f", big.mark=",", digits = 2),
        CEONUMBER = CEONUMBER_df$percentage,
        HIREDOUTSIDE = HIREDOUTSIDE_df$percentage,
        PERFORMANCEBASED = PERFORMANCEBASED_df$percentage,
        SUCCESSION        = SUCCESSION_df$percentage
        ,WRITTENCONTRACT   = WRITTENCONTRACT_df$percentage
        ,QUANTMEASURES     = QUANTMEASURES_df$percentage
        ,COMPBENCHMARKS    = COMPBENCHMARKS_df$percentage
        #,TIERTYPE = TIERTYPE_df$TIERTYPEa
        
      ) %>% mutate_all(~ifelse(is.nan(.), NA, .)) #%>% replace(is.na(.), "NA") 
    
    
    
  })
  peer_ceo1_wave1<-reactive({
    if (nrow(peer_ceo_wave1())==0) {
      
      df1<-as_tibble(t(peer_ceo_wave1()), rownames = "Variable")
      V1<-"text"
      peer_ceo1_wave1<-cbind(df1,V1)
      
      
      
    }else {
      peer_ceo1_wave1 <-  as_tibble(t(peer_ceo_wave1()), rownames = "Variable")
    }
  })
  # peer_ceo1_wave1 <-  reactive({as_tibble(t(peer_ceo_wave1()), rownames = "Variable")
  # }) 
  # 
 peer_ceo2_wave1 <- reactive({
    peer_ceo1_wave1() %>% inner_join(ceo_list_wave1, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })  
  
  
  
  ceo_df1_wave1 <- reactive({
    
    ceo_df_wave1() %>% 
      left_join(peer_ceo2_wave1(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  }) 
  
  own_ceo1_wave2 <- reactive({
    
    df <- data1 %>% 
      filter(Wave==2) %>%
      mutate(
        CEONUMBER         = formatC(CEONUMBER, format="f", big.mark=",", digits = 2),
        HIREDOUTSIDE     = formatC(HIREDOUTSIDE, format="f", big.mark=",", digits = 2),
        PERFORMANCEBASED       = formatC(PERFORMANCEBASED, format="f", big.mark=",", digits = 2)
        
      ) %>%  
      
      
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(ceo_list_wave2$Variable))
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_ceo1_wave2<-cbind(df1,V1)
    }else {
      own_ceo1_wave2 <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_ceo1_wave2 <-  as_tibble(t(df), rownames = "Variable")
    
  }) 
  own_ceo2_wave2 <- reactive({
    own_ceo1_wave2() %>% inner_join(ceo_list_wave2, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation") #ATTN:AHMAD MOBIN
  })  
  
  
  
  ceo_df_wave2 <- reactive({
    
    variables_selected() %>% filter(Category == "Chief Executive Officer") %>% 
      inner_join(own_ceo2_wave2(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
  })  
  
  
  
  
  peer_ceo_wave2 <- reactive({
    
    df <-   data1 %>% filter(Organization %in% peergroups_selected_Wave2()$Organization) %>%
      filter(Wave==2) %>%
      select(starts_with(ceo_list_wave2$Variable))
    SUCCESSION_df<-summarydata (df,"SUCCESSION")
    WRITTENCONTRACT_df<-summarydata (df,"WRITTENCONTRACT")
    QUANTMEASURES_df<-summarydata (df,"QUANTMEASURES")
    COMPBENCHMARKS_df<-summarydata (df,"COMPBENCHMARKS")
    CEONUMBER_df<-meandata (df,CEONUMBER,0)
    HIREDOUTSIDE_df<-meandata (df,HIREDOUTSIDE,0)
    PERFORMANCEBASED_df<-meandata (df,PERFORMANCEBASED,0)
    peer_ceo_wave2_french <-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
      ) %>%
      
      mutate(
        CEONUMBER = CEONUMBER_df$percentage,
        HIREDOUTSIDE = HIREDOUTSIDE_df$percentage,
        PERFORMANCEBASED = PERFORMANCEBASED_df$percentage,
        
        
        SUCCESSION        = SUCCESSION_df$percentage
        ,WRITTENCONTRACT   = WRITTENCONTRACT_df$percentage
        ,QUANTMEASURES     = QUANTMEASURES_df$percentage
        ,COMPBENCHMARKS    = COMPBENCHMARKS_df$percentage
        #,TIERTYPE = TIERTYPE_df$TIERTYPEa
        
      ) %>% mutate_all(~ifelse(is.nan(.), NA, .)) #%>% replace(is.na(.), "NA") 
    
    
    
  })
  peer_ceo1_wave2<-reactive({
    if (nrow(peer_ceo_wave2())==0) {
      
      df1<-as_tibble(t(peer_ceo_wave2()), rownames = "Variable")
      V1<-"text"
      peer_ceo1_wave2<-cbind(df1,V1)
    }else {
      peer_ceo1_wave2 <-  as_tibble(t(peer_ceo_wave2()), rownames = "Variable")
    }
  })
  # peer_ceo1_wave2 <-  reactive({as_tibble(t(peer_ceo_wave2()), rownames = "Variable")
  # }) 
  peer_ceo2_wave2 <- reactive({
    peer_ceo1_wave2() %>% inner_join(ceo_list_wave2, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })  
  
  
  
  ceo_df1_wave2 <- reactive({
    
    ceo_df_wave2() %>% 
      left_join(peer_ceo2_wave2(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  }) 
  
  # ~~~ Board Composition ~~~ ----
  boardcomp_list_wave1 <- selectors_tbl %>%
    filter(Category == "Board Composition") %>% 
    arrange(Order) %>% 
    filter(is.na(Wave)==TRUE)
  boardcomp_list_wave2 <- selectors_tbl %>%
    filter(Category == "Board Composition") %>% arrange(Order)
  
  
  
  own_boardcomp1_wave1 <- reactive({
    
    df <- data1 %>%
      filter(Wave==1) %>%
      
      mutate(
        NUMBEROFBM         = formatC(NUMBEROFBM, format="f", big.mark=",", digits = 2),
        MAXMINSPECIFY_A1     = formatC(MAXMINSPECIFY_A1, format="f", big.mark=",", digits = 2),
        MAXMINSPECIFY_A2       = formatC(MAXMINSPECIFY_A2, format="f", big.mark=",", digits = 2),
        DIRECTOREMPLOYEES       = formatC(DIRECTOREMPLOYEES, format="f", big.mark=",", digits = 2)
        ,HOWMANYNONMEMBERS         = formatC(HOWMANYNONMEMBERS, format="f", big.mark=",", digits = 2)
        ,AGEOFBM  =  formatC(AGEOFBM, format="f", big.mark=",", digits = 2 )
        ,HOWLONGBMSERVE =  formatC(HOWLONGBMSERVE, format="f", big.mark=",", digits = 2 )
        ,DIRECTOREXECUTIVE         = formatC(DIRECTOREXECUTIVE, format="f", big.mark=",", digits = 2)
        ,DIRECTORINDUSTRY         = formatC(DIRECTORINDUSTRY, format="f", big.mark=",", digits = 2)
        ,DIRECTORMULTIPLE         = formatC(DIRECTORMULTIPLE, format="f", big.mark=",", digits = 2)
        
      )  %>% 
      
      
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(boardcomp_list_wave1$Variable))
    
    #Transpose df 
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_boardcomp1_wave1<-cbind(df1,V1)
    }else {
      own_boardcomp1_wave1 <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_boardcomp1_wave1 <-  as_tibble(t(df), rownames = "Variable")
    
  })   
  
  
  
  own_boardcomp2_wave1 <- reactive({
    own_boardcomp1_wave1() %>% inner_join(boardcomp_list_wave1, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation")
  })   
  
  
  
  
  boardcomp_df_wave1 <- reactive({
    
    variables_selected() %>% filter(Category %in% "Board Composition") %>% 
      inner_join(own_boardcomp2_wave1(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
  })    
  
  
  
  peer_boardcomp_wave1 <- reactive({
    
    df <-   data1 %>% filter(Organization %in% peergroups_selected_Wave1()$Organization) %>%
      filter(Wave==1) %>%
      select(starts_with(boardcomp_list_wave1$Variable))  
    
    SETMINMAXDIRECTORS_df<-summarydata (df,"SETMINMAXDIRECTORS")
    REGULATOR_df<-summarydata (df,"REGULATOR")
    NONMEMBERDIRECTORS_df<-summarydata (df,"NONMEMBERDIRECTORS")
    NONMEMDIRECTOR_df<-summarydata (df,"NONMEMDIRECTOR")
    
    NUMBEROFBM_df<-meandata (df,NUMBEROFBM,0)
    MAXMINSPECIFY_A1_df<-meandata (df,MAXMINSPECIFY_A1,0)
    MAXMINSPECIFY_A2_df<-meandata (df,MAXMINSPECIFY_A2,0)
    DIRECTOREMPLOYEES_df<-meandata (df,DIRECTOREMPLOYEES,0)
    HOWMANYNONMEMBERS_df<-meandata (df,HOWMANYNONMEMBERS,0)
    AGEOFBM_df<-meandata (df,AGEOFBM,0)
    HOWLONGBMSERVE_df<-meandata (df,HOWLONGBMSERVE,0)
    DIRECTOREXECUTIVE_df<-meandata (df,DIRECTOREXECUTIVE,0)
    DIRECTORINDUSTRY_df<-meandata (df,DIRECTORINDUSTRY,0)
    DIRECTORMULTIPLE_df<-meandata (df,DIRECTORMULTIPLE,0)
    
    peer_boardcomp_wave1<-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
                
      ) %>%   
      
      mutate(
        NUMBEROFBM        = NUMBEROFBM_df$percentage,
        MAXMINSPECIFY_A1        = MAXMINSPECIFY_A1_df$percentage,
        MAXMINSPECIFY_A2        = MAXMINSPECIFY_A2_df$percentage,
        DIRECTOREMPLOYEES        = DIRECTOREMPLOYEES_df$percentage,
        HOWMANYNONMEMBERS        = HOWMANYNONMEMBERS_df$percentage,
        # NUMBEROFBM         = formatC(NUMBEROFBM, format="f", big.mark=",", digits = 2),
        # MAXMINSPECIFY_A1     = formatC(MAXMINSPECIFY_A1, format="f", big.mark=",", digits = 2),
        # MAXMINSPECIFY_A2       = formatC(MAXMINSPECIFY_A2, format="f", big.mark=",", digits = 2),
        # DIRECTOREMPLOYEES       = formatC(DIRECTOREMPLOYEES, format="f", big.mark=",", digits = 2),
        # HOWMANYNONMEMBERS         = formatC(HOWMANYNONMEMBERS, format="f", big.mark=",", digits = 2),
        SETMINMAXDIRECTORS        = ifelse(length(SETMINMAXDIRECTORS_df) == 0, "No Response", SETMINMAXDIRECTORS_df$percentage)
        
        ,REGULATOR   = ifelse(length(REGULATOR_df) == 0, "No Response", REGULATOR_df$percentage) 
        ,NONMEMBERDIRECTORS   = ifelse(length(NONMEMBERDIRECTORS_df) == 0, "No Response", NONMEMBERDIRECTORS_df$percentage)
        ,NONMEMDIRECTOR   = ifelse(length(NONMEMDIRECTOR_df) == 0, "No Response", NONMEMDIRECTOR_df$percentage)
        ,AGEOFBM        = AGEOFBM_df$percentage
        ,HOWLONGBMSERVE   = HOWLONGBMSERVE_df$percentage
        ,DIRECTOREXECUTIVE   = DIRECTOREXECUTIVE_df$percentage
        ,DIRECTORINDUSTRY   = DIRECTORINDUSTRY_df$percentage
        ,DIRECTORMULTIPLE   = DIRECTORMULTIPLE_df$percentage
        # ,AGEOFBM  =  formatC(AGEOFBM, format="f", big.mark=",", digits = 2 )
        # ,HOWLONGBMSERVE =  formatC(HOWLONGBMSERVE, format="f", big.mark=",", digits = 2 )
        # ,DIRECTOREXECUTIVE         = formatC(DIRECTOREXECUTIVE, format="f", big.mark=",", digits = 2)
        # ,DIRECTORINDUSTRY         = formatC(DIRECTORINDUSTRY, format="f", big.mark=",", digits = 2)
        # ,DIRECTORMULTIPLE         = formatC(DIRECTORMULTIPLE, format="f", big.mark=",", digits = 2)
        
        
        
      ) %>% mutate_all(~ifelse(is.nan(.), NA, .))  #%>% replace(is.na(.), "NA") 
    
    
    
  })
  
  peer_boardcomp1_wave1<-reactive({
    if (nrow(peer_boardcomp_wave1())==0) {
      
      df1<-as_tibble(t(peer_boardcomp_wave1()), rownames = "Variable")
      V1<-"text"
      peer_boardcomp1_wave1<-cbind(df1,V1)
    }else {
      peer_boardcomp1_wave1 <-  as_tibble(t(peer_boardcomp_wave1()), rownames = "Variable")
    }
  })
  
  
  # peer_boardcomp1_wave1 <-  reactive({as_tibble(t(peer_boardcomp_wave1()), rownames = "Variable")
  # }) 
  
  
  
  peer_boardcomp2_wave1 <- reactive({
    peer_boardcomp1_wave1() %>% inner_join(boardcomp_list_wave1, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })  
  
  
  
  
  boardcomp_df1_wave1 <- reactive({
    
    boardcomp_df_wave1() %>% 
      left_join(peer_boardcomp2_wave1(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  }) 
  
  
  
  own_boardcomp1_wave2 <- reactive({
    
    df <- data1 %>%
      filter(Wave==2) %>%
      
      mutate(
        NUMBEROFBM         = formatC(NUMBEROFBM, format="f", big.mark=",", digits = 2),
        MAXMINSPECIFY_A1     = formatC(MAXMINSPECIFY_A1, format="f", big.mark=",", digits = 2),
        MAXMINSPECIFY_A2       = formatC(MAXMINSPECIFY_A2, format="f", big.mark=",", digits = 2),
        DIRECTOREMPLOYEES       = formatC(DIRECTOREMPLOYEES, format="f", big.mark=",", digits = 2)
        ,HOWMANYNONMEMBERS         = formatC(HOWMANYNONMEMBERS, format="f", big.mark=",", digits = 2)
        ,AGEOFBM  =  formatC(AGEOFBM, format="f", big.mark=",", digits = 2 )
        ,HOWLONGBMSERVE =  formatC(HOWLONGBMSERVE, format="f", big.mark=",", digits = 2 )
        ,DIRECTOREXECUTIVE         = formatC(DIRECTOREXECUTIVE, format="f", big.mark=",", digits = 2)
        ,DIRECTORINDUSTRY         = formatC(DIRECTORINDUSTRY, format="f", big.mark=",", digits = 2)
        ,DIRECTORMULTIPLE         = formatC(DIRECTORMULTIPLE, format="f", big.mark=",", digits = 2)
        
      )  %>% 
      
      
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(boardcomp_list_wave2$Variable))
    
    #Transpose df 
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_boardcomp1_wave2<-cbind(df1,V1)
    }else {
      own_boardcomp1_wave2 <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_boardcomp1_wave2 <-  as_tibble(t(df), rownames = "Variable")
    
  })   
  
  
  
  own_boardcomp2_wave2 <- reactive({
    own_boardcomp1_wave2() %>% inner_join(boardcomp_list_wave2, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation")
  })   
  
  
  
  
  boardcomp_df_wave2 <- reactive({
    
    variables_selected() %>% filter(Category %in% "Board Composition") %>% 
      inner_join(own_boardcomp2_wave2(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
  })    
  
  
  
  peer_boardcomp_wave2 <- reactive({
    
    df <-   data1 %>% filter(Organization %in% peergroups_selected_Wave2()$Organization) %>%
      filter(Wave==2) %>%
      select(starts_with(boardcomp_list_wave2$Variable))  
    
    SETMINMAXDIRECTORS_df<-summarydata (df,"SETMINMAXDIRECTORS")
    REGULATOR_df<-summarydata (df,"REGULATOR")
    NONMEMBERDIRECTORS_df<-summarydata (df,"NONMEMBERDIRECTORS")
    NONMEMDIRECTOR_df<-summarydata (df,"NONMEMDIRECTOR")
    
    NUMBEROFBM_df<-meandata (df,NUMBEROFBM,0)
    MAXMINSPECIFY_A1_df<-meandata (df,MAXMINSPECIFY_A1,0)
    MAXMINSPECIFY_A2_df<-meandata (df,MAXMINSPECIFY_A2,0)
    DIRECTOREMPLOYEES_df<-meandata (df,DIRECTOREMPLOYEES,0)
    HOWMANYNONMEMBERS_df<-meandata (df,HOWMANYNONMEMBERS,0)
    AGEOFBM_df<-meandata (df,AGEOFBM,0)
    HOWLONGBMSERVE_df<-meandata (df,HOWLONGBMSERVE,0)
    DIRECTOREXECUTIVE_df<-meandata (df,DIRECTOREXECUTIVE,0)
    DIRECTORINDUSTRY_df<-meandata (df,DIRECTORINDUSTRY,0)
    DIRECTORMULTIPLE_df<-meandata (df,DIRECTORMULTIPLE,0)
    
    peer_boardcomp_wave2<-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
                
      ) %>%   
      
      mutate(
        NUMBEROFBM        = NUMBEROFBM_df$percentage,
        MAXMINSPECIFY_A1        = MAXMINSPECIFY_A1_df$percentage,
        MAXMINSPECIFY_A2        = MAXMINSPECIFY_A2_df$percentage,
        DIRECTOREMPLOYEES        = DIRECTOREMPLOYEES_df$percentage,
        HOWMANYNONMEMBERS        = HOWMANYNONMEMBERS_df$percentage,
        # 
        # NUMBEROFBM         = formatC(NUMBEROFBM, format="f", big.mark=",", digits = 2),
        # MAXMINSPECIFY_A1     = formatC(MAXMINSPECIFY_A1, format="f", big.mark=",", digits = 2),
        # MAXMINSPECIFY_A2       = formatC(MAXMINSPECIFY_A2, format="f", big.mark=",", digits = 2),
        # DIRECTOREMPLOYEES       = formatC(DIRECTOREMPLOYEES, format="f", big.mark=",", digits = 2),
        # HOWMANYNONMEMBERS         = formatC(HOWMANYNONMEMBERS, format="f", big.mark=",", digits = 2),
        SETMINMAXDIRECTORS        = ifelse(length(SETMINMAXDIRECTORS_df) == 0, "No Response", SETMINMAXDIRECTORS_df$percentage)
        
        ,REGULATOR   = ifelse(length(REGULATOR_df) == 0, "No Response", REGULATOR_df$percentage) 
        ,NONMEMBERDIRECTORS   = ifelse(length(NONMEMBERDIRECTORS_df) == 0, "No Response", NONMEMBERDIRECTORS_df$percentage)
        ,NONMEMDIRECTOR   = ifelse(length(NONMEMDIRECTOR_df) == 0, "No Response", NONMEMDIRECTOR_df$percentage)
        ,AGEOFBM        = AGEOFBM_df$percentage
        ,HOWLONGBMSERVE   = HOWLONGBMSERVE_df$percentage
        ,DIRECTOREXECUTIVE   = DIRECTOREXECUTIVE_df$percentage
        ,DIRECTORINDUSTRY   = DIRECTORINDUSTRY_df$percentage
        ,DIRECTORMULTIPLE   = DIRECTORMULTIPLE_df$percentage
        
        # ,AGEOFBM  =  formatC(AGEOFBM, format="f", big.mark=",", digits = 2 )
        # ,HOWLONGBMSERVE =  formatC(HOWLONGBMSERVE, format="f", big.mark=",", digits = 2 )
        # 
        # ,DIRECTOREXECUTIVE         = formatC(DIRECTOREXECUTIVE, format="f", big.mark=",", digits = 2)
        # ,DIRECTORINDUSTRY         = formatC(DIRECTORINDUSTRY, format="f", big.mark=",", digits = 2)
        # ,DIRECTORMULTIPLE         = formatC(DIRECTORMULTIPLE, format="f", big.mark=",", digits = 2)
        
        
        
      ) %>% mutate_all(~ifelse(is.nan(.), NA, .))  #%>% replace(is.na(.), "NA") 
    
    
    
  })
  
  peer_boardcomp1_wave2<-reactive({
    if (nrow(peer_boardcomp_wave2())==0) {
      
      df1<-as_tibble(t(peer_boardcomp_wave2()), rownames = "Variable")
      V1<-"text"
      peer_boardcomp1_wave2<-cbind(df1,V1)
    }else {
      peer_boardcomp1_wave2 <-  as_tibble(t(peer_boardcomp_wave2()), rownames = "Variable")
    }
  })
  
  
  # peer_boardcomp1_wave2 <-  reactive({as_tibble(t(peer_boardcomp_wave2()), rownames = "Variable")
  # }) 
  
  
  
  peer_boardcomp2_wave2 <- reactive({
    peer_boardcomp1_wave2() %>% inner_join(boardcomp_list_wave2, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })  
  
  
  
  
  boardcomp_df1_wave2 <- reactive({
    
    boardcomp_df_wave2() %>% 
      left_join(peer_boardcomp2_wave2(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  }) 
  
# ~~~Board Equity, Diversity, and Inclusion~~~  ----
  bedi_list_wave1 <- selectors_tbl %>%
    filter(str_detect(Category,"Board Equity, Diversity, and Inclusion")) %>% 
    arrange(Order) %>%
    filter(is.na(Wave)==TRUE)
  bedi_list_wave2 <- selectors_tbl %>%
    filter(str_detect(Category,"Board Equity, Diversity, and Inclusion")) %>% arrange(Order)
  
  own_bedi1_wave1 <- reactive({
    
    df <- data1 %>% 
      filter(Wave==1) %>%
      
      mutate(
        DIRECTORFEMALE         = formatC(DIRECTORFEMALE, format="f", big.mark=",", digits = 2),
        DIRECTORMINORITY         = formatC(DIRECTORMINORITY, format="f", big.mark=",", digits = 2),
        DIRECTORINDIGENOUS         = formatC(DIRECTORINDIGENOUS, format="f", big.mark=",", digits = 2)
      ) %>%  replace(is.na(.), "-") %>%  
      
      
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(bedi_list_wave1$Variable))
    
    #Transpose df 
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_bedi1_wave1<-cbind(df1,V1)
    }else {
      own_bedi1_wave1 <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_bedi1_wave1 <-  as_tibble(t(df), rownames = "Variable")
    
  })   
  
  
  
  own_bedi2_wave1 <- reactive({
    own_bedi1_wave1() %>% inner_join(bedi_list_wave1, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation")
  })  
  
  
  
  bedi_df_wave1 <- reactive({
    
    variables_selected() %>% filter(str_detect(Category,"Board Equity, Diversity, and Inclusion")) %>% 
      inner_join(own_bedi2_wave1(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
  }) 
  
  
  
  
  peer_bedi_wave1 <- reactive({
    
    df <-   data1 %>%
      filter(Organization %in% peergroups_selected_Wave1()$Organization) %>%
      filter(Wave==1) %>%
      select(starts_with(bedi_list_wave1$Variable))
    DIRECTORFEMALE_df<-meandata (df,DIRECTORFEMALE,0)
    DIRECTORMINORITY_df<-meandata (df,DIRECTORMINORITY,0)
    DIRECTORINDIGENOUS_df<-meandata (df,DIRECTORINDIGENOUS,0)
    DIVERSITYTARGET_df<-summarydata (df,"DIVERSITYTARGET")
    
    # DIVERSITYTARGET_df <- df %>% group_by(DIVERSITYTARGET) %>% arrange(DIVERSITYTARGET) %>% 
    #   filter(!is.na(DIVERSITYTARGET)) %>% 
    #   summarise(n=n()) %>% mutate(Percent = paste0(round(100 * n/sum(n), 0), "%")) %>% 
    #   unite("DIVERSITYTARGET", c(DIVERSITYTARGET,Percent), sep = ":") %>% 
    #   mutate(DIVERSITYTARGETa    = (paste(unique(DIVERSITYTARGET), collapse = ";")))%>%
    #   filter(row_number()==1) 
    
    
    
    
    peer_bedi_wave1<-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
                
      ) %>%
      
      mutate(
        DIRECTORFEMALE         = DIRECTORFEMALE_df$percentage,
        DIRECTORMINORITY       = DIRECTORMINORITY_df$percentage,
        DIRECTORINDIGENOUS     = DIRECTORINDIGENOUS_df$percentage,
        # DIRECTORFEMALE         = formatC(DIRECTORFEMALE, format="f", big.mark=",", digits = 2),
        # DIRECTORMINORITY         = formatC(DIRECTORMINORITY, format="f", big.mark=",", digits = 2),
        # DIRECTORINDIGENOUS         = formatC(DIRECTORINDIGENOUS, format="f", big.mark=",", digits = 2),
        DIVERSITYTARGET        = ifelse(length(DIVERSITYTARGET_df) == 0, "No Response", DIVERSITYTARGET_df$percentage)
        
      )  %>% mutate_all(~ifelse(is.nan(.), NA, .)) # %>% replace(is.na(.), "NA") 
    
  })
  
  peer_bedi1_wave1<-reactive({
    if (nrow(peer_bedi_wave1())==0) {
      
      df1<-as_tibble(t(peer_bedi_wave1()), rownames = "Variable")
      V1<-"text"
      peer_bedi1_wave1<-cbind(df1,V1)
    }else {
      peer_bedi1_wave1 <-  as_tibble(t(peer_bedi_wave1()), rownames = "Variable")
    }
  })
  
  # peer_bedi1_wave1 <-  reactive({as_tibble(t(peer_bedi_wave1()), rownames = "Variable")
  # }) 
  
  
  peer_bedi2_wave1 <- reactive({
    peer_bedi1_wave1() %>% inner_join(bedi_list_wave1, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })   
  
  
  bedi_df1_wave1 <- reactive({
    
    bedi_df_wave1() %>% 
      left_join(peer_bedi2_wave1(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  })
  
  own_bedi1_wave2 <- reactive({
    
    df <- data1 %>% 
      filter(Wave==2) %>%
      
      mutate(
        DIRECTORFEMALE         = formatC(DIRECTORFEMALE, format="f", big.mark=",", digits = 2),
        DIRECTORMINORITY         = formatC(DIRECTORMINORITY, format="f", big.mark=",", digits = 2),
        DIRECTORINDIGENOUS         = formatC(DIRECTORINDIGENOUS, format="f", big.mark=",", digits = 2),
        DIRECTORIDENTIFY_A2         = formatC(DIRECTORIDENTIFY_A2, format="f", big.mark=",", digits = 2),
        DIRECTORIDENTIFY_A3         = formatC(DIRECTORIDENTIFY_A3, format="f", big.mark=",", digits = 2)
      ) %>%  replace(is.na(.), "-") %>%  
      
      
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(bedi_list_wave2$Variable))
    
    #Transpose df 
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_bedi1_wave2<-cbind(df1,V1)
    }else {
      own_bedi1_wave2 <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_bedi1_wave2 <-  as_tibble(t(df), rownames = "Variable")
    
  })   
  
  
  
  own_bedi2_wave2 <- reactive({
    own_bedi1_wave2() %>% inner_join(bedi_list_wave2, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation")
  })  
  
  
  
  bedi_df_wave2 <- reactive({
    
    variables_selected() %>% filter(str_detect(Category,"Board Equity, Diversity, and Inclusion")) %>% 
      inner_join(own_bedi2_wave2(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
  }) 
  
  
  
  
  peer_bedi_wave2 <- reactive({
    
    df <-   data1 %>%
      filter(Organization %in% peergroups_selected_Wave2()$Organization) %>%
      filter(Wave==2) %>%
      select(starts_with(bedi_list_wave2$Variable))
    DIVERSITYTARGET_df<-summarydata (df,"DIVERSITYTARGET")
    LOCATIONREP_df<-summarydata (df,"LOCATIONREP")
    DIRECTORFEMALE_df<-meandata (df,DIRECTORFEMALE,0)
    DIRECTORMINORITY_df<-meandata (df,DIRECTORMINORITY,0)
    DIRECTORINDIGENOUS_df<-meandata (df,DIRECTORINDIGENOUS,0)
    DIRECTORIDENTIFY_A2_df<-meandata (df,DIRECTORIDENTIFY_A2,0)
    DIRECTORIDENTIFY_A3_df<-meandata (df,DIRECTORIDENTIFY_A3,0)
    
    # DIVERSITYTARGET_df <- df %>% group_by(DIVERSITYTARGET) %>% arrange(DIVERSITYTARGET) %>% 
    #   filter(!is.na(DIVERSITYTARGET)) %>% 
    #   summarise(n=n()) %>% mutate(Percent = paste0(round(100 * n/sum(n), 0), "%")) %>% 
    #   unite("DIVERSITYTARGET", c(DIVERSITYTARGET,Percent), sep = ":") %>% 
    #   mutate(DIVERSITYTARGETa    = (paste(unique(DIVERSITYTARGET), collapse = ";")))%>%
    #   filter(row_number()==1) 
    
    
    
    
    peer_bedi_wave2<-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
                
      ) %>%
      
      mutate(
        DIRECTORFEMALE         = DIRECTORFEMALE_df$percentage,
        DIRECTORMINORITY       = DIRECTORMINORITY_df$percentage,
        DIRECTORINDIGENOUS     = DIRECTORINDIGENOUS_df$percentage,
        DIRECTORIDENTIFY_A2    = DIRECTORIDENTIFY_A2_df$percentage,
        DIRECTORIDENTIFY_A3    = DIRECTORIDENTIFY_A3_df$percentage,
        # DIRECTORFEMALE         = formatC(DIRECTORFEMALE, format="f", big.mark=",", digits = 2),
        # DIRECTORMINORITY         = formatC(DIRECTORMINORITY, format="f", big.mark=",", digits = 2),
        # DIRECTORINDIGENOUS         = formatC(DIRECTORINDIGENOUS, format="f", big.mark=",", digits = 2),
        # DIRECTORIDENTIFY_A2         = formatC(DIRECTORIDENTIFY_A2, format="f", big.mark=",", digits = 2),
        # DIRECTORIDENTIFY_A3         = formatC(DIRECTORIDENTIFY_A3, format="f", big.mark=",", digits = 2),
        DIVERSITYTARGET        = ifelse(length(DIVERSITYTARGET_df) == 0, "No Response", DIVERSITYTARGET_df$percentage),
        LOCATIONREP            = ifelse(length(LOCATIONREP_df) == 0, "No Response", LOCATIONREP_df$percentage)
        
      )  %>% mutate_all(~ifelse(is.nan(.), NA, .)) # %>% replace(is.na(.), "NA") 
    
  })
  
  
  peer_bedi1_wave2<-reactive({
    if (nrow(peer_bedi_wave2())==0) {
      
      df1<-as_tibble(t(peer_bedi_wave2()), rownames = "Variable")
      V1<-"text"
      peer_bedi1_wave2<-cbind(df1,V1)
    }else {
      peer_bedi1_wave2 <-  as_tibble(t(peer_bedi_wave2()), rownames = "Variable")
    }
  })
  # peer_bedi1_wave2 <-  reactive({as_tibble(t(peer_bedi_wave2()), rownames = "Variable")
  # }) 
  # 
  
  peer_bedi2_wave2 <- reactive({
    peer_bedi1_wave2() %>% inner_join(bedi_list_wave2, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })   
  
  
  bedi_df1_wave2 <- reactive({
    
    bedi_df_wave2() %>% 
      left_join(peer_bedi2_wave2(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  })
  
  # ~~~Chair of the Board ~~~ ----  
  cob_list_wave1 <- selectors_tbl %>%
    filter(str_detect(Category,"Chair of the Board")) %>% 
    arrange(Order) %>%
    filter(is.na(Wave)==TRUE)
  cob_list_wave2 <- selectors_tbl %>%
    filter(str_detect(Category,"Chair of the Board")) %>% arrange(Order)
  
  own_cob1_wave1 <- reactive({
    
    df <- data1 %>% 
      filter(Wave==1) %>%
      
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(cob_list_wave1$Variable)) 
    
    #Transpose df 
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_cob1_wave1<-cbind(df1,V1)
    }else {
      own_cob1_wave1 <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_cob1_wave1 <-  as_tibble(t(df), rownames = "Variable")
    
  }) 
  
  
  
  own_cob2_wave1 <- reactive({
    own_cob1_wave1() %>% inner_join(cob_list_wave1, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation") 
  })  
  
  
  
  
  cob_df_wave1 <- reactive({
    
    variables_selected() %>% filter(str_detect(Category,"Chair of the Board")) %>%  
      inner_join(own_cob2_wave1(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
  })    
  
  
  
  peer_cob_wave1 <- reactive({
    
    df <-   data1 %>%
      filter(Organization %in% peergroups_selected_Wave1()$Organization) %>%
      filter(Wave==1) %>%
      select(starts_with(cob_list_wave1$Variable))
    
    CHAIRSTATUS_df<-summarydatasort (df,"CHAIRSTATUS")
    CHAIRCEO_df<-summarydata (df,"CHAIRCEO")
    
    peer_cob_wave1<-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
                
      ) %>%
      
      mutate(
        CHAIRSTATUS        = ifelse(length(CHAIRSTATUS_df) == 0, "No Response", CHAIRSTATUS_df$percentage),
        CHAIRCEO        = ifelse(length(CHAIRCEO_df) == 0, "No Response", CHAIRCEO_df$percentage)
        
      )  %>% mutate_all(~ifelse(is.nan(.), NA, .)) #%>% replace(is.na(.), "NA") 
    
  })  
  
  peer_cob1_wave1<-reactive({
    if (nrow(peer_cob_wave1())==0) {
      
      df1<-as_tibble(t(peer_cob_wave1()), rownames = "Variable")
      V1<-"text"
      peer_cob1_wave1<-cbind(df1,V1)
    }else {
      peer_cob1_wave1 <-  as_tibble(t(peer_cob_wave1()), rownames = "Variable")
    }
  })
  
  # peer_cob1_wave1 <-  reactive({as_tibble(t(peer_cob_wave1()), rownames = "Variable")
  # }) 
  
  
  
  peer_cob2_wave1 <- reactive({
    peer_cob1_wave1() %>% inner_join(cob_list_wave1, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })    
  
  
  
  cob_df1_wave1 <- reactive({
    
    cob_df_wave1() %>% 
      left_join(peer_cob2_wave1(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  })   
  
  
  own_cob1_wave2 <- reactive({
    
    df <- data1 %>% 
      filter(Wave==2) %>%
      
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(cob_list_wave2$Variable)) 
    
    #Transpose df
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_cob1_wave2<-cbind(df1,V1)
    }else {
      own_cob1_wave2 <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_cob1_wave2 <-  as_tibble(t(df), rownames = "Variable")
    
  }) 
  
  
  
  own_cob2_wave2 <- reactive({
    own_cob1_wave2() %>% inner_join(cob_list_wave2, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation") 
  })  
  
  
  
  
  cob_df_wave2 <- reactive({
    
    variables_selected() %>% filter(str_detect(Category,"Chair of the Board")) %>%  
      inner_join(own_cob2_wave2(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
  })    
  
  
  
  peer_cob_wave2 <- reactive({
    
    df <-   data1 %>%
      filter(Organization %in% peergroups_selected_Wave2()$Organization) %>%
      filter(Wave==2) %>%
      select(starts_with(cob_list_wave2$Variable))
    CHAIRSTATUS_df<-summarydatasort (df,"CHAIRSTATUS")
    CHAIRCEO_df<-summarydata (df,"CHAIRCEO")
    peer_cob_wave2<-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
                
      ) %>%
      
      mutate(
        CHAIRSTATUS        = ifelse(length(CHAIRSTATUS_df) == 0, "No Response", CHAIRSTATUS_df$percentage),
        CHAIRCEO        = ifelse(length(CHAIRCEO_df) == 0, "No Response", CHAIRCEO_df$percentage)
        
      )  %>% mutate_all(~ifelse(is.nan(.), NA, .)) #%>% replace(is.na(.), "NA") 
    
  })  
  
  peer_cob1_wave2<-reactive({
    if (nrow(peer_cob_wave2())==0) {
      
      df1<-as_tibble(t(peer_cob_wave2()), rownames = "Variable")
      V1<-"text"
      peer_cob1_wave2<-cbind(df1,V1)
    }else {
      peer_cob1_wave2 <-  as_tibble(t(peer_cob_wave2()), rownames = "Variable")
    }
  })
  
  # peer_cob1_wave2 <-  reactive({as_tibble(t(peer_cob_wave2()), rownames = "Variable")
  # }) 
  
  
  
  peer_cob2_wave2 <- reactive({
    peer_cob1_wave2() %>% inner_join(cob_list_wave2, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })    
  
  
  
  cob_df1_wave2 <- reactive({
    
    cob_df_wave2() %>% 
      left_join(peer_cob2_wave2(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  })  
  #~~~Board Practices ~~~ ---- 
  
  bp_list_wave1 <- selectors_tbl %>%
    filter(str_detect(Category,"Board Practices")) %>% 
    arrange(Order) %>%
    filter(is.na(Wave)==TRUE)
  bp_list_wave2 <- selectors_tbl %>%
    filter(str_detect(Category,"Board Practices")) %>% arrange(Order)
  
  
  own_bp1_wave1 <- reactive({ 
    df <- data1 %>% 
      filter(Wave==1) %>%
      
      mutate(
        RUNREELECTION         = formatC(RUNREELECTION, format="f", big.mark=",", digits = 2),
        MAXIMUMSERVICE        = formatC(MAXIMUMSERVICE, format="f", big.mark=",", digits = 2),
        RETIREMENTAGEBM       = formatC(RETIREMENTAGEBM, format="f", big.mark=",", digits = 2),
        # TERMLIMITS            = TERMLIMITS,
        BOARDMEETINGS         = formatC(BOARDMEETINGS, format="f", big.mark=",", digits = 2),
        MEETINGDURATION       = formatC(MEETINGDURATION, format="f", big.mark=",", digits = 2),
        BOARDCOMMITTEES       = formatC(BOARDCOMMITTEES, format="f", big.mark=",", digits = 2),
        AFMEETINGS            = formatC(AFMEETINGS, format="f", big.mark=",", digits = 2),
        COMPENSATIONMEETING   = formatC(COMPENSATIONMEETING, format="f", big.mark=",", digits = 2),
        
      ) %>% 
      
      
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(bp_list_wave1$Variable))
    
    #Transpose df 
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_bp1_wave1<-cbind(df1,V1)
    }else {
      own_bp1_wave1 <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_bp1_wave1 <-  as_tibble(t(df), rownames = "Variable")
    
    
  }) 
  
  
  
  
  own_bp2_wave1 <- reactive({
    own_bp1_wave1() %>% inner_join(bp_list_wave1, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation") 
  })  
  
  
  
  
  bp_df_wave1 <- reactive({
    
    variables_selected() %>% filter(str_detect(Category,"Board Practices")) %>%  
      inner_join(own_bp2_wave1(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
  })  
  
  
  
  peer_bp_wave1 <- reactive({  
    
    
    df <-   data1 %>%
      filter(Organization %in% peergroups_selected_Wave1()$Organization) %>%
      filter(Wave==1) %>%
      select(starts_with(bp_list_wave1$Variable))  
    
    DIRECTORTRAINING_df<-summarydata (df,"DIRECTORTRAINING")
    PROCESSREMOVE_df<-summarydata (df,"PROCESSREMOVE")
    HAVETOREMOVE_df<-summarydata (df,"HAVETOREMOVE")
    POSITIONSTATEMENT_A1_df<-summarydata (df,"POSITIONSTATEMENT_A1")
    POSITIONSTATEMENT_A2_df<-summarydata (df,"POSITIONSTATEMENT_A2")
    POSITIONSTATEMENT_A3_df<-summarydata (df,"POSITIONSTATEMENT_A3")
    POSITIONSTATEMENT_A4_df<-summarydata (df,"POSITIONSTATEMENT_A4")
    POSITIONSTATEMENT_A5_df<-summarydata (df,"POSITIONSTATEMENT_A5")
    POSITIONSTATEMENT_A6_df<-summarydata (df,"POSITIONSTATEMENT_A6")
    POSITIONSTATEMENT_A7_df<-summarydata (df,"POSITIONSTATEMENT_A7")
    TOTALTERMLIMIT_df<-summarydata (df,"TOTALTERMLIMIT")
    RETIREMENTREQUIREMENT_df<-summarydata (df,"RETIREMENTREQUIREMENT")
    BOARDEVAL_df<-summarydata (df,"BOARDEVAL")
    EVALUATIONTYPEC1_df<-summarydata (df,"EVALUATIONTYPEC1")
    EVALUATIONTYPEC2_df<-summarydata (df,"EVALUATIONTYPEC2")
    EVALUATIONTYPEC3_df<-summarydata (df,"EVALUATIONTYPEC3")
    EVALUATIONFREQ_df<-summarydatasort (df,"EVALUATIONFREQ")
    EVALUATIONASPECTSC1_df<-summarydata (df,"EVALUATIONASPECTSC1")
    EVALUATIONASPECTSC2_df<-summarydata (df,"EVALUATIONASPECTSC2")
    EVALUATIONASPECTSC3_df<-summarydata (df,"EVALUATIONASPECTSC3")
    EVALUATIONASPECTSC4_df<-summarydata (df,"EVALUATIONASPECTSC4")
    EVALUATIONASPECTSC5_df<-summarydata (df,"EVALUATIONASPECTSC5")
    EVALUATIONASPECTSC6_df<-summarydata (df,"EVALUATIONASPECTSC6")
    EVALUATIONASPECTSC7_df<-summarydata (df,"EVALUATIONASPECTSC7")
    EVALUATIONASPECTSC8_df<-summarydata (df,"EVALUATIONASPECTSC8")
    EVALUATIONASPECTSC9_df<-summarydata (df,"EVALUATIONASPECTSC9")
    EVALUATIONASPECTSC10_df<-summarydata (df,"EVALUATIONASPECTSC10")
    EVALUATIONASPECTSC11_df<-summarydata (df,"EVALUATIONASPECTSC11")
    EVALUATIONASPECTSC12_df<-summarydata (df,"EVALUATIONASPECTSC12")
    EVALUATIONASPECTSC13_df<-summarydata (df,"EVALUATIONASPECTSC13")
    EVALUATIONASPECTSC14_df<-summarydata (df,"EVALUATIONASPECTSC14")
    EVALUATIONASPECTSC15_df<-summarydata (df,"EVALUATIONASPECTSC15")
    BOARDCULTUREC1_df<-summarydata (df,"BOARDCULTUREC1")
    BOARDCULTUREC2_df<-summarydata (df,"BOARDCULTUREC2")
    BOARDCULTUREC3_df<-summarydata (df,"BOARDCULTUREC3")
    BOARDCULTUREC4_df<-summarydata (df,"BOARDCULTUREC4")
    BOARDCULTUREC5_df<-summarydata (df,"BOARDCULTUREC5")
    BOARDCULTUREC6_df<-summarydata (df,"BOARDCULTUREC6")
    BOARDCULTUREC7_df<-summarydata (df,"BOARDCULTUREC7")
    ASSESSPERFORMANCE_A1_df<-summarydata (df,"ASSESSPERFORMANCE_A1")
    ASSESSPERFORMANCE_A2_df<-summarydata (df,"ASSESSPERFORMANCE_A2")
    ASSESSPERFORMANCE_A3_df<-summarydata (df,"ASSESSPERFORMANCE_A3")
    ASSESSPERFORMANCE_A4_df<-summarydata (df,"ASSESSPERFORMANCE_A4")
    ASSESSPERFORMANCE_A5_df<-summarydata (df,"ASSESSPERFORMANCE_A5")
    ASSESSPERFORMANCE_A6_df<-summarydata (df,"ASSESSPERFORMANCE_A6")
    ASSESSPERFORMANCE_A7_df<-summarydata (df,"ASSESSPERFORMANCE_A7")
    ASSESSPERFORMANCE_A8_df<-summarydata (df,"ASSESSPERFORMANCE_A8")
    ASSESSPERFORMANCE_A9_df<-summarydata (df,"ASSESSPERFORMANCE_A9")
    ASSESSPERFORMANCE_A10_df<-summarydata (df,"ASSESSPERFORMANCE_A10")
    ASSESSPERFORMANCE_A11_df<-summarydata (df,"ASSESSPERFORMANCE_A11")
    ASSESSPERFORMANCE_A12_df<-summarydata (df,"ASSESSPERFORMANCE_A12")
    ASSESSPERFORMANCE_A13_df<-summarydata (df,"ASSESSPERFORMANCE_A13")
    ASSESSPERFORMANCE_A14_df<-summarydata (df,"ASSESSPERFORMANCE_A14")
    ASSESSPERFORMANCE_A15_df<-summarydata (df,"ASSESSPERFORMANCE_A15")
    ASSESSPERFORMANCE_A16_df<-summarydata (df,"ASSESSPERFORMANCE_A16")
    ASSESSPERFORMANCE_A17_df<-summarydata (df,"ASSESSPERFORMANCE_A17")
    COMMITTEETYPESC1_df<-summarydata (df,"COMMITTEETYPESC1")
    COMMITTEETYPESC2_df<-summarydata (df,"COMMITTEETYPESC2")
    COMMITTEETYPESC3_df<-summarydata (df,"COMMITTEETYPESC3")
    COMMITTEETYPESC4_df<-summarydata (df,"COMMITTEETYPESC4")
    COMMITTEETYPESC5_df<-summarydata (df,"COMMITTEETYPESC5")
    COMMITTEETYPESC6_df<-summarydata (df,"COMMITTEETYPESC6")
    COMMITTEETYPESC7_df<-summarydata (df,"COMMITTEETYPESC7")
    COMMITTEETYPESC8_df<-summarydata (df,"COMMITTEETYPESC8")
    COMMITTEETYPESC9_df<-summarydata (df,"COMMITTEETYPESC9")
    COMMITTEETYPESC10_df<-summarydata (df,"COMMITTEETYPESC10")
    COMMITTEETYPESC11_df<-summarydata (df,"COMMITTEETYPESC11")
    COMMITTEETYPESC12_df<-summarydata (df,"COMMITTEETYPESC12")
    RUNREELECTION_df<-meandata (df,RUNREELECTION,0)
    MAXIMUMSERVICE_df<-meandata (df,MAXIMUMSERVICE,0)
    RETIREMENTAGEBM_df<-meandata (df,RETIREMENTAGEBM,0)
    BOARDMEETINGS_df<-meandata (df,BOARDMEETINGS,0)
    MEETINGDURATION_df<-meandata (df,MEETINGDURATION,0)
    BOARDCOMMITTEES_df<-meandata (df,BOARDCOMMITTEES,0)
    AFMEETINGS_df<-meandata (df,AFMEETINGS,0)
    COMPENSATIONMEETING_df<-meandata (df,COMPENSATIONMEETING,0)
    
    # TERMLIMITS_df<-summarydata (df,"TERMLIMITS")
    peer_bp_wave1<-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
                
      ) %>%
      
      mutate(
        DIRECTORTRAINING        = ifelse(length(DIRECTORTRAINING_df) == 0, "No Response", DIRECTORTRAINING_df$percentage),
        PROCESSREMOVE        = ifelse(length(PROCESSREMOVE_df) == 0, "No Response", PROCESSREMOVE_df$percentage),
        HAVETOREMOVE        = ifelse(length(HAVETOREMOVE_df) == 0, "No Response", HAVETOREMOVE_df$percentage),
        POSITIONSTATEMENT_A1        = ifelse(length(POSITIONSTATEMENT_A1_df) == 0, "No Response", POSITIONSTATEMENT_A1_df$percentage),
        POSITIONSTATEMENT_A2        = ifelse(length(POSITIONSTATEMENT_A2_df) == 0, "No Response", POSITIONSTATEMENT_A2_df$percentage),
        POSITIONSTATEMENT_A3        = ifelse(length(POSITIONSTATEMENT_A3_df) == 0, "No Response", POSITIONSTATEMENT_A3_df$percentage),
        POSITIONSTATEMENT_A4        = ifelse(length(POSITIONSTATEMENT_A4_df) == 0, "No Response", POSITIONSTATEMENT_A4_df$percentage),
        POSITIONSTATEMENT_A5        = ifelse(length(POSITIONSTATEMENT_A5_df) == 0, "No Response", POSITIONSTATEMENT_A5_df$percentage),
        POSITIONSTATEMENT_A6        = ifelse(length(POSITIONSTATEMENT_A6_df) == 0, "No Response", POSITIONSTATEMENT_A6_df$percentage),
        POSITIONSTATEMENT_A7        = ifelse(length(POSITIONSTATEMENT_A7_df) == 0, "No Response", POSITIONSTATEMENT_A7_df$percentage)
        
        
        ,TERMLIMITS            =  NA
        ,RUNREELECTION         =RUNREELECTION_df$percentage
        # ,RUNREELECTION         = formatC(RUNREELECTION, format="f", big.mark=",", digits = 2)
        ,TOTALTERMLIMIT   = ifelse(length(TOTALTERMLIMIT_df) == 0, "No Response", TOTALTERMLIMIT_df$percentage)
        # ,MAXIMUMSERVICE        = formatC(MAXIMUMSERVICE, format="f", big.mark=",", digits = 2)
        ,MAXIMUMSERVICE=MAXIMUMSERVICE_df$percentage
        ,RETIREMENTREQUIREMENT        = ifelse(length(RETIREMENTREQUIREMENT_df) == 0, "No Response", RETIREMENTREQUIREMENT_df$percentage)
        # ,RETIREMENTAGEBM       = formatC(RETIREMENTAGEBM, format="f", big.mark=",", digits = 2)
        ,RETIREMENTAGEBM=RETIREMENTAGEBM_df$percentage
        ,BOARDEVAL        = ifelse(length(BOARDEVAL_df) == 0, "No Response", BOARDEVAL_df$percentage)
        ,EVALUATIONTYPEC1        = ifelse(length(EVALUATIONTYPEC1_df) == 0, "No Response", EVALUATIONTYPEC1_df$percentage)
        ,EVALUATIONTYPEC2        = ifelse(length(EVALUATIONTYPEC2_df) == 0, "No Response", EVALUATIONTYPEC2_df$percentage)
        ,EVALUATIONTYPEC3        = ifelse(length(EVALUATIONTYPEC3_df) == 0, "No Response", EVALUATIONTYPEC3_df$percentage)
        ,EVALUATIONFREQ        = ifelse(length(EVALUATIONFREQ_df) == 0, "No Response", EVALUATIONFREQ_df$percentage)
        ,EVALUATIONASPECTSC1        = ifelse(length(EVALUATIONASPECTSC1_df) == 0, "No Response", EVALUATIONASPECTSC1_df$percentage)
        ,EVALUATIONASPECTSC2        = ifelse(length(EVALUATIONASPECTSC2_df) == 0, "No Response", EVALUATIONASPECTSC2_df$percentage)
        ,EVALUATIONASPECTSC3        = ifelse(length(EVALUATIONASPECTSC3_df) == 0, "No Response", EVALUATIONASPECTSC3_df$percentage)
        ,EVALUATIONASPECTSC4        = ifelse(length(EVALUATIONASPECTSC4_df) == 0, "No Response", EVALUATIONASPECTSC4_df$percentage)
        ,EVALUATIONASPECTSC5        = ifelse(length(EVALUATIONASPECTSC5_df) == 0, "No Response", EVALUATIONASPECTSC5_df$percentage)
        ,EVALUATIONASPECTSC6        = ifelse(length(EVALUATIONASPECTSC6_df) == 0, "No Response", EVALUATIONASPECTSC6_df$percentage)
        ,EVALUATIONASPECTSC7        = ifelse(length(EVALUATIONASPECTSC7_df) == 0, "No Response", EVALUATIONASPECTSC7_df$percentage)
        ,EVALUATIONASPECTSC8        = ifelse(length(EVALUATIONASPECTSC8_df) == 0, "No Response", EVALUATIONASPECTSC8_df$percentage)
        ,EVALUATIONASPECTSC9        = ifelse(length(EVALUATIONASPECTSC9_df) == 0, "No Response", EVALUATIONASPECTSC9_df$percentage)
        ,EVALUATIONASPECTSC10        = ifelse(length(EVALUATIONASPECTSC10_df) == 0, "No Response", EVALUATIONASPECTSC10_df$percentage)
        ,EVALUATIONASPECTSC11        = ifelse(length(EVALUATIONASPECTSC11_df) == 0, "No Response", EVALUATIONASPECTSC11_df$percentage)
        ,EVALUATIONASPECTSC12        = ifelse(length(EVALUATIONASPECTSC12_df) == 0, "No Response", EVALUATIONASPECTSC12_df$percentage)
        ,EVALUATIONASPECTSC13        = ifelse(length(EVALUATIONASPECTSC13_df) == 0, "No Response", EVALUATIONASPECTSC13_df$percentage)
        ,EVALUATIONASPECTSC14        = ifelse(length(EVALUATIONASPECTSC14_df) == 0, "No Response", EVALUATIONASPECTSC14_df$percentage)
        ,EVALUATIONASPECTSC15        = ifelse(length(EVALUATIONASPECTSC15_df) == 0, "No Response", EVALUATIONASPECTSC15_df$percentage)
        ,BOARDCULTUREC1              = ifelse(length(BOARDCULTUREC1_df) == 0, "No Response", BOARDCULTUREC1_df$percentage)
        ,BOARDCULTUREC2              = ifelse(length(BOARDCULTUREC2_df) == 0, "No Response", BOARDCULTUREC2_df$percentage)
        ,BOARDCULTUREC3              = ifelse(length(BOARDCULTUREC3_df) == 0, "No Response", BOARDCULTUREC3_df$percentage)
        ,BOARDCULTUREC4              = ifelse(length(BOARDCULTUREC4_df) == 0, "No Response", BOARDCULTUREC4_df$percentage)
        ,BOARDCULTUREC5              = ifelse(length(BOARDCULTUREC5_df) == 0, "No Response", BOARDCULTUREC5_df$percentage)
        ,BOARDCULTUREC6              = ifelse(length(BOARDCULTUREC6_df) == 0, "No Response", BOARDCULTUREC6_df$percentage)
        ,BOARDCULTUREC7              = ifelse(length(BOARDCULTUREC7_df) == 0, "No Response", BOARDCULTUREC7_df$percentage)
        
        
        ,ASSESSPERFORMANCE_A1        = ifelse(length(ASSESSPERFORMANCE_A1_df) == 0, "No Response", ASSESSPERFORMANCE_A1_df$percentage)
        ,ASSESSPERFORMANCE_A2        = ifelse(length(ASSESSPERFORMANCE_A2_df) == 0, "No Response", ASSESSPERFORMANCE_A2_df$percentage)
        ,ASSESSPERFORMANCE_A3        = ifelse(length(ASSESSPERFORMANCE_A3_df) == 0, "No Response", ASSESSPERFORMANCE_A3_df$percentage)
        ,ASSESSPERFORMANCE_A4        = ifelse(length(ASSESSPERFORMANCE_A4_df) == 0, "No Response", ASSESSPERFORMANCE_A4_df$percentage)
        ,ASSESSPERFORMANCE_A5        = ifelse(length(ASSESSPERFORMANCE_A5_df) == 0, "No Response", ASSESSPERFORMANCE_A5_df$percentage)
        ,ASSESSPERFORMANCE_A6        = ifelse(length(ASSESSPERFORMANCE_A6_df) == 0, "No Response", ASSESSPERFORMANCE_A6_df$percentage)
        ,ASSESSPERFORMANCE_A7        = ifelse(length(ASSESSPERFORMANCE_A7_df) == 0, "No Response", ASSESSPERFORMANCE_A7_df$percentage)
        ,ASSESSPERFORMANCE_A8        = ifelse(length(ASSESSPERFORMANCE_A8_df) == 0, "No Response", ASSESSPERFORMANCE_A8_df$percentage)
        ,ASSESSPERFORMANCE_A9        = ifelse(length(ASSESSPERFORMANCE_A9_df) == 0, "No Response", ASSESSPERFORMANCE_A9_df$percentage)
        ,ASSESSPERFORMANCE_A10        = ifelse(length(ASSESSPERFORMANCE_A10_df) == 0, "No Response", ASSESSPERFORMANCE_A10_df$percentage)
        ,ASSESSPERFORMANCE_A11        = ifelse(length(ASSESSPERFORMANCE_A11_df) == 0, "No Response", ASSESSPERFORMANCE_A11_df$percentage)
        ,ASSESSPERFORMANCE_A12        = ifelse(length(ASSESSPERFORMANCE_A12_df) == 0, "No Response", ASSESSPERFORMANCE_A12_df$percentage)
        ,ASSESSPERFORMANCE_A13        = ifelse(length(ASSESSPERFORMANCE_A13_df) == 0, "No Response", ASSESSPERFORMANCE_A13_df$percentage)
        ,ASSESSPERFORMANCE_A14        = ifelse(length(ASSESSPERFORMANCE_A14_df) == 0, "No Response", ASSESSPERFORMANCE_A14_df$percentage)
        ,ASSESSPERFORMANCE_A15        = ifelse(length(ASSESSPERFORMANCE_A15_df) == 0, "No Response", ASSESSPERFORMANCE_A15_df$percentage)
        ,ASSESSPERFORMANCE_A16        = ifelse(length(ASSESSPERFORMANCE_A16_df) == 0, "No Response", ASSESSPERFORMANCE_A16_df$percentage)
        ,ASSESSPERFORMANCE_A17        = ifelse(length(ASSESSPERFORMANCE_A17_df) == 0, "No Response", ASSESSPERFORMANCE_A17_df$percentage)
        
        # ,BOARDMEETINGS         = formatC(BOARDMEETINGS, format="f", big.mark=",", digits = 2)
        # ,MEETINGDURATION        = formatC(MEETINGDURATION, format="f", big.mark=",", digits = 2)
        # ,BOARDCOMMITTEES       = formatC(BOARDCOMMITTEES, format="f", big.mark=",", digits = 2) 
        ,BOARDMEETINGS               =BOARDMEETINGS_df$percentage
        ,MEETINGDURATION             =MEETINGDURATION_df$percentage
        ,BOARDCOMMITTEES             =BOARDCOMMITTEES_df$percentage
        
        ,COMMITTEETYPESC1        = ifelse(length(COMMITTEETYPESC1_df) == 0, "No Response", COMMITTEETYPESC1_df$percentage)
        ,COMMITTEETYPESC2        = ifelse(length(COMMITTEETYPESC2_df) == 0, "No Response", COMMITTEETYPESC2_df$percentage)
        ,COMMITTEETYPESC3        = ifelse(length(COMMITTEETYPESC3_df) == 0, "No Response", COMMITTEETYPESC3_df$percentage)
        ,COMMITTEETYPESC4        = ifelse(length(COMMITTEETYPESC4_df) == 0, "No Response", COMMITTEETYPESC4_df$percentage)
        ,COMMITTEETYPESC5        = ifelse(length(COMMITTEETYPESC5_df) == 0, "No Response", COMMITTEETYPESC5_df$percentage)
        ,COMMITTEETYPESC6        = ifelse(length(COMMITTEETYPESC6_df) == 0, "No Response", COMMITTEETYPESC6_df$percentage)
        ,COMMITTEETYPESC7        = ifelse(length(COMMITTEETYPESC7_df) == 0, "No Response", COMMITTEETYPESC7_df$percentage)
        ,COMMITTEETYPESC8        = ifelse(length(COMMITTEETYPESC8_df) == 0, "No Response", COMMITTEETYPESC8_df$percentage)
        ,COMMITTEETYPESC9        = ifelse(length(COMMITTEETYPESC9_df) == 0, "No Response", COMMITTEETYPESC9_df$percentage)
        ,COMMITTEETYPESC10        = ifelse(length(COMMITTEETYPESC10_df) == 0, "No Response", COMMITTEETYPESC10_df$percentage)
        ,COMMITTEETYPESC11        = ifelse(length(COMMITTEETYPESC11_df) == 0, "No Response", COMMITTEETYPESC11_df$percentage)
        ,COMMITTEETYPESC12        = ifelse(length(COMMITTEETYPESC12_df) == 0, "No Response", COMMITTEETYPESC12_df$percentage)
        # ,AFMEETINGS              = formatC(AFMEETINGS, format="f", big.mark=",", digits = 2)
        # ,COMPENSATIONMEETING     = formatC(COMPENSATIONMEETING, format="f", big.mark=",", digits = 2)
        ,AFMEETINGS              =AFMEETINGS_df$percentage
        ,COMPENSATIONMEETING     =COMPENSATIONMEETING_df$percentage
      ) %>% mutate_all(~ifelse(is.nan(.), NA, .)) #%>% replace(is.na(.), "NA") 
    
  })
  
  
  
  peer_bp1_wave1<-reactive({
    if (nrow(peer_bp_wave1())==0) {
      
      df1<-as_tibble(t(peer_bp_wave1()), rownames = "Variable")
      V1<-"text"
      peer_bp1_wave1<-cbind(df1,V1)
    }else {
      peer_bp1_wave1 <-  as_tibble(t(peer_bp_wave1()), rownames = "Variable")
    }
  })
  # peer_bp1_wave1 <-  reactive({as_tibble(t(peer_bp_wave1()), rownames = "Variable")
  # }) 
  
  
  
  peer_bp2_wave1 <- reactive({
    peer_bp1_wave1() %>% inner_join(bp_list_wave1, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })  
  
  bp_df1_wave1 <- reactive({
    
    bp_df_wave1() %>% 
      left_join(peer_bp2_wave1(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  })  
  
  
  own_bp1_wave2 <- reactive({ 
    df <- data1 %>% 
      filter(Wave==2) %>%
      
      mutate(
        RUNREELECTION         = formatC(RUNREELECTION, format="f", big.mark=",", digits = 2),
        MAXIMUMSERVICE        = formatC(MAXIMUMSERVICE, format="f", big.mark=",", digits = 2),
        RETIREMENTAGEBM       = formatC(RETIREMENTAGEBM, format="f", big.mark=",", digits = 2),
        # TERMLIMITS            = TERMLIMITS,
        BOARDMEETINGS         = formatC(BOARDMEETINGS, format="f", big.mark=",", digits = 2),
        MEETINGDURATION        = formatC(MEETINGDURATION, format="f", big.mark=",", digits = 2),
        BOARDCOMMITTEES       = formatC(BOARDCOMMITTEES, format="f", big.mark=",", digits = 2),
        
      ) %>% 
      
      
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(bp_list_wave2$Variable))
    
    #Transpose df 
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_bp1_wave2<-cbind(df1,V1)
    }else {
      own_bp1_wave2 <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_bp1_wave2 <-  as_tibble(t(df), rownames = "Variable")
    
    
  }) 
  
  
  
  
  own_bp2_wave2 <- reactive({
    own_bp1_wave2() %>% inner_join(bp_list_wave2, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation") 
  })  
  
  
  
  
  bp_df_wave2 <- reactive({
    
    variables_selected() %>% filter(str_detect(Category,"Board Practices")) %>%  
      inner_join(own_bp2_wave2(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
  })  
  
  
  
  peer_bp_wave2 <- reactive({  
    
    
    df <-   data1 %>%
      filter(Organization %in% peergroups_selected_Wave2()$Organization) %>%
      filter(Wave==2) %>%
      select(starts_with(bp_list_wave2$Variable))  
    
    DIRECTORTRAINING_df<-summarydata (df,"DIRECTORTRAINING")
    PROCESSREMOVE_df<-summarydata (df,"PROCESSREMOVE")
    HAVETOREMOVE_df<-summarydata (df,"HAVETOREMOVE")
    POSITIONSTATEMENT_A1_df<-summarydata (df,"POSITIONSTATEMENT_A1")
    POSITIONSTATEMENT_A2_df<-summarydata (df,"POSITIONSTATEMENT_A2")
    POSITIONSTATEMENT_A3_df<-summarydata (df,"POSITIONSTATEMENT_A3")
    POSITIONSTATEMENT_A4_df<-summarydata (df,"POSITIONSTATEMENT_A4")
    POSITIONSTATEMENT_A5_df<-summarydata (df,"POSITIONSTATEMENT_A5")
    POSITIONSTATEMENT_A6_df<-summarydata (df,"POSITIONSTATEMENT_A6")
    POSITIONSTATEMENT_A7_df<-summarydata (df,"POSITIONSTATEMENT_A7")
    TOTALTERMLIMIT_df<-summarydata (df,"TOTALTERMLIMIT")
    RETIREMENTREQUIREMENT_df<-summarydata (df,"RETIREMENTREQUIREMENT")
    BOARDEVAL_df<-summarydata (df,"BOARDEVAL")
    EVALUATIONTYPEC1_df<-summarydata (df,"EVALUATIONTYPEC1")
    EVALUATIONTYPEC2_df<-summarydata (df,"EVALUATIONTYPEC2")
    EVALUATIONTYPEC3_df<-summarydata (df,"EVALUATIONTYPEC3")
    EVALUATIONFREQ_df<-summarydatasort (df,"EVALUATIONFREQ")
    EVALUATIONASPECTSC1_df<-summarydata (df,"EVALUATIONASPECTSC1")
    EVALUATIONASPECTSC2_df<-summarydata (df,"EVALUATIONASPECTSC2")
    EVALUATIONASPECTSC3_df<-summarydata (df,"EVALUATIONASPECTSC3")
    EVALUATIONASPECTSC4_df<-summarydata (df,"EVALUATIONASPECTSC4")
    EVALUATIONASPECTSC5_df<-summarydata (df,"EVALUATIONASPECTSC5")
    EVALUATIONASPECTSC6_df<-summarydata (df,"EVALUATIONASPECTSC6")
    EVALUATIONASPECTSC7_df<-summarydata (df,"EVALUATIONASPECTSC7")
    EVALUATIONASPECTSC8_df<-summarydata (df,"EVALUATIONASPECTSC8")
    EVALUATIONASPECTSC9_df<-summarydata (df,"EVALUATIONASPECTSC9")
    EVALUATIONASPECTSC10_df<-summarydata (df,"EVALUATIONASPECTSC10")
    EVALUATIONASPECTSC11_df<-summarydata (df,"EVALUATIONASPECTSC11")
    EVALUATIONASPECTSC12_df<-summarydata (df,"EVALUATIONASPECTSC12")
    EVALUATIONASPECTSC13_df<-summarydata (df,"EVALUATIONASPECTSC13")
    EVALUATIONASPECTSC14_df<-summarydata (df,"EVALUATIONASPECTSC14")
    EVALUATIONASPECTSC15_df<-summarydata (df,"EVALUATIONASPECTSC15")
    BOARDCULTUREC1_df<-summarydata (df,"BOARDCULTUREC1")
    BOARDCULTUREC2_df<-summarydata (df,"BOARDCULTUREC2")
    BOARDCULTUREC3_df<-summarydata (df,"BOARDCULTUREC3")
    BOARDCULTUREC4_df<-summarydata (df,"BOARDCULTUREC4")
    BOARDCULTUREC5_df<-summarydata (df,"BOARDCULTUREC5")
    BOARDCULTUREC6_df<-summarydata (df,"BOARDCULTUREC6")
    BOARDCULTUREC7_df<-summarydata (df,"BOARDCULTUREC7")
    ASSESSPERFORMANCE_A1_df<-summarydata (df,"ASSESSPERFORMANCE_A1")
    ASSESSPERFORMANCE_A2_df<-summarydata (df,"ASSESSPERFORMANCE_A2")
    ASSESSPERFORMANCE_A3_df<-summarydata (df,"ASSESSPERFORMANCE_A3")
    ASSESSPERFORMANCE_A4_df<-summarydata (df,"ASSESSPERFORMANCE_A4")
    ASSESSPERFORMANCE_A5_df<-summarydata (df,"ASSESSPERFORMANCE_A5")
    ASSESSPERFORMANCE_A6_df<-summarydata (df,"ASSESSPERFORMANCE_A6")
    ASSESSPERFORMANCE_A7_df<-summarydata (df,"ASSESSPERFORMANCE_A7")
    ASSESSPERFORMANCE_A8_df<-summarydata (df,"ASSESSPERFORMANCE_A8")
    ASSESSPERFORMANCE_A9_df<-summarydata (df,"ASSESSPERFORMANCE_A9")
    ASSESSPERFORMANCE_A10_df<-summarydata (df,"ASSESSPERFORMANCE_A10")
    ASSESSPERFORMANCE_A11_df<-summarydata (df,"ASSESSPERFORMANCE_A11")
    ASSESSPERFORMANCE_A12_df<-summarydata (df,"ASSESSPERFORMANCE_A12")
    ASSESSPERFORMANCE_A13_df<-summarydata (df,"ASSESSPERFORMANCE_A13")
    ASSESSPERFORMANCE_A14_df<-summarydata (df,"ASSESSPERFORMANCE_A14")
    ASSESSPERFORMANCE_A15_df<-summarydata (df,"ASSESSPERFORMANCE_A15")
    ASSESSPERFORMANCE_A16_df<-summarydata (df,"ASSESSPERFORMANCE_A16")
    ASSESSPERFORMANCE_A17_df<-summarydata (df,"ASSESSPERFORMANCE_A17")
    COMMITTEETYPESC1_df<-summarydata (df,"COMMITTEETYPESC1")
    COMMITTEETYPESC2_df<-summarydata (df,"COMMITTEETYPESC2")
    COMMITTEETYPESC3_df<-summarydata (df,"COMMITTEETYPESC3")
    COMMITTEETYPESC4_df<-summarydata (df,"COMMITTEETYPESC4")
    COMMITTEETYPESC5_df<-summarydata (df,"COMMITTEETYPESC5")
    COMMITTEETYPESC6_df<-summarydata (df,"COMMITTEETYPESC6")
    COMMITTEETYPESC7_df<-summarydata (df,"COMMITTEETYPESC7")
    COMMITTEETYPESC8_df<-summarydata (df,"COMMITTEETYPESC8")
    COMMITTEETYPESC9_df<-summarydata (df,"COMMITTEETYPESC9")
    COMMITTEETYPESC10_df<-summarydata (df,"COMMITTEETYPESC10")
    COMMITTEETYPESC11_df<-summarydata (df,"COMMITTEETYPESC11")
    COMMITTEETYPESC12_df<-summarydata (df,"COMMITTEETYPESC12")
    TERMLIMITS_df<-summarydata (df,"TERMLIMITS")
    RECRUITMEMBERS_A1_df<-summarydata (df,"RECRUITMEMBERS_A1")
    RECRUITMEMBERS_A2_df<-summarydata (df,"RECRUITMEMBERS_A2")
    RECRUITMEMBERS_A3_df<-summarydata (df,"RECRUITMEMBERS_A3")
    RECRUITMEMBERS_A4_df<-summarydata (df,"RECRUITMEMBERS_A4")
    RECRUITMEMBERS_A5_df<-summarydata (df,"RECRUITMEMBERS_A5")
    RECRUITMEMBERS_A6_df<-summarydata (df,"RECRUITMEMBERS_A6")
    ONBOARDING_A1_df<-summarydata (df,"ONBOARDING_A1")
    ONBOARDING_A2_df<-summarydata (df,"ONBOARDING_A2")
    ONBOARDING_A3_df<-summarydata (df,"ONBOARDING_A3")
    ONBOARDING_A4_df<-summarydata (df,"ONBOARDING_A4")
    ONBOARDING_A5_df<-summarydata (df,"ONBOARDING_A5")
    ONBOARDING_A6_df<-summarydata (df,"ONBOARDING_A6")
    ONBOARDING_A7_df<-summarydata (df,"ONBOARDING_A7")
    ONBOARDING_A8_df<-summarydata (df,"ONBOARDING_A8")
    MEMBERTRAINING_A1_df<-summarydata (df,"MEMBERTRAINING_A1")
    MEMBERTRAINING_A2_df<-summarydata (df,"MEMBERTRAINING_A2")
    MEMBERTRAINING_A3_df<-summarydata (df,"MEMBERTRAINING_A3")
    MEMBERTRAINING_A4_df<-summarydata (df,"MEMBERTRAINING_A4")
    MEMBERTRAINING_A5_df<-summarydata (df,"MEMBERTRAINING_A5")
    MEMBERTRAINING_A6_df<-summarydata (df,"MEMBERTRAINING_A6")
    MEMBERTRAINING_A7_df<-summarydata (df,"MEMBERTRAINING_A7")
    MEMBERTRAINING_A8_df<-summarydata (df,"MEMBERTRAINING_A8")
    MEMBERTRAINING_A9_df<-summarydata (df,"MEMBERTRAINING_A9")
    MEMBERTRAINING_A10_df<-summarydata (df,"MEMBERTRAINING_A10")
    MEMBERTRAINING_A11_df<-summarydata (df,"MEMBERTRAINING_A11")
    INCAMERAPROPORTION_df<-summarydata (df,"INCAMERAPROPORTION")
    INCAMERAEXECUTIVES_A1_df<-summarydata (df,"INCAMERAEXECUTIVES_A1")
    INCAMERAEXECUTIVES_A2_df<-summarydata (df,"INCAMERAEXECUTIVES_A2")
    INCAMERAEXECUTIVES_A3_df<-summarydata (df,"INCAMERAEXECUTIVES_A3")
    INCAMERAEXECUTIVES_A4_df<-summarydata (df,"INCAMERAEXECUTIVES_A4")
    INCAMERAEXECUTIVES_A5_df<-summarydata (df,"INCAMERAEXECUTIVES_A5")
    AFINCAMERA_A1_df<-summarydata (df,"AFINCAMERA_A1")
    AFINCAMERA_A2_df<-summarydata (df,"AFINCAMERA_A2")
    AFINCAMERA_A3_df<-summarydata (df,"AFINCAMERA_A3")
    GOVINCAMERA_A1_df<-summarydata (df,"GOVINCAMERA_A1")
    HRINCAMERA_A1_df<-summarydata (df,"HRINCAMERA_A1")
    RUNREELECTION_df<-meandata (df,RUNREELECTION,0)
    MAXIMUMSERVICE_df<-meandata (df,MAXIMUMSERVICE,0)
    RETIREMENTAGEBM_df<-meandata (df,RETIREMENTAGEBM,0)
    BOARDMEETINGS_df<-meandata (df,BOARDMEETINGS,0)
    MEETINGDURATION_df<-meandata (df,MEETINGDURATION,0)
    BOARDCOMMITTEES_df<-meandata (df,BOARDCOMMITTEES,0)
    AFMEETINGS_df<-meandata (df,AFMEETINGS,0)
    COMPENSATIONMEETING_df<-meandata (df,COMPENSATIONMEETING,0)
    
    peer_bp_wave2<-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
                
      ) %>%
      
      mutate(
        DIRECTORTRAINING        = ifelse(length(DIRECTORTRAINING_df) == 0, "No Response", DIRECTORTRAINING_df$percentage),
        PROCESSREMOVE        = ifelse(length(PROCESSREMOVE_df) == 0, "No Response", PROCESSREMOVE_df$percentage),
        HAVETOREMOVE        = ifelse(length(HAVETOREMOVE_df) == 0, "No Response", HAVETOREMOVE_df$percentage),
        POSITIONSTATEMENT_A1        = ifelse(length(POSITIONSTATEMENT_A1_df) == 0, "No Response", POSITIONSTATEMENT_A1_df$percentage),
        POSITIONSTATEMENT_A2        = ifelse(length(POSITIONSTATEMENT_A2_df) == 0, "No Response", POSITIONSTATEMENT_A2_df$percentage),
        POSITIONSTATEMENT_A3        = ifelse(length(POSITIONSTATEMENT_A3_df) == 0, "No Response", POSITIONSTATEMENT_A3_df$percentage),
        POSITIONSTATEMENT_A4        = ifelse(length(POSITIONSTATEMENT_A4_df) == 0, "No Response", POSITIONSTATEMENT_A4_df$percentage),
        POSITIONSTATEMENT_A5        = ifelse(length(POSITIONSTATEMENT_A5_df) == 0, "No Response", POSITIONSTATEMENT_A5_df$percentage),
        POSITIONSTATEMENT_A6        = ifelse(length(POSITIONSTATEMENT_A6_df) == 0, "No Response", POSITIONSTATEMENT_A6_df$percentage),
        POSITIONSTATEMENT_A7        = ifelse(length(POSITIONSTATEMENT_A7_df) == 0, "No Response", POSITIONSTATEMENT_A7_df$percentage)
        
        
        ,TERMLIMITS            =  ifelse(length(TERMLIMITS_df) == 0, "No Response", TERMLIMITS_df$percentage)
        # ,RUNREELECTION       = formatC(RUNREELECTION, format="f", big.mark=",", digits = 2)
        ,RUNREELECTION         = RUNREELECTION_df$percentage
        ,TOTALTERMLIMIT        = ifelse(length(TOTALTERMLIMIT_df) == 0, "No Response", TOTALTERMLIMIT_df$percentage)
        # ,MAXIMUMSERVICE      = formatC(MAXIMUMSERVICE, format="f", big.mark=",", digits = 2)
        ,MAXIMUMSERVICE        =MAXIMUMSERVICE_df$percentage
        ,RETIREMENTREQUIREMENT        = ifelse(length(RETIREMENTREQUIREMENT_df) == 0, "No Response", RETIREMENTREQUIREMENT_df$percentage)
        # ,RETIREMENTAGEBM       = formatC(RETIREMENTAGEBM, format="f", big.mark=",", digits = 2)
        ,RETIREMENTAGEBM       = RETIREMENTAGEBM_df$percentage
        ,BOARDEVAL        = ifelse(length(BOARDEVAL_df) == 0, "No Response", BOARDEVAL_df$percentage)
        ,EVALUATIONTYPEC1        = ifelse(length(EVALUATIONTYPEC1_df) == 0, "No Response", EVALUATIONTYPEC1_df$percentage)
        ,EVALUATIONTYPEC2        = ifelse(length(EVALUATIONTYPEC2_df) == 0, "No Response", EVALUATIONTYPEC2_df$percentage)
        ,EVALUATIONTYPEC3        = ifelse(length(EVALUATIONTYPEC3_df) == 0, "No Response", EVALUATIONTYPEC3_df$percentage)
        ,EVALUATIONFREQ        = ifelse(length(EVALUATIONFREQ_df) == 0, "No Response", EVALUATIONFREQ_df$percentage)
        ,EVALUATIONASPECTSC1        = ifelse(length(EVALUATIONASPECTSC1_df) == 0, "No Response", EVALUATIONASPECTSC1_df$percentage)
        ,EVALUATIONASPECTSC2        = ifelse(length(EVALUATIONASPECTSC2_df) == 0, "No Response", EVALUATIONASPECTSC2_df$percentage)
        ,EVALUATIONASPECTSC3        = ifelse(length(EVALUATIONASPECTSC3_df) == 0, "No Response", EVALUATIONASPECTSC3_df$percentage)
        ,EVALUATIONASPECTSC4        = ifelse(length(EVALUATIONASPECTSC4_df) == 0, "No Response", EVALUATIONASPECTSC4_df$percentage)
        ,EVALUATIONASPECTSC5        = ifelse(length(EVALUATIONASPECTSC5_df) == 0, "No Response", EVALUATIONASPECTSC5_df$percentage)
        ,EVALUATIONASPECTSC6        = ifelse(length(EVALUATIONASPECTSC6_df) == 0, "No Response", EVALUATIONASPECTSC6_df$percentage)
        ,EVALUATIONASPECTSC7        = ifelse(length(EVALUATIONASPECTSC7_df) == 0, "No Response", EVALUATIONASPECTSC7_df$percentage)
        ,EVALUATIONASPECTSC8        = ifelse(length(EVALUATIONASPECTSC8_df) == 0, "No Response", EVALUATIONASPECTSC8_df$percentage)
        ,EVALUATIONASPECTSC9        = ifelse(length(EVALUATIONASPECTSC9_df) == 0, "No Response", EVALUATIONASPECTSC9_df$percentage)
        ,EVALUATIONASPECTSC10        = ifelse(length(EVALUATIONASPECTSC10_df) == 0, "No Response", EVALUATIONASPECTSC10_df$percentage)
        ,EVALUATIONASPECTSC11        = ifelse(length(EVALUATIONASPECTSC11_df) == 0, "No Response", EVALUATIONASPECTSC11_df$percentage)
        ,EVALUATIONASPECTSC12        = ifelse(length(EVALUATIONASPECTSC12_df) == 0, "No Response", EVALUATIONASPECTSC12_df$percentage)
        ,EVALUATIONASPECTSC13        = ifelse(length(EVALUATIONASPECTSC13_df) == 0, "No Response", EVALUATIONASPECTSC13_df$percentage)
        ,EVALUATIONASPECTSC14        = ifelse(length(EVALUATIONASPECTSC14_df) == 0, "No Response", EVALUATIONASPECTSC14_df$percentage)
        ,EVALUATIONASPECTSC15        = ifelse(length(EVALUATIONASPECTSC15_df) == 0, "No Response", EVALUATIONASPECTSC15_df$percentage)
        ,BOARDCULTUREC1              = ifelse(length(BOARDCULTUREC1_df) == 0, "No Response", BOARDCULTUREC1_df$percentage)
        ,BOARDCULTUREC2              = ifelse(length(BOARDCULTUREC2_df) == 0, "No Response", BOARDCULTUREC2_df$percentage)
        ,BOARDCULTUREC3              = ifelse(length(BOARDCULTUREC3_df) == 0, "No Response", BOARDCULTUREC3_df$percentage)
        ,BOARDCULTUREC4              = ifelse(length(BOARDCULTUREC4_df) == 0, "No Response", BOARDCULTUREC4_df$percentage)
        ,BOARDCULTUREC5              = ifelse(length(BOARDCULTUREC5_df) == 0, "No Response", BOARDCULTUREC5_df$percentage)
        ,BOARDCULTUREC6              = ifelse(length(BOARDCULTUREC6_df) == 0, "No Response", BOARDCULTUREC6_df$percentage)
        ,BOARDCULTUREC7            = ifelse(length(BOARDCULTUREC7_df) == 0, "No Response", BOARDCULTUREC7_df$percentage)
        
        
        ,ASSESSPERFORMANCE_A1        = ifelse(length(ASSESSPERFORMANCE_A1_df) == 0, "No Response", ASSESSPERFORMANCE_A1_df$percentage)
        ,ASSESSPERFORMANCE_A2        = ifelse(length(ASSESSPERFORMANCE_A2_df) == 0, "No Response", ASSESSPERFORMANCE_A2_df$percentage)
        ,ASSESSPERFORMANCE_A3        = ifelse(length(ASSESSPERFORMANCE_A3_df) == 0, "No Response", ASSESSPERFORMANCE_A3_df$percentage)
        ,ASSESSPERFORMANCE_A4        = ifelse(length(ASSESSPERFORMANCE_A4_df) == 0, "No Response", ASSESSPERFORMANCE_A4_df$percentage)
        ,ASSESSPERFORMANCE_A5        = ifelse(length(ASSESSPERFORMANCE_A5_df) == 0, "No Response", ASSESSPERFORMANCE_A5_df$percentage)
        ,ASSESSPERFORMANCE_A6        = ifelse(length(ASSESSPERFORMANCE_A6_df) == 0, "No Response", ASSESSPERFORMANCE_A6_df$percentage)
        ,ASSESSPERFORMANCE_A7        = ifelse(length(ASSESSPERFORMANCE_A7_df) == 0, "No Response", ASSESSPERFORMANCE_A7_df$percentage)
        ,ASSESSPERFORMANCE_A8        = ifelse(length(ASSESSPERFORMANCE_A8_df) == 0, "No Response", ASSESSPERFORMANCE_A8_df$percentage)
        ,ASSESSPERFORMANCE_A9        = ifelse(length(ASSESSPERFORMANCE_A9_df) == 0, "No Response", ASSESSPERFORMANCE_A9_df$percentage)
        ,ASSESSPERFORMANCE_A10        = ifelse(length(ASSESSPERFORMANCE_A10_df) == 0, "No Response", ASSESSPERFORMANCE_A10_df$percentage)
        ,ASSESSPERFORMANCE_A11        = ifelse(length(ASSESSPERFORMANCE_A11_df) == 0, "No Response", ASSESSPERFORMANCE_A11_df$percentage)
        ,ASSESSPERFORMANCE_A12        = ifelse(length(ASSESSPERFORMANCE_A12_df) == 0, "No Response", ASSESSPERFORMANCE_A12_df$percentage)
        ,ASSESSPERFORMANCE_A13        = ifelse(length(ASSESSPERFORMANCE_A13_df) == 0, "No Response", ASSESSPERFORMANCE_A13_df$percentage)
        ,ASSESSPERFORMANCE_A14        = ifelse(length(ASSESSPERFORMANCE_A14_df) == 0, "No Response", ASSESSPERFORMANCE_A14_df$percentage)
        ,ASSESSPERFORMANCE_A15        = ifelse(length(ASSESSPERFORMANCE_A15_df) == 0, "No Response", ASSESSPERFORMANCE_A15_df$percentage)
        ,ASSESSPERFORMANCE_A16        = ifelse(length(ASSESSPERFORMANCE_A16_df) == 0, "No Response", ASSESSPERFORMANCE_A16_df$percentage)
        ,ASSESSPERFORMANCE_A17        = ifelse(length(ASSESSPERFORMANCE_A17_df) == 0, "No Response", ASSESSPERFORMANCE_A17_df$percentage)
        
        # ,BOARDMEETINGS         = formatC(BOARDMEETINGS, format="f", big.mark=",", digits = 2)
        # ,MEETINGDURATION        = formatC(MEETINGDURATION, format="f", big.mark=",", digits = 2)
        # ,BOARDCOMMITTEES       = formatC(BOARDCOMMITTEES, format="f", big.mark=",", digits = 2) 
        ,BOARDMEETINGS         = BOARDMEETINGS_df$percentage
        ,MEETINGDURATION       = MEETINGDURATION_df$percentage
        ,BOARDCOMMITTEES       = BOARDCOMMITTEES_df$percentage
        
        
        ,COMMITTEETYPESC1        = ifelse(length(COMMITTEETYPESC1_df) == 0, "No Response", COMMITTEETYPESC1_df$percentage)
        ,COMMITTEETYPESC2        = ifelse(length(COMMITTEETYPESC2_df) == 0, "No Response", COMMITTEETYPESC2_df$percentage)
        ,COMMITTEETYPESC3        = ifelse(length(COMMITTEETYPESC3_df) == 0, "No Response", COMMITTEETYPESC3_df$percentage)
        ,COMMITTEETYPESC4        = ifelse(length(COMMITTEETYPESC4_df) == 0, "No Response", COMMITTEETYPESC4_df$percentage)
        ,COMMITTEETYPESC5        = ifelse(length(COMMITTEETYPESC5_df) == 0, "No Response", COMMITTEETYPESC5_df$percentage)
        ,COMMITTEETYPESC6        = ifelse(length(COMMITTEETYPESC6_df) == 0, "No Response", COMMITTEETYPESC6_df$percentage)
        ,COMMITTEETYPESC7        = ifelse(length(COMMITTEETYPESC7_df) == 0, "No Response", COMMITTEETYPESC7_df$percentage)
        ,COMMITTEETYPESC8        = ifelse(length(COMMITTEETYPESC8_df) == 0, "No Response", COMMITTEETYPESC8_df$percentage)
        ,COMMITTEETYPESC9        = ifelse(length(COMMITTEETYPESC9_df) == 0, "No Response", COMMITTEETYPESC9_df$percentage)
        ,COMMITTEETYPESC10        = ifelse(length(COMMITTEETYPESC10_df) == 0, "No Response", COMMITTEETYPESC10_df$percentage)
        ,COMMITTEETYPESC11        = ifelse(length(COMMITTEETYPESC11_df) == 0, "No Response", COMMITTEETYPESC11_df$percentage)
        ,COMMITTEETYPESC12        = ifelse(length(COMMITTEETYPESC12_df) == 0, "No Response", COMMITTEETYPESC12_df$percentage)
        ,RECRUITMEMBERS_A1        = ifelse(length(RECRUITMEMBERS_A1_df) == 0, "No Response", RECRUITMEMBERS_A1_df$percentage)
        ,RECRUITMEMBERS_A2        = ifelse(length(RECRUITMEMBERS_A2_df) == 0, "No Response", RECRUITMEMBERS_A2_df$percentage)
        ,RECRUITMEMBERS_A3        = ifelse(length(RECRUITMEMBERS_A3_df) == 0, "No Response", RECRUITMEMBERS_A3_df$percentage)
        ,RECRUITMEMBERS_A4        = ifelse(length(RECRUITMEMBERS_A4_df) == 0, "No Response", RECRUITMEMBERS_A4_df$percentage)
        ,RECRUITMEMBERS_A5        = ifelse(length(RECRUITMEMBERS_A5_df) == 0, "No Response", RECRUITMEMBERS_A5_df$percentage)
        ,RECRUITMEMBERS_A6        = ifelse(length(RECRUITMEMBERS_A6_df) == 0, "No Response", RECRUITMEMBERS_A6_df$percentage)
        ,ONBOARDING_A1        = ifelse(length(ONBOARDING_A1_df) == 0, "No Response", ONBOARDING_A1_df$percentage)
        ,ONBOARDING_A2        = ifelse(length(ONBOARDING_A2_df) == 0, "No Response", ONBOARDING_A2_df$percentage)
        ,ONBOARDING_A3        = ifelse(length(ONBOARDING_A3_df) == 0, "No Response", ONBOARDING_A3_df$percentage)
        ,ONBOARDING_A4        = ifelse(length(ONBOARDING_A4_df) == 0, "No Response", ONBOARDING_A4_df$percentage)
        ,ONBOARDING_A5        = ifelse(length(ONBOARDING_A5_df) == 0, "No Response", ONBOARDING_A5_df$percentage)
        ,ONBOARDING_A6        = ifelse(length(ONBOARDING_A6_df) == 0, "No Response", ONBOARDING_A6_df$percentage)
        ,ONBOARDING_A7        = ifelse(length(ONBOARDING_A7_df) == 0, "No Response", ONBOARDING_A7_df$percentage)
        ,ONBOARDING_A8        = ifelse(length(ONBOARDING_A8_df) == 0, "No Response", ONBOARDING_A8_df$percentage)
        ,MEMBERTRAINING_A1        = ifelse(length(MEMBERTRAINING_A1_df) == 0, "No Response", MEMBERTRAINING_A1_df$percentage)
        ,MEMBERTRAINING_A2        = ifelse(length(MEMBERTRAINING_A2_df) == 0, "No Response", MEMBERTRAINING_A2_df$percentage)
        ,MEMBERTRAINING_A3        = ifelse(length(MEMBERTRAINING_A3_df) == 0, "No Response", MEMBERTRAINING_A3_df$percentage)
        ,MEMBERTRAINING_A4        = ifelse(length(MEMBERTRAINING_A4_df) == 0, "No Response", MEMBERTRAINING_A4_df$percentage)
        ,MEMBERTRAINING_A5        = ifelse(length(MEMBERTRAINING_A5_df) == 0, "No Response", MEMBERTRAINING_A5_df$percentage)
        ,MEMBERTRAINING_A6        = ifelse(length(MEMBERTRAINING_A6_df) == 0, "No Response", MEMBERTRAINING_A6_df$percentage)
        ,MEMBERTRAINING_A7        = ifelse(length(MEMBERTRAINING_A7_df) == 0, "No Response", MEMBERTRAINING_A7_df$percentage)
        ,MEMBERTRAINING_A8        = ifelse(length(MEMBERTRAINING_A8_df) == 0, "No Response", MEMBERTRAINING_A8_df$percentage)
        ,MEMBERTRAINING_A9        = ifelse(length(MEMBERTRAINING_A9_df) == 0, "No Response", MEMBERTRAINING_A9_df$percentage)
        ,MEMBERTRAINING_A10        = ifelse(length(MEMBERTRAINING_A10_df) == 0, "No Response", MEMBERTRAINING_A10_df$percentage)
        ,MEMBERTRAINING_A11        = ifelse(length(MEMBERTRAINING_A11_df) == 0, "No Response", MEMBERTRAINING_A11_df$percentage)
        ,INCAMERAPROPORTION        = ifelse(length(INCAMERAPROPORTION_df) == 0, "No Response", INCAMERAPROPORTION_df$percentage)
        ,INCAMERAEXECUTIVES_A1       = ifelse(length(INCAMERAEXECUTIVES_A1_df) == 0, "No Response", INCAMERAEXECUTIVES_A1_df$percentage)
        ,INCAMERAEXECUTIVES_A2       = ifelse(length(INCAMERAEXECUTIVES_A2_df) == 0, "No Response", INCAMERAEXECUTIVES_A2_df$percentage)
        ,INCAMERAEXECUTIVES_A3       = ifelse(length(INCAMERAEXECUTIVES_A3_df) == 0, "No Response", INCAMERAEXECUTIVES_A3_df$percentage)
        ,INCAMERAEXECUTIVES_A4       = ifelse(length(INCAMERAEXECUTIVES_A4_df) == 0, "No Response", INCAMERAEXECUTIVES_A4_df$percentage)
        ,INCAMERAEXECUTIVES_A5       = ifelse(length(INCAMERAEXECUTIVES_A5_df) == 0, "No Response", INCAMERAEXECUTIVES_A5_df$percentage)
        ,AFINCAMERA_A1       = ifelse(length(AFINCAMERA_A1_df) == 0, "No Response", AFINCAMERA_A1_df$percentage)
        ,AFINCAMERA_A2       = ifelse(length(AFINCAMERA_A2_df) == 0, "No Response", AFINCAMERA_A2_df$percentage)
        ,AFINCAMERA_A3       = ifelse(length(AFINCAMERA_A3_df) == 0, "No Response", AFINCAMERA_A3_df$percentage)
        ,AFINCAMERA_A3       = ifelse(length(AFINCAMERA_A3_df) == 0, "No Response", AFINCAMERA_A3_df$percentage)
        ,GOVINCAMERA_A1       = ifelse(length(GOVINCAMERA_A1_df) == 0, "No Response", GOVINCAMERA_A1_df$percentage)
        ,HRINCAMERA_A1       = ifelse(length(HRINCAMERA_A1_df) == 0, "No Response", HRINCAMERA_A1_df$percentage)
        # ,AFMEETINGS          = formatC(AFMEETINGS, format="f", big.mark=",", digits = 2)
        # ,COMPENSATIONMEETING = formatC(COMPENSATIONMEETING, format="f", big.mark=",", digits = 2)
        ,AFMEETINGS          = AFMEETINGS_df$percentage
        ,COMPENSATIONMEETING = COMPENSATIONMEETING_df$percentage
        
      ) %>% mutate_all(~ifelse(is.nan(.), NA, .)) #%>% replace(is.na(.), "NA") 
    
  })
  
  
  peer_bp1_wave2<-reactive({
    if (nrow(peer_bp_wave2())==0) {
      
      df1<-as_tibble(t(peer_bp_wave2()), rownames = "Variable")
      V1<-"text"
      peer_bp1_wave2<-cbind(df1,V1)
    }else {
      peer_bp1_wave2 <-  as_tibble(t(peer_bp_wave2()), rownames = "Variable")
    }
  })
  
  # peer_bp1_wave2 <-  reactive({as_tibble(t(peer_bp_wave2()), rownames = "Variable")
  # }) 
  
  
  
  peer_bp2_wave2 <- reactive({
    peer_bp1_wave2() %>% inner_join(bp_list_wave2, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })  
  
  
  
  bp_df1_wave2 <- reactive({
    
    bp_df_wave2() %>% 
      left_join(peer_bp2_wave2(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  })  
  # ~~~Compensation ~~~----  
  compen_list_wave1 <- selectors_tbl %>%
    filter(str_detect(Category,"^Compensation")) %>% 
    arrange(Order) %>%
    filter(is.na(Wave)==TRUE)
  compen_list_wave2 <- selectors_tbl %>%
    filter(str_detect(Category,"^Compensation")) %>% arrange(Order)
  own_compen1_wave1 <- reactive({
    
    df <- data1 %>%
      filter(Wave==1) %>%
      
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(compen_list_wave1$Variable)) %>%
      
      mutate(
        MAXCOMPENSATIONFIXED = scales::dollar(MAXCOMPENSATIONFIXED),
        BOARDCOMPENSATION = scales::dollar(BOARDCOMPENSATION),
        
        DIRECTORCOMPENSATION_A1 = scales::dollar(DIRECTORCOMPENSATION_A1),
        DIRECTORCOMPENSATION_A2 = scales::dollar(DIRECTORCOMPENSATION_A2),
        DIRECTORCOMPENSATION_A3 = scales::dollar(DIRECTORCOMPENSATION_A3),
        DIRECTORCOMPENSATION_A4 = scales::dollar(DIRECTORCOMPENSATION_A4),
        
        TOTALCOMPENSATION_A2 = scales::dollar(TOTALCOMPENSATION_A2),
        TOTALCOMPENSATION_A3 = scales::dollar(TOTALCOMPENSATION_A3),
        TOTALCOMPENSATION_A4 = scales::dollar(TOTALCOMPENSATION_A4),
        TOTALCOMPENSATION_A5 = scales::dollar(TOTALCOMPENSATION_A5),
        
        REIMBURSEMENTDIRECTORCOSTS_A1 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A1),
        REIMBURSEMENTDIRECTORCOSTS_A2 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A2),
        REIMBURSEMENTDIRECTORCOSTS_A3 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A3),
        REIMBURSEMENTDIRECTORCOSTS_A4 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A4),
        REIMBURSEMENTDIRECTORCOSTS_A5 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A5),
        REIMBURSEMENTDIRECTORCOSTS_A6 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A6),
        REIMBURSEMENTDIRECTORCOSTS_A7 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A7),
        REIMBURSEMENTDIRECTORCOSTS_A8 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A8 ),
        REIMBURSEMENTDIRECTORCOSTS_A9 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A9 ),
        LASTREVIEW         = formatC(LASTREVIEW, format="f", big.mark=",", digits = 2)
        ,AFCHAIRCOMPENSATION = scales::dollar(AFCHAIRCOMPENSATION )
        ,AFCOMPENSATION = scales::dollar(AFCOMPENSATION )
        
      ) 
    
    
    #Transpose df 
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_compen1_wave1<-cbind(df1,V1)
    }else {
      own_compen1_wave1 <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_compen1_wave1 <-  as_tibble(t(df), rownames = "Variable")
    
  })   
  
  
  
  own_compen2_wave1 <- reactive({
    own_compen1_wave1()%>% inner_join(compen_list_wave1, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation") 
    
  })     
  
  
  
  compen_df_wave1 <- reactive({
    
    variables_selected() %>% filter(str_detect(Category,"^Compensation")) %>%  
      inner_join(own_compen2_wave1(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
    
  }) 
  
  
  
  
  peer_compen_wave1 <- reactive({
    
    df <-   data1 %>%
      filter(Organization %in% peergroups_selected_Wave1()$Organization) %>%
      filter(Wave==1) %>%
      select(starts_with(compen_list_wave1$Variable))  
    MAXCOMPENSATION_df<-summarydata (df,"MAXCOMPENSATION")
    DISCLOSECOMPENSATION_df<-summarydata (df,"DISCLOSECOMPENSATION")
    COMPENSATIONREVIEW_df<-summarydata (df,"COMPENSATIONREVIEW")
    PAYDIRECTORS_df<-summarydata (df,"PAYDIRECTORS")
    BASEDONFCL_df<-summarydata (df,"BASEDONFCL")
    DIRECTORBENEFITS_A1_df<-summarydata (df,"DIRECTORBENEFITS_A1")
    DIRECTORBENEFITS_A2_df<-summarydata (df,"DIRECTORBENEFITS_A2")
    DIRECTORBENEFITS_A3_df<-summarydata (df,"DIRECTORBENEFITS_A3")
    DIRECTORBENEFITS_A4_df<-summarydata (df,"DIRECTORBENEFITS_A4")
    DIRECTORBENEFITS_A5_df<-summarydata (df,"DIRECTORBENEFITS_A5")
    DIRECTORBENEFITS_A6_df<-summarydata (df,"DIRECTORBENEFITS_A6")
    MAXCOMPENSATIONFIXED_df<-meandata (df,MAXCOMPENSATIONFIXED,1)
    MAXCOMPENSATIONPERCENT_df<-meandata (df,MAXCOMPENSATIONPERCENT,0)
    BOARDCOMPENSATION_df<-meandata (df,BOARDCOMPENSATION,1)
    DIRECTORCOMPENSATION_A1_df<-meandata (df,DIRECTORCOMPENSATION_A1,1)
    DIRECTORCOMPENSATION_A2_df<-meandata (df,DIRECTORCOMPENSATION_A2,1)
    DIRECTORCOMPENSATION_A3_df<-meandata (df,DIRECTORCOMPENSATION_A3,1)
    DIRECTORCOMPENSATION_A4_df<-meandata (df,DIRECTORCOMPENSATION_A4,1)
    TOTALCOMPENSATION_A2_df<-meandata (df,TOTALCOMPENSATION_A2,1)
    TOTALCOMPENSATION_A3_df<-meandata (df,TOTALCOMPENSATION_A3,1)
    TOTALCOMPENSATION_A4_df<-meandata (df,TOTALCOMPENSATION_A4,1)
    TOTALCOMPENSATION_A5_df<-meandata (df,TOTALCOMPENSATION_A5,1)
    REIMBURSEMENTDIRECTORCOSTS_A1_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A1,1)
    REIMBURSEMENTDIRECTORCOSTS_A2_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A2,1)
    REIMBURSEMENTDIRECTORCOSTS_A3_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A3,1)
    REIMBURSEMENTDIRECTORCOSTS_A4_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A4,1)
    REIMBURSEMENTDIRECTORCOSTS_A5_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A5,1)
    REIMBURSEMENTDIRECTORCOSTS_A6_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A6,1)
    REIMBURSEMENTDIRECTORCOSTS_A7_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A7,1)
    REIMBURSEMENTDIRECTORCOSTS_A8_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A8,1)
    REIMBURSEMENTDIRECTORCOSTS_A9_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A9,1)
    LASTREVIEW_df<-meandata (df,LASTREVIEW,0)
    AFCHAIRCOMPENSATION_df<-meandata (df,AFCHAIRCOMPENSATION,1)
    AFCOMPENSATION_df<-meandata (df,AFCOMPENSATION,1)
    
    peer_compen_wave1<-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
                
                
      ) %>%
      
      mutate(
        COMPENSATIONREVIEWO = NA
        ,LASTREVIEW         = LASTREVIEW_df$percentage
        ,MAXCOMPENSATION        = ifelse(length(MAXCOMPENSATION_df) == 0, "No Response", MAXCOMPENSATION_df$percentage)
        ,MAXCOMPENSATIONFIXED = MAXCOMPENSATIONFIXED_df$percentage
        ,MAXCOMPENSATIONPERCENT = MAXCOMPENSATIONPERCENT_df$percentage
        ,BOARDCOMPENSATION = BOARDCOMPENSATION_df$percentage
        ,DISCLOSECOMPENSATION   = ifelse(length(DISCLOSECOMPENSATION_df) == 0, "No Response", DISCLOSECOMPENSATION_df$percentage)
        ,COMPENSATIONREVIEW   = ifelse(length(COMPENSATIONREVIEW_df) == 0, "No Response", COMPENSATIONREVIEW_df$percentage)
        ,PAYDIRECTORS        = ifelse(length(PAYDIRECTORS_df) == 0, "No Response", PAYDIRECTORS_df$percentage)
        ,BASEDONFCL   = ifelse(length(BASEDONFCL_df) == 0, "No Response", BASEDONFCL_df$percentage)
        
        ,DIRECTORCOMPENSATION_A1 = DIRECTORCOMPENSATION_A1_df$percentage
        ,DIRECTORCOMPENSATION_A2 = DIRECTORCOMPENSATION_A2_df$percentage
        ,DIRECTORCOMPENSATION_A3 = DIRECTORCOMPENSATION_A3_df$percentage
        ,DIRECTORCOMPENSATION_A4 = DIRECTORCOMPENSATION_A4_df$percentage
        
        ,TOTALCOMPENSATION_A2 = TOTALCOMPENSATION_A2_df$percentage
        ,TOTALCOMPENSATION_A3 = TOTALCOMPENSATION_A3_df$percentage
        ,TOTALCOMPENSATION_A4 = TOTALCOMPENSATION_A4_df$percentage
        ,TOTALCOMPENSATION_A5 = TOTALCOMPENSATION_A5_df$percentage
        
        ,REIMBURSEMENTDIRECTORCOSTS_A1 = REIMBURSEMENTDIRECTORCOSTS_A1_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A2 = REIMBURSEMENTDIRECTORCOSTS_A2_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A3 = REIMBURSEMENTDIRECTORCOSTS_A3_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A4 = REIMBURSEMENTDIRECTORCOSTS_A4_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A5 = REIMBURSEMENTDIRECTORCOSTS_A5_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A6 = REIMBURSEMENTDIRECTORCOSTS_A6_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A7 = REIMBURSEMENTDIRECTORCOSTS_A7_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A8 = REIMBURSEMENTDIRECTORCOSTS_A8_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A9 = REIMBURSEMENTDIRECTORCOSTS_A9_df$percentage
        
        ,DIRECTORBENEFITS_A1        = ifelse(length(DIRECTORBENEFITS_A1_df) == 0, "No Response", DIRECTORBENEFITS_A1_df$percentage)
        ,DIRECTORBENEFITS_A2   = ifelse(length(DIRECTORBENEFITS_A2_df) == 0, "No Response", DIRECTORBENEFITS_A2_df$percentage)
        ,DIRECTORBENEFITS_A3   = ifelse(length(DIRECTORBENEFITS_A3_df) == 0, "No Response", DIRECTORBENEFITS_A3_df$percentage)
        ,DIRECTORBENEFITS_A4   = ifelse(length(DIRECTORBENEFITS_A4_df) == 0, "No Response", DIRECTORBENEFITS_A4_df$percentage)
        ,DIRECTORBENEFITS_A5   = ifelse(length(DIRECTORBENEFITS_A5_df) == 0, "No Response", DIRECTORBENEFITS_A5_df$percentage)
        ,DIRECTORBENEFITS_A6   = ifelse(length(DIRECTORBENEFITS_A6_df) == 0, "No Response", DIRECTORBENEFITS_A6_df$percentage)
        ,AFCHAIRCOMPENSATION = AFCHAIRCOMPENSATION_df$percentage
        ,AFCOMPENSATION = AFCOMPENSATION_df$percentage
      )  %>% mutate_all(~ifelse(is.nan(.), NA, .))# %>% replace(is.na(.), "NA")    
    
    
    
  })
  
  
  peer_compen1_wave1<-reactive({
    if (nrow(peer_compen_wave1())==0) {
      
      df1<-as_tibble(t(peer_compen_wave1()), rownames = "Variable")
      V1<-"text"
      peer_compen1_wave1<-cbind(df1,V1)
    }else {
      peer_compen1_wave1 <-  as_tibble(t(peer_compen_wave1()), rownames = "Variable")
    }
  })
  
  # peer_compen1_wave1 <-  reactive({as_tibble(t(peer_compen_wave1()), rownames = "Variable")
  # }) 
  # 
  
  
  
  peer_compen2_wave1 <- reactive({
    peer_compen1_wave1() %>% inner_join(compen_list_wave1, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })   
  
  
  
  
  compen_df1_wave1 <- reactive({
    
    compen_df_wave1() %>% 
      left_join(peer_compen2_wave1(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  })      
  
  own_compen1_wave2 <- reactive({
    
    df <- data1 %>% 
      filter(Wave==2) %>%
      
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(compen_list_wave2$Variable)) %>%
      
      mutate(
        MAXCOMPENSATIONFIXED = scales::dollar(MAXCOMPENSATIONFIXED ),
        BOARDCOMPENSATION = scales::dollar(BOARDCOMPENSATION ),
        
        DIRECTORCOMPENSATION_A1 = scales::dollar(DIRECTORCOMPENSATION_A1),
        DIRECTORCOMPENSATION_A2 = scales::dollar(DIRECTORCOMPENSATION_A2),
        DIRECTORCOMPENSATION_A3 = scales::dollar(DIRECTORCOMPENSATION_A3),
        DIRECTORCOMPENSATION_A4 = scales::dollar(DIRECTORCOMPENSATION_A4),
        
        TOTALCOMPENSATION_A2 = scales::dollar(TOTALCOMPENSATION_A2),
        TOTALCOMPENSATION_A3 = scales::dollar(TOTALCOMPENSATION_A3),
        TOTALCOMPENSATION_A4 = scales::dollar(TOTALCOMPENSATION_A4),
        TOTALCOMPENSATION_A5 = scales::dollar(TOTALCOMPENSATION_A5),
        
        REIMBURSEMENTDIRECTORCOSTS_A1 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A1),
        REIMBURSEMENTDIRECTORCOSTS_A2 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A2),
        REIMBURSEMENTDIRECTORCOSTS_A3 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A3),
        REIMBURSEMENTDIRECTORCOSTS_A4 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A4),
        REIMBURSEMENTDIRECTORCOSTS_A5 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A5),
        REIMBURSEMENTDIRECTORCOSTS_A6 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A6),
        REIMBURSEMENTDIRECTORCOSTS_A7 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A7),
        REIMBURSEMENTDIRECTORCOSTS_A8 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A8),
        REIMBURSEMENTDIRECTORCOSTS_A9 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A9),
        LASTREVIEW         = formatC(LASTREVIEW, format="f", big.mark=",", digits = 2)
        ,AFCHAIRCOMPENSATION = scales::dollar(AFCHAIRCOMPENSATION)
        ,AFCOMPENSATION = scales::dollar(AFCOMPENSATION)
        ,COMPENSATION_B1 = scales::dollar(COMPENSATION_B1)
        ,COMPENSATION_B2 = scales::dollar(COMPENSATION_B2)
        ,COMPENSATION_B3 = scales::dollar(COMPENSATION_B3)
        ,COMPENSATION_B4 = scales::dollar(COMPENSATION_B4)
        ,COMPENSATION_C1 = scales::dollar(COMPENSATION_C1)
        ,COMPENSATION_C2 = scales::dollar(COMPENSATION_C2)
        ,COMPENSATION_C3 = scales::dollar(COMPENSATION_C3)
        ,COMPENSATION_C4 = scales::dollar(COMPENSATION_C4)
        ,COMPENSATION_D1 = scales::dollar(COMPENSATION_D1)
        ,COMPENSATION_D2 = scales::dollar(COMPENSATION_D2)
        ,COMPENSATION_D3 = scales::dollar(COMPENSATION_D3)
        ,COMPENSATION_D4 = scales::dollar(COMPENSATION_D4)
        
      ) 
    
    
    #Transpose df 
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_compen1_wave2<-cbind(df1,V1)
    }else {
      own_compen1_wave2 <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_compen1_wave2 <-  as_tibble(t(df), rownames = "Variable")
    
  })   
  
  
  
  own_compen2_wave2 <- reactive({
    own_compen1_wave2()%>% inner_join(compen_list_wave2, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation") 
    
  })     
  
  
  
  compen_df_wave2 <- reactive({
    
    variables_selected() %>% filter(str_detect(Category,"^Compensation")) %>%  
      inner_join(own_compen2_wave2(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
    
  }) 
  
  
  
  
  peer_compen_wave2 <- reactive({
    
    df <-   data1 %>%
      filter(Organization %in% peergroups_selected_Wave2()$Organization) %>%
      filter(Wave==2) %>%
      select(starts_with(compen_list_wave2$Variable))  
    MAXCOMPENSATION_df<-summarydata (df,"MAXCOMPENSATION")
    DISCLOSECOMPENSATION_df<-summarydata (df,"DISCLOSECOMPENSATION")
    COMPENSATIONREVIEW_df<-summarydata (df,"COMPENSATIONREVIEW")
    PAYDIRECTORS_df<-summarydata (df,"PAYDIRECTORS")
    BASEDONFCL_df<-summarydata (df,"BASEDONFCL")
    DIRECTORBENEFITS_A1_df<-summarydata (df,"DIRECTORBENEFITS_A1")
    DIRECTORBENEFITS_A2_df<-summarydata (df,"DIRECTORBENEFITS_A2")
    DIRECTORBENEFITS_A3_df<-summarydata (df,"DIRECTORBENEFITS_A3")
    DIRECTORBENEFITS_A4_df<-summarydata (df,"DIRECTORBENEFITS_A4")
    DIRECTORBENEFITS_A5_df<-summarydata (df,"DIRECTORBENEFITS_A5")
    DIRECTORBENEFITS_A6_df<-summarydata (df,"DIRECTORBENEFITS_A6")
    MAXCOMPENSATIONFIXED_df<-meandata (df,MAXCOMPENSATIONFIXED,1)
    MAXCOMPENSATIONPERCENT_df<-meandata (df,MAXCOMPENSATIONPERCENT,0)
    BOARDCOMPENSATION_df<-meandata (df,BOARDCOMPENSATION,1)
    DIRECTORCOMPENSATION_A1_df<-meandata (df,DIRECTORCOMPENSATION_A1,1)
    DIRECTORCOMPENSATION_A2_df<-meandata (df,DIRECTORCOMPENSATION_A2,1)
    DIRECTORCOMPENSATION_A3_df<-meandata (df,DIRECTORCOMPENSATION_A3,1)
    DIRECTORCOMPENSATION_A4_df<-meandata (df,DIRECTORCOMPENSATION_A4,1)
    TOTALCOMPENSATION_A2_df<-meandata (df,TOTALCOMPENSATION_A2,1)
    TOTALCOMPENSATION_A3_df<-meandata (df,TOTALCOMPENSATION_A3,1)
    TOTALCOMPENSATION_A4_df<-meandata (df,TOTALCOMPENSATION_A4,1)
    TOTALCOMPENSATION_A5_df<-meandata (df,TOTALCOMPENSATION_A5,1)
    REIMBURSEMENTDIRECTORCOSTS_A1_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A1,1)
    REIMBURSEMENTDIRECTORCOSTS_A2_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A2,1)
    REIMBURSEMENTDIRECTORCOSTS_A3_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A3,1)
    REIMBURSEMENTDIRECTORCOSTS_A4_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A4,1)
    REIMBURSEMENTDIRECTORCOSTS_A5_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A5,1)
    REIMBURSEMENTDIRECTORCOSTS_A6_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A6,1)
    REIMBURSEMENTDIRECTORCOSTS_A7_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A7,1)
    REIMBURSEMENTDIRECTORCOSTS_A8_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A8,1)
    REIMBURSEMENTDIRECTORCOSTS_A9_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A9,1)
    COMPENSATION_B1_df<-meandata(df,COMPENSATION_B1,1)
    COMPENSATION_B2_df<-meandata(df,COMPENSATION_B2,1)
    COMPENSATION_B3_df<-meandata(df,COMPENSATION_B3,1)
    COMPENSATION_B4_df<-meandata(df,COMPENSATION_B4,1)
    COMPENSATION_C1_df<-meandata(df,COMPENSATION_C1,1)
    COMPENSATION_C2_df<-meandata(df,COMPENSATION_C2,1)
    COMPENSATION_C3_df<-meandata(df,COMPENSATION_C3,1)
    COMPENSATION_C4_df<-meandata(df,COMPENSATION_C4,1)
    COMPENSATION_D1_df<-meandata(df,COMPENSATION_D1,1)
    COMPENSATION_D2_df<-meandata(df,COMPENSATION_D2,1)
    COMPENSATION_D3_df<-meandata(df,COMPENSATION_D3,1)
    COMPENSATION_D4_df<-meandata(df,COMPENSATION_D4,1)
    LASTREVIEW_df<-meandata (df,LASTREVIEW,0)
    AFCHAIRCOMPENSATION_df<-meandata (df,AFCHAIRCOMPENSATION,1)
    AFCOMPENSATION_df<-meandata (df,AFCOMPENSATION,1)
    peer_compen_wave1<-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
                
                
      ) %>%
      
      mutate(
        COMPENSATIONREVIEWO = NA
        
        ,LASTREVIEW         = LASTREVIEW_df$percentage
        ,MAXCOMPENSATION        = ifelse(length(MAXCOMPENSATION_df) == 0, "No Response", MAXCOMPENSATION_df$percentage)
        ,MAXCOMPENSATIONFIXED = MAXCOMPENSATIONFIXED_df$percentage
        ,MAXCOMPENSATIONPERCENT = MAXCOMPENSATIONPERCENT_df$percentage
        ,BOARDCOMPENSATION = BOARDCOMPENSATION_df$percentage
        ,DISCLOSECOMPENSATION   = ifelse(length(DISCLOSECOMPENSATION_df) == 0, "No Response", DISCLOSECOMPENSATION_df$percentage)
        ,COMPENSATIONREVIEW   = ifelse(length(COMPENSATIONREVIEW_df) == 0, "No Response", COMPENSATIONREVIEW_df$percentage)
        ,PAYDIRECTORS        = ifelse(length(PAYDIRECTORS_df) == 0, "No Response", PAYDIRECTORS_df$percentage)
        ,BASEDONFCL   = ifelse(length(BASEDONFCL_df) == 0, "No Response", BASEDONFCL_df$percentage)
        
        ,DIRECTORCOMPENSATION_A1 = DIRECTORCOMPENSATION_A1_df$percentage
        ,DIRECTORCOMPENSATION_A2 = DIRECTORCOMPENSATION_A2_df$percentage
        ,DIRECTORCOMPENSATION_A3 = DIRECTORCOMPENSATION_A3_df$percentage
        ,DIRECTORCOMPENSATION_A4 = DIRECTORCOMPENSATION_A4_df$percentage
        
        ,TOTALCOMPENSATION_A2 = TOTALCOMPENSATION_A2_df$percentage
        ,TOTALCOMPENSATION_A3 = TOTALCOMPENSATION_A3_df$percentage
        ,TOTALCOMPENSATION_A4 = TOTALCOMPENSATION_A4_df$percentage
        ,TOTALCOMPENSATION_A5 = TOTALCOMPENSATION_A5_df$percentage
        
        ,REIMBURSEMENTDIRECTORCOSTS_A1 = REIMBURSEMENTDIRECTORCOSTS_A1_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A2 = REIMBURSEMENTDIRECTORCOSTS_A2_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A3 = REIMBURSEMENTDIRECTORCOSTS_A3_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A4 = REIMBURSEMENTDIRECTORCOSTS_A4_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A5 = REIMBURSEMENTDIRECTORCOSTS_A5_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A6 = REIMBURSEMENTDIRECTORCOSTS_A6_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A7 = REIMBURSEMENTDIRECTORCOSTS_A7_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A8 = REIMBURSEMENTDIRECTORCOSTS_A8_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A9 = REIMBURSEMENTDIRECTORCOSTS_A9_df$percentage
        
        ,DIRECTORBENEFITS_A1        = ifelse(length(DIRECTORBENEFITS_A1_df) == 0, "No Response", DIRECTORBENEFITS_A1_df$percentage)
        ,DIRECTORBENEFITS_A2   = ifelse(length(DIRECTORBENEFITS_A2_df) == 0, "No Response", DIRECTORBENEFITS_A2_df$percentage)
        ,DIRECTORBENEFITS_A3   = ifelse(length(DIRECTORBENEFITS_A3_df) == 0, "No Response", DIRECTORBENEFITS_A3_df$percentage)
        ,DIRECTORBENEFITS_A4   = ifelse(length(DIRECTORBENEFITS_A4_df) == 0, "No Response", DIRECTORBENEFITS_A4_df$percentage)
        ,DIRECTORBENEFITS_A5   = ifelse(length(DIRECTORBENEFITS_A5_df) == 0, "No Response", DIRECTORBENEFITS_A5_df$percentage)
        ,DIRECTORBENEFITS_A6   = ifelse(length(DIRECTORBENEFITS_A6_df) == 0, "No Response", DIRECTORBENEFITS_A6_df$percentage)
        ,COMPENSATION_B1 = COMPENSATION_B1_df$percentage
        ,COMPENSATION_B2 = COMPENSATION_B2_df$percentage
        ,COMPENSATION_B3 = COMPENSATION_B3_df$percentage
        ,COMPENSATION_B4 = COMPENSATION_B4_df$percentage
        ,COMPENSATION_C1 = COMPENSATION_C1_df$percentage
        ,COMPENSATION_C2 = COMPENSATION_C2_df$percentage
        ,COMPENSATION_C3 = COMPENSATION_C3_df$percentage
        ,COMPENSATION_C4 = COMPENSATION_C4_df$percentage
        ,COMPENSATION_D1 = COMPENSATION_D1_df$percentage
        ,COMPENSATION_D2 = COMPENSATION_D2_df$percentage
        ,COMPENSATION_D3 = COMPENSATION_D3_df$percentage
        ,COMPENSATION_D4 = COMPENSATION_D4_df$percentage
        ,AFCHAIRCOMPENSATION =AFCHAIRCOMPENSATION_df$percentage
        ,AFCOMPENSATION = AFCOMPENSATION_df$percentage
      )  %>% mutate_all(~ifelse(is.nan(.), NA, .))# %>% replace(is.na(.), "NA")    
    
    
    
  })
  
  
  peer_compen1_wave2<-reactive({
    if (nrow(peer_compen_wave2())==0) {
      
      df1<-as_tibble(t(peer_compen_wave2()), rownames = "Variable")
      V1<-"text"
      peer_compen1_wave2<-cbind(df1,V1)
    }else {
      peer_compen1_wave2 <-  as_tibble(t(peer_compen_wave2()), rownames = "Variable")
    }
  })
  
  # peer_compen1_wave2 <-  reactive({as_tibble(t(peer_compen_wave2()), rownames = "Variable")
  # }) 
  
  
  
  
  peer_compen2_wave2 <- reactive({
    peer_compen1_wave2() %>% inner_join(compen_list_wave2, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })   
  
  
  
  
  compen_df1_wave2 <- reactive({
    
    compen_df_wave2() %>% 
      left_join(peer_compen2_wave2(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  })      
  # ~~~Democratic Process~~~----  
  tldel_list_wave1 <- selectors_tbl %>%
    filter(str_detect(Category,"Democratic Process")) %>% 
    arrange(Order) %>%
    filter(is.na(Wave)==TRUE)
  tldel_list_wave2 <- selectors_tbl %>%
    filter(str_detect(Category,"Democratic Process")) %>% arrange(Order)
  
  
  own_tldel1_wave1 <- reactive({
    
    df <- data1 %>% 
      filter(Wave==1) %>%
      
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(tldel_list_wave1$Variable)
      ) %>% mutate(
        
        MEMBERAGMATTENDANCE         = formatC(MEMBERAGMATTENDANCE, format="f", big.mark=",", digits = 2),
        DELEGAGMATTENDANCE         = formatC(DELEGAGMATTENDANCE, format="f", big.mark=",", digits = 2),
        ONLINEATTENDANCE         = formatC(ONLINEATTENDANCE, format="f", big.mark=",", digits = 2),
        ONLINEATTENDANCE_POSTC19         = formatC(ONLINEATTENDANCE_POSTC19, format="f", big.mark=",", digits = 2),
        LASTAGMDURATION         = formatC(LASTAGMDURATION, format="f", big.mark=",", digits = 2),
        ELECTIONSPAST10         = formatC(ELECTIONSPAST10, format="f", big.mark=",", digits = 2),
        CONTESTEDSEATS         = formatC(CONTESTEDSEATS, format="f", big.mark=",", digits = 2)
      ) 
    
    
    #Transpose df 
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_tldel1_wave1<-cbind(df1,V1)
    }else {
      own_tldel1_wave1 <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_tldel1_wave1 <-  as_tibble(t(df), rownames = "Variable")
    
  })   
  
  
  
  own_tldel2_wave1 <- reactive({
    own_tldel1_wave1()%>% inner_join(tldel_list_wave1, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation")
    
  }) 
  
  
  
  
  tldel_df_wave1 <- reactive({
    
    variables_selected() %>% filter(str_detect(Category,"Democratic Process")) %>%  
      inner_join(own_tldel2_wave1(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
    
  })   
  
  
  
  peer_tldel_wave1 <- reactive({
    
    df <-   data1 %>%
      filter(Organization %in% peergroups_selected_Wave1()$Organization) %>%
      filter(Wave==1) %>%
      select(starts_with(tldel_list_wave1$Variable))  
    WHOATTENDAGM_df<-summarydatasort (df,"WHOATTENDAGM")
    HOWVOTEC1_df<-summarydata (df,"HOWVOTEC1")
    HOWVOTEC2_df<-summarydata (df,"HOWVOTEC2")
    HOWVOTEC3_df<-summarydata (df,"HOWVOTEC3")
    HOWVOTEC4_df<-summarydata (df,"HOWVOTEC4")
    HOWVOTEC5_df<-summarydata (df,"HOWVOTEC5")
    BOARDELECTEDMEMBERS_df<-summarydatasort (df,"BOARDELECTEDMEMBERS")
    STAGGEREDELECTIONS_df<-summarydata (df,"STAGGEREDELECTIONS")
    DEMOCRATICPRINCIPLE_df<-summarydata (df,"DEMOCRATICPRINCIPLE")
    OTHERDEMOCRATICPRACTICE_df<-summarydata (df,"OTHERDEMOCRATICPRACTICE")
    DIRECTORSELECTIONM1_df<-summarydatasort (df,"DIRECTORSELECTIONM1")
    DIRECTORSELECTIONM2_df<-summarydatasort (df,"DIRECTORSELECTIONM2")
    DIRECTORSELECTIONM3_df<-summarydatasort (df,"DIRECTORSELECTIONM3")
    DIRECTORSELECTIONM4_df<-summarydatasort (df,"DIRECTORSELECTIONM4")
    DIRECTORSELECTIONM5_df<-summarydatasort (df,"DIRECTORSELECTIONM5")
    MEMBERAGMATTENDANCE_df<-meandata (df,MEMBERAGMATTENDANCE,0)
    DELEGAGMATTENDANCE_df<-meandata (df,DELEGAGMATTENDANCE,0)
    ONLINEATTENDANCE_df<-meandata (df,ONLINEATTENDANCE,0)
    ONLINEATTENDANCE_POSTC19_df<-meandata (df,ONLINEATTENDANCE_POSTC19,0)
    LASTAGMDURATION_df<-meandata (df,LASTAGMDURATION,0)
    ELECTIONSPAST10_df<-meandata (df,ELECTIONSPAST10,0)
    CONTESTEDSEATS_df<-meandata (df,CONTESTEDSEATS,0)
    
    peer_tldel_wave1<-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
                
                
      ) %>%
      
      mutate(
        WHOATTENDAGM        = ifelse(length(WHOATTENDAGM_df) == 0, "No Response", WHOATTENDAGM_df$percentage)
        ,MEMBERAGMATTENDANCE         = MEMBERAGMATTENDANCE_df$percentage
        ,DELEGAGMATTENDANCE         = DELEGAGMATTENDANCE_df$percentage
        ,ONLINEATTENDANCE         = ONLINEATTENDANCE_df$percentage
        , ONLINEATTENDANCE_POSTC19         = ONLINEATTENDANCE_POSTC19_df$percentage
        #,HOWVOTEO = NA
        ,HOWVOTEC1        = ifelse(length(HOWVOTEC1_df) == 0, "No Response", HOWVOTEC1_df$percentage)
        ,HOWVOTEC2        = ifelse(length(HOWVOTEC2_df) == 0, "No Response", HOWVOTEC2_df$percentage)
        ,HOWVOTEC3        = ifelse(length(HOWVOTEC3_df) == 0, "No Response", HOWVOTEC3_df$percentage)
        ,HOWVOTEC4        = ifelse(length(HOWVOTEC4_df) == 0, "No Response", HOWVOTEC4_df$percentage)
        ,HOWVOTEC5        = ifelse(length(HOWVOTEC5_df) == 0, "No Response", HOWVOTEC5_df$percentage)
        ,LASTAGMDURATION         = LASTAGMDURATION_df$percentage
        ,BOARDELECTEDMEMBERS   = ifelse(length(BOARDELECTEDMEMBERS_df) == 0, "No Response", BOARDELECTEDMEMBERS_df$percentage)
        ,STAGGEREDELECTIONS   = ifelse(length(STAGGEREDELECTIONS_df) == 0, "No Response", STAGGEREDELECTIONS_df$percentage)
        ,ELECTIONSPAST10         = ELECTIONSPAST10_df$percentage
        ,CONTESTEDSEATS         = CONTESTEDSEATS_df$percentage
        ,DEMOCRATICPRINCIPLE        = ifelse(length(DEMOCRATICPRINCIPLE_df) == 0, "No Response", DEMOCRATICPRINCIPLE_df$percentage)
        ,OTHERDEMOCRATICPRACTICE   = ifelse(length(OTHERDEMOCRATICPRACTICE_df) == 0, "No Response", OTHERDEMOCRATICPRACTICE_df$percentage)
        ,DIRECTORSELECTIONM1   = ifelse(length(DIRECTORSELECTIONM1_df) == 0, "No Response", DIRECTORSELECTIONM1_df$percentage)
        ,DIRECTORSELECTIONM2   = ifelse(length(DIRECTORSELECTIONM2_df) == 0, "No Response", DIRECTORSELECTIONM2_df$percentage)
        ,DIRECTORSELECTIONM3   = ifelse(length(DIRECTORSELECTIONM3_df) == 0, "No Response", DIRECTORSELECTIONM3_df$percentage)
        ,DIRECTORSELECTIONM4   = ifelse(length(DIRECTORSELECTIONM4_df) == 0, "No Response", DIRECTORSELECTIONM4_df$percentage)
        ,DIRECTORSELECTIONM5   = ifelse(length(DIRECTORSELECTIONM5_df) == 0, "No Response", DIRECTORSELECTIONM5_df$percentage)
        
        
        
      ) %>% mutate_all(~ifelse(is.nan(.), NA, .)) #%>% replace(is.na(.), "NA") 
    
  })    
  
  peer_tldel1_wave1<-reactive({
    if (nrow(peer_tldel_wave1())==0) {
      
      df1<-as_tibble(t(peer_tldel_wave1()), rownames = "Variable")
      V1<-"text"
      peer_tldel1_wave1<-cbind(df1,V1)
    }else {
      peer_tldel1_wave1 <-  as_tibble(t(peer_tldel_wave1()), rownames = "Variable")
    }
  })
  # peer_tldel1_wave1 <-  reactive({as_tibble(t(peer_tldel_wave1()), rownames = "Variable")
  # }) 
  
  
  
  peer_tldel2_wave1 <- reactive({
    peer_tldel1_wave1() %>% inner_join(tldel_list_wave1, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })  
  
  
  
  tldel_df1_wave1 <- reactive({
    
    tldel_df_wave1() %>% 
      left_join(peer_tldel2_wave1(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  })      
  
  own_tldel1_wave2 <- reactive({
    
    df <- data1 %>% 
      filter(Wave==2) %>%
      
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(tldel_list_wave2$Variable)
      ) %>% mutate(
        
        MEMBERAGMATTENDANCE         = formatC(MEMBERAGMATTENDANCE, format="f", big.mark=",", digits = 2),
        DELEGAGMATTENDANCE         = formatC(DELEGAGMATTENDANCE, format="f", big.mark=",", digits = 2),
        ONLINEATTENDANCE         = formatC(ONLINEATTENDANCE, format="f", big.mark=",", digits = 2),
        ONLINEATTENDANCE_POSTC19         = formatC(ONLINEATTENDANCE_POSTC19, format="f", big.mark=",", digits = 2),
        LASTAGMDURATION         = formatC(LASTAGMDURATION, format="f", big.mark=",", digits = 2),
        ELECTIONSPAST10         = formatC(ELECTIONSPAST10, format="f", big.mark=",", digits = 2),
        CONTESTEDSEATS         = formatC(CONTESTEDSEATS, format="f", big.mark=",", digits = 2)
      ) 
    
    
    #Transpose df
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_tldel1_wave2<-cbind(df1,V1)
    }else {
      own_tldel1_wave2 <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_tldel1_wave2 <-  as_tibble(t(df), rownames = "Variable")
    
  })   
  
  
  
  own_tldel2_wave2 <- reactive({
    own_tldel1_wave2()%>% inner_join(tldel_list_wave2, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation")
    
  }) 
  
  
  
  
  tldel_df_wave2 <- reactive({
    
    variables_selected() %>% filter(str_detect(Category,"Democratic Process")) %>%  
      inner_join(own_tldel2_wave2(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
    
  })   
  
  
  
  peer_tldel_wave2 <- reactive({
    
    df <-   data1 %>%
      filter(Organization %in% peergroups_selected_Wave2()$Organization) %>%
      filter(Wave==2) %>%
      select(starts_with(tldel_list_wave2$Variable))  
    WHOATTENDAGM_df<-summarydatasort (df,"WHOATTENDAGM")
    HOWVOTEC1_df<-summarydata (df,"HOWVOTEC1")
    HOWVOTEC2_df<-summarydata (df,"HOWVOTEC2")
    HOWVOTEC3_df<-summarydata (df,"HOWVOTEC3")
    HOWVOTEC4_df<-summarydata (df,"HOWVOTEC4")
    HOWVOTEC5_df<-summarydata (df,"HOWVOTEC5")
    BOARDELECTEDMEMBERS_df<-summarydatasort (df,"BOARDELECTEDMEMBERS")
    STAGGEREDELECTIONS_df<-summarydata (df,"STAGGEREDELECTIONS")
    DEMOCRATICPRINCIPLE_df<-summarydata (df,"DEMOCRATICPRINCIPLE")
    OTHERDEMOCRATICPRACTICE_df<-summarydata (df,"OTHERDEMOCRATICPRACTICE")
    AGMPARTICIPATION_A1_df<-summarydata (df,"AGMPARTICIPATION_A1")
    AGMPARTICIPATION_A2_df<-summarydata (df,"AGMPARTICIPATION_A2")
    AGMPARTICIPATION_A3_df<-summarydata (df,"AGMPARTICIPATION_A3")
    AGMPARTICIPATION_A4_df<-summarydata (df,"AGMPARTICIPATION_A4")
    AGMPARTICIPATION_A5_df<-summarydata (df,"AGMPARTICIPATION_A5")
    AGMPARTICIPATION_A6_df<-summarydata (df,"AGMPARTICIPATION_A6")
    AGMPARTICIPATION_A7_df<-summarydata (df,"AGMPARTICIPATION_A7")
    AGMPARTICIPATION_A8_df<-summarydata (df,"AGMPARTICIPATION_A8")
    DIRECTORSELECTIONM1_df<-summarydatasort (df,"DIRECTORSELECTIONM1")
    DIRECTORSELECTIONM2_df<-summarydatasort (df,"DIRECTORSELECTIONM2")
    DIRECTORSELECTIONM3_df<-summarydatasort (df,"DIRECTORSELECTIONM3")
    DIRECTORSELECTIONM4_df<-summarydatasort (df,"DIRECTORSELECTIONM4")
    DIRECTORSELECTIONM5_df<-summarydatasort (df,"DIRECTORSELECTIONM5")
    MEMBERAGMATTENDANCE_df<-meandata (df,MEMBERAGMATTENDANCE,0)
    DELEGAGMATTENDANCE_df<-meandata (df,DELEGAGMATTENDANCE,0)
    ONLINEATTENDANCE_df<-meandata (df,ONLINEATTENDANCE,0)
    ONLINEATTENDANCE_POSTC19_df<-meandata (df,ONLINEATTENDANCE_POSTC19,0)
    LASTAGMDURATION_df<-meandata (df,LASTAGMDURATION,0)
    ELECTIONSPAST10_df<-meandata (df,ELECTIONSPAST10,0)
    CONTESTEDSEATS_df<-meandata (df,CONTESTEDSEATS,0)
    peer_tldel_wave2<-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
                
                
      ) %>%
      
      mutate(
        WHOATTENDAGM        = ifelse(length(WHOATTENDAGM_df) == 0, "No Response", WHOATTENDAGM_df$percentage)
        ,MEMBERAGMATTENDANCE         = MEMBERAGMATTENDANCE_df$percentage
        ,DELEGAGMATTENDANCE         = DELEGAGMATTENDANCE_df$percentage
        ,ONLINEATTENDANCE         = ONLINEATTENDANCE_df$percentage
        , ONLINEATTENDANCE_POSTC19         = ONLINEATTENDANCE_POSTC19_df$percentage
        #,HOWVOTEO = NA
        ,HOWVOTEC1        = ifelse(length(HOWVOTEC1_df) == 0, "No Response", HOWVOTEC1_df$percentage)
        ,HOWVOTEC2        = ifelse(length(HOWVOTEC2_df) == 0, "No Response", HOWVOTEC2_df$percentage)
        ,HOWVOTEC3        = ifelse(length(HOWVOTEC3_df) == 0, "No Response", HOWVOTEC3_df$percentage)
        ,HOWVOTEC4        = ifelse(length(HOWVOTEC4_df) == 0, "No Response", HOWVOTEC4_df$percentage)
        ,HOWVOTEC5        = ifelse(length(HOWVOTEC5_df) == 0, "No Response", HOWVOTEC5_df$percentage)
        ,LASTAGMDURATION         = LASTAGMDURATION_df$percentage
        ,BOARDELECTEDMEMBERS   = ifelse(length(BOARDELECTEDMEMBERS_df) == 0, "No Response", BOARDELECTEDMEMBERS_df$percentage)
        ,STAGGEREDELECTIONS   = ifelse(length(STAGGEREDELECTIONS_df) == 0, "No Response", STAGGEREDELECTIONS_df$percentage)
        ,ELECTIONSPAST10         = ELECTIONSPAST10_df$percentage
        ,CONTESTEDSEATS         = CONTESTEDSEATS_df$percentage
        ,DEMOCRATICPRINCIPLE        = ifelse(length(DEMOCRATICPRINCIPLE_df) == 0, "No Response", DEMOCRATICPRINCIPLE_df$percentage)
        ,OTHERDEMOCRATICPRACTICE   = ifelse(length(OTHERDEMOCRATICPRACTICE_df) == 0, "No Response", OTHERDEMOCRATICPRACTICE_df$percentage)
        ,AGMPARTICIPATION_A1   = ifelse(length(AGMPARTICIPATION_A1_df) == 0, "No Response", AGMPARTICIPATION_A1_df$percentage)
        ,AGMPARTICIPATION_A2   = ifelse(length(AGMPARTICIPATION_A2_df) == 0, "No Response", AGMPARTICIPATION_A2_df$percentage)
        ,AGMPARTICIPATION_A3   = ifelse(length(AGMPARTICIPATION_A3_df) == 0, "No Response", AGMPARTICIPATION_A3_df$percentage)
        ,AGMPARTICIPATION_A4   = ifelse(length(AGMPARTICIPATION_A4_df) == 0, "No Response", AGMPARTICIPATION_A4_df$percentage)
        ,AGMPARTICIPATION_A5   = ifelse(length(AGMPARTICIPATION_A5_df) == 0, "No Response", AGMPARTICIPATION_A5_df$percentage)
        ,AGMPARTICIPATION_A6   = ifelse(length(AGMPARTICIPATION_A6_df) == 0, "No Response", AGMPARTICIPATION_A6_df$percentage)
        ,AGMPARTICIPATION_A7   = ifelse(length(AGMPARTICIPATION_A7_df) == 0, "No Response", AGMPARTICIPATION_A7_df$percentage)
        ,AGMPARTICIPATION_A8   = ifelse(length(AGMPARTICIPATION_A8_df) == 0, "No Response", AGMPARTICIPATION_A8_df$percentage)
        ,DIRECTORSELECTIONM1   = ifelse(length(DIRECTORSELECTIONM1_df) == 0, "No Response", DIRECTORSELECTIONM1_df$percentage)
        ,DIRECTORSELECTIONM2   = ifelse(length(DIRECTORSELECTIONM2_df) == 0, "No Response", DIRECTORSELECTIONM2_df$percentage)
        ,DIRECTORSELECTIONM3   = ifelse(length(DIRECTORSELECTIONM3_df) == 0, "No Response", DIRECTORSELECTIONM3_df$percentage)
        ,DIRECTORSELECTIONM4   = ifelse(length(DIRECTORSELECTIONM4_df) == 0, "No Response", DIRECTORSELECTIONM4_df$percentage)
        ,DIRECTORSELECTIONM5   = ifelse(length(DIRECTORSELECTIONM5_df) == 0, "No Response", DIRECTORSELECTIONM5_df$percentage)
        
        
      ) %>% mutate_all(~ifelse(is.nan(.), NA, .)) #%>% replace(is.na(.), "NA") 
    
  })    
  
  peer_tldel1_wave2<-reactive({
    if (nrow(peer_tldel_wave2())==0) {
      
      df1<-as_tibble(t(peer_tldel_wave2()), rownames = "Variable")
      V1<-"text"
      peer_tldel1_wave2<-cbind(df1,V1)
    }else {
      peer_tldel1_wave2 <-  as_tibble(t(peer_tldel_wave2()), rownames = "Variable")
    }
  })
  # peer_tldel1_wave2 <-  reactive({as_tibble(t(peer_tldel_wave2()), rownames = "Variable")
  # }) 
  
  
  
  peer_tldel2_wave2 <- reactive({
    peer_tldel1_wave2() %>% inner_join(tldel_list_wave2, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })  
  
  
  
  tldel_df1_wave2 <- reactive({
    
    tldel_df_wave2() %>% 
      left_join(peer_tldel2_wave2(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  })      
  
  
  # tldel_varlist <- reactive({
  #   df<-selectors_tbl[selected1(),1:2 ] # Variable, Category 
  #   tldel_varlist <-df%>%
  #     filter(str_detect(Category,"Democratic Process"))  
  # })
  # own_tldel1<-function(x=data, wave=1,text="data not available ",orglist=tldel_list){
  #   own_tldel1 <- reactive({
  #     
  #     df <- x %>% 
  #       filter(Wave==wave) %>%
  #       
  #       filter(Organization %in% reactive_values$organization) %>% 
  #       select(starts_with(orglist$Variable)
  #       ) %>% mutate(
  #         
  #         MEMBERAGMATTENDANCE         = formatC(MEMBERAGMATTENDANCE, format="f", big.mark=",", digits = 2),
  #         DELEGAGMATTENDANCE         = formatC(DELEGAGMATTENDANCE, format="f", big.mark=",", digits = 2),
  #         ONLINEATTENDANCE         = formatC(ONLINEATTENDANCE, format="f", big.mark=",", digits = 2),
  #         ONLINEATTENDANCE_POSTC19         = formatC(ONLINEATTENDANCE_POSTC19, format="f", big.mark=",", digits = 2),
  #         LASTAGMDURATION         = formatC(LASTAGMDURATION, format="f", big.mark=",", digits = 2),
  #         ELECTIONSPAST10         = formatC(ELECTIONSPAST10, format="f", big.mark=",", digits = 2),
  #         CONTESTEDSEATS         = formatC(CONTESTEDSEATS, format="f", big.mark=",", digits = 2)
  #       ) 
  #     
  #     
  #     #Transpose df 
  #     if (nrow(df)==0) {
  #       own_tldel1 <-  as_tibble(t(df), rownames = "Variable")
  #       own_tldel1$V1<-text
  #     }else {
  #       own_tldel1 <-  as_tibble(t(df), rownames = "Variable")
  #     }
  #     return(own_tldel1)
  #     
  #   })
  # }
  # peer_tldel<-function(x=data1,tb=peergroups_selected_Wave1(),orglist=tldel_list){
  #   peer_tldel <- reactive({
  #     
  #     df <-   x %>%
  #       filter(Organization %in% tb$Organization) %>%
  #       select(starts_with(orglist$Variable))
  #     WHOATTENDAGM_df<-summarydatasort (df,"WHOATTENDAGM")
  #     HOWVOTEC1_df<-summarydata (df,"HOWVOTEC1")
  #     HOWVOTEC2_df<-summarydata (df,"HOWVOTEC2")
  #     HOWVOTEC3_df<-summarydata (df,"HOWVOTEC3")
  #     HOWVOTEC4_df<-summarydata (df,"HOWVOTEC4")
  #     HOWVOTEC5_df<-summarydata (df,"HOWVOTEC5")
  #     BOARDELECTEDMEMBERS_df<-summarydatasort (df,"BOARDELECTEDMEMBERS")
  #     STAGGEREDELECTIONS_df<-summarydata (df,"STAGGEREDELECTIONS")
  #     DEMOCRATICPRINCIPLE_df<-summarydata (df,"DEMOCRATICPRINCIPLE")
  #     OTHERDEMOCRATICPRACTICE_df<-summarydata (df,"OTHERDEMOCRATICPRACTICE")
  #     peer_tldel<-  df %>% 
  #       summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
  #                 
  #                 
  #       ) %>%
  #       
  #       mutate(
  #         WHOATTENDAGM        = ifelse(length(WHOATTENDAGM_df) == 0, "No Response", WHOATTENDAGM_df$percentage)
  #         ,MEMBERAGMATTENDANCE         = formatC(MEMBERAGMATTENDANCE, format="f", big.mark=",", digits = 2)
  #         ,DELEGAGMATTENDANCE         = formatC(DELEGAGMATTENDANCE, format="f", big.mark=",", digits = 2)
  #         ,ONLINEATTENDANCE         = formatC(ONLINEATTENDANCE, format="f", big.mark=",", digits = 2)
  #         , ONLINEATTENDANCE_POSTC19         = formatC(ONLINEATTENDANCE_POSTC19, format="f", big.mark=",", digits = 2)
  #         #,HOWVOTEO = NA
  #         ,HOWVOTEC1        = ifelse(length(HOWVOTEC1_df) == 0, "No Response", HOWVOTEC1_df$percentage)
  #         ,HOWVOTEC2        = ifelse(length(HOWVOTEC2_df) == 0, "No Response", HOWVOTEC2_df$percentage)
  #         ,HOWVOTEC3        = ifelse(length(HOWVOTEC3_df) == 0, "No Response", HOWVOTEC3_df$percentage)
  #         ,HOWVOTEC4        = ifelse(length(HOWVOTEC4_df) == 0, "No Response", HOWVOTEC4_df$percentage)
  #         ,HOWVOTEC5        = ifelse(length(HOWVOTEC5_df) == 0, "No Response", HOWVOTEC5_df$percentage)
  #         ,LASTAGMDURATION         = formatC(LASTAGMDURATION, format="f", big.mark=",", digits = 2)
  #         ,BOARDELECTEDMEMBERS   = ifelse(length(BOARDELECTEDMEMBERS_df) == 0, "No Response", BOARDELECTEDMEMBERS_df$percentage)
  #         ,STAGGEREDELECTIONS   = ifelse(length(STAGGEREDELECTIONS_df) == 0, "No Response", STAGGEREDELECTIONS_df$percentage)
  #         ,ELECTIONSPAST10         = formatC(ELECTIONSPAST10, format="f", big.mark=",", digits = 2)
  #         ,CONTESTEDSEATS         = formatC(CONTESTEDSEATS, format="f", big.mark=",", digits = 2)
  #         ,DEMOCRATICPRINCIPLE        = ifelse(length(DEMOCRATICPRINCIPLE_df) == 0, "No Response", DEMOCRATICPRINCIPLE_df$percentage)
  #         ,OTHERDEMOCRATICPRACTICE   = ifelse(length(OTHERDEMOCRATICPRACTICE_df) == 0, "No Response", OTHERDEMOCRATICPRACTICE_df$percentage)
  #         
  #         
  #         
  #       ) %>% mutate_all(~ifelse(is.nan(.), NA, .)) #%>% replace(is.na(.), "NA") 
  #     
  #   })    
  #   
  #   
  #   peer_tldel1 <-  reactive({as_tibble(t(peer_tldel()), rownames = "Variable")
  #   }) 
  #   
  #   
  #   
  #   peer_tldel2 <- reactive({
  #     peer_tldel1() %>% inner_join(orglist, by = "Variable") %>% 
  #       select("Variable", 
  #              #"Label",
  #              "V1") %>% rename( "Peer Group" = "V1"
  #                                #,"Measure"             = "Label"
  #              )
  #   })  
  #   return(peer_tldel2)
  # }
  # own_tldel1_wave1<-own_tldel1(x=data1, wave=1,text="data not available ",orglist=tldel_list)
  # own_tldel_df_wave1<-own_org_profile2(ownorg=own_tldel1_wave1(),orglist=tldel_list, varlist=tldel_varlist())
  # peer_tldel_wave1<-peer_tldel(x=data1,tb=peergroups_selected_Wave1(),orglist=tldel_list)
  # tldel_df1_wave1<-org_profile_df1(data1=own_tldel_df_wave1(), data2=peer_tldel_wave1())
  # own_tldel1_wave2<-own_tldel1(x=data1, wave=2,text="data not available ",orglist=tldel_list)
  # own_tldel_df_wave2<-own_org_profile2(ownorg=own_tldel1_wave2(),orglist=tldel_list, varlist=tldel_varlist())
  # peer_tldel_wave2<-peer_tldel(x=data1,tb=peergroups_selected_Wave2(),orglist=tldel_list)
  # tldel_df1_wave2<-org_profile_df1(data1=own_tldel_df_wave2(), data2=peer_tldel_wave2())
  # ~~~ Delegates~~~ ----  
  del_list_wave1 <- selectors_tbl %>%
    filter(str_detect(Category,"^Delegates")) %>% 
    arrange(Order) %>%
    filter(is.na(Wave)==TRUE)
  del_list_wave2 <- selectors_tbl %>%
    filter(str_detect(Category,"^Delegates")) %>% arrange(Order)
  
  own_del1_wave1 <- reactive({
    
    df <- data1 %>% 
      filter(Wave==1) %>%
      
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(del_list_wave1$Variable)
      ) %>% mutate(
        NUMBEROFDELEGATES         = formatC(NUMBEROFDELEGATES, format="f", big.mark=",", digits = 2),
        AGEOFDELEG                = formatC(AGEOFDELEG, format="f", big.mark=",", digits = 2),
        HOWLONGDELEGSERVE         = formatC(HOWLONGDELEGSERVE, format="f", big.mark=",", digits = 2),
        DELEGMEMBERS        = formatC(DELEGMEMBERS, format="f", big.mark=",", digits = 2),
        DELEGEMPLOYEES        = formatC(DELEGEMPLOYEES, format="f", big.mark=",", digits = 2),
        DELEGNONELECT       = formatC(DELEGNONELECT, format="f", big.mark=",", digits = 2),
        DELEGFEMALE         = formatC(DELEGFEMALE, format="f", big.mark=",", digits = 2),
        DELEGMINORITY        = formatC(DELEGMINORITY, format="f", big.mark=",", digits = 2),
        DELEGINDIGENOUS        = formatC(DELEGINDIGENOUS, format="f", big.mark=",", digits = 2),
        DELEGEXECUTIVE         = formatC(DELEGEXECUTIVE, format="f", big.mark=",", digits = 2),
        DELEGINDUSTRY        = formatC(DELEGINDUSTRY, format="f", big.mark=",", digits = 2),
        DELEGATECOMPENSATIONAMOUNT_A1         = scales::dollar(DELEGATECOMPENSATIONAMOUNT_A1),
        DELEGATECOMPENSATIONAMOUNT_A2          = scales::dollar(DELEGATECOMPENSATIONAMOUNT_A2),
        DELEGATECOMPENSATIONAMOUNT_A3          = scales::dollar(DELEGATECOMPENSATIONAMOUNT_A3),
        DELEGATECOMPENSATIONAMOUNT_A4          = scales::dollar(DELEGATECOMPENSATIONAMOUNT_A4),
        
        REIMBURSEMENTDELEGATES_A1          = scales::dollar(REIMBURSEMENTDELEGATES_A1),
        REIMBURSEMENTDELEGATES_A2          = scales::dollar(REIMBURSEMENTDELEGATES_A2),
        REIMBURSEMENTDELEGATES_A3          = scales::dollar(REIMBURSEMENTDELEGATES_A3),
        REIMBURSEMENTDELEGATES_A4          = scales::dollar(REIMBURSEMENTDELEGATES_A4),
        REIMBURSEMENTDELEGATES_A5          = scales::dollar(REIMBURSEMENTDELEGATES_A5),
        REIMBURSEMENTDELEGATES_A6          = scales::dollar(REIMBURSEMENTDELEGATES_A6),
        TOTALDELEGCOMPENSATION          = scales::dollar(TOTALDELEGCOMPENSATION),
        DELEGRUNREELECTION         = formatC(DELEGRUNREELECTION, format="f", big.mark=",", digits = 2),
        DELEGMAXIMUMSERVICE        = formatC(DELEGMAXIMUMSERVICE, format="f", big.mark=",", digits = 2),
        DELEGRETIREMENTAGE        = formatC(DELEGRETIREMENTAGE, format="f", big.mark=",", digits = 2)
        
      ) 
    
    
    #Transpose df 
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_del1_wave1<-cbind(df1,V1)
    }else {
      own_del1_wave1 <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_del1_wave1 <-  as_tibble(t(df), rownames = "Variable")
    
  })     
  
  
  own_del2_wave1 <- reactive({
    own_del1_wave1()%>% inner_join(del_list_wave1, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation")
    
  }) 
  
  
  
  
  
  del_df_wave1 <- reactive({
    
    variables_selected() %>% filter(str_detect(Category,"^Delegates")) %>%  
      inner_join(own_del2_wave1(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
    
  }) 
  
  
  
  
  peer_del_wave1 <- reactive({
    
    df <-   data1 %>% filter(Organization %in% peergroups_selected_Wave1()$Organization) %>%
      filter(Wave==1) %>%
      select(starts_with(del_list_wave1$Variable))
    
    DELEGATECOMPENSATION_df<-summarydata (df,"DELEGATECOMPENSATION")
    OTHERCOMPENSATION_df<-summarydata (df,"OTHERCOMPENSATION")
    REIMBURSEDELEGATESOTHER_df<-summarydata (df,"REIMBURSEDELEGATESOTHER")
    DELEGRETIREREQUIREMENT_df<-summarydata (df,"DELEGRETIREREQUIREMENT")
    DELEGATESPOUSETRAVEL_df<-summarydata (df,"DELEGATESPOUSETRAVEL")
    DELEGTERMLIMITS_df<-summarydata (df,"DELEGTERMLIMITS")
    TOTALDELEGLIMIT_df<-summarydata (df,"TOTALDELEGLIMIT")
    HOWVOTEDELEGC1_df<-summarydata (df,"HOWVOTEDELEGC1")
    HOWVOTEDELEGC2_df<-summarydata (df,"HOWVOTEDELEGC2")
    HOWVOTEDELEGC3_df<-summarydata (df,"HOWVOTEDELEGC3")
    HOWVOTEDELEGC4_df<-summarydata (df,"HOWVOTEDELEGC4")
    HOWVOTEDELEGC5_df<-summarydata (df,"HOWVOTEDELEGC5")
    NUMBEROFDELEGATES_df<-meandata (df,NUMBEROFDELEGATES,0)
    AGEOFDELEG_df<-meandata (df,AGEOFDELEG,0)
    HOWLONGDELEGSERVE_df<-meandata (df,HOWLONGDELEGSERVE,0)
    DELEGMEMBERS_df<-meandata (df,DELEGMEMBERS,0)
    DELEGEMPLOYEES_df<-meandata (df,DELEGEMPLOYEES,0)
    DELEGNONELECT_df<-meandata (df,DELEGNONELECT,0)
    DELEGFEMALE_df<-meandata (df,DELEGFEMALE,0)
    DELEGMINORITY_df<-meandata (df,DELEGMINORITY,0)
    DELEGINDIGENOUS_df<-meandata (df,DELEGINDIGENOUS,0)
    DELEGEXECUTIVE_df<-meandata (df,DELEGEXECUTIVE,0)
    DELEGINDUSTRY_df<-meandata (df,DELEGINDUSTRY,0)
    DELEGATECOMPENSATIONAMOUNT_A1_df<-meandata (df,DELEGATECOMPENSATIONAMOUNT_A1,1)
    DELEGATECOMPENSATIONAMOUNT_A2_df<-meandata (df,DELEGATECOMPENSATIONAMOUNT_A2,1)
    DELEGATECOMPENSATIONAMOUNT_A3_df<-meandata (df,DELEGATECOMPENSATIONAMOUNT_A3,1)
    DELEGATECOMPENSATIONAMOUNT_A4_df<-meandata (df,DELEGATECOMPENSATIONAMOUNT_A4,1)
    REIMBURSEMENTDELEGATES_A1_df<-meandata (df,REIMBURSEMENTDELEGATES_A1,1)
    REIMBURSEMENTDELEGATES_A2_df<-meandata (df,REIMBURSEMENTDELEGATES_A2,1)
    REIMBURSEMENTDELEGATES_A3_df<-meandata (df,REIMBURSEMENTDELEGATES_A3,1)
    REIMBURSEMENTDELEGATES_A4_df<-meandata (df,REIMBURSEMENTDELEGATES_A4,1)
    REIMBURSEMENTDELEGATES_A5_df<-meandata (df,REIMBURSEMENTDELEGATES_A5,1)
    REIMBURSEMENTDELEGATES_A6_df<-meandata (df,REIMBURSEMENTDELEGATES_A6,1)
    TOTALDELEGCOMPENSATION_df<-meandata (df,TOTALDELEGCOMPENSATION,1)
    DELEGRUNREELECTION_df<-meandata (df,DELEGRUNREELECTION,0)
    DELEGMAXIMUMSERVICE_df<-meandata (df,DELEGMAXIMUMSERVICE,0)
    DELEGRETIREMENTAGE_df<-meandata (df,DELEGRETIREMENTAGE,0)
    peer_del_wave1<-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
      ) %>%  
      
      
      mutate(
        NUMBEROFDELEGATES         = NUMBEROFDELEGATES_df$percentage,
        AGEOFDELEG                = AGEOFDELEG_df$percentage,
        HOWLONGDELEGSERVE         = HOWLONGDELEGSERVE_df$percentage,
        DELEGMEMBERS              = DELEGMEMBERS_df$percentage,
        DELEGEMPLOYEES            = DELEGEMPLOYEES_df$percentage,
        DELEGNONELECT             = DELEGNONELECT_df$percentage,
        DELEGFEMALE               = DELEGFEMALE_df$percentage,
        DELEGMINORITY             = DELEGMINORITY_df$percentage,
        DELEGINDIGENOUS           = DELEGINDIGENOUS_df$percentage,
        DELEGEXECUTIVE            = DELEGEXECUTIVE_df$percentage,
        DELEGINDUSTRY             = DELEGINDUSTRY_df$percentage,
        DELEGATECOMPENSATION      = ifelse(length(DELEGATECOMPENSATION_df) == 0, "No Response", DELEGATECOMPENSATION_df$percentage),
        DELEGATECOMPENSATIONAMOUNT_A1          = DELEGATECOMPENSATIONAMOUNT_A1_df$percentage,
        DELEGATECOMPENSATIONAMOUNT_A2          = DELEGATECOMPENSATIONAMOUNT_A2_df$percentage,
        DELEGATECOMPENSATIONAMOUNT_A3          = DELEGATECOMPENSATIONAMOUNT_A3_df$percentage,
        DELEGATECOMPENSATIONAMOUNT_A4          = DELEGATECOMPENSATIONAMOUNT_A4_df$percentage,
        OTHERCOMPENSATION                      = ifelse(length(OTHERCOMPENSATION_df) == 0, "No Response", OTHERCOMPENSATION_df$percentage),
        REIMBURSEMENTDELEGATES_A1          = REIMBURSEMENTDELEGATES_A1_df$percentage,
        REIMBURSEMENTDELEGATES_A2          = REIMBURSEMENTDELEGATES_A2_df$percentage,
        REIMBURSEMENTDELEGATES_A3          = REIMBURSEMENTDELEGATES_A3_df$percentage,
        REIMBURSEMENTDELEGATES_A4          = REIMBURSEMENTDELEGATES_A4_df$percentage,
        REIMBURSEMENTDELEGATES_A5          = REIMBURSEMENTDELEGATES_A5_df$percentage,
        REIMBURSEMENTDELEGATES_A6          = REIMBURSEMENTDELEGATES_A6_df$percentage,
        REIMBURSEDELEGATESOTHER        = ifelse(length(REIMBURSEDELEGATESOTHER_df) == 0, "No Response", REIMBURSEDELEGATESOTHER_df$percentage),
        DELEGATESPOUSETRAVEL        = ifelse(length(DELEGATESPOUSETRAVEL_df) == 0, "No Response", DELEGATESPOUSETRAVEL_df$percentage),
        TOTALDELEGCOMPENSATION          = TOTALDELEGCOMPENSATION_df$percentage,
        DELEGRUNREELECTION         = DELEGRUNREELECTION_df$percentage,
        DELEGMAXIMUMSERVICE        = DELEGMAXIMUMSERVICE_df$percentage,
        DELEGRETIREREQUIREMENT        = ifelse(length(DELEGRETIREREQUIREMENT_df) == 0, "No Response", DELEGRETIREREQUIREMENT_df$percentage),
        DELEGRETIREMENTAGE        = DELEGRETIREMENTAGE_df$percentage,
        # ,DELEGTERMLIMITS        = NA,
        DELEGTERMLIMITS        = ifelse(length(DELEGTERMLIMITS_df) == 0, "No Response", DELEGTERMLIMITS_df$percentage),
        TOTALDELEGLIMIT        = ifelse(length(TOTALDELEGLIMIT_df) == 0, "No Response", TOTALDELEGLIMIT_df$percentage),
        HOWVOTEDELEGC1        = ifelse(length(HOWVOTEDELEGC1_df) == 0, "No Response", HOWVOTEDELEGC1_df$percentage),
        HOWVOTEDELEGC2        = ifelse(length(HOWVOTEDELEGC2_df) == 0, "No Response", HOWVOTEDELEGC2_df$percentage),
        HOWVOTEDELEGC3        = ifelse(length(HOWVOTEDELEGC3_df) == 0, "No Response", HOWVOTEDELEGC3_df$percentage),
        HOWVOTEDELEGC4        = ifelse(length(HOWVOTEDELEGC4_df) == 0, "No Response", HOWVOTEDELEGC4_df$percentage),
        HOWVOTEDELEGC5        = ifelse(length(HOWVOTEDELEGC5_df) == 0, "No Response", HOWVOTEDELEGC5_df$percentage),
        
        
      )  %>% mutate_all(~ifelse(is.nan(.), NA, .)) %>% replace(is.na(.), "NA") 
    
  })
  
  peer_del1_wave1<-reactive({
    if (nrow(peer_del_wave1())==0) {
      
      df1<-as_tibble(t(peer_del_wave1()), rownames = "Variable")
      V1<-"text"
      peer_del1_wave1<-cbind(df1,V1)
    }else {
      peer_del1_wave1 <-  as_tibble(t(peer_del_wave1()), rownames = "Variable")
    }
  })
  
  # peer_del1_wave1 <-  reactive({as_tibble(t(peer_del_wave1()), rownames = "Variable")
  # })  
  
  
  
  peer_del2_wave1 <- reactive({
    peer_del1_wave1() %>% inner_join(del_list_wave1, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })  
  
  
  
  
  
  del_df1_wave1 <- reactive({
    
    del_df_wave1() %>% 
      left_join(peer_del2_wave1(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  })  
  
  own_del1_wave2 <- reactive({
    
    df <- data1 %>% 
      filter(Wave==2) %>%
      
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(del_list_wave2$Variable)
      ) %>% mutate(
        NUMBEROFDELEGATES         = formatC(NUMBEROFDELEGATES, format="f", big.mark=",", digits = 2),
        AGEOFDELEG                = formatC(AGEOFDELEG, format="f", big.mark=",", digits = 2),
        HOWLONGDELEGSERVE         = formatC(HOWLONGDELEGSERVE, format="f", big.mark=",", digits = 2),
        DELEGMEMBERS        = formatC(DELEGMEMBERS, format="f", big.mark=",", digits = 2),
        DELEGEMPLOYEES        = formatC(DELEGEMPLOYEES, format="f", big.mark=",", digits = 2),
        DELEGNONELECT       = formatC(DELEGNONELECT, format="f", big.mark=",", digits = 2),
        DELEGFEMALE         = formatC(DELEGFEMALE, format="f", big.mark=",", digits = 2),
        DELEGMINORITY        = formatC(DELEGMINORITY, format="f", big.mark=",", digits = 2),
        DELEGINDIGENOUS        = formatC(DELEGINDIGENOUS, format="f", big.mark=",", digits = 2),
        DELEGEXECUTIVE         = formatC(DELEGEXECUTIVE, format="f", big.mark=",", digits = 2),
        DELEGINDUSTRY        = formatC(DELEGINDUSTRY, format="f", big.mark=",", digits = 2),
        DELEGATECOMPENSATIONAMOUNT_A1         = scales::dollar(DELEGATECOMPENSATIONAMOUNT_A1),
        DELEGATECOMPENSATIONAMOUNT_A2          = scales::dollar(DELEGATECOMPENSATIONAMOUNT_A2),
        DELEGATECOMPENSATIONAMOUNT_A3          = scales::dollar(DELEGATECOMPENSATIONAMOUNT_A3),
        DELEGATECOMPENSATIONAMOUNT_A4          = scales::dollar(DELEGATECOMPENSATIONAMOUNT_A4),
        
        REIMBURSEMENTDELEGATES_A1          = scales::dollar(REIMBURSEMENTDELEGATES_A1),
        REIMBURSEMENTDELEGATES_A2          = scales::dollar(REIMBURSEMENTDELEGATES_A2),
        REIMBURSEMENTDELEGATES_A3          = scales::dollar(REIMBURSEMENTDELEGATES_A3),
        REIMBURSEMENTDELEGATES_A4          = scales::dollar(REIMBURSEMENTDELEGATES_A4),
        REIMBURSEMENTDELEGATES_A5          = scales::dollar(REIMBURSEMENTDELEGATES_A5),
        REIMBURSEMENTDELEGATES_A6          = scales::dollar(REIMBURSEMENTDELEGATES_A6),
        TOTALDELEGCOMPENSATION          = scales::dollar(TOTALDELEGCOMPENSATION),
        DELEGRUNREELECTION         = formatC(DELEGRUNREELECTION, format="f", big.mark=",", digits = 2),
        DELEGMAXIMUMSERVICE        = formatC(DELEGMAXIMUMSERVICE, format="f", big.mark=",", digits = 2),
        DELEGRETIREMENTAGE        = formatC(DELEGRETIREMENTAGE, format="f", big.mark=",", digits = 2)
        
      ) 
    
    
    #Transpose df 
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_del1_wave2<-cbind(df1,V1)
    }else {
      own_del1_wave2 <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_del1_wave2 <-  as_tibble(t(df), rownames = "Variable")
    
  })     
  
  
  own_del2_wave2 <- reactive({
    own_del1_wave2()%>% inner_join(del_list_wave2, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation")
    
  }) 
  
  
  
  
  
  del_df_wave2 <- reactive({
    
    variables_selected() %>% filter(str_detect(Category,"^Delegates")) %>%  
      inner_join(own_del2_wave2(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
    
  }) 
  
  
  
  
  peer_del_wave2 <- reactive({
    
    df <-   data1 %>% filter(Organization %in% peergroups_selected_Wave2()$Organization) %>%
      filter(Wave==2) %>%
      select(starts_with(del_list_wave2$Variable))
    
    DELEGATECOMPENSATION_df<-summarydata (df,"DELEGATECOMPENSATION")
    OTHERCOMPENSATION_df<-summarydata (df,"OTHERCOMPENSATION")
    REIMBURSEDELEGATESOTHER_df<-summarydata (df,"REIMBURSEDELEGATESOTHER")
    DELEGRETIREREQUIREMENT_df<-summarydata (df,"DELEGRETIREREQUIREMENT")
    DELEGATESPOUSETRAVEL_df<-summarydata (df,"DELEGATESPOUSETRAVEL")
    DELEGTERMLIMITS_df<-summarydata (df,"DELEGTERMLIMITS")
    TOTALDELEGLIMIT_df<-summarydata (df,"TOTALDELEGLIMIT")
    HOWVOTEDELEGC1_df<-summarydata (df,"HOWVOTEDELEGC1")
    HOWVOTEDELEGC2_df<-summarydata (df,"HOWVOTEDELEGC2")
    HOWVOTEDELEGC3_df<-summarydata (df,"HOWVOTEDELEGC3")
    HOWVOTEDELEGC4_df<-summarydata (df,"HOWVOTEDELEGC4")
    HOWVOTEDELEGC5_df<-summarydata (df,"HOWVOTEDELEGC5")
    NUMBEROFDELEGATES_df<-meandata (df,NUMBEROFDELEGATES,0)
    AGEOFDELEG_df<-meandata (df,AGEOFDELEG,0)
    HOWLONGDELEGSERVE_df<-meandata (df,HOWLONGDELEGSERVE,0)
    DELEGMEMBERS_df<-meandata (df,DELEGMEMBERS,0)
    DELEGEMPLOYEES_df<-meandata (df,DELEGEMPLOYEES,0)
    DELEGNONELECT_df<-meandata (df,DELEGNONELECT,0)
    DELEGFEMALE_df<-meandata (df,DELEGFEMALE,0)
    DELEGMINORITY_df<-meandata (df,DELEGMINORITY,0)
    DELEGINDIGENOUS_df<-meandata (df,DELEGINDIGENOUS,0)
    DELEGEXECUTIVE_df<-meandata (df,DELEGEXECUTIVE,0)
    DELEGINDUSTRY_df<-meandata (df,DELEGINDUSTRY,0)
    DELEGATECOMPENSATIONAMOUNT_A1_df<-meandata (df,DELEGATECOMPENSATIONAMOUNT_A1,1)
    DELEGATECOMPENSATIONAMOUNT_A2_df<-meandata (df,DELEGATECOMPENSATIONAMOUNT_A2,1)
    DELEGATECOMPENSATIONAMOUNT_A3_df<-meandata (df,DELEGATECOMPENSATIONAMOUNT_A3,1)
    DELEGATECOMPENSATIONAMOUNT_A4_df<-meandata (df,DELEGATECOMPENSATIONAMOUNT_A4,1)
    REIMBURSEMENTDELEGATES_A1_df<-meandata (df,REIMBURSEMENTDELEGATES_A1,1)
    REIMBURSEMENTDELEGATES_A2_df<-meandata (df,REIMBURSEMENTDELEGATES_A2,1)
    REIMBURSEMENTDELEGATES_A3_df<-meandata (df,REIMBURSEMENTDELEGATES_A3,1)
    REIMBURSEMENTDELEGATES_A4_df<-meandata (df,REIMBURSEMENTDELEGATES_A4,1)
    REIMBURSEMENTDELEGATES_A5_df<-meandata (df,REIMBURSEMENTDELEGATES_A5,1)
    REIMBURSEMENTDELEGATES_A6_df<-meandata (df,REIMBURSEMENTDELEGATES_A6,1)
    TOTALDELEGCOMPENSATION_df<-meandata (df,TOTALDELEGCOMPENSATION,1)
    DELEGRUNREELECTION_df<-meandata (df,DELEGRUNREELECTION,0)
    DELEGMAXIMUMSERVICE_df<-meandata (df,DELEGMAXIMUMSERVICE,0)
    DELEGRETIREMENTAGE_df<-meandata (df,DELEGRETIREMENTAGE,0)
    peer_del_wave2<-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
      ) %>%  
      
      
      mutate(
        NUMBEROFDELEGATES         = NUMBEROFDELEGATES_df$percentage,
        AGEOFDELEG                = AGEOFDELEG_df$percentage,
        HOWLONGDELEGSERVE         = HOWLONGDELEGSERVE_df$percentage,
        DELEGMEMBERS     = DELEGMEMBERS_df$percentage,
        DELEGEMPLOYEES = DELEGEMPLOYEES_df$percentage,
        DELEGNONELECT       = DELEGNONELECT_df$percentage,
        DELEGFEMALE         = DELEGFEMALE_df$percentage,
        DELEGMINORITY        = DELEGMINORITY_df$percentage,
        DELEGINDIGENOUS        = DELEGINDIGENOUS_df$percentage,
        DELEGEXECUTIVE         = DELEGEXECUTIVE_df$percentage,
        DELEGINDUSTRY        = DELEGINDUSTRY_df$percentage,
        DELEGATECOMPENSATION        = ifelse(length(DELEGATECOMPENSATION_df) == 0, "No Response", DELEGATECOMPENSATION_df$percentage),
        DELEGATECOMPENSATIONAMOUNT_A1         = DELEGATECOMPENSATIONAMOUNT_A1_df$percentage,
        DELEGATECOMPENSATIONAMOUNT_A2          = DELEGATECOMPENSATIONAMOUNT_A2_df$percentage,
        DELEGATECOMPENSATIONAMOUNT_A3          = DELEGATECOMPENSATIONAMOUNT_A3_df$percentage,
        DELEGATECOMPENSATIONAMOUNT_A4          = DELEGATECOMPENSATIONAMOUNT_A4_df$percentage,
        OTHERCOMPENSATION        = ifelse(length(OTHERCOMPENSATION_df) == 0, "No Response", OTHERCOMPENSATION_df$percentage),
        REIMBURSEMENTDELEGATES_A1          = REIMBURSEMENTDELEGATES_A1_df$percentage,
        REIMBURSEMENTDELEGATES_A2          = REIMBURSEMENTDELEGATES_A2_df$percentage,
        REIMBURSEMENTDELEGATES_A3          = REIMBURSEMENTDELEGATES_A3_df$percentage,
        REIMBURSEMENTDELEGATES_A4          = REIMBURSEMENTDELEGATES_A4_df$percentage,
        REIMBURSEMENTDELEGATES_A5          = REIMBURSEMENTDELEGATES_A5_df$percentage,
        REIMBURSEMENTDELEGATES_A6          = REIMBURSEMENTDELEGATES_A6_df$percentage,
        REIMBURSEDELEGATESOTHER        = ifelse(length(REIMBURSEDELEGATESOTHER_df) == 0, "No Response", REIMBURSEDELEGATESOTHER_df$percentage),
        DELEGATESPOUSETRAVEL        = ifelse(length(DELEGATESPOUSETRAVEL_df) == 0, "No Response", DELEGATESPOUSETRAVEL_df$percentage),
        TOTALDELEGCOMPENSATION          = TOTALDELEGCOMPENSATION_df$percentage,
        DELEGRUNREELECTION         = DELEGRUNREELECTION_df$percentage,
        DELEGMAXIMUMSERVICE        = DELEGMAXIMUMSERVICE_df$percentage,
        DELEGRETIREREQUIREMENT        = ifelse(length(DELEGRETIREREQUIREMENT_df) == 0, "No Response", DELEGRETIREREQUIREMENT_df$percentage),
        DELEGRETIREMENTAGE        = DELEGRETIREMENTAGE_df$percentage,
        DELEGTERMLIMITS        = ifelse(length(DELEGTERMLIMITS_df) == 0, "No Response", DELEGTERMLIMITS_df$percentage),
        TOTALDELEGLIMIT        = ifelse(length(TOTALDELEGLIMIT_df) == 0, "No Response", TOTALDELEGLIMIT_df$percentage),
        HOWVOTEDELEGC1        = ifelse(length(HOWVOTEDELEGC1_df) == 0, "No Response", HOWVOTEDELEGC1_df$percentage),
        HOWVOTEDELEGC2        = ifelse(length(HOWVOTEDELEGC2_df) == 0, "No Response", HOWVOTEDELEGC2_df$percentage),
        HOWVOTEDELEGC3        = ifelse(length(HOWVOTEDELEGC3_df) == 0, "No Response", HOWVOTEDELEGC3_df$percentage),
        HOWVOTEDELEGC4        = ifelse(length(HOWVOTEDELEGC4_df) == 0, "No Response", HOWVOTEDELEGC4_df$percentage),
        HOWVOTEDELEGC5        = ifelse(length(HOWVOTEDELEGC5_df) == 0, "No Response", HOWVOTEDELEGC5_df$percentage),
        
      )  %>% mutate_all(~ifelse(is.nan(.), NA, .)) %>% replace(is.na(.), "NA") 
    
  })
  
  peer_del1_wave2<-reactive({
    if (nrow(peer_del_wave2())==0) {
      
      df1<-as_tibble(t(peer_del_wave2()), rownames = "Variable")
      V1<-"text"
      peer_del1_wave2<-cbind(df1,V1)
    }else {
      peer_del1_wave2 <-  as_tibble(t(peer_del_wave2()), rownames = "Variable")
    }
  })
  
  # peer_del1_wave2 <-  reactive({as_tibble(t(peer_del_wave2()), rownames = "Variable")
  # })  
  
  
  
  peer_del2_wave2 <- reactive({
    peer_del1_wave2() %>% inner_join(del_list_wave2, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })  
  
  
  
  
  
  del_df1_wave2 <- reactive({
    
    del_df_wave2() %>% 
      left_join(peer_del2_wave2(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  })  
  
  # del_varlist <- reactive({
  #   df<-selectors_tbl[selected1(),1:2 ] # Variable, Category 
  #   del_varlist <-df%>%
  #     filter(str_detect(Category,"^Delegates"))  
  # })
  # 
  # own_del1<-function(x=data, wave=1,text="data not available ",orglist=del_list){
  #   own_del1 <- reactive({
  #     
  #     df <- x %>% 
  #       filter(Wave==wave) %>%
  #       
  #       filter(Organization %in% reactive_values$organization) %>% 
  #       select(starts_with(orglist$Variable)) %>% 
  #       mutate(
  #         NUMBEROFDELEGATES         = formatC(NUMBEROFDELEGATES, format="f", big.mark=",", digits = 2),
  #         DELEGMEMBERS        = formatC(DELEGMEMBERS, format="f", big.mark=",", digits = 2),
  #         DELEGEMPLOYEES        = formatC(DELEGEMPLOYEES, format="f", big.mark=",", digits = 2),
  #         DELEGNONELECT       = formatC(DELEGNONELECT, format="f", big.mark=",", digits = 2),
  #         DELEGFEMALE         = formatC(DELEGFEMALE, format="f", big.mark=",", digits = 2),
  #         DELEGMINORITY        = formatC(DELEGMINORITY, format="f", big.mark=",", digits = 2),
  #         DELEGINDIGENOUS        = formatC(DELEGINDIGENOUS, format="f", big.mark=",", digits = 2),
  #         DELEGEXECUTIVE         = formatC(DELEGEXECUTIVE, format="f", big.mark=",", digits = 2),
  #         DELEGINDUSTRY        = formatC(DELEGINDUSTRY, format="f", big.mark=",", digits = 2),
  #         DELEGATECOMPENSATIONAMOUNT_A1         = scales::dollar(DELEGATECOMPENSATIONAMOUNT_A1),
  #         DELEGATECOMPENSATIONAMOUNT_A2          = scales::dollar(DELEGATECOMPENSATIONAMOUNT_A2),
  #         DELEGATECOMPENSATIONAMOUNT_A3          = scales::dollar(DELEGATECOMPENSATIONAMOUNT_A3),
  #         DELEGATECOMPENSATIONAMOUNT_A4          = scales::dollar(DELEGATECOMPENSATIONAMOUNT_A4),
  #         
  #         REIMBURSEMENTDELEGATES_A1          = scales::dollar(REIMBURSEMENTDELEGATES_A1),
  #         REIMBURSEMENTDELEGATES_A2          = scales::dollar(REIMBURSEMENTDELEGATES_A2),
  #         REIMBURSEMENTDELEGATES_A3          = scales::dollar(REIMBURSEMENTDELEGATES_A3),
  #         REIMBURSEMENTDELEGATES_A4          = scales::dollar(REIMBURSEMENTDELEGATES_A4),
  #         REIMBURSEMENTDELEGATES_A5          = scales::dollar(REIMBURSEMENTDELEGATES_A5),
  #         REIMBURSEMENTDELEGATES_A6          = scales::dollar(REIMBURSEMENTDELEGATES_A6),
  #         TOTALDELEGCOMPENSATION          = scales::dollar(TOTALDELEGCOMPENSATION),
  #         DELEGRUNREELECTION         = formatC(DELEGRUNREELECTION, format="f", big.mark=",", digits = 2),
  #         DELEGMAXIMUMSERVICE        = formatC(DELEGMAXIMUMSERVICE, format="f", big.mark=",", digits = 2),
  #         DELEGRETIREMENTAGE        = formatC(DELEGRETIREMENTAGE, format="f", big.mark=",", digits = 2)
  #         
  #       ) 
  #     
  #     
  #     #Transpose df 
  #     if (nrow(df)==0) {
  #       own_del1 <-  as_tibble(t(df), rownames = "Variable")
  #       own_del1$V1<-text
  #     }else {
  #       own_del1 <-  as_tibble(t(df), rownames = "Variable")
  #     }
  #     return(own_del1)
  #   })
  # }
  # 
  # 
  # peer_del<-function(x=data1,tb=peergroups_selected_Wave1(),orglist=del_list){
  #   peer_del <- reactive({
  #     
  #     df <-   data1 %>% filter(Organization %in% tb$Organization) %>%
  #       select(starts_with(orglist$Variable))
  #     
  #     DELEGATECOMPENSATION_df<-summarydata (df,"DELEGATECOMPENSATION")
  #     OTHERCOMPENSATION_df<-summarydata (df,"OTHERCOMPENSATION")
  #     REIMBURSEDELEGATESOTHER_df<-summarydata (df,"REIMBURSEDELEGATESOTHER")
  #     DELEGRETIREREQUIREMENT_df<-summarydata (df,"DELEGRETIREREQUIREMENT")
  #     DELEGATESPOUSETRAVEL_df<-summarydata (df,"DELEGATESPOUSETRAVEL")
  #     peer_del<-  df %>% 
  #       summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
  #       ) %>%  
  #       
  #       
  #       mutate(
  #         NUMBEROFDELEGATES         = formatC(NUMBEROFDELEGATES, format="f", big.mark=",", digits = 2),
  #         DELEGMEMBERS     = formatC(DELEGMEMBERS, format="f", big.mark=",", digits = 2),
  #         DELEGEMPLOYEES = formatC(DELEGEMPLOYEES, format="f", big.mark=",", digits = 2),
  #         DELEGNONELECT       = formatC(DELEGNONELECT, format="f", big.mark=",", digits = 2),
  #         DELEGFEMALE         = formatC(DELEGFEMALE, format="f", big.mark=",", digits = 2),
  #         DELEGMINORITY        = formatC(DELEGMINORITY, format="f", big.mark=",", digits = 2),
  #         DELEGINDIGENOUS        = formatC(DELEGINDIGENOUS, format="f", big.mark=",", digits = 2),
  #         DELEGEXECUTIVE         = formatC(DELEGEXECUTIVE, format="f", big.mark=",", digits = 2),
  #         DELEGINDUSTRY        = formatC(DELEGINDUSTRY, format="f", big.mark=",", digits = 2),
  #         DELEGATECOMPENSATION        = ifelse(length(DELEGATECOMPENSATION_df) == 0, "No Response", DELEGATECOMPENSATION_df$percentage),
  #         DELEGATECOMPENSATIONAMOUNT_A1         = scales::dollar(DELEGATECOMPENSATIONAMOUNT_A1),
  #         DELEGATECOMPENSATIONAMOUNT_A2          = scales::dollar(DELEGATECOMPENSATIONAMOUNT_A2),
  #         DELEGATECOMPENSATIONAMOUNT_A3          = scales::dollar(DELEGATECOMPENSATIONAMOUNT_A3),
  #         DELEGATECOMPENSATIONAMOUNT_A4          = scales::dollar(DELEGATECOMPENSATIONAMOUNT_A4),
  #         OTHERCOMPENSATION        = ifelse(length(OTHERCOMPENSATION_df) == 0, "No Response", OTHERCOMPENSATION_df$percentage),
  #         REIMBURSEMENTDELEGATES_A1          = scales::dollar(REIMBURSEMENTDELEGATES_A1),
  #         REIMBURSEMENTDELEGATES_A2          = scales::dollar(REIMBURSEMENTDELEGATES_A2),
  #         REIMBURSEMENTDELEGATES_A3          = scales::dollar(REIMBURSEMENTDELEGATES_A3),
  #         REIMBURSEMENTDELEGATES_A4          = scales::dollar(REIMBURSEMENTDELEGATES_A4),
  #         REIMBURSEMENTDELEGATES_A5          = scales::dollar(REIMBURSEMENTDELEGATES_A5),
  #         REIMBURSEMENTDELEGATES_A6          = scales::dollar(REIMBURSEMENTDELEGATES_A6),
  #         REIMBURSEDELEGATESOTHER        = ifelse(length(REIMBURSEDELEGATESOTHER_df) == 0, "No Response", REIMBURSEDELEGATESOTHER_df$percentage),
  #         DELEGATESPOUSETRAVEL        = ifelse(length(DELEGATESPOUSETRAVEL_df) == 0, "No Response", DELEGATESPOUSETRAVEL_df$percentage),
  #         TOTALDELEGCOMPENSATION          = scales::dollar(TOTALDELEGCOMPENSATION),
  #         DELEGRUNREELECTION         = formatC(DELEGRUNREELECTION, format="f", big.mark=",", digits = 2),
  #         DELEGMAXIMUMSERVICE        = formatC(DELEGMAXIMUMSERVICE, format="f", big.mark=",", digits = 2),
  #         DELEGRETIREREQUIREMENT        = ifelse(length(DELEGRETIREREQUIREMENT_df) == 0, "No Response", DELEGRETIREREQUIREMENT_df$percentage),
  #         DELEGRETIREMENTAGE        = formatC(DELEGRETIREMENTAGE, format="f", big.mark=",", digits = 2)
  #         
  #         
  #       )  %>% mutate_all(~ifelse(is.nan(.), NA, .)) %>% replace(is.na(.), "NA") 
  #     
  #   })
  #   
  #   
  #   
  #   peer_del1 <-  reactive({as_tibble(t(peer_del()), rownames = "Variable")
  #   })  
  #   
  #   
  #   
  #   peer_del2 <- reactive({
  #     peer_del1() %>% inner_join(orglist, by = "Variable") %>% 
  #       select("Variable", 
  #              #"Label",
  #              "V1") %>% rename( "Peer Group" = "V1"
  #                                #,"Measure"             = "Label"
  #              )
  #   })  
  #   return(peer_del2)
  # }
  # own_del1_wave1<-own_del1(x=data1, wave=1,text="data not available ",orglist=del_list)
  # own_del_df_wave1<-own_org_profile2(ownorg=own_del1_wave1(),orglist=del_list, varlist=del_varlist())
  # peer_del_wave1<-peer_del(x=data1,tb=peergroups_selected_Wave1(),orglist=del_list)
  # del_df1_wave1<-org_profile_df1(data1=own_del_df_wave1(), data2=peer_del_wave1())
  # own_del1_wave2<-own_del1(x=data1, wave=2,text="data not available ",orglist=del_list)
  # own_del_df_wave2<-own_org_profile2(ownorg=own_del1_wave2(),orglist=del_list, varlist=del_varlist())
  # peer_del_wave2<-peer_del(x=data1,tb=peergroups_selected_Wave2(),orglist=del_list)
  # del_df1_wave2<-org_profile_df1(data1=own_del_df_wave2(), data2=peer_del_wave2())
  # ~~~FULL TABLE~~~ ---- 
  full_table_wave1 <- reactive ({
    rbind(org_profile_df1_wave1(), ceo_df1_wave1(),boardcomp_df1_wave1(), bedi_df1_wave1(), cob_df1_wave1(), bp_df1_wave1(), compen_df1_wave1(), tldel_df1_wave1(), del_df1_wave1()
    )
    
  })
  full_table_wave2 <- reactive ({
    rbind(org_profile_df1_wave2(), ceo_df1_wave2(),boardcomp_df1_wave2(), bedi_df1_wave2(), cob_df1_wave2(), bp_df1_wave2(), compen_df1_wave2(), tldel_df1_wave2(), del_df1_wave2()
    )
    
  })
  
  # 6.0: EXCEL French---- 
  observeEvent(input$init_2_french, {
    if(nrow(selected_Wave2_french())>=5 & length(selected1_french())>=1 & (nrow(selected_Wave1_french())==0 |  nrow(selected_Wave1_french())>=5)){
      shinyjs::runjs("document.getElementById('download2_french').click();")
    }
    else if (nrow(selected_Wave1_french())>=5 & length(selected1_french())>=1 & (nrow(selected_Wave2_french())==0 |  nrow(selected_Wave2_french())>=5)){
      shinyjs::runjs("document.getElementById('download2_french').click();")
    }
    else {
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = tags$b(translate$t("*Select a minimum of 5 organizations from each wave and a minimum of 1 comparison selector to proceed!"),
                      style = "color:#FF0000;"),
        type = "error"
      )
    }
    
  })
  rv2_french <- reactiveValues(download_flag= 0) # for the shinyAlert 
  
  output$download2_french <- downloadHandler(
    
    filename = function() {
      "report_french.xlsx"
    }, 
    
    content = function(file) {
      
      #Loading message
      withProgress(message = translate$t("Creating Report. Please wait..."),{      
        
        customstyle <- createStyle(wrapText = TRUE) #wraps text in a cell
        
        my_workbook_french <- createWorkbook()
        
        # *Home ----
        addWorksheet(wb = my_workbook_french, sheetName = "Page d'accueil")
        
        writeData(
          my_workbook_french,
          sheet = 1,
          c("Rapport d'analyse du groupe de pairs",
            paste0("Date de la dernière compilation: ", format(Sys.time(), '%d %B, %Y'))),
          startRow = 14,
          startCol = 1
        ) 
        addStyle(
          my_workbook_french,
          sheet = 1,
          rows = 14:15,
          cols = 1,
          style = createStyle(
            fontSize = "24",
            textDecoration = "bold"
          )
        )
        showGridLines(
          my_workbook_french,
          sheet = 1,
          showGridLines = FALSE
        )
        
        insertImage(
          my_workbook_french,
          sheet = 1,
          file = "JSGS_CCSC_Logo_CMYK-highnobg.jpg", 
          width = 3,
          height = 1.25,
          startRow = 2,
          startCol = 1,
          units = "in",
          dpi = 250
        )
        insertImage(
          my_workbook_french,
          sheet = 1,
          file = "JSGS_CCSC_Logo_CMYK-highnobg.jpg", #"1200px-United_Farmers_of_Alberta_Logo.jpg"
          width = 3,
          height = 1.25,
          startRow = 2,
          startCol = 5,
          units = "in",
          dpi = 300
        )  
        insertImage(
          my_workbook_french,
          sheet = 1,
          file = "USask_CHASR_Logo.png",
          width = 3,
          height = 1.25,
          startRow = 2,
          startCol = 9,
          units = "in",
          dpi = 300
        )
        
        #~~~Peer Groups Wave-1~~~ ----   
        if (nrow(peergroups_selected_Wave1_french())>=1) {
          
          addWorksheet(wb = my_workbook_french, sheetName = "Groupes de partage Vague-1", gridLines = TRUE)
          
          
          writeDataTable(
            my_workbook_french,
            sheet = "Groupes de partage Vague-1",
            peergroups_selected_Wave1_french(), 
            startRow = 5,
            startCol = 1,
            tableStyle = "TableStylelight9",
            withFilter = openxlsx_getOp("withFilter", FALSE),
            headerStyle = createStyle(
              textDecoration = "Bold",
              halign = "center",
              border = "bottom",
              #fgFill = "#1a5bc4",
              #fontColour = "#ffffff",
              fontSize = 16
            )
          )
          
          mergeCells(my_workbook_french, "Groupes de partage Vague-1", cols = 1:4, rows =4)
          writeData(
            my_workbook_french,
            sheet = "Groupes de partage Vague-1",
            "Tableau 0 : Groupes de pairs dans l'analyse Vague-1",
            startRow = 4,
            startCol = 1
          )
          addStyle(
            my_workbook_french,
            sheet = "Groupes de partage Vague-1",
            rows = 4,
            cols = 1:4,
            style = createStyle(
              fontSize = "16",
              textDecoration = "bold",
              fontColour  = "#000000"
            )
          )
          if (nrow(peergroups_selected_Wave1_french())>0 & nrow(peergroups_selected_Wave1_french())<5) {
            mergeCells(my_workbook_french, "Groupes de partage Vague-1", cols = 1:4, rows =nrow(peergroups_selected_Wave1_french())+6)
            writeData(
              my_workbook_french,
              sheet = "Groupes de partage Vague-1",
              "Note: Une sélection minimale de cinq organisations est requise",
              startRow = nrow(peergroups_selected_Wave1_french())+6,
              startCol = 1,
            )
            addStyle(
              my_workbook,
              sheet = "Groupes de partage Vague-1",
              rows = nrow(peergroups_selected_Wave1_french())+6,
              cols = 1,
              style = createStyle(
                fontSize = "20",
                textDecoration = "bold",
                fontColour  = "#CC0000"
              )
            )
          }
          addStyle(my_workbook_french, sheet = "Groupes de partage Vague-1", customstyle, rows = 1:100, cols = 1:100)
          setColWidths(my_workbook_french, sheet = "Groupes de partage Vague-1", cols = 1:2, widths = "auto")
          
        }
        # ~~~ Full Table Wave-1~~~ ----  
        if (nrow(peergroups_selected_Wave1_french())>=5) {
          addWorksheet(wb = my_workbook_french, sheetName = "Tableau complet Vague-1", gridLines = TRUE)
          mergeCells(my_workbook_french, "Tableau complet Vague-1", cols = 1:4, rows =4)
          writeData(
            my_workbook_french,
            sheet = "Tableau complet Vague-1",
            "Tableau 1 : Groupes de pairs créés sur mesure Vague-1",
            startRow = 4,
            startCol = 1
          ) 
          addStyle(
            my_workbook_french,
            sheet = "Tableau complet Vague-1",
            rows = 4,
            cols = 1:4,
            style = createStyle(
              fontSize = "16",
              textDecoration = "bold",
              fontColour  = "#000000"
            )
          )
          writeDataTable(
            my_workbook_french,
            sheet = "Tableau complet Vague-1",
            full_table_wave1_french(), 
            startRow = 5,
            startCol = 1,
            tableStyle = "TableStylelight9",
            withFilter = openxlsx_getOp("withFilter", FALSE),
            headerStyle = createStyle(
              textDecoration = "Bold",
              halign = "center",
              border = "bottom",
              #fgFill = "#1a5bc4",
              #fontColour = "#ffffff",
              fontSize = 16
            )
          )
          
          setColWidths(my_workbook_french, sheet = "Tableau complet Vague-1", cols = 1, widths = 40)
          setColWidths(my_workbook_french, sheet = "Tableau complet Vague-1", cols = 3, widths = 40)
          setColWidths(my_workbook_french, sheet = "Tableau complet Vague-1", cols = c(2,4), widths = 80) 
        }
        
        #~~~Peer Groups Wave-2~~~ ----   
        if (nrow(peergroups_selected_Wave2_french())>=1) {
          addWorksheet(wb = my_workbook_french, sheetName = "Groupes de partage Vague-2", gridLines = TRUE)
          
          mergeCells(my_workbook_french, "Groupes de partage Vague-2", cols = 1:4, rows =4)
          writeData(
            my_workbook_french,
            sheet = "Groupes de partage Vague-2",
            "Tableau 0 : Groupes de pairs dans l'analyse Vague-2",
            startRow = 4,
            startCol = 1
          )
          addStyle(
            my_workbook_french,
            sheet = "Groupes de partage Vague-2",
            rows = 4,
            cols = 1:4,
            style = createStyle(
              fontSize = "16",
              textDecoration = "bold",
              fontColour  = "#000000"
            )
          )
          writeDataTable(
            my_workbook_french,
            sheet = "Groupes de partage Vague-2",
            peergroups_selected_Wave2_french(), 
            startRow = 5,
            startCol = 1,
            tableStyle = "TableStylelight11",
            withFilter = openxlsx_getOp("withFilter", FALSE),
            headerStyle = createStyle(
              textDecoration = "Bold",
              halign = "center",
              border = "bottom",
              #fgFill = "#1a5bc4",
              #fontColour = "#ffffff",
              fontSize = 16
            )
          )
          
          if (nrow(peergroups_selected_Wave2_french())>0 & nrow(peergroups_selected_Wave2_french())<5) {
            mergeCells(my_workbook_french, "Groupes de partage Vague-2", cols = 1:4, rows =nrow(peergroups_selected_Wave2_french())+6)
            writeData(
              my_workbook,
              sheet = "Groupes de partage Vague-2",
              "Note: Une sélection minimale de cinq organisations est requise",
              startRow = nrow(peergroups_selected_Wave2_french())+6,
              startCol = 1,
            )
            addStyle(
              my_workbook,
              sheet = "Groupes de partage Vague-2",
              rows = nrow(peergroups_selected_Wave2_french())+6,
              cols = 1,
              style = createStyle(
                fontSize = "20",
                textDecoration = "bold",
                fontColour  = "#CC0000"
              )
            )
          }
          addStyle(my_workbook_french, sheet = "Groupes de partage Vague-2", customstyle, rows = 1:100, cols = 1:100)
          setColWidths(my_workbook_french, sheet = "Groupes de partage Vague-2", cols = 1:2, widths = "auto")
          
        }
        # ~~~ Full Table Wave-2~~~ ----  
        if (nrow(peergroups_selected_Wave2_french())>=5) {
          addWorksheet(wb = my_workbook_french, sheetName = "Tableau complet Vague-2", gridLines = TRUE)
          mergeCells(my_workbook_french, "Tableau complet Vague-2", cols = 1:4, rows =4)
          writeData(
            my_workbook_french,
            sheet = "Tableau complet Vague-2",
            "Tableau 1 : Groupes de pairs créés sur mesure Vague-1",
            startRow = 4,
            startCol = 1
          ) 
          addStyle(
            my_workbook_french,
            sheet = "Tableau complet Vague-2",
            rows = 4,
            cols = 1:4,
            style = createStyle(
              fontSize = "16",
              textDecoration = "bold",
              fontColour  = "#000000"
            )
          )
          writeDataTable(
            my_workbook_french,
            sheet = "Tableau complet Vague-2",
            full_table_wave2_french(), 
            startRow = 5,
            startCol = 1,
            tableStyle = "TableStylelight11",
            withFilter = openxlsx_getOp("withFilter", FALSE),
            headerStyle = createStyle(
              textDecoration = "Bold",
              halign = "center",
              border = "bottom",
              #fgFill = "#1a5bc4",
              #fontColour = "#ffffff",
              fontSize = 16
            )
          )
          
          setColWidths(my_workbook_french, sheet = "Tableau complet Vague-2", cols = 1, widths = 40)
          setColWidths(my_workbook_french, sheet = "Tableau complet Vague-2", cols = 3, widths = 40)
          setColWidths(my_workbook_french, sheet = "Tableau complet Vague-2", cols = c(2,4), widths = 80)
          
        }
        
        # ~~~
        
        saveWorkbook(my_workbook_french, file)
        
        
        # When the downloadHandler function runs, increment rv2$download_flag
        rv2_french$download_flag <- rv2_french$download_flag + 1
        
        if(rv2_french$download_flag > 0){  # trigger event whenever the value of rv2$download_flag changes
          shinyjs::alert(translate$t("File downloaded!"))
        }
        
      })      
      
    }
  )
  # ~~~TABLE CALCULATIONS~~~ ----
  # Peer Groups Selected 
  peergroups_selected_french <- reactive({
    # req(length(selected()) >= 5)
    reactiveDf_french()[selected_french(),-1 ] # Remove CaseID
  })
  peergroups_selected_Wave2_french<-reactive({
    peergroups_selected_french () %>%
      filter(Wave=="Vague-2")
  })
  peergroups_selected_Wave1_french<-reactive({
    peergroups_selected_french () %>%
      filter(Wave=="Vague-1")
  })
  
  # *User Selections: Selectors ----
  variables_selected_french <- reactive({
    selectors_tbl_french[selected1_french(),1:2 ] # Variable, Category  
    
  }) 
  # ~~~ORGANIZATIONAL PROFILE~~~ ----   

  
  # ~~~ORGANIZATIONAL PROFILE~~~ ----   
  
  
  org_profile_list_wave1_french <- selectors_tbl_french %>%
    filter(Category == "Profil organisationnel") %>% 
    arrange(Order) %>%
    filter(is.na(Wave)==TRUE)
  org_profile_list_wave2_french <- selectors_tbl_french %>%
    filter(Category == "Profil organisationnel") %>% arrange(Order)
  
  
  own_org_profile1_wave1_french <- reactive({
    
    df <- data1_french %>% 
      filter(Wave==1) %>%
      
      mutate(
        REVENUE         = scales::dollar(REVENUE),
        ASSETS          = scales::dollar(ASSETS),
        MEMBERS         = formatC(MEMBERS, format="f", big.mark=",", digits = 2),
        FTEMPLOYEES     = formatC(FTEMPLOYEES, format="f", big.mark=",", digits = 2),
        EMPLOYEES       = formatC(EMPLOYEES, format="f", big.mark=",", digits = 2),
        YEARSINBUSINESS = formatC(YEARSINBUSINESS, format="f", digits = 2)
        
      ) %>%  
      
      
      filter(Organization %in% reactive_values$organization) %>% 
      #mutate_all(~ifelse(is.nan(.), NA, .)) %>% replace(is.na(.), "-") %>% 
      select(starts_with(org_profile_list_wave1_french$Variable)) # (Step 0)
    
    # Transpose df from wide to long
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_org_profile1_wave1_french<-cbind(df1,V1)
    }else {
      own_org_profile1_wave1_french <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_org_profile1_wave1_french <-  as_tibble(t(df), rownames = "Variable")  
    
  })    
  
  
  own_org_profile2_wave1_french <- reactive({
    own_org_profile1_wave1_french() %>% inner_join(org_profile_list_wave1_french, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation") #ATTN:AHMAD MOBIN
  })
  
  
  
  org_profile_df_wave1_french <- reactive({
    
    variables_selected_french() %>% filter(Category == "Profil organisationnel") %>% 
      inner_join(own_org_profile2_wave1_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable") #ATTN:AHMAD MOBIN
    
  })
  
  
  peer_org_profile_wave1_french <- reactive({  
    df <-   data1_french %>% filter(Organization %in% peergroups_selected_Wave1_french()$Organization) %>%
      filter(Wave==1) %>%
      select(starts_with(org_profile_list_wave1_french$Variable))
    MARKET_df<-summarydatasort (df,"MARKET")
    PROVINCEC1_df<-summarydata (df,"PROVINCEC1")
    PROVINCEC2_df<-summarydata (df,"PROVINCEC2")
    PROVINCEC3_df<-summarydata (df,"PROVINCEC3")
    PROVINCEC4_df<-summarydata (df,"PROVINCEC4")
    PROVINCEC5_df<-summarydata (df,"PROVINCEC5")
    PROVINCEC6_df<-summarydata (df,"PROVINCEC6")
    PROVINCEC7_df<-summarydata (df,"PROVINCEC7")
    PROVINCEC8_df<-summarydata (df,"PROVINCEC8")
    PROVINCEC9_df<-summarydata (df,"PROVINCEC9")
    PROVINCEC10_df<-summarydata (df,"PROVINCEC10")
    PROVINCEC11_df<-summarydata (df,"PROVINCEC11")
    PROVINCEC12_df<-summarydata (df,"PROVINCEC12")
    PROVINCEC13_df<-summarydata (df,"PROVINCEC13")
    PROVINCEC14_df<-summarydata (df,"PROVINCEC14")
    SECTORSC1_df<-summarydata (df,"SECTORSC1")
    SECTORSC2_df<-summarydata (df,"SECTORSC2")
    SECTORSC3_df<-summarydata (df,"SECTORSC3")
    SECTORSC4_df<-summarydata (df,"SECTORSC4")
    SECTORSC5_df<-summarydata (df,"SECTORSC5")
    SECTORSC6_df<-summarydata (df,"SECTORSC6")
    SECTORSC7_df<-summarydata (df,"SECTORSC7")
    SECTORSC8_df<-summarydata (df,"SECTORSC8")
    SECTORSC9_df<-summarydata (df,"SECTORSC9")
    PRIMARYSECTOR_df<-summarydatasort (df,"PRIMARYSECTOR")
    TYPE_df<-summarydatasort (df,"TYPE")
    TIERTYPE_df<-summarydata (df,"TIERTYPE")
    ASSETS_df<-meandata (df,ASSETS,1)
    REVENUE_df<-meandata (df,REVENUE,1)
    MEMBERS_df<-meandata (df,MEMBERS,0)
    FTEMPLOYEES_df<-meandata (df,FTEMPLOYEES,0)
    EMPLOYEES_df<-meandata (df,EMPLOYEES,0)
    YEARSINBUSINESS_df<-meandata (df,YEARSINBUSINESS,0)
    peer_org_profile_wave1 <-  df %>% #filter(Organization %in% peergroups_selected()$Organization) %>%
      #select(starts_with(org_profile_list$Variable)) %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
                
                
                
                PROVINCEC14O = (paste(na.omit(unique(PROVINCEC14O)), collapse = ";")),
                SECTORSC9O    = (paste(na.omit(unique(SECTORSC9O)), collapse = ";"))
                
      ) %>%
      
      mutate(
        
        REVENUE     = REVENUE_df$percentage,
        ASSETS      = ASSETS_df$percentage,
        MEMBERS     = MEMBERS_df$percentage,
        FTEMPLOYEES = FTEMPLOYEES_df$percentage,
        EMPLOYEES   = EMPLOYEES_df$percentage,
        YEARSINBUSINESS = YEARSINBUSINESS_df$percentage,
        MARKET     = MARKET_df$percentage
        ,PROVINCEC1  = PROVINCEC1_df$percentage
        ,PROVINCEC2  = PROVINCEC2_df$percentage
        ,PROVINCEC3  = PROVINCEC3_df$percentage
        ,PROVINCEC4  = PROVINCEC4_df$percentage
        ,PROVINCEC5  = PROVINCEC5_df$percentage
        ,PROVINCEC6  = PROVINCEC6_df$percentage
        ,PROVINCEC7  = PROVINCEC7_df$percentage
        ,PROVINCEC8  = PROVINCEC8_df$percentage
        ,PROVINCEC9  = PROVINCEC9_df$percentage
        ,PROVINCEC10 = PROVINCEC10_df$percentage
        ,PROVINCEC11 = PROVINCEC11_df$percentage
        ,PROVINCEC12 = PROVINCEC12_df$percentage
        ,PROVINCEC13 = PROVINCEC13_df$percentage
        ,PROVINCEC14 = PROVINCEC14_df$percentage
        ,SECTORSC1 = SECTORSC1_df$percentage
        ,SECTORSC2 = SECTORSC2_df$percentage
        ,SECTORSC3 = SECTORSC3_df$percentage
        ,SECTORSC4 = SECTORSC4_df$percentage
        ,SECTORSC5 = SECTORSC5_df$percentage
        ,SECTORSC6 = SECTORSC6_df$percentage
        ,SECTORSC7 = SECTORSC7_df$percentage
        ,SECTORSC8 = SECTORSC8_df$percentage
        ,SECTORSC9 = SECTORSC9_df$percentage
        ,PRIMARYSECTOR = PRIMARYSECTOR_df$percentage
        ,TYPE = TYPE_df$percentage
        ,TIERTYPE = TIERTYPE_df$percentage
        
        
      )  %>% mutate_all(~ifelse(is.nan(.), NA, .)
      ) #%>% replace(is.na(.), "NA")
    
  })  
  
  peer_org_profile1_wave1_french<-reactive({
    if (nrow(peer_org_profile_wave1_french())==0) {
      
      df1<-as_tibble(t(peer_org_profile_wave1_french()), rownames = "Variable")
      V1<-"text"
      peer_org_profile1_wave1_french<-cbind(df1,V1)
      
      
      
    }else {
      peer_org_profile1_wave1_french <-  as_tibble(t(peer_org_profile_wave1_french()), rownames = "Variable")
    }
  })
  # peer_org_profile1_wave1_french <-  reactive({as_tibble(t(peer_org_profile_wave1_french()), rownames = "Variable")
  # }) 
  
  
  peer_org_profile2_wave1_french <- reactive({
    peer_org_profile1_wave1_french() %>% inner_join(org_profile_list_wave1_french, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })   
  
  
  
  org_profile_df1_wave1_french <- reactive({
    
    org_profile_df_wave1_french() %>% 
      inner_join(peer_org_profile2_wave1_french(), by = "Variable") %>%
      arrange(Theme) %>% 
      select("Theme","Measure", "Your Organization", "Peer Group") %>% 
      rename("Your Organization" = "Your Organization")
    
  })   
  
  own_org_profile1_wave2_french <- reactive({
    
    df <- data1_french %>% 
      filter(Wave==2) %>%
      
      mutate(
        REVENUE         = scales::dollar(REVENUE),
        ASSETS          = scales::dollar(ASSETS),
        MEMBERS         = formatC(MEMBERS, format="f", big.mark=",", digits = 2),
        FTEMPLOYEES     = formatC(FTEMPLOYEES, format="f", big.mark=",", digits = 2),
        EMPLOYEES       = formatC(EMPLOYEES, format="f", big.mark=",", digits = 2),
        YEARSINBUSINESS = formatC(YEARSINBUSINESS, format="f", digits = 2)
        
      ) %>%  
      
      
      filter(Organization %in% reactive_values$organization) %>% 
      #mutate_all(~ifelse(is.nan(.), NA, .)) %>% replace(is.na(.), "-") %>% 
      select(starts_with(org_profile_list_wave2_french$Variable)) # (Step 0)
    
    # Transpose df from wide to long 
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_org_profile1_wave2_french<-cbind(df1,V1)
    }else {
      own_org_profile1_wave2_french <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_org_profile1_wave2_french <-  as_tibble(t(df), rownames = "Variable")  
    
  })    
  
  
  own_org_profile2_wave2_french <- reactive({
    own_org_profile1_wave2_french() %>% inner_join(org_profile_list_wave2_french, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation") #ATTN:AHMAD MOBIN
  })
  
  
  
  org_profile_df_wave2_french <- reactive({
    
    variables_selected_french() %>% filter(Category == "Profil organisationnel") %>% 
      inner_join(own_org_profile2_wave2_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable") #ATTN:AHMAD MOBIN
    
  })
  
  
  peer_org_profile_wave2_french <- reactive({  
    df <-   data1_french %>% filter(Organization %in% peergroups_selected_Wave2_french()$Organization) %>%
      filter(Wave==2) %>%
      select(starts_with(org_profile_list_wave2_french$Variable))
    MARKET_df<-summarydatasort (df,"MARKET")
    PROVINCEC1_df<-summarydata (df,"PROVINCEC1")
    PROVINCEC2_df<-summarydata (df,"PROVINCEC2")
    PROVINCEC3_df<-summarydata (df,"PROVINCEC3")
    PROVINCEC4_df<-summarydata (df,"PROVINCEC4")
    PROVINCEC5_df<-summarydata (df,"PROVINCEC5")
    PROVINCEC6_df<-summarydata (df,"PROVINCEC6")
    PROVINCEC7_df<-summarydata (df,"PROVINCEC7")
    PROVINCEC8_df<-summarydata (df,"PROVINCEC8")
    PROVINCEC9_df<-summarydata (df,"PROVINCEC9")
    PROVINCEC10_df<-summarydata (df,"PROVINCEC10")
    PROVINCEC11_df<-summarydata (df,"PROVINCEC11")
    PROVINCEC12_df<-summarydata (df,"PROVINCEC12")
    PROVINCEC13_df<-summarydata (df,"PROVINCEC13")
    PROVINCEC14_df<-summarydata (df,"PROVINCEC14")
    SECTORSC1_df<-summarydata (df,"SECTORSC1")
    SECTORSC2_df<-summarydata (df,"SECTORSC2")
    SECTORSC3_df<-summarydata (df,"SECTORSC3")
    SECTORSC4_df<-summarydata (df,"SECTORSC4")
    SECTORSC5_df<-summarydata (df,"SECTORSC5")
    SECTORSC6_df<-summarydata (df,"SECTORSC6")
    SECTORSC7_df<-summarydata (df,"SECTORSC7")
    SECTORSC8_df<-summarydata (df,"SECTORSC8")
    SECTORSC9_df<-summarydata (df,"SECTORSC9")
    PRIMARYSECTOR_df<-summarydatasort (df,"PRIMARYSECTOR")
    TYPE_df<-summarydatasort (df,"TYPE")
    TIERTYPE_df<-summarydata (df,"TIERTYPE")
    COOPLOCATION_df<-summarydata (df,"COOPLOCATION")
    ASSETS_df<-meandata (df,ASSETS,1)
    REVENUE_df<-meandata (df,REVENUE,1)
    MEMBERS_df<-meandata (df,MEMBERS,0)
    FTEMPLOYEES_df<-meandata (df,FTEMPLOYEES,0)
    EMPLOYEES_df<-meandata (df,EMPLOYEES,0)
    YEARSINBUSINESS_df<-meandata (df,YEARSINBUSINESS,0)
    peer_org_profile_wave2_french <-  df %>% #filter(Organization %in% peergroups_selected()$Organization) %>%
      #select(starts_with(org_profile_list$Variable)) %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
                
                
                
                PROVINCEC14O = (paste(na.omit(unique(PROVINCEC14O)), collapse = ";")),
                SECTORSC9O    = (paste(na.omit(unique(SECTORSC9O)), collapse = ";"))
                
      ) %>%
      
      mutate(
        
        REVENUE     = REVENUE_df$percentage,
        ASSETS      = ASSETS_df$percentage,
        MEMBERS     = MEMBERS_df$percentage,
        FTEMPLOYEES = FTEMPLOYEES_df$percentage,
        EMPLOYEES   = EMPLOYEES_df$percentage,
        YEARSINBUSINESS = YEARSINBUSINESS_df$percentage,
        MARKET     = MARKET_df$percentage
        ,PROVINCEC1  = PROVINCEC1_df$percentage
        ,PROVINCEC2  = PROVINCEC2_df$percentage
        ,PROVINCEC3  = PROVINCEC3_df$percentage
        ,PROVINCEC4  = PROVINCEC4_df$percentage
        ,PROVINCEC5  = PROVINCEC5_df$percentage
        ,PROVINCEC6  = PROVINCEC6_df$percentage
        ,PROVINCEC7  = PROVINCEC7_df$percentage
        ,PROVINCEC8  = PROVINCEC8_df$percentage
        ,PROVINCEC9  = PROVINCEC9_df$percentage
        ,PROVINCEC10 = PROVINCEC10_df$percentage
        ,PROVINCEC11 = PROVINCEC11_df$percentage
        ,PROVINCEC12 = PROVINCEC12_df$percentage
        ,PROVINCEC13 = PROVINCEC13_df$percentage
        ,PROVINCEC14 = PROVINCEC14_df$percentage
        ,SECTORSC1 = SECTORSC1_df$percentage
        ,SECTORSC2 = SECTORSC2_df$percentage
        ,SECTORSC3 = SECTORSC3_df$percentage
        ,SECTORSC4 = SECTORSC4_df$percentage
        ,SECTORSC5 = SECTORSC5_df$percentage
        ,SECTORSC6 = SECTORSC6_df$percentage
        ,SECTORSC7 = SECTORSC7_df$percentage
        ,SECTORSC8 = SECTORSC8_df$percentage
        ,SECTORSC9 = SECTORSC9_df$percentage
        ,PRIMARYSECTOR = PRIMARYSECTOR_df$percentage
        ,TYPE = TYPE_df$percentage
        ,TIERTYPE = TIERTYPE_df$percentage
        ,COOPLOCATION = COOPLOCATION_df$percentage
        
        
      )  %>% mutate_all(~ifelse(is.nan(.), NA, .)
      ) #%>% replace(is.na(.), "NA")
    
  })  
  
  peer_org_profile1_wave2_french<-reactive({
    if (nrow(peer_org_profile_wave2_french())==0) {
      
      df1<-as_tibble(t(peer_org_profile_wave2_french()), rownames = "Variable")
      V1<-"text"
      peer_org_profile1_wave2_french<-cbind(df1,V1)
      
      
      
    }else {
      peer_org_profile1_wave2_french <-  as_tibble(t(peer_org_profile_wave2_french()), rownames = "Variable")
    }
  })
  # peer_org_profile1_wave2_french <-  reactive({as_tibble(t(peer_org_profile_wave2_french()), rownames = "Variable")
  # }) 
  
  
  peer_org_profile2_wave2_french <- reactive({
    peer_org_profile1_wave2_french() %>% inner_join(org_profile_list_wave2_french, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })   
  
  
  
  org_profile_df1_wave2_french <- reactive({
    
    org_profile_df_wave2_french() %>% 
      inner_join(peer_org_profile2_wave2_french(), by = "Variable") %>%
      arrange(Theme) %>% 
      select("Theme","Measure", "Your Organization", "Peer Group") %>% 
      rename("Your Organization" = "Your Organization")
    
  })   
  
  
  
  
  # org_profile_varlist_french <- reactive({
  #   df_french<-selectors_tbl_french[selected1_french(),1:2 ] # Variable, Category 
  #   org_profile_varlist_french <-df_french %>%
  #     filter(Category == "Profil organisationnel")  
  # }) 
  # own_org_profile1_wave1_french<-own_org_profile1(data1_french,1,text="Données non disponibles",orglist=org_profile_list_french)
  # org_profile_df_wave1_french<-own_org_profile2(ownorg = own_org_profile1_wave1_french(),orglist=org_profile_list_french,varlist=org_profile_varlist_french())
  # peer_org_profile2_wave1_french<-peer_org_profile(x=data1_french,tb=peergroups_selected_Wave1_french(),orglist=org_profile_list_french)
  # org_profile_df1_wave1_french<-org_profile_df1(data1=org_profile_df_wave1_french(), data2=peer_org_profile2_wave1_french())
  # own_org_profile1_wave2_french<-own_org_profile1(data1_french,2,text="Données non disponibles",orglist=org_profile_list_french)
  # org_profile_df_wave2_french<-own_org_profile2(ownorg = own_org_profile1_wave2_french(),orglist=org_profile_list_french,varlist=org_profile_varlist_french())
  # peer_org_profile2_wave2_french<-peer_org_profile(x=data1_french,tb=peergroups_selected_Wave2_french(),orglist=org_profile_list_french)
  # org_profile_df1_wave2_french<-org_profile_df1(data1=org_profile_df_wave2_french(), data2=peer_org_profile2_wave2_french())
  
  # ~~~CHEIF EXECUTIVE OFFICER~~~ ----
  
  
  ceo_list_wave1_french <- selectors_tbl_french %>%
    filter(Category == "Président directeur général") %>% 
    arrange(Order) %>%
    filter(is.na(Wave)==TRUE)
  ceo_list_wave2_french <- selectors_tbl_french %>%
    filter(Category == "Président directeur général") %>% arrange(Order)
  
  own_ceo1_wave1_french <- reactive({
    df <- data1_french %>% 
      filter(Wave==1) %>%
      mutate(
        CEONUMBER         = formatC(CEONUMBER, format="f", big.mark=",", digits = 2),
        HIREDOUTSIDE     = formatC(HIREDOUTSIDE, format="f", big.mark=",", digits = 2),
        PERFORMANCEBASED       = formatC(PERFORMANCEBASED, format="f", big.mark=",", digits = 2)
        
      ) %>%  
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(ceo_list_wave1_french$Variable))
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_ceo1_wave1_french<-cbind(df1,V1)
    }else {
      own_ceo1_wave1_french <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_ceo1_wave1_french <-  as_tibble(t(df), rownames = "Variable")
    
  }) 
  own_ceo2_wave1_french <- reactive({
    own_ceo1_wave1_french() %>% inner_join(ceo_list_wave1_french, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation") #ATTN:AHMAD MOBIN
  })  
  ceo_df_wave1_french <- reactive({
    variables_selected_french() %>% filter(Category == "Président directeur général") %>% 
      inner_join(own_ceo2_wave1_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
  })  
  
  peer_ceo_wave1_french <- reactive({
    
    df <-   data1_french %>% filter(Organization %in% peergroups_selected_Wave1_french()$Organization) %>%
      filter(Wave==1) %>%
      select(starts_with(ceo_list_wave1_french$Variable))
    SUCCESSION_df<-summarydata (df,"SUCCESSION")
    WRITTENCONTRACT_df<-summarydata (df,"WRITTENCONTRACT")
    QUANTMEASURES_df<-summarydata (df,"QUANTMEASURES")
    COMPBENCHMARKS_df<-summarydata (df,"COMPBENCHMARKS")
    CEONUMBER_df<-meandata (df,CEONUMBER,0)
    HIREDOUTSIDE_df<-meandata (df,HIREDOUTSIDE,0)
    PERFORMANCEBASED_df<-meandata (df,PERFORMANCEBASED,0)
    peer_ceo_wave1_french <-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
      ) %>%
      mutate(
        CEONUMBER = CEONUMBER_df$percentage,
        HIREDOUTSIDE = HIREDOUTSIDE_df$percentage,
        PERFORMANCEBASED = PERFORMANCEBASED_df$percentage,
        SUCCESSION        = SUCCESSION_df$percentage
        ,WRITTENCONTRACT   = WRITTENCONTRACT_df$percentage
        ,QUANTMEASURES     = QUANTMEASURES_df$percentage
        ,COMPBENCHMARKS    = COMPBENCHMARKS_df$percentage
        #,TIERTYPE = TIERTYPE_df$TIERTYPEa
        
      ) %>% mutate_all(~ifelse(is.nan(.), NA, .)) #%>% replace(is.na(.), "NA") 
    
    
    
  })
  peer_ceo1_wave1_french<-reactive({
    if (nrow(peer_ceo_wave1_french())==0) {
      
      df1<-as_tibble(t(peer_ceo_wave1_french()), rownames = "Variable")
      V1<-"text"
      peer_ceo1_wave1_french<-cbind(df1,V1)
    }else {
      peer_ceo1_wave1_french <-  as_tibble(t(peer_ceo_wave1_french()), rownames = "Variable")
    }
  })
  # peer_ceo1_wave1_french <-  reactive({as_tibble(t(peer_ceo_wave1_french()), rownames = "Variable")
  # }) 
  
  peer_ceo2_wave1_french <- reactive({
    peer_ceo1_wave1_french() %>% inner_join(ceo_list_wave1_french, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })  
  
  
  
  ceo_df1_wave1_french <- reactive({
    
    ceo_df_wave1_french() %>% 
      left_join(peer_ceo2_wave1_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  }) 
  own_ceo1_wave2_french <- reactive({
    
    df <- data1_french %>% 
      filter(Wave==2) %>%
      mutate(
        CEONUMBER         = formatC(CEONUMBER, format="f", big.mark=",", digits = 2),
        HIREDOUTSIDE     = formatC(HIREDOUTSIDE, format="f", big.mark=",", digits = 2),
        PERFORMANCEBASED       = formatC(PERFORMANCEBASED, format="f", big.mark=",", digits = 2)
        
      ) %>%  
      
      
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(ceo_list_wave2_french$Variable))
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_ceo1_wave2_french<-cbind(df1,V1)
    }else {
      own_ceo1_wave2_french <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_ceo1_wave2_french <-  as_tibble(t(df), rownames = "Variable")
    
  }) 
  own_ceo2_wave2_french <- reactive({
    own_ceo1_wave2_french() %>% inner_join(ceo_list_wave2_french, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation") #ATTN:AHMAD MOBIN
  })  
  
  
  
  ceo_df_wave2_french <- reactive({
    
    variables_selected_french() %>% filter(Category == "Président directeur général") %>% 
      inner_join(own_ceo2_wave2_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
  })  
  
  
  
  
  peer_ceo_wave2_french <- reactive({
    
    df <-   data1_french %>% filter(Organization %in% peergroups_selected_Wave2_french()$Organization) %>%
      filter(Wave==2) %>%
      select(starts_with(ceo_list_wave2_french$Variable))
    SUCCESSION_df<-summarydata (df,"SUCCESSION")
    WRITTENCONTRACT_df<-summarydata (df,"WRITTENCONTRACT")
    QUANTMEASURES_df<-summarydata (df,"QUANTMEASURES")
    COMPBENCHMARKS_df<-summarydata (df,"COMPBENCHMARKS")
    CEONUMBER_df<-meandata (df,CEONUMBER,0)
    HIREDOUTSIDE_df<-meandata (df,HIREDOUTSIDE,0)
    PERFORMANCEBASED_df<-meandata (df,PERFORMANCEBASED,0)
    peer_ceo_wave2_french <-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
      ) %>%
      
      mutate(
        CEONUMBER = CEONUMBER_df$percentage,
        HIREDOUTSIDE = HIREDOUTSIDE_df$percentage,
        PERFORMANCEBASED = PERFORMANCEBASED_df$percentage,
        
        
        SUCCESSION        = SUCCESSION_df$percentage
        ,WRITTENCONTRACT   = WRITTENCONTRACT_df$percentage
        ,QUANTMEASURES     = QUANTMEASURES_df$percentage
        ,COMPBENCHMARKS    = COMPBENCHMARKS_df$percentage
        #,TIERTYPE = TIERTYPE_df$TIERTYPEa
        
      ) %>% mutate_all(~ifelse(is.nan(.), NA, .)) #%>% replace(is.na(.), "NA") 
    
    
    
  })
  peer_ceo1_wave2_french<-reactive({
    if (nrow(peer_ceo_wave2_french())==0) {
      
      df1<-as_tibble(t(peer_ceo_wave2_french()), rownames = "Variable")
      V1<-"text"
      peer_ceo1_wave2_french<-cbind(df1,V1)
    }else {
      peer_ceo1_wave2_french <-  as_tibble(t(peer_ceo_wave2_french()), rownames = "Variable")
    }
  })
  # peer_ceo1_wave2_french <-  reactive({as_tibble(t(peer_ceo_wave2_french()), rownames = "Variable")
  # }) 
  peer_ceo2_wave2_french <- reactive({
    peer_ceo1_wave2_french() %>% inner_join(ceo_list_wave2_french, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })  
  
  
  
  ceo_df1_wave2_french <- reactive({
    
    ceo_df_wave2_french() %>% 
      left_join(peer_ceo2_wave2_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  }) 
  
  # ceo_varlist_french <- reactive({
  #   df<-selectors_tbl_french[selected1_french(),1:2 ] # Variable, Category 
  #   ceo_varlist_french <-df%>%
  #     filter(Category == "Président directeur général")  
  # }) 
  
  # own_ceo1_wave1_french<-own_ceo1(x=data1_french, wave=1,text="Données non disponibles",orglist=ceo_list_french)
  # own_ceo_df_wave1_french<-own_org_profile2(ownorg=own_ceo1_wave1_french(),orglist=ceo_list_french, varlist=ceo_varlist_french())
  # peer_ceo_wave1_french<-peer_ceo(data1_french,tb=peergroups_selected_Wave1_french(),orglist=ceo_list_french)
  # ceo_df1_wave1_french<-org_profile_df1(data1=own_ceo_df_wave1_french(), data2=peer_ceo_wave1_french())
  # own_ceo1_wave2_french<-own_ceo1(x=data1_french, wave=2,text="Données non disponibles",orglist=ceo_list_french)
  # own_ceo_df_wave2_french<-own_org_profile2(ownorg=own_ceo1_wave2_french(),orglist=ceo_list_french, varlist=ceo_varlist_french())
  # peer_ceo_wave2_french<-peer_ceo(data1_french,tb=peergroups_selected_Wave2_french(),orglist=ceo_list_french)
  # ceo_df1_wave2_french<-org_profile_df1(data1=own_ceo_df_wave2_french(), data2=peer_ceo_wave2_french())
  # ~~~ Board Composition ~~~ ----
  boardcomp_list_wave1_french <- selectors_tbl_french %>%
    filter(Category == "Composition du conseil") %>% 
    arrange(Order) %>%
    filter(is.na(Wave)==TRUE)
  boardcomp_list_wave2_french <- selectors_tbl_french %>%
    filter(Category == "Composition du conseil") %>% arrange(Order)
  own_boardcomp1_wave1_french <- reactive({
    
    df <- data1_french %>%
      filter(Wave==1) %>%
      
      mutate(
        NUMBEROFBM         = formatC(NUMBEROFBM, format="f", big.mark=",", digits = 2),
        MAXMINSPECIFY_A1     = formatC(MAXMINSPECIFY_A1, format="f", big.mark=",", digits = 2),
        MAXMINSPECIFY_A2       = formatC(MAXMINSPECIFY_A2, format="f", big.mark=",", digits = 2),
        DIRECTOREMPLOYEES       = formatC(DIRECTOREMPLOYEES, format="f", big.mark=",", digits = 2)
        ,HOWMANYNONMEMBERS         = formatC(HOWMANYNONMEMBERS, format="f", big.mark=",", digits = 2)
        ,AGEOFBM  =  formatC(AGEOFBM, format="f", big.mark=",", digits = 2 )
        ,HOWLONGBMSERVE =  formatC(HOWLONGBMSERVE, format="f", big.mark=",", digits = 2 )
        ,DIRECTOREXECUTIVE         = formatC(DIRECTOREXECUTIVE, format="f", big.mark=",", digits = 2)
        ,DIRECTORINDUSTRY         = formatC(DIRECTORINDUSTRY, format="f", big.mark=",", digits = 2)
        ,DIRECTORMULTIPLE         = formatC(DIRECTORMULTIPLE, format="f", big.mark=",", digits = 2)
        
      )  %>% 
      
      
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(boardcomp_list_wave1_french$Variable))
    
    #Transpose df 
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_boardcomp1_wave1_french<-cbind(df1,V1)
    }else {
      own_boardcomp1_wave1_french <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_boardcomp1_wave1_french <-  as_tibble(t(df), rownames = "Variable")
    
  })   
  
  
  
  own_boardcomp2_wave1_french <- reactive({
    own_boardcomp1_wave1_french() %>% inner_join(boardcomp_list_wave1_french, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation")
  })   
  
  
  
  
  boardcomp_df_wave1_french <- reactive({
    
    variables_selected_french() %>% filter(Category %in% "Composition du conseil") %>% 
      inner_join(own_boardcomp2_wave1_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
  })    
  
  
  
  peer_boardcomp_wave1_french <- reactive({
    
    df <-   data1_french %>% filter(Organization %in% peergroups_selected_Wave1_french()$Organization) %>%
      filter(Wave==1) %>%
      select(starts_with(boardcomp_list_wave1_french$Variable))  
    
    SETMINMAXDIRECTORS_df<-summarydata (df,"SETMINMAXDIRECTORS")
    REGULATOR_df<-summarydata (df,"REGULATOR")
    NONMEMBERDIRECTORS_df<-summarydata (df,"NONMEMBERDIRECTORS")
    NONMEMDIRECTOR_df<-summarydata (df,"NONMEMDIRECTOR")
    
    NUMBEROFBM_df<-meandata (df,NUMBEROFBM,0)
    MAXMINSPECIFY_A1_df<-meandata (df,MAXMINSPECIFY_A1,0)
    MAXMINSPECIFY_A2_df<-meandata (df,MAXMINSPECIFY_A2,0)
    DIRECTOREMPLOYEES_df<-meandata (df,DIRECTOREMPLOYEES,0)
    HOWMANYNONMEMBERS_df<-meandata (df,HOWMANYNONMEMBERS,0)
    AGEOFBM_df<-meandata (df,AGEOFBM,0)
    HOWLONGBMSERVE_df<-meandata (df,HOWLONGBMSERVE,0)
    DIRECTOREXECUTIVE_df<-meandata (df,DIRECTOREXECUTIVE,0)
    DIRECTORINDUSTRY_df<-meandata (df,DIRECTORINDUSTRY,0)
    DIRECTORMULTIPLE_df<-meandata (df,DIRECTORMULTIPLE,0)
    
    peer_boardcomp_wave1<-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
                
      ) %>%   
      
      mutate(
        NUMBEROFBM=NUMBEROFBM_df$percentage,
        MAXMINSPECIFY_A1=MAXMINSPECIFY_A1_df$percentage,
        MAXMINSPECIFY_A2=MAXMINSPECIFY_A2_df$percentage,
        DIRECTOREMPLOYEES=DIRECTOREMPLOYEES_df$percentage,
        HOWMANYNONMEMBERS=HOWMANYNONMEMBERS_df$percentage,
        # NUMBEROFBM         = formatC(NUMBEROFBM, format="f", big.mark=",", digits = 2),
        # MAXMINSPECIFY_A1     = formatC(MAXMINSPECIFY_A1, format="f", big.mark=",", digits = 2),
        # MAXMINSPECIFY_A2       = formatC(MAXMINSPECIFY_A2, format="f", big.mark=",", digits = 2),
        # DIRECTOREMPLOYEES       = formatC(DIRECTOREMPLOYEES, format="f", big.mark=",", digits = 2),
        # HOWMANYNONMEMBERS         = formatC(HOWMANYNONMEMBERS, format="f", big.mark=",", digits = 2),
        SETMINMAXDIRECTORS        = ifelse(length(SETMINMAXDIRECTORS_df) == 0, "No Response", SETMINMAXDIRECTORS_df$percentage)
        
        ,REGULATOR   = ifelse(length(REGULATOR_df) == 0, "No Response", REGULATOR_df$percentage) 
        ,NONMEMBERDIRECTORS   = ifelse(length(NONMEMBERDIRECTORS_df) == 0, "No Response", NONMEMBERDIRECTORS_df$percentage)
        ,NONMEMDIRECTOR   = ifelse(length(NONMEMDIRECTOR_df) == 0, "No Response", NONMEMDIRECTOR_df$percentage)
        
        ,AGEOFBM=AGEOFBM_df$percentage
        ,HOWLONGBMSERVE=HOWLONGBMSERVE_df$percentage
        ,DIRECTOREXECUTIVE=DIRECTOREXECUTIVE_df$percentage
        ,DIRECTORINDUSTRY=DIRECTORINDUSTRY_df$percentage
        ,DIRECTORMULTIPLE=DIRECTORMULTIPLE_df$percentage
        
        # ,AGEOFBM  =  formatC(AGEOFBM, format="f", big.mark=",", digits = 2 )
        # ,HOWLONGBMSERVE =  formatC(HOWLONGBMSERVE, format="f", big.mark=",", digits = 2 )
        # 
        # ,DIRECTOREXECUTIVE         = formatC(DIRECTOREXECUTIVE, format="f", big.mark=",", digits = 2)
        # ,DIRECTORINDUSTRY         = formatC(DIRECTORINDUSTRY, format="f", big.mark=",", digits = 2)
        # ,DIRECTORMULTIPLE         = formatC(DIRECTORMULTIPLE, format="f", big.mark=",", digits = 2)
        
        
        
      ) %>% mutate_all(~ifelse(is.nan(.), NA, .))  #%>% replace(is.na(.), "NA") 
    
    
    
  })
  
  
  
  peer_boardcomp1_wave1_french<-reactive({
    if (nrow(peer_boardcomp_wave1_french())==0) {
      
      df1<-as_tibble(t(peer_boardcomp_wave1_french()), rownames = "Variable")
      V1<-"text"
      peer_boardcomp1_wave1_french<-cbind(df1,V1)
    }else {
      peer_boardcomp1_wave1_french <-  as_tibble(t(peer_boardcomp_wave1_french()), rownames = "Variable")
    }
  })
  # peer_boardcomp1_wave1_french <-  reactive({as_tibble(t(peer_boardcomp_wave1_french()), rownames = "Variable")
  # }) 
  
  
  
  peer_boardcomp2_wave1_french <- reactive({
    peer_boardcomp1_wave1_french() %>% inner_join(boardcomp_list_wave1_french, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })  
  
  
  
  
  boardcomp_df1_wave1_french <- reactive({
    
    boardcomp_df_wave1_french() %>% 
      left_join(peer_boardcomp2_wave1_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  }) 
  
  
  
  own_boardcomp1_wave2_french <- reactive({
    
    df <- data1_french %>%
      filter(Wave==2) %>%
      
      mutate(
        NUMBEROFBM         = formatC(NUMBEROFBM, format="f", big.mark=",", digits = 2),
        MAXMINSPECIFY_A1     = formatC(MAXMINSPECIFY_A1, format="f", big.mark=",", digits = 2),
        MAXMINSPECIFY_A2       = formatC(MAXMINSPECIFY_A2, format="f", big.mark=",", digits = 2),
        DIRECTOREMPLOYEES       = formatC(DIRECTOREMPLOYEES, format="f", big.mark=",", digits = 2)
        ,HOWMANYNONMEMBERS         = formatC(HOWMANYNONMEMBERS, format="f", big.mark=",", digits = 2)
        ,AGEOFBM  =  formatC(AGEOFBM, format="f", big.mark=",", digits = 2 )
        ,HOWLONGBMSERVE =  formatC(HOWLONGBMSERVE, format="f", big.mark=",", digits = 2 )
        ,DIRECTOREXECUTIVE         = formatC(DIRECTOREXECUTIVE, format="f", big.mark=",", digits = 2)
        ,DIRECTORINDUSTRY         = formatC(DIRECTORINDUSTRY, format="f", big.mark=",", digits = 2)
        ,DIRECTORMULTIPLE         = formatC(DIRECTORMULTIPLE, format="f", big.mark=",", digits = 2)
        
      )  %>% 
      
      
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(boardcomp_list_wave2_french$Variable))
    
    #Transpose df 
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_boardcomp1_wave2_french<-cbind(df1,V1)
    }else {
      own_boardcomp1_wave2_french <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_boardcomp1_wave2_french <-  as_tibble(t(df), rownames = "Variable")
    
  })   
  
  
  
  own_boardcomp2_wave2_french <- reactive({
    own_boardcomp1_wave2_french() %>% inner_join(boardcomp_list_wave2_french, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation")
  })   
  
  
  
  
  boardcomp_df_wave2_french <- reactive({
    
    variables_selected_french() %>% filter(Category %in% "Composition du conseil") %>% 
      inner_join(own_boardcomp2_wave2_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
  })    
  
  
  
  peer_boardcomp_wave2_french <- reactive({
    
    df <-   data1_french %>% filter(Organization %in% peergroups_selected_Wave2_french()$Organization) %>%
      filter(Wave==2) %>%
      select(starts_with(boardcomp_list_wave2_french$Variable))  
    
    SETMINMAXDIRECTORS_df<-summarydata (df,"SETMINMAXDIRECTORS")
    REGULATOR_df<-summarydata (df,"REGULATOR")
    NONMEMBERDIRECTORS_df<-summarydata (df,"NONMEMBERDIRECTORS")
    NONMEMDIRECTOR_df<-summarydata (df,"NONMEMDIRECTOR")
    NUMBEROFBM_df<-meandata (df,NUMBEROFBM,0)
    MAXMINSPECIFY_A1_df<-meandata (df,MAXMINSPECIFY_A1,0)
    MAXMINSPECIFY_A2_df<-meandata (df,MAXMINSPECIFY_A2,0)
    DIRECTOREMPLOYEES_df<-meandata (df,DIRECTOREMPLOYEES,0)
    HOWMANYNONMEMBERS_df<-meandata (df,HOWMANYNONMEMBERS,0)
    AGEOFBM_df<-meandata (df,AGEOFBM,0)
    HOWLONGBMSERVE_df<-meandata (df,HOWLONGBMSERVE,0)
    DIRECTOREXECUTIVE_df<-meandata (df,DIRECTOREXECUTIVE,0)
    DIRECTORINDUSTRY_df<-meandata (df,DIRECTORINDUSTRY,0)
    DIRECTORMULTIPLE_df<-meandata (df,DIRECTORMULTIPLE,0)
    
    peer_boardcomp_wave2_french<-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
                
      ) %>%   
      
      mutate(
        NUMBEROFBM=NUMBEROFBM_df$percentage,
        MAXMINSPECIFY_A1=MAXMINSPECIFY_A1_df$percentage,
        MAXMINSPECIFY_A2=MAXMINSPECIFY_A2_df$percentage,
        DIRECTOREMPLOYEES=DIRECTOREMPLOYEES_df$percentage,
        HOWMANYNONMEMBERS=HOWMANYNONMEMBERS_df$percentage,
        # NUMBEROFBM         = formatC(NUMBEROFBM, format="f", big.mark=",", digits = 2),
        # MAXMINSPECIFY_A1     = formatC(MAXMINSPECIFY_A1, format="f", big.mark=",", digits = 2),
        # MAXMINSPECIFY_A2       = formatC(MAXMINSPECIFY_A2, format="f", big.mark=",", digits = 2),
        # DIRECTOREMPLOYEES       = formatC(DIRECTOREMPLOYEES, format="f", big.mark=",", digits = 2),
        # HOWMANYNONMEMBERS         = formatC(HOWMANYNONMEMBERS, format="f", big.mark=",", digits = 2),
        SETMINMAXDIRECTORS        = ifelse(length(SETMINMAXDIRECTORS_df) == 0, "No Response", SETMINMAXDIRECTORS_df$percentage)
        
        ,REGULATOR   = ifelse(length(REGULATOR_df) == 0, "No Response", REGULATOR_df$percentage) 
        ,NONMEMBERDIRECTORS   = ifelse(length(NONMEMBERDIRECTORS_df) == 0, "No Response", NONMEMBERDIRECTORS_df$percentage)
        ,NONMEMDIRECTOR   = ifelse(length(NONMEMDIRECTOR_df) == 0, "No Response", NONMEMDIRECTOR_df$percentage)
        ,AGEOFBM=AGEOFBM_df$percentage
        ,HOWLONGBMSERVE=HOWLONGBMSERVE_df$percentage
        ,DIRECTOREXECUTIVE=DIRECTOREXECUTIVE_df$percentage
        ,DIRECTORINDUSTRY=DIRECTORINDUSTRY_df$percentage
        ,DIRECTORMULTIPLE=DIRECTORMULTIPLE_df$percentage
        
        # ,AGEOFBM  =  formatC(AGEOFBM, format="f", big.mark=",", digits = 2 )
        # ,HOWLONGBMSERVE =  formatC(HOWLONGBMSERVE, format="f", big.mark=",", digits = 2 )
        # 
        # ,DIRECTOREXECUTIVE         = formatC(DIRECTOREXECUTIVE, format="f", big.mark=",", digits = 2)
        # ,DIRECTORINDUSTRY         = formatC(DIRECTORINDUSTRY, format="f", big.mark=",", digits = 2)
        # ,DIRECTORMULTIPLE         = formatC(DIRECTORMULTIPLE, format="f", big.mark=",", digits = 2)
        
        
        
      ) %>% mutate_all(~ifelse(is.nan(.), NA, .))  #%>% replace(is.na(.), "NA") 
    
    
    
  })
  
  
  peer_boardcomp1_wave2_french<-reactive({
    if (nrow(peer_boardcomp_wave2_french())==0) {
      
      df1<-as_tibble(t(peer_boardcomp_wave2_french()), rownames = "Variable")
      V1<-"text"
      peer_boardcomp1_wave2_french<-cbind(df1,V1)
    }else {
      peer_boardcomp1_wave2_french <-  as_tibble(t(peer_boardcomp_wave2_french()), rownames = "Variable")
    }
  })
  
  # peer_boardcomp1_wave2_french <-  reactive({as_tibble(t(peer_boardcomp_wave2_french()), rownames = "Variable")
  # }) 
  
  
  
  peer_boardcomp2_wave2_french <- reactive({
    peer_boardcomp1_wave2_french() %>% inner_join(boardcomp_list_wave2_french, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })  
  
  
  
  
  boardcomp_df1_wave2_french <- reactive({
    
    boardcomp_df_wave2_french() %>% 
      left_join(peer_boardcomp2_wave2_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  }) 
  
  
  # boardcomp_varlist_french <- reactive({
  #   df<-selectors_tbl_french[selected1_french(),1:2 ] # Variable, Category 
  #   boardcomp_varlist_french <-df%>%
  #     filter(Category == "Composition du conseil")  
  # })
  # own_boardcomp1_wave1_french<-own_boardcomp1(x=data1_french, wave=1,text="Données non disponibles",orglist=boardcomp_list_french)
  # own_boardcomp_df_wave1_french<-own_org_profile2(ownorg=own_boardcomp1_wave1_french(),orglist=boardcomp_list_french, varlist=boardcomp_varlist_french())
  # peer_boardcomp_wave1_french<-peer_boardcomp(x=data1_french,tb=peergroups_selected_Wave1_french(),orglist=boardcomp_list_french)
  # boardcomp_df1_wave1_french<-org_profile_df1(data1=own_boardcomp_df_wave1_french(), data2=peer_boardcomp_wave1_french())
  # 
  # own_boardcomp1_wave2_french<-own_boardcomp1(x=data1_french, wave=2,text="Données non disponibles",orglist=boardcomp_list_french)
  # own_boardcomp_df_wave2_french<-own_org_profile2(ownorg=own_boardcomp1_wave2_french(),orglist=boardcomp_list_french, varlist=boardcomp_varlist_french())
  # peer_boardcomp_wave2_french<-peer_boardcomp(x=data1_french,tb=peergroups_selected_Wave2_french(),orglist=boardcomp_list_french)
  # boardcomp_df1_wave2_french<-org_profile_df1(data1=own_boardcomp_df_wave2_french(), data2=peer_boardcomp_wave2_french())
  # ~~~Board Equity, Diversity, and Inclusion~~~  ----
  bedi_list_wave1_french <- selectors_tbl_french %>%
    filter(str_detect(Category,"Équité, diversité et inclusion au sein du conseil")) %>% 
    arrange(Order) %>%
    filter(is.na(Wave)==TRUE)
  bedi_list_wave2_french <- selectors_tbl_french %>%
    filter(str_detect(Category,"Équité, diversité et inclusion au sein du conseil")) %>% arrange(Order)
  
  own_bedi1_wave1_french <- reactive({
    
    df <- data1_french %>% 
      filter(Wave==1) %>%
      
      mutate(
        DIRECTORFEMALE         = formatC(DIRECTORFEMALE, format="f", big.mark=",", digits = 2),
        DIRECTORMINORITY         = formatC(DIRECTORMINORITY, format="f", big.mark=",", digits = 2),
        DIRECTORINDIGENOUS         = formatC(DIRECTORINDIGENOUS, format="f", big.mark=",", digits = 2)
      ) %>%  replace(is.na(.), "-") %>%  
      
      
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(bedi_list_wave1_french$Variable))
    
    #Transpose df 
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_bedi1_wave1_french<-cbind(df1,V1)
    }else {
      own_bedi1_wave1_french <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_bedi1_wave1_french <-  as_tibble(t(df), rownames = "Variable")
    
  })   
  
  
  
  own_bedi2_wave1_french <- reactive({
    own_bedi1_wave1_french() %>% inner_join(bedi_list_wave1_french, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation")
  })  
  
  
  
  bedi_df_wave1_french <- reactive({
    
    variables_selected_french() %>% filter(str_detect(Category,"Équité, diversité et inclusion au sein du conseil")) %>% 
      inner_join(own_bedi2_wave1_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
  }) 
  
  
  
  
  peer_bedi_wave1_french <- reactive({
    
    df <-   data1_french %>%
      filter(Organization %in% peergroups_selected_Wave1_french()$Organization) %>%
      filter(Wave==1) %>%
      select(starts_with(bedi_list_wave1_french$Variable))
    DIVERSITYTARGET_df<-summarydata (df,"DIVERSITYTARGET")
    DIRECTORFEMALE_df<-meandata (df,DIRECTORFEMALE,0)
    DIRECTORMINORITY_df<-meandata (df,DIRECTORMINORITY,0)
    DIRECTORINDIGENOUS_df<-meandata (df,DIRECTORINDIGENOUS,0)
    # DIVERSITYTARGET_df <- df %>% group_by(DIVERSITYTARGET) %>% arrange(DIVERSITYTARGET) %>% 
    #   filter(!is.na(DIVERSITYTARGET)) %>% 
    #   summarise(n=n()) %>% mutate(Percent = paste0(round(100 * n/sum(n), 0), "%")) %>% 
    #   unite("DIVERSITYTARGET", c(DIVERSITYTARGET,Percent), sep = ":") %>% 
    #   mutate(DIVERSITYTARGETa    = (paste(unique(DIVERSITYTARGET), collapse = ";")))%>%
    #   filter(row_number()==1) 
    
    
    
    
    peer_bedi_wave1_french<-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
                
      ) %>%
      
      mutate(
        DIRECTORFEMALE         = DIRECTORFEMALE_df$percentage,
        DIRECTORMINORITY       = DIRECTORMINORITY_df$percentage,
        DIRECTORINDIGENOUS     = DIRECTORINDIGENOUS_df$percentage,
        
        # DIRECTORFEMALE         = formatC(DIRECTORFEMALE, format="f", big.mark=",", digits = 2),
        # DIRECTORMINORITY         = formatC(DIRECTORMINORITY, format="f", big.mark=",", digits = 2),
        # DIRECTORINDIGENOUS         = formatC(DIRECTORINDIGENOUS, format="f", big.mark=",", digits = 2),
        
        DIVERSITYTARGET        = ifelse(length(DIVERSITYTARGET_df) == 0, "No Response", DIVERSITYTARGET_df$percentage)
        
      )  %>% mutate_all(~ifelse(is.nan(.), NA, .)) # %>% replace(is.na(.), "NA") 
    
  })
  
  
  peer_bedi1_wave1_french<-reactive({
    if (nrow(peer_bedi_wave1_french())==0) {
      
      df1<-as_tibble(t(peer_bedi_wave1_french()), rownames = "Variable")
      V1<-"text"
      peer_bedi1_wave1_french<-cbind(df1,V1)
    }else {
      peer_bedi1_wave1_french<-  as_tibble(t(peer_bedi_wave1_french()), rownames = "Variable")
    }
  })
  # peer_bedi1_wave1_french <-  reactive({as_tibble(t(peer_bedi_wave1_french()), rownames = "Variable")
  # }) 
  
  
  peer_bedi2_wave1_french <- reactive({
    peer_bedi1_wave1_french() %>% inner_join(bedi_list_wave1_french, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })   
  
  
  bedi_df1_wave1_french <- reactive({
    
    bedi_df_wave1_french() %>% 
      left_join(peer_bedi2_wave1_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  })
  
  own_bedi1_wave2_french <- reactive({
    
    df <- data1_french %>% 
      filter(Wave==2) %>%
      
      mutate(
        DIRECTORFEMALE         = formatC(DIRECTORFEMALE, format="f", big.mark=",", digits = 2),
        DIRECTORMINORITY         = formatC(DIRECTORMINORITY, format="f", big.mark=",", digits = 2),
        DIRECTORINDIGENOUS         = formatC(DIRECTORINDIGENOUS, format="f", big.mark=",", digits = 2),
        DIRECTORIDENTIFY_A2         = formatC(DIRECTORIDENTIFY_A2, format="f", big.mark=",", digits = 2),
        DIRECTORIDENTIFY_A3         = formatC(DIRECTORIDENTIFY_A3, format="f", big.mark=",", digits = 2)
      ) %>%  replace(is.na(.), "-") %>%  
      
      
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(bedi_list_wave2_french$Variable))
    
    #Transpose df 
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_bedi1_wave2_french<-cbind(df1,V1)
    }else {
      own_bedi1_wave2_french <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_bedi1_wave2_french <-  as_tibble(t(df), rownames = "Variable")
    
  })   
  
  
  
  own_bedi2_wave2_french <- reactive({
    own_bedi1_wave2_french() %>% inner_join(bedi_list_wave2_french, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation")
  })  
  
  
  
  bedi_df_wave2_french <- reactive({
    
    variables_selected_french() %>% filter(str_detect(Category,"Équité, diversité et inclusion au sein du conseil")) %>% 
      inner_join(own_bedi2_wave2_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
  }) 
  
  
  
  
  peer_bedi_wave2_french <- reactive({
    
    df <-   data1_french %>%
      filter(Organization %in% peergroups_selected_Wave2_french()$Organization) %>%
      filter(Wave==2) %>%
      select(starts_with(bedi_list_wave2_french$Variable))
    DIVERSITYTARGET_df<-summarydata (df,"DIVERSITYTARGET")
    LOCATIONREP_df<-summarydata (df,"LOCATIONREP")
    DIRECTORFEMALE_df<-meandata (df,DIRECTORFEMALE,0)
    DIRECTORMINORITY_df<-meandata (df,DIRECTORMINORITY,0)
    DIRECTORINDIGENOUS_df<-meandata (df,DIRECTORINDIGENOUS,0)
    DIRECTORIDENTIFY_A2_df<-meandata (df,DIRECTORIDENTIFY_A2,0)
    DIRECTORIDENTIFY_A3_df<-meandata (df,DIRECTORIDENTIFY_A3,0)
    
    # DIVERSITYTARGET_df <- df %>% group_by(DIVERSITYTARGET) %>% arrange(DIVERSITYTARGET) %>% 
    #   filter(!is.na(DIVERSITYTARGET)) %>% 
    #   summarise(n=n()) %>% mutate(Percent = paste0(round(100 * n/sum(n), 0), "%")) %>% 
    #   unite("DIVERSITYTARGET", c(DIVERSITYTARGET,Percent), sep = ":") %>% 
    #   mutate(DIVERSITYTARGETa    = (paste(unique(DIVERSITYTARGET), collapse = ";")))%>%
    #   filter(row_number()==1) 
    
    
    
    
    peer_bedi_wave2_french<-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
                
      ) %>%
      
      mutate(
        DIRECTORFEMALE         = DIRECTORFEMALE_df$percentage,
        DIRECTORMINORITY       = DIRECTORMINORITY_df$percentage,
        DIRECTORINDIGENOUS     = DIRECTORINDIGENOUS_df$percentage,
        DIRECTORIDENTIFY_A2    = DIRECTORIDENTIFY_A2_df$percentage,
        DIRECTORIDENTIFY_A3    = DIRECTORIDENTIFY_A3_df$percentage,
        
        # DIRECTORFEMALE         = formatC(DIRECTORFEMALE, format="f", big.mark=",", digits = 2),
        # DIRECTORMINORITY         = formatC(DIRECTORMINORITY, format="f", big.mark=",", digits = 2),
        # DIRECTORINDIGENOUS         = formatC(DIRECTORINDIGENOUS, format="f", big.mark=",", digits = 2),
        
        DIVERSITYTARGET        = ifelse(length(DIVERSITYTARGET_df) == 0, "No Response", DIVERSITYTARGET_df$percentage),
        LOCATIONREP        = ifelse(length(LOCATIONREP_df) == 0, "No Response", LOCATIONREP_df$percentage)
        
      )  %>% mutate_all(~ifelse(is.nan(.), NA, .)) # %>% replace(is.na(.), "NA") 
    
  })
  
  peer_bedi1_wave2_french<-reactive({
    if (nrow(peer_bedi_wave2_french())==0) {
      
      df1<-as_tibble(t(peer_bedi_wave2_french()), rownames = "Variable")
      V1<-"text"
      peer_bedi1_wave2_french<-cbind(df1,V1)
    }else {
      peer_bedi1_wave2_french<-  as_tibble(t(peer_bedi_wave2_french()), rownames = "Variable")
    }
  })
  
  # peer_bedi1_wave2_french <-  reactive({as_tibble(t(peer_bedi_wave2_french()), rownames = "Variable")
  # }) 
  
  
  peer_bedi2_wave2_french <- reactive({
    peer_bedi1_wave2_french() %>% inner_join(bedi_list_wave2_french, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })   
  
  
  bedi_df1_wave2_french <- reactive({
    
    bedi_df_wave2_french() %>% 
      left_join(peer_bedi2_wave2_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  })
  
  
  
  
  # bedi_varlist_french <- reactive({
  #   df<-selectors_tbl_french[selected1_french(),1:2 ] # Variable, Category 
  #   bedi_varlist_french <-df%>%
  #     filter(str_detect(Category,"Board Equity, Diversity, and Inclusion"))  
  # })
  # own_bedi1_wave1_french<-own_bedi1(x=data1_french, wave=1,text="Données non disponibles",orglist=bedi_list_french)
  # own_bedi_df_wave1_french<-own_org_profile2(ownorg=own_bedi1_wave1_french(),orglist=bedi_list_french, varlist=bedi_varlist_french())
  # peer_bedi_wave1_french<-peer_bedi(x=data1_french,tb=peergroups_selected_Wave1_french(),orglist=bedi_list_french)
  # bedi_df1_wave1_french<-org_profile_df1(data1=own_bedi_df_wave1_french(), data2=peer_bedi_wave1_french())
  # own_bedi1_wave2_french<-own_bedi1(x=data1_french, wave=2,text="Données non disponibles",orglist=bedi_list_french)
  # own_bedi_df_wave2_french<-own_org_profile2(ownorg=own_bedi1_wave2_french(),orglist=bedi_list_french, varlist=bedi_varlist_french())
  # peer_bedi_wave2_french<-peer_bedi(x=data1_french,tb=peergroups_selected_Wave2_french(),orglist=bedi_list_french)
  # bedi_df1_wave2_french<-org_profile_df1(data1=own_bedi_df_wave2_french(), data2=peer_bedi_wave2_french()) 
  # ~~~Chair of the Board ~~~ ----  
  cob_list_wave1_french <- selectors_tbl_french %>%
    filter(str_detect(Category,"Président du conseil")) %>% 
    arrange(Order) %>%
    filter(is.na(Wave)==TRUE)
  cob_list_wave2_french <- selectors_tbl_french %>%
    filter(str_detect(Category,"Président du conseil")) %>% arrange(Order)
  
  
  
  own_cob1_wave1_french <- reactive({
    
    df <- data1_french %>% 
      filter(Wave==1) %>%
      
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(cob_list_wave1_french$Variable)) 
    
    #Transpose df
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_cob1_wave1_french<-cbind(df1,V1)
    }else {
      own_cob1_wave1_french <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_cob1_wave1_french <-  as_tibble(t(df), rownames = "Variable")
    
  }) 
  
  
  
  own_cob2_wave1_french <- reactive({
    own_cob1_wave1_french() %>% inner_join(cob_list_wave1_french, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation") 
  })  
  
  
  
  
  cob_df_wave1_french <- reactive({
    
    variables_selected_french() %>% filter(str_detect(Category,"Président du conseil")) %>%  
      inner_join(own_cob2_wave1_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
  })    
  
  
  
  peer_cob_wave1_french <- reactive({
    
    df <-   data1_french %>%
      filter(Organization %in% peergroups_selected_Wave1_french()$Organization) %>%
      filter(Wave==1) %>%
      select(starts_with(cob_list_wave1_french$Variable))
    CHAIRSTATUS_df<-summarydatasort (df,"CHAIRSTATUS")
    CHAIRCEO_df<-summarydata (df,"CHAIRCEO")
    
    peer_cob_wave1_french<-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
                
      ) %>%
      
      mutate(
        CHAIRSTATUS        = ifelse(length(CHAIRSTATUS_df) == 0, "No Response", CHAIRSTATUS_df$percentage),
        CHAIRCEO        = ifelse(length(CHAIRCEO_df) == 0, "No Response", CHAIRCEO_df$percentage)
        
      )  %>% mutate_all(~ifelse(is.nan(.), NA, .)) #%>% replace(is.na(.), "NA") 
    
  })  
  
  
  peer_cob1_wave1_french<-reactive({
    if (nrow(peer_cob_wave1_french())==0) {
      
      df1<-as_tibble(t(peer_cob_wave1_french()), rownames = "Variable")
      V1<-"text"
      peer_cob1_wave1_french<-cbind(df1,V1)
    }else {
      peer_cob1_wave1_french <-  as_tibble(t(peer_cob_wave1_french()), rownames = "Variable")
    }
  })
  # peer_cob1_wave1_french <-  reactive({as_tibble(t(peer_cob_wave1_french()), rownames = "Variable")
  # }) 
  
  
  
  peer_cob2_wave1_french <- reactive({
    peer_cob1_wave1_french() %>% inner_join(cob_list_wave1_french, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })    
  
  
  
  cob_df1_wave1_french <- reactive({
    
    cob_df_wave1_french() %>% 
      left_join(peer_cob2_wave1_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  })   
  
  
  own_cob1_wave2_french <- reactive({
    
    df <- data1_french %>% 
      filter(Wave==2) %>%
      
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(cob_list_wave2_french$Variable)) 
    
    #Transpose df 
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_cob1_wave2_french<-cbind(df1,V1)
    }else {
      own_cob1_wave2_french <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_cob1_wave2_french <-  as_tibble(t(df), rownames = "Variable")
    
  }) 
  
  
  
  own_cob2_wave2_french <- reactive({
    own_cob1_wave2_french() %>% inner_join(cob_list_wave2_french, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation") 
  })  
  
  
  
  
  cob_df_wave2_french <- reactive({
    
    variables_selected_french() %>% filter(str_detect(Category,"Président du conseil")) %>%  
      inner_join(own_cob2_wave2_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
  })    
  
  
  
  peer_cob_wave2_french <- reactive({
    
    df <-   data1_french %>%
      filter(Organization %in% peergroups_selected_Wave2_french()$Organization) %>%
      filter(Wave==2) %>%
      select(starts_with(cob_list_wave2_french$Variable))
    CHAIRSTATUS_df<-summarydatasort (df,"CHAIRSTATUS")
    CHAIRCEO_df<-summarydata (df,"CHAIRCEO")
    
    peer_cob_wave2_french<-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
                
      ) %>%
      
      mutate(
        CHAIRSTATUS        = ifelse(length(CHAIRSTATUS_df) == 0, "No Response", CHAIRSTATUS_df$percentage),
        CHAIRCEO        = ifelse(length(CHAIRCEO_df) == 0, "No Response", CHAIRCEO_df$percentage)
        
      )  %>% mutate_all(~ifelse(is.nan(.), NA, .)) #%>% replace(is.na(.), "NA") 
    
  })  
  
  
  peer_cob1_wave2_french<-reactive({
    if (nrow(peer_cob_wave2_french())==0) {
      
      df1<-as_tibble(t(peer_cob_wave2_french()), rownames = "Variable")
      V1<-"text"
      peer_cob1_wave2_french<-cbind(df1,V1)
    }else {
      peer_cob1_wave2_french <-  as_tibble(t(peer_cob_wave2_french()), rownames = "Variable")
    }
  })
  # peer_cob1_wave2_french <-  reactive({as_tibble(t(peer_cob_wave2_french()), rownames = "Variable")
  # }) 
  
  
  
  peer_cob2_wave2_french <- reactive({
    peer_cob1_wave2_french() %>% inner_join(cob_list_wave2_french, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })    
  
  
  
  cob_df1_wave2_french <- reactive({
    
    cob_df_wave2_french() %>% 
      left_join(peer_cob2_wave2_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  })   
  
  
  
  
  
  # cob_varlist_french  <- reactive({
  #   df<-selectors_tbl_french [selected1_french (),1:2 ] # Variable, Category 
  #   cob_varlist_french  <-df%>%
  #     filter(str_detect(Category,"Président du conseil"))  
  # })
  # 
  # own_cob1_wave1_french<-own_cob1(x=data1_french, wave=1,text="Données non disponibles",orglist=cob_list_french)
  # own_cob_df_wave1_french<-own_org_profile2(ownorg=own_cob1_wave1_french(),orglist=cob_list_french, varlist=cob_varlist_french())
  # peer_cob_wave1_french<-peer_cob(x=data1_french,tb=peergroups_selected_Wave1_french(),orglist=cob_list_french)
  # cob_df1_wave1_french<-org_profile_df1(data1=own_cob_df_wave1_french(), data2=peer_cob_wave1_french())
  # own_cob1_wave2_french<-own_cob1(x=data1_french, wave=2,text="Données non disponibles",orglist=cob_list_french)
  # own_cob_df_wave2_french<-own_org_profile2(ownorg=own_cob1_wave2_french(),orglist=cob_list_french, varlist=cob_varlist_french())
  # peer_cob_wave2_french<-peer_cob(x=data1_french,tb=peergroups_selected_Wave2_french(),orglist=cob_list_french)
  # cob_df1_wave2_french<-org_profile_df1(data1=own_cob_df_wave2_french(), data2=peer_cob_wave2_french())
  #~~~Board Practices ~~~ ---- 
  
  
  bp_list_wave1_french <- selectors_tbl_french %>%
    filter(str_detect(Category,"Pratiques du conseil")) %>% 
    arrange(Order) %>%
    filter(is.na(Wave)==TRUE)
  bp_list_wave2_french <- selectors_tbl_french %>%
    filter(str_detect(Category,"Pratiques du conseil")) %>% arrange(Order)
  
  own_bp1_wave1_french <- reactive({ 
    df <- data1_french %>% 
      filter(Wave==1) %>%
      
      mutate(
        RUNREELECTION         = formatC(RUNREELECTION, format="f", big.mark=",", digits = 2),
        MAXIMUMSERVICE        = formatC(MAXIMUMSERVICE, format="f", big.mark=",", digits = 2),
        RETIREMENTAGEBM       = formatC(RETIREMENTAGEBM, format="f", big.mark=",", digits = 2),
        # TERMLIMITS            = TERMLIMITS,
        BOARDMEETINGS         = formatC(BOARDMEETINGS, format="f", big.mark=",", digits = 2),
        MEETINGDURATION        = formatC(MEETINGDURATION, format="f", big.mark=",", digits = 2),
        BOARDCOMMITTEES       = formatC(BOARDCOMMITTEES, format="f", big.mark=",", digits = 2),
        
      ) %>% 
      
      
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(bp_list_wave1_french$Variable))
    
    #Transpose df 
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_bp1_wave1_french<-cbind(df1,V1)
    }else {
      own_bp1_wave1_french <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_bp1_wave1_french <-  as_tibble(t(df), rownames = "Variable")
    
    
  }) 
  
  
  
  
  own_bp2_wave1_french <- reactive({
    own_bp1_wave1_french() %>% inner_join(bp_list_wave1_french, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation") 
  })  
  
  
  
  
  bp_df_wave1_french <- reactive({
    
    variables_selected_french() %>% filter(str_detect(Category,"Pratiques du conseil")) %>%  
      inner_join(own_bp2_wave1_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
  })  
  
  
  
  peer_bp_wave1_french <- reactive({  
    
    
    df <-   data1_french %>%
      filter(Organization %in% peergroups_selected_Wave1_french()$Organization) %>%
      filter(Wave==1) %>%
      select(starts_with(bp_list_wave1_french$Variable))  
    
    DIRECTORTRAINING_df<-summarydata (df,"DIRECTORTRAINING")
    PROCESSREMOVE_df<-summarydata (df,"PROCESSREMOVE")
    HAVETOREMOVE_df<-summarydata (df,"HAVETOREMOVE")
    POSITIONSTATEMENT_A1_df<-summarydata (df,"POSITIONSTATEMENT_A1")
    POSITIONSTATEMENT_A2_df<-summarydata (df,"POSITIONSTATEMENT_A2")
    POSITIONSTATEMENT_A3_df<-summarydata (df,"POSITIONSTATEMENT_A3")
    POSITIONSTATEMENT_A4_df<-summarydata (df,"POSITIONSTATEMENT_A4")
    POSITIONSTATEMENT_A5_df<-summarydata (df,"POSITIONSTATEMENT_A5")
    POSITIONSTATEMENT_A6_df<-summarydata (df,"POSITIONSTATEMENT_A6")
    POSITIONSTATEMENT_A7_df<-summarydata (df,"POSITIONSTATEMENT_A7")
    TOTALTERMLIMIT_df<-summarydata (df,"TOTALTERMLIMIT")
    RETIREMENTREQUIREMENT_df<-summarydata (df,"RETIREMENTREQUIREMENT")
    BOARDEVAL_df<-summarydata (df,"BOARDEVAL")
    EVALUATIONTYPEC1_df<-summarydata (df,"EVALUATIONTYPEC1")
    EVALUATIONTYPEC2_df<-summarydata (df,"EVALUATIONTYPEC2")
    EVALUATIONTYPEC3_df<-summarydata (df,"EVALUATIONTYPEC3")
    EVALUATIONFREQ_df<-summarydatasort (df,"EVALUATIONFREQ")
    EVALUATIONASPECTSC1_df<-summarydata (df,"EVALUATIONASPECTSC1")
    EVALUATIONASPECTSC2_df<-summarydata (df,"EVALUATIONASPECTSC2")
    EVALUATIONASPECTSC3_df<-summarydata (df,"EVALUATIONASPECTSC3")
    EVALUATIONASPECTSC4_df<-summarydata (df,"EVALUATIONASPECTSC4")
    EVALUATIONASPECTSC5_df<-summarydata (df,"EVALUATIONASPECTSC5")
    EVALUATIONASPECTSC6_df<-summarydata (df,"EVALUATIONASPECTSC6")
    EVALUATIONASPECTSC7_df<-summarydata (df,"EVALUATIONASPECTSC7")
    EVALUATIONASPECTSC8_df<-summarydata (df,"EVALUATIONASPECTSC8")
    EVALUATIONASPECTSC9_df<-summarydata (df,"EVALUATIONASPECTSC9")
    EVALUATIONASPECTSC10_df<-summarydata (df,"EVALUATIONASPECTSC10")
    EVALUATIONASPECTSC11_df<-summarydata (df,"EVALUATIONASPECTSC11")
    EVALUATIONASPECTSC12_df<-summarydata (df,"EVALUATIONASPECTSC12")
    EVALUATIONASPECTSC13_df<-summarydata (df,"EVALUATIONASPECTSC13")
    EVALUATIONASPECTSC14_df<-summarydata (df,"EVALUATIONASPECTSC14")
    EVALUATIONASPECTSC15_df<-summarydata (df,"EVALUATIONASPECTSC15")
    BOARDCULTUREC1_df<-summarydata (df,"BOARDCULTUREC1")
    BOARDCULTUREC2_df<-summarydata (df,"BOARDCULTUREC2")
    BOARDCULTUREC3_df<-summarydata (df,"BOARDCULTUREC3")
    BOARDCULTUREC4_df<-summarydata (df,"BOARDCULTUREC4")
    BOARDCULTUREC5_df<-summarydata (df,"BOARDCULTUREC5")
    BOARDCULTUREC6_df<-summarydata (df,"BOARDCULTUREC6")
    BOARDCULTUREC7_df<-summarydata (df,"BOARDCULTUREC7")
    ASSESSPERFORMANCE_A1_df<-summarydata (df,"ASSESSPERFORMANCE_A1")
    ASSESSPERFORMANCE_A2_df<-summarydata (df,"ASSESSPERFORMANCE_A2")
    ASSESSPERFORMANCE_A3_df<-summarydata (df,"ASSESSPERFORMANCE_A3")
    ASSESSPERFORMANCE_A4_df<-summarydata (df,"ASSESSPERFORMANCE_A4")
    ASSESSPERFORMANCE_A5_df<-summarydata (df,"ASSESSPERFORMANCE_A5")
    ASSESSPERFORMANCE_A6_df<-summarydata (df,"ASSESSPERFORMANCE_A6")
    ASSESSPERFORMANCE_A7_df<-summarydata (df,"ASSESSPERFORMANCE_A7")
    ASSESSPERFORMANCE_A8_df<-summarydata (df,"ASSESSPERFORMANCE_A8")
    ASSESSPERFORMANCE_A9_df<-summarydata (df,"ASSESSPERFORMANCE_A9")
    ASSESSPERFORMANCE_A10_df<-summarydata (df,"ASSESSPERFORMANCE_A10")
    ASSESSPERFORMANCE_A11_df<-summarydata (df,"ASSESSPERFORMANCE_A11")
    ASSESSPERFORMANCE_A12_df<-summarydata (df,"ASSESSPERFORMANCE_A12")
    ASSESSPERFORMANCE_A13_df<-summarydata (df,"ASSESSPERFORMANCE_A13")
    ASSESSPERFORMANCE_A14_df<-summarydata (df,"ASSESSPERFORMANCE_A14")
    ASSESSPERFORMANCE_A15_df<-summarydata (df,"ASSESSPERFORMANCE_A15")
    ASSESSPERFORMANCE_A16_df<-summarydata (df,"ASSESSPERFORMANCE_A16")
    ASSESSPERFORMANCE_A17_df<-summarydata (df,"ASSESSPERFORMANCE_A17")
    COMMITTEETYPESC1_df<-summarydata (df,"COMMITTEETYPESC1")
    COMMITTEETYPESC2_df<-summarydata (df,"COMMITTEETYPESC2")
    COMMITTEETYPESC3_df<-summarydata (df,"COMMITTEETYPESC3")
    COMMITTEETYPESC4_df<-summarydata (df,"COMMITTEETYPESC4")
    COMMITTEETYPESC5_df<-summarydata (df,"COMMITTEETYPESC5")
    COMMITTEETYPESC6_df<-summarydata (df,"COMMITTEETYPESC6")
    COMMITTEETYPESC7_df<-summarydata (df,"COMMITTEETYPESC7")
    COMMITTEETYPESC8_df<-summarydata (df,"COMMITTEETYPESC8")
    COMMITTEETYPESC9_df<-summarydata (df,"COMMITTEETYPESC9")
    COMMITTEETYPESC10_df<-summarydata (df,"COMMITTEETYPESC10")
    COMMITTEETYPESC11_df<-summarydata (df,"COMMITTEETYPESC11")
    COMMITTEETYPESC12_df<-summarydata (df,"COMMITTEETYPESC12")
    RUNREELECTION_df<-meandata (df,RUNREELECTION,0)
    MAXIMUMSERVICE_df<-meandata (df,MAXIMUMSERVICE,0)
    RETIREMENTAGEBM_df<-meandata (df,RETIREMENTAGEBM,0)
    BOARDMEETINGS_df<-meandata (df,BOARDMEETINGS,0)
    MEETINGDURATION_df<-meandata (df,MEETINGDURATION,0)
    BOARDCOMMITTEES_df<-meandata (df,BOARDCOMMITTEES,0)
    AFMEETINGS_df<-meandata (df,AFMEETINGS,0)
    COMPENSATIONMEETING_df<-meandata (df,COMPENSATIONMEETING,0)
    
    # TERMLIMITS_df<-summarydata (df,"TERMLIMITS")
    
    # TERMLIMITS=NA
    peer_bp_wave1_french<-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
                
      ) %>%
      
      mutate(
        DIRECTORTRAINING        = ifelse(length(DIRECTORTRAINING_df) == 0, "No Response", DIRECTORTRAINING_df$percentage),
        PROCESSREMOVE        = ifelse(length(PROCESSREMOVE_df) == 0, "No Response", PROCESSREMOVE_df$percentage),
        HAVETOREMOVE        = ifelse(length(HAVETOREMOVE_df) == 0, "No Response", HAVETOREMOVE_df$percentage),
        POSITIONSTATEMENT_A1        = ifelse(length(POSITIONSTATEMENT_A1_df) == 0, "No Response", POSITIONSTATEMENT_A1_df$percentage),
        POSITIONSTATEMENT_A2        = ifelse(length(POSITIONSTATEMENT_A2_df) == 0, "No Response", POSITIONSTATEMENT_A2_df$percentage),
        POSITIONSTATEMENT_A3        = ifelse(length(POSITIONSTATEMENT_A3_df) == 0, "No Response", POSITIONSTATEMENT_A3_df$percentage),
        POSITIONSTATEMENT_A4        = ifelse(length(POSITIONSTATEMENT_A4_df) == 0, "No Response", POSITIONSTATEMENT_A4_df$percentage),
        POSITIONSTATEMENT_A5        = ifelse(length(POSITIONSTATEMENT_A5_df) == 0, "No Response", POSITIONSTATEMENT_A5_df$percentage),
        POSITIONSTATEMENT_A6        = ifelse(length(POSITIONSTATEMENT_A6_df) == 0, "No Response", POSITIONSTATEMENT_A6_df$percentage),
        POSITIONSTATEMENT_A7        = ifelse(length(POSITIONSTATEMENT_A7_df) == 0, "No Response", POSITIONSTATEMENT_A7_df$percentage)
        # ,TERMLIMITS            =  ifelse(length(TERMLIMITS_df) == 0, "No Response", TERMLIMITS_df$percentage)
        ,TERMLIMITS            = NA
        # ,RUNREELECTION         = formatC(RUNREELECTION, format="f", big.mark=",", digits = 2)
        ,RUNREELECTION        =RUNREELECTION_df$percentage
        ,TOTALTERMLIMIT   = ifelse(length(TOTALTERMLIMIT_df) == 0, "No Response", TOTALTERMLIMIT_df$percentage)
        # ,MAXIMUMSERVICE        = formatC(MAXIMUMSERVICE, format="f", big.mark=",", digits = 2)
        ,MAXIMUMSERVICE = MAXIMUMSERVICE_df$percentage
        ,RETIREMENTREQUIREMENT        = ifelse(length(RETIREMENTREQUIREMENT_df) == 0, "No Response", RETIREMENTREQUIREMENT_df$percentage)
        # ,RETIREMENTAGEBM       = formatC(RETIREMENTAGEBM, format="f", big.mark=",", digits = 2)
        ,RETIREMENTAGEBM= RETIREMENTAGEBM_df$percentage
        ,BOARDEVAL        = ifelse(length(BOARDEVAL_df) == 0, "No Response", BOARDEVAL_df$percentage)
        ,EVALUATIONTYPEC1        = ifelse(length(EVALUATIONTYPEC1_df) == 0, "No Response", EVALUATIONTYPEC1_df$percentage)
        ,EVALUATIONTYPEC2        = ifelse(length(EVALUATIONTYPEC2_df) == 0, "No Response", EVALUATIONTYPEC2_df$percentage)
        ,EVALUATIONTYPEC3        = ifelse(length(EVALUATIONTYPEC3_df) == 0, "No Response", EVALUATIONTYPEC3_df$percentage)
        ,EVALUATIONFREQ        = ifelse(length(EVALUATIONFREQ_df) == 0, "No Response", EVALUATIONFREQ_df$percentage)
        ,EVALUATIONASPECTSC1        = ifelse(length(EVALUATIONASPECTSC1_df) == 0, "No Response", EVALUATIONASPECTSC1_df$percentage)
        ,EVALUATIONASPECTSC2        = ifelse(length(EVALUATIONASPECTSC2_df) == 0, "No Response", EVALUATIONASPECTSC2_df$percentage)
        ,EVALUATIONASPECTSC3        = ifelse(length(EVALUATIONASPECTSC3_df) == 0, "No Response", EVALUATIONASPECTSC3_df$percentage)
        ,EVALUATIONASPECTSC4        = ifelse(length(EVALUATIONASPECTSC4_df) == 0, "No Response", EVALUATIONASPECTSC4_df$percentage)
        ,EVALUATIONASPECTSC5        = ifelse(length(EVALUATIONASPECTSC5_df) == 0, "No Response", EVALUATIONASPECTSC5_df$percentage)
        ,EVALUATIONASPECTSC6        = ifelse(length(EVALUATIONASPECTSC6_df) == 0, "No Response", EVALUATIONASPECTSC6_df$percentage)
        ,EVALUATIONASPECTSC7        = ifelse(length(EVALUATIONASPECTSC7_df) == 0, "No Response", EVALUATIONASPECTSC7_df$percentage)
        ,EVALUATIONASPECTSC8        = ifelse(length(EVALUATIONASPECTSC8_df) == 0, "No Response", EVALUATIONASPECTSC8_df$percentage)
        ,EVALUATIONASPECTSC9        = ifelse(length(EVALUATIONASPECTSC9_df) == 0, "No Response", EVALUATIONASPECTSC9_df$percentage)
        ,EVALUATIONASPECTSC10        = ifelse(length(EVALUATIONASPECTSC10_df) == 0, "No Response", EVALUATIONASPECTSC10_df$percentage)
        ,EVALUATIONASPECTSC11        = ifelse(length(EVALUATIONASPECTSC11_df) == 0, "No Response", EVALUATIONASPECTSC11_df$percentage)
        ,EVALUATIONASPECTSC12        = ifelse(length(EVALUATIONASPECTSC12_df) == 0, "No Response", EVALUATIONASPECTSC12_df$percentage)
        ,EVALUATIONASPECTSC13        = ifelse(length(EVALUATIONASPECTSC13_df) == 0, "No Response", EVALUATIONASPECTSC13_df$percentage)
        ,EVALUATIONASPECTSC14        = ifelse(length(EVALUATIONASPECTSC14_df) == 0, "No Response", EVALUATIONASPECTSC14_df$percentage)
        ,EVALUATIONASPECTSC15        = ifelse(length(EVALUATIONASPECTSC15_df) == 0, "No Response", EVALUATIONASPECTSC15_df$percentage)
        ,BOARDCULTUREC1              = ifelse(length(BOARDCULTUREC1_df) == 0, "No Response", BOARDCULTUREC1_df$percentage)
        ,BOARDCULTUREC2              = ifelse(length(BOARDCULTUREC2_df) == 0, "No Response", BOARDCULTUREC2_df$percentage)
        ,BOARDCULTUREC3              = ifelse(length(BOARDCULTUREC3_df) == 0, "No Response", BOARDCULTUREC3_df$percentage)
        ,BOARDCULTUREC4              = ifelse(length(BOARDCULTUREC4_df) == 0, "No Response", BOARDCULTUREC4_df$percentage)
        ,BOARDCULTUREC5              = ifelse(length(BOARDCULTUREC5_df) == 0, "No Response", BOARDCULTUREC5_df$percentage)
        ,BOARDCULTUREC6              = ifelse(length(BOARDCULTUREC6_df) == 0, "No Response", BOARDCULTUREC6_df$percentage)
        ,BOARDCULTUREC7              = ifelse(length(BOARDCULTUREC7_df) == 0, "No Response", BOARDCULTUREC7_df$percentage)
        
        
        ,ASSESSPERFORMANCE_A1        = ifelse(length(ASSESSPERFORMANCE_A1_df) == 0, "No Response", ASSESSPERFORMANCE_A1_df$percentage)
        ,ASSESSPERFORMANCE_A2        = ifelse(length(ASSESSPERFORMANCE_A2_df) == 0, "No Response", ASSESSPERFORMANCE_A2_df$percentage)
        ,ASSESSPERFORMANCE_A3        = ifelse(length(ASSESSPERFORMANCE_A3_df) == 0, "No Response", ASSESSPERFORMANCE_A3_df$percentage)
        ,ASSESSPERFORMANCE_A4        = ifelse(length(ASSESSPERFORMANCE_A4_df) == 0, "No Response", ASSESSPERFORMANCE_A4_df$percentage)
        ,ASSESSPERFORMANCE_A5        = ifelse(length(ASSESSPERFORMANCE_A5_df) == 0, "No Response", ASSESSPERFORMANCE_A5_df$percentage)
        ,ASSESSPERFORMANCE_A6        = ifelse(length(ASSESSPERFORMANCE_A6_df) == 0, "No Response", ASSESSPERFORMANCE_A6_df$percentage)
        ,ASSESSPERFORMANCE_A7        = ifelse(length(ASSESSPERFORMANCE_A7_df) == 0, "No Response", ASSESSPERFORMANCE_A7_df$percentage)
        ,ASSESSPERFORMANCE_A8        = ifelse(length(ASSESSPERFORMANCE_A8_df) == 0, "No Response", ASSESSPERFORMANCE_A8_df$percentage)
        ,ASSESSPERFORMANCE_A9        = ifelse(length(ASSESSPERFORMANCE_A9_df) == 0, "No Response", ASSESSPERFORMANCE_A9_df$percentage)
        ,ASSESSPERFORMANCE_A10        = ifelse(length(ASSESSPERFORMANCE_A10_df) == 0, "No Response", ASSESSPERFORMANCE_A10_df$percentage)
        ,ASSESSPERFORMANCE_A11        = ifelse(length(ASSESSPERFORMANCE_A11_df) == 0, "No Response", ASSESSPERFORMANCE_A11_df$percentage)
        ,ASSESSPERFORMANCE_A12        = ifelse(length(ASSESSPERFORMANCE_A12_df) == 0, "No Response", ASSESSPERFORMANCE_A12_df$percentage)
        ,ASSESSPERFORMANCE_A13        = ifelse(length(ASSESSPERFORMANCE_A13_df) == 0, "No Response", ASSESSPERFORMANCE_A13_df$percentage)
        ,ASSESSPERFORMANCE_A14        = ifelse(length(ASSESSPERFORMANCE_A14_df) == 0, "No Response", ASSESSPERFORMANCE_A14_df$percentage)
        ,ASSESSPERFORMANCE_A15        = ifelse(length(ASSESSPERFORMANCE_A15_df) == 0, "No Response", ASSESSPERFORMANCE_A15_df$percentage)
        ,ASSESSPERFORMANCE_A16        = ifelse(length(ASSESSPERFORMANCE_A16_df) == 0, "No Response", ASSESSPERFORMANCE_A16_df$percentage)
        ,ASSESSPERFORMANCE_A17        = ifelse(length(ASSESSPERFORMANCE_A17_df) == 0, "No Response", ASSESSPERFORMANCE_A17_df$percentage)
        
        # ,BOARDMEETINGS         = formatC(BOARDMEETINGS, format="f", big.mark=",", digits = 2)
        # ,MEETINGDURATION        = formatC(MEETINGDURATION, format="f", big.mark=",", digits = 2)
        # ,BOARDCOMMITTEES       = formatC(BOARDCOMMITTEES, format="f", big.mark=",", digits = 2) 
        ,BOARDMEETINGS=BOARDMEETINGS_df$percentage
        ,MEETINGDURATION=MEETINGDURATION_df$percentage
        ,BOARDCOMMITTEES=BOARDCOMMITTEES_df$percentage
        
        
        
        ,COMMITTEETYPESC1        = ifelse(length(COMMITTEETYPESC1_df) == 0, "No Response", COMMITTEETYPESC1_df$percentage)
        ,COMMITTEETYPESC2        = ifelse(length(COMMITTEETYPESC2_df) == 0, "No Response", COMMITTEETYPESC2_df$percentage)
        ,COMMITTEETYPESC3        = ifelse(length(COMMITTEETYPESC3_df) == 0, "No Response", COMMITTEETYPESC3_df$percentage)
        ,COMMITTEETYPESC4        = ifelse(length(COMMITTEETYPESC4_df) == 0, "No Response", COMMITTEETYPESC4_df$percentage)
        ,COMMITTEETYPESC5        = ifelse(length(COMMITTEETYPESC5_df) == 0, "No Response", COMMITTEETYPESC5_df$percentage)
        ,COMMITTEETYPESC6        = ifelse(length(COMMITTEETYPESC6_df) == 0, "No Response", COMMITTEETYPESC6_df$percentage)
        ,COMMITTEETYPESC7        = ifelse(length(COMMITTEETYPESC7_df) == 0, "No Response", COMMITTEETYPESC7_df$percentage)
        ,COMMITTEETYPESC8        = ifelse(length(COMMITTEETYPESC8_df) == 0, "No Response", COMMITTEETYPESC8_df$percentage)
        ,COMMITTEETYPESC9        = ifelse(length(COMMITTEETYPESC9_df) == 0, "No Response", COMMITTEETYPESC9_df$percentage)
        ,COMMITTEETYPESC10        = ifelse(length(COMMITTEETYPESC10_df) == 0, "No Response", COMMITTEETYPESC10_df$percentage)
        ,COMMITTEETYPESC11        = ifelse(length(COMMITTEETYPESC11_df) == 0, "No Response", COMMITTEETYPESC11_df$percentage)
        ,COMMITTEETYPESC12        = ifelse(length(COMMITTEETYPESC12_df) == 0, "No Response", COMMITTEETYPESC12_df$percentage)
        # ,AFMEETINGS         = formatC(AFMEETINGS, format="f", big.mark=",", digits = 2)
        # ,COMPENSATIONMEETING         = formatC(COMPENSATIONMEETING, format="f", big.mark=",", digits = 2)
        ,AFMEETINGS=AFMEETINGS_df$percentage
        ,COMPENSATIONMEETING=COMPENSATIONMEETING_df$percentage
        
      ) %>% mutate_all(~ifelse(is.nan(.), NA, .)) #%>% replace(is.na(.), "NA") 
    
  })
  
  
  
  peer_bp1_wave1_french<-reactive({
    if (nrow(peer_bp_wave1_french())==0) {
      
      df1<-as_tibble(t(peer_bp_wave1_french()), rownames = "Variable")
      V1<-"text"
      peer_bp1_wave1_french<-cbind(df1,V1)
    }else {
      peer_bp1_wave1_french <-  as_tibble(t(peer_bp_wave1_french()), rownames = "Variable")
    }
  })
  # peer_bp1_wave1_french <-  reactive({as_tibble(t(peer_bp_wave1_french()), rownames = "Variable")
  # }) 
  
  
  
  peer_bp2_wave1_french <- reactive({
    peer_bp1_wave1_french() %>% inner_join(bp_list_wave1_french, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })  
  
  
  
  bp_df1_wave1_french <- reactive({
    
    bp_df_wave1_french() %>% 
      left_join(peer_bp2_wave1_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  })  
  
  
  own_bp1_wave2_french <- reactive({ 
    df <- data1_french %>% 
      filter(Wave==2) %>%
      
      mutate(
        RUNREELECTION         = formatC(RUNREELECTION, format="f", big.mark=",", digits = 2),
        MAXIMUMSERVICE        = formatC(MAXIMUMSERVICE, format="f", big.mark=",", digits = 2),
        RETIREMENTAGEBM       = formatC(RETIREMENTAGEBM, format="f", big.mark=",", digits = 2),
        # TERMLIMITS            = TERMLIMITS,
        BOARDMEETINGS         = formatC(BOARDMEETINGS, format="f", big.mark=",", digits = 2),
        MEETINGDURATION        = formatC(MEETINGDURATION, format="f", big.mark=",", digits = 2),
        BOARDCOMMITTEES       = formatC(BOARDCOMMITTEES, format="f", big.mark=",", digits = 2),
        
      ) %>% 
      
      
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(bp_list_wave2_french$Variable))
    
    #Transpose df 
    # own_bp1_wave2_french <-  as_tibble(t(df), rownames = "Variable")
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_bp1_wave2_french<-cbind(df1,V1)
    }else {
      own_bp1_wave2_french <-  as_tibble(t(df), rownames = "Variable")
    }
    
    
  }) 
  
  
  
  
  own_bp2_wave2_french <- reactive({
    own_bp1_wave2_french() %>% inner_join(bp_list_wave2_french, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation") 
  })  
  
  
  
  
  bp_df_wave2_french <- reactive({
    
    variables_selected_french() %>% filter(str_detect(Category,"Pratiques du conseil")) %>%  
      inner_join(own_bp2_wave2_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
  })  
  
  # ,TERMLIMITS            =  NA
  
  peer_bp_wave2_french <- reactive({  
    
    
    df <-   data1_french %>%
      filter(Organization %in% peergroups_selected_Wave2_french()$Organization) %>%
      filter(Wave==2)%>%
      select(starts_with(bp_list_wave2_french$Variable))  
    
    DIRECTORTRAINING_df<-summarydata (df,"DIRECTORTRAINING")
    PROCESSREMOVE_df<-summarydata (df,"PROCESSREMOVE")
    HAVETOREMOVE_df<-summarydata (df,"HAVETOREMOVE")
    POSITIONSTATEMENT_A1_df<-summarydata (df,"POSITIONSTATEMENT_A1")
    POSITIONSTATEMENT_A2_df<-summarydata (df,"POSITIONSTATEMENT_A2")
    POSITIONSTATEMENT_A3_df<-summarydata (df,"POSITIONSTATEMENT_A3")
    POSITIONSTATEMENT_A4_df<-summarydata (df,"POSITIONSTATEMENT_A4")
    POSITIONSTATEMENT_A5_df<-summarydata (df,"POSITIONSTATEMENT_A5")
    POSITIONSTATEMENT_A6_df<-summarydata (df,"POSITIONSTATEMENT_A6")
    POSITIONSTATEMENT_A7_df<-summarydata (df,"POSITIONSTATEMENT_A7")
    TOTALTERMLIMIT_df<-summarydata (df,"TOTALTERMLIMIT")
    RETIREMENTREQUIREMENT_df<-summarydata (df,"RETIREMENTREQUIREMENT")
    BOARDEVAL_df<-summarydata (df,"BOARDEVAL")
    EVALUATIONTYPEC1_df<-summarydata (df,"EVALUATIONTYPEC1")
    EVALUATIONTYPEC2_df<-summarydata (df,"EVALUATIONTYPEC2")
    EVALUATIONTYPEC3_df<-summarydata (df,"EVALUATIONTYPEC3")
    EVALUATIONFREQ_df<-summarydatasort (df,"EVALUATIONFREQ")
    EVALUATIONASPECTSC1_df<-summarydata (df,"EVALUATIONASPECTSC1")
    EVALUATIONASPECTSC2_df<-summarydata (df,"EVALUATIONASPECTSC2")
    EVALUATIONASPECTSC3_df<-summarydata (df,"EVALUATIONASPECTSC3")
    EVALUATIONASPECTSC4_df<-summarydata (df,"EVALUATIONASPECTSC4")
    EVALUATIONASPECTSC5_df<-summarydata (df,"EVALUATIONASPECTSC5")
    EVALUATIONASPECTSC6_df<-summarydata (df,"EVALUATIONASPECTSC6")
    EVALUATIONASPECTSC7_df<-summarydata (df,"EVALUATIONASPECTSC7")
    EVALUATIONASPECTSC8_df<-summarydata (df,"EVALUATIONASPECTSC8")
    EVALUATIONASPECTSC9_df<-summarydata (df,"EVALUATIONASPECTSC9")
    EVALUATIONASPECTSC10_df<-summarydata (df,"EVALUATIONASPECTSC10")
    EVALUATIONASPECTSC11_df<-summarydata (df,"EVALUATIONASPECTSC11")
    EVALUATIONASPECTSC12_df<-summarydata (df,"EVALUATIONASPECTSC12")
    EVALUATIONASPECTSC13_df<-summarydata (df,"EVALUATIONASPECTSC13")
    EVALUATIONASPECTSC14_df<-summarydata (df,"EVALUATIONASPECTSC14")
    EVALUATIONASPECTSC15_df<-summarydata (df,"EVALUATIONASPECTSC15")
    BOARDCULTUREC1_df<-summarydata (df,"BOARDCULTUREC1")
    BOARDCULTUREC2_df<-summarydata (df,"BOARDCULTUREC2")
    BOARDCULTUREC3_df<-summarydata (df,"BOARDCULTUREC3")
    BOARDCULTUREC4_df<-summarydata (df,"BOARDCULTUREC4")
    BOARDCULTUREC5_df<-summarydata (df,"BOARDCULTUREC5")
    BOARDCULTUREC6_df<-summarydata (df,"BOARDCULTUREC6")
    BOARDCULTUREC7_df<-summarydata (df,"BOARDCULTUREC7")
    ASSESSPERFORMANCE_A1_df<-summarydata (df,"ASSESSPERFORMANCE_A1")
    ASSESSPERFORMANCE_A2_df<-summarydata (df,"ASSESSPERFORMANCE_A2")
    ASSESSPERFORMANCE_A3_df<-summarydata (df,"ASSESSPERFORMANCE_A3")
    ASSESSPERFORMANCE_A4_df<-summarydata (df,"ASSESSPERFORMANCE_A4")
    ASSESSPERFORMANCE_A5_df<-summarydata (df,"ASSESSPERFORMANCE_A5")
    ASSESSPERFORMANCE_A6_df<-summarydata (df,"ASSESSPERFORMANCE_A6")
    ASSESSPERFORMANCE_A7_df<-summarydata (df,"ASSESSPERFORMANCE_A7")
    ASSESSPERFORMANCE_A8_df<-summarydata (df,"ASSESSPERFORMANCE_A8")
    ASSESSPERFORMANCE_A9_df<-summarydata (df,"ASSESSPERFORMANCE_A9")
    ASSESSPERFORMANCE_A10_df<-summarydata (df,"ASSESSPERFORMANCE_A10")
    ASSESSPERFORMANCE_A11_df<-summarydata (df,"ASSESSPERFORMANCE_A11")
    ASSESSPERFORMANCE_A12_df<-summarydata (df,"ASSESSPERFORMANCE_A12")
    ASSESSPERFORMANCE_A13_df<-summarydata (df,"ASSESSPERFORMANCE_A13")
    ASSESSPERFORMANCE_A14_df<-summarydata (df,"ASSESSPERFORMANCE_A14")
    ASSESSPERFORMANCE_A15_df<-summarydata (df,"ASSESSPERFORMANCE_A15")
    ASSESSPERFORMANCE_A16_df<-summarydata (df,"ASSESSPERFORMANCE_A16")
    ASSESSPERFORMANCE_A17_df<-summarydata (df,"ASSESSPERFORMANCE_A17")
    COMMITTEETYPESC1_df<-summarydata (df,"COMMITTEETYPESC1")
    COMMITTEETYPESC2_df<-summarydata (df,"COMMITTEETYPESC2")
    COMMITTEETYPESC3_df<-summarydata (df,"COMMITTEETYPESC3")
    COMMITTEETYPESC4_df<-summarydata (df,"COMMITTEETYPESC4")
    COMMITTEETYPESC5_df<-summarydata (df,"COMMITTEETYPESC5")
    COMMITTEETYPESC6_df<-summarydata (df,"COMMITTEETYPESC6")
    COMMITTEETYPESC7_df<-summarydata (df,"COMMITTEETYPESC7")
    COMMITTEETYPESC8_df<-summarydata (df,"COMMITTEETYPESC8")
    COMMITTEETYPESC9_df<-summarydata (df,"COMMITTEETYPESC9")
    COMMITTEETYPESC10_df<-summarydata (df,"COMMITTEETYPESC10")
    COMMITTEETYPESC11_df<-summarydata (df,"COMMITTEETYPESC11")
    COMMITTEETYPESC12_df<-summarydata (df,"COMMITTEETYPESC12")
    TERMLIMITS_df<-summarydata (df,"TERMLIMITS")
    RECRUITMEMBERS_A1_df<-summarydata (df,"RECRUITMEMBERS_A1")
    RECRUITMEMBERS_A2_df<-summarydata (df,"RECRUITMEMBERS_A2")
    RECRUITMEMBERS_A3_df<-summarydata (df,"RECRUITMEMBERS_A3")
    RECRUITMEMBERS_A4_df<-summarydata (df,"RECRUITMEMBERS_A4")
    RECRUITMEMBERS_A5_df<-summarydata (df,"RECRUITMEMBERS_A5")
    RECRUITMEMBERS_A6_df<-summarydata (df,"RECRUITMEMBERS_A6")
    ONBOARDING_A1_df<-summarydata (df,"ONBOARDING_A1")
    ONBOARDING_A2_df<-summarydata (df,"ONBOARDING_A2")
    ONBOARDING_A3_df<-summarydata (df,"ONBOARDING_A3")
    ONBOARDING_A4_df<-summarydata (df,"ONBOARDING_A4")
    ONBOARDING_A5_df<-summarydata (df,"ONBOARDING_A5")
    ONBOARDING_A6_df<-summarydata (df,"ONBOARDING_A6")
    ONBOARDING_A7_df<-summarydata (df,"ONBOARDING_A7")
    ONBOARDING_A8_df<-summarydata (df,"ONBOARDING_A8")
    MEMBERTRAINING_A1_df<-summarydata (df,"MEMBERTRAINING_A1")
    MEMBERTRAINING_A2_df<-summarydata (df,"MEMBERTRAINING_A2")
    MEMBERTRAINING_A3_df<-summarydata (df,"MEMBERTRAINING_A3")
    MEMBERTRAINING_A4_df<-summarydata (df,"MEMBERTRAINING_A4")
    MEMBERTRAINING_A5_df<-summarydata (df,"MEMBERTRAINING_A5")
    MEMBERTRAINING_A6_df<-summarydata (df,"MEMBERTRAINING_A6")
    MEMBERTRAINING_A7_df<-summarydata (df,"MEMBERTRAINING_A7")
    MEMBERTRAINING_A8_df<-summarydata (df,"MEMBERTRAINING_A8")
    MEMBERTRAINING_A9_df<-summarydata (df,"MEMBERTRAINING_A9")
    MEMBERTRAINING_A10_df<-summarydata (df,"MEMBERTRAINING_A10")
    MEMBERTRAINING_A11_df<-summarydata (df,"MEMBERTRAINING_A11")
    INCAMERAPROPORTION_df<-summarydata (df,"INCAMERAPROPORTION")
    INCAMERAEXECUTIVES_A1_df<-summarydata (df,"INCAMERAEXECUTIVES_A1")
    INCAMERAEXECUTIVES_A2_df<-summarydata (df,"INCAMERAEXECUTIVES_A2")
    INCAMERAEXECUTIVES_A3_df<-summarydata (df,"INCAMERAEXECUTIVES_A3")
    INCAMERAEXECUTIVES_A4_df<-summarydata (df,"INCAMERAEXECUTIVES_A4")
    INCAMERAEXECUTIVES_A5_df<-summarydata (df,"INCAMERAEXECUTIVES_A5")
    AFINCAMERA_A1_df<-summarydata (df,"AFINCAMERA_A1")
    AFINCAMERA_A2_df<-summarydata (df,"AFINCAMERA_A2")
    AFINCAMERA_A3_df<-summarydata (df,"AFINCAMERA_A3")
    GOVINCAMERA_A1_df<-summarydata (df,"GOVINCAMERA_A1")
    HRINCAMERA_A1_df<-summarydata (df,"HRINCAMERA_A1")
    RUNREELECTION_df<-meandata (df,RUNREELECTION,0)
    MAXIMUMSERVICE_df<-meandata (df,MAXIMUMSERVICE,0)
    RETIREMENTAGEBM_df<-meandata (df,RETIREMENTAGEBM,0)
    BOARDMEETINGS_df<-meandata (df,BOARDMEETINGS,0)
    MEETINGDURATION_df<-meandata (df,MEETINGDURATION,0)
    BOARDCOMMITTEES_df<-meandata (df,BOARDCOMMITTEES,0)
    AFMEETINGS_df<-meandata (df,AFMEETINGS,0)
    COMPENSATIONMEETING_df<-meandata (df,COMPENSATIONMEETING,0)
    peer_bp_wave2<-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
                
      ) %>%
      
      mutate(
        DIRECTORTRAINING        = ifelse(length(DIRECTORTRAINING_df) == 0, "No Response", DIRECTORTRAINING_df$percentage),
        PROCESSREMOVE        = ifelse(length(PROCESSREMOVE_df) == 0, "No Response", PROCESSREMOVE_df$percentage),
        HAVETOREMOVE        = ifelse(length(HAVETOREMOVE_df) == 0, "No Response", HAVETOREMOVE_df$percentage),
        POSITIONSTATEMENT_A1        = ifelse(length(POSITIONSTATEMENT_A1_df) == 0, "No Response", POSITIONSTATEMENT_A1_df$percentage),
        POSITIONSTATEMENT_A2        = ifelse(length(POSITIONSTATEMENT_A2_df) == 0, "No Response", POSITIONSTATEMENT_A2_df$percentage),
        POSITIONSTATEMENT_A3        = ifelse(length(POSITIONSTATEMENT_A3_df) == 0, "No Response", POSITIONSTATEMENT_A3_df$percentage),
        POSITIONSTATEMENT_A4        = ifelse(length(POSITIONSTATEMENT_A4_df) == 0, "No Response", POSITIONSTATEMENT_A4_df$percentage),
        POSITIONSTATEMENT_A5        = ifelse(length(POSITIONSTATEMENT_A5_df) == 0, "No Response", POSITIONSTATEMENT_A5_df$percentage),
        POSITIONSTATEMENT_A6        = ifelse(length(POSITIONSTATEMENT_A6_df) == 0, "No Response", POSITIONSTATEMENT_A6_df$percentage),
        POSITIONSTATEMENT_A7        = ifelse(length(POSITIONSTATEMENT_A7_df) == 0, "No Response", POSITIONSTATEMENT_A7_df$percentage)
        ,TERMLIMITS        = ifelse(length(TERMLIMITS_df) == 0, "No Response", TERMLIMITS_df$percentage)
        # ,RUNREELECTION         = formatC(RUNREELECTION, format="f", big.mark=",", digits = 2)
        ,RUNREELECTION = RUNREELECTION_df$percentage
        ,TOTALTERMLIMIT   = ifelse(length(TOTALTERMLIMIT_df) == 0, "No Response", TOTALTERMLIMIT_df$percentage)
        # ,MAXIMUMSERVICE        = formatC(MAXIMUMSERVICE, format="f", big.mark=",", digits = 2)
        ,MAXIMUMSERVICE = MAXIMUMSERVICE_df$percentage
        ,RETIREMENTREQUIREMENT        = ifelse(length(RETIREMENTREQUIREMENT_df) == 0, "No Response", RETIREMENTREQUIREMENT_df$percentage)
        # ,RETIREMENTAGEBM       = formatC(RETIREMENTAGEBM, format="f", big.mark=",", digits = 2)
        ,RETIREMENTAGEBM = RETIREMENTAGEBM_df$percentage
        ,BOARDEVAL        = ifelse(length(BOARDEVAL_df) == 0, "No Response", BOARDEVAL_df$percentage)
        ,EVALUATIONTYPEC1        = ifelse(length(EVALUATIONTYPEC1_df) == 0, "No Response", EVALUATIONTYPEC1_df$percentage)
        ,EVALUATIONTYPEC2        = ifelse(length(EVALUATIONTYPEC2_df) == 0, "No Response", EVALUATIONTYPEC2_df$percentage)
        ,EVALUATIONTYPEC3        = ifelse(length(EVALUATIONTYPEC3_df) == 0, "No Response", EVALUATIONTYPEC3_df$percentage)
        ,EVALUATIONFREQ        = ifelse(length(EVALUATIONFREQ_df) == 0, "No Response", EVALUATIONFREQ_df$percentage)
        ,EVALUATIONASPECTSC1        = ifelse(length(EVALUATIONASPECTSC1_df) == 0, "No Response", EVALUATIONASPECTSC1_df$percentage)
        ,EVALUATIONASPECTSC2        = ifelse(length(EVALUATIONASPECTSC2_df) == 0, "No Response", EVALUATIONASPECTSC2_df$percentage)
        ,EVALUATIONASPECTSC3        = ifelse(length(EVALUATIONASPECTSC3_df) == 0, "No Response", EVALUATIONASPECTSC3_df$percentage)
        ,EVALUATIONASPECTSC4        = ifelse(length(EVALUATIONASPECTSC4_df) == 0, "No Response", EVALUATIONASPECTSC4_df$percentage)
        ,EVALUATIONASPECTSC5        = ifelse(length(EVALUATIONASPECTSC5_df) == 0, "No Response", EVALUATIONASPECTSC5_df$percentage)
        ,EVALUATIONASPECTSC6        = ifelse(length(EVALUATIONASPECTSC6_df) == 0, "No Response", EVALUATIONASPECTSC6_df$percentage)
        ,EVALUATIONASPECTSC7        = ifelse(length(EVALUATIONASPECTSC7_df) == 0, "No Response", EVALUATIONASPECTSC7_df$percentage)
        ,EVALUATIONASPECTSC8        = ifelse(length(EVALUATIONASPECTSC8_df) == 0, "No Response", EVALUATIONASPECTSC8_df$percentage)
        ,EVALUATIONASPECTSC9        = ifelse(length(EVALUATIONASPECTSC9_df) == 0, "No Response", EVALUATIONASPECTSC9_df$percentage)
        ,EVALUATIONASPECTSC10        = ifelse(length(EVALUATIONASPECTSC10_df) == 0, "No Response", EVALUATIONASPECTSC10_df$percentage)
        ,EVALUATIONASPECTSC11        = ifelse(length(EVALUATIONASPECTSC11_df) == 0, "No Response", EVALUATIONASPECTSC11_df$percentage)
        ,EVALUATIONASPECTSC12        = ifelse(length(EVALUATIONASPECTSC12_df) == 0, "No Response", EVALUATIONASPECTSC12_df$percentage)
        ,EVALUATIONASPECTSC13        = ifelse(length(EVALUATIONASPECTSC13_df) == 0, "No Response", EVALUATIONASPECTSC13_df$percentage)
        ,EVALUATIONASPECTSC14        = ifelse(length(EVALUATIONASPECTSC14_df) == 0, "No Response", EVALUATIONASPECTSC14_df$percentage)
        ,EVALUATIONASPECTSC15        = ifelse(length(EVALUATIONASPECTSC15_df) == 0, "No Response", EVALUATIONASPECTSC15_df$percentage)
        ,BOARDCULTUREC1              = ifelse(length(BOARDCULTUREC1_df) == 0, "No Response", BOARDCULTUREC1_df$percentage)
        ,BOARDCULTUREC2              = ifelse(length(BOARDCULTUREC2_df) == 0, "No Response", BOARDCULTUREC2_df$percentage)
        ,BOARDCULTUREC3              = ifelse(length(BOARDCULTUREC3_df) == 0, "No Response", BOARDCULTUREC3_df$percentage)
        ,BOARDCULTUREC4              = ifelse(length(BOARDCULTUREC4_df) == 0, "No Response", BOARDCULTUREC4_df$percentage)
        ,BOARDCULTUREC5              = ifelse(length(BOARDCULTUREC5_df) == 0, "No Response", BOARDCULTUREC5_df$percentage)
        ,BOARDCULTUREC6              = ifelse(length(BOARDCULTUREC6_df) == 0, "No Response", BOARDCULTUREC6_df$percentage)
        ,BOARDCULTUREC7              = ifelse(length(BOARDCULTUREC7_df) == 0, "No Response", BOARDCULTUREC7_df$percentage)
        
        
        ,ASSESSPERFORMANCE_A1        = ifelse(length(ASSESSPERFORMANCE_A1_df) == 0, "No Response", ASSESSPERFORMANCE_A1_df$percentage)
        ,ASSESSPERFORMANCE_A2        = ifelse(length(ASSESSPERFORMANCE_A2_df) == 0, "No Response", ASSESSPERFORMANCE_A2_df$percentage)
        ,ASSESSPERFORMANCE_A3        = ifelse(length(ASSESSPERFORMANCE_A3_df) == 0, "No Response", ASSESSPERFORMANCE_A3_df$percentage)
        ,ASSESSPERFORMANCE_A4        = ifelse(length(ASSESSPERFORMANCE_A4_df) == 0, "No Response", ASSESSPERFORMANCE_A4_df$percentage)
        ,ASSESSPERFORMANCE_A5        = ifelse(length(ASSESSPERFORMANCE_A5_df) == 0, "No Response", ASSESSPERFORMANCE_A5_df$percentage)
        ,ASSESSPERFORMANCE_A6        = ifelse(length(ASSESSPERFORMANCE_A6_df) == 0, "No Response", ASSESSPERFORMANCE_A6_df$percentage)
        ,ASSESSPERFORMANCE_A7        = ifelse(length(ASSESSPERFORMANCE_A7_df) == 0, "No Response", ASSESSPERFORMANCE_A7_df$percentage)
        ,ASSESSPERFORMANCE_A8        = ifelse(length(ASSESSPERFORMANCE_A8_df) == 0, "No Response", ASSESSPERFORMANCE_A8_df$percentage)
        ,ASSESSPERFORMANCE_A9        = ifelse(length(ASSESSPERFORMANCE_A9_df) == 0, "No Response", ASSESSPERFORMANCE_A9_df$percentage)
        ,ASSESSPERFORMANCE_A10        = ifelse(length(ASSESSPERFORMANCE_A10_df) == 0, "No Response", ASSESSPERFORMANCE_A10_df$percentage)
        ,ASSESSPERFORMANCE_A11        = ifelse(length(ASSESSPERFORMANCE_A11_df) == 0, "No Response", ASSESSPERFORMANCE_A11_df$percentage)
        ,ASSESSPERFORMANCE_A12        = ifelse(length(ASSESSPERFORMANCE_A12_df) == 0, "No Response", ASSESSPERFORMANCE_A12_df$percentage)
        ,ASSESSPERFORMANCE_A13        = ifelse(length(ASSESSPERFORMANCE_A13_df) == 0, "No Response", ASSESSPERFORMANCE_A13_df$percentage)
        ,ASSESSPERFORMANCE_A14        = ifelse(length(ASSESSPERFORMANCE_A14_df) == 0, "No Response", ASSESSPERFORMANCE_A14_df$percentage)
        ,ASSESSPERFORMANCE_A15        = ifelse(length(ASSESSPERFORMANCE_A15_df) == 0, "No Response", ASSESSPERFORMANCE_A15_df$percentage)
        ,ASSESSPERFORMANCE_A16        = ifelse(length(ASSESSPERFORMANCE_A16_df) == 0, "No Response", ASSESSPERFORMANCE_A16_df$percentage)
        ,ASSESSPERFORMANCE_A17        = ifelse(length(ASSESSPERFORMANCE_A17_df) == 0, "No Response", ASSESSPERFORMANCE_A17_df$percentage)
        # 
        # ,BOARDMEETINGS         = formatC(BOARDMEETINGS, format="f", big.mark=",", digits = 2)
        # ,MEETINGDURATION        = formatC(MEETINGDURATION, format="f", big.mark=",", digits = 2)
        # ,BOARDCOMMITTEES       = formatC(BOARDCOMMITTEES, format="f", big.mark=",", digits = 2) 
        ,BOARDMEETINGS = BOARDMEETINGS_df$percentage
        ,MEETINGDURATION = MEETINGDURATION_df$percentage
        ,BOARDCOMMITTEES = BOARDCOMMITTEES_df$percentage
        
        ,COMMITTEETYPESC1        = ifelse(length(COMMITTEETYPESC1_df) == 0, "No Response", COMMITTEETYPESC1_df$percentage)
        ,COMMITTEETYPESC2        = ifelse(length(COMMITTEETYPESC2_df) == 0, "No Response", COMMITTEETYPESC2_df$percentage)
        ,COMMITTEETYPESC3        = ifelse(length(COMMITTEETYPESC3_df) == 0, "No Response", COMMITTEETYPESC3_df$percentage)
        ,COMMITTEETYPESC4        = ifelse(length(COMMITTEETYPESC4_df) == 0, "No Response", COMMITTEETYPESC4_df$percentage)
        ,COMMITTEETYPESC5        = ifelse(length(COMMITTEETYPESC5_df) == 0, "No Response", COMMITTEETYPESC5_df$percentage)
        ,COMMITTEETYPESC6        = ifelse(length(COMMITTEETYPESC6_df) == 0, "No Response", COMMITTEETYPESC6_df$percentage)
        ,COMMITTEETYPESC7        = ifelse(length(COMMITTEETYPESC7_df) == 0, "No Response", COMMITTEETYPESC7_df$percentage)
        ,COMMITTEETYPESC8        = ifelse(length(COMMITTEETYPESC8_df) == 0, "No Response", COMMITTEETYPESC8_df$percentage)
        ,COMMITTEETYPESC9        = ifelse(length(COMMITTEETYPESC9_df) == 0, "No Response", COMMITTEETYPESC9_df$percentage)
        ,COMMITTEETYPESC10        = ifelse(length(COMMITTEETYPESC10_df) == 0, "No Response", COMMITTEETYPESC10_df$percentage)
        ,COMMITTEETYPESC11        = ifelse(length(COMMITTEETYPESC11_df) == 0, "No Response", COMMITTEETYPESC11_df$percentage)
        ,COMMITTEETYPESC12        = ifelse(length(COMMITTEETYPESC12_df) == 0, "No Response", COMMITTEETYPESC12_df$percentage)
        
        ,RECRUITMEMBERS_A1        = ifelse(length(RECRUITMEMBERS_A1_df) == 0, "No Response", RECRUITMEMBERS_A1_df$percentage)
        ,RECRUITMEMBERS_A2        = ifelse(length(RECRUITMEMBERS_A2_df) == 0, "No Response", RECRUITMEMBERS_A2_df$percentage)
        ,RECRUITMEMBERS_A3        = ifelse(length(RECRUITMEMBERS_A3_df) == 0, "No Response", RECRUITMEMBERS_A3_df$percentage)
        ,RECRUITMEMBERS_A4        = ifelse(length(RECRUITMEMBERS_A4_df) == 0, "No Response", RECRUITMEMBERS_A4_df$percentage)
        ,RECRUITMEMBERS_A5        = ifelse(length(RECRUITMEMBERS_A5_df) == 0, "No Response", RECRUITMEMBERS_A5_df$percentage)
        ,RECRUITMEMBERS_A6        = ifelse(length(RECRUITMEMBERS_A6_df) == 0, "No Response", RECRUITMEMBERS_A6_df$percentage)
        ,ONBOARDING_A1        = ifelse(length(ONBOARDING_A1_df) == 0, "No Response", ONBOARDING_A1_df$percentage)
        ,ONBOARDING_A2        = ifelse(length(ONBOARDING_A2_df) == 0, "No Response", ONBOARDING_A2_df$percentage)
        ,ONBOARDING_A3        = ifelse(length(ONBOARDING_A3_df) == 0, "No Response", ONBOARDING_A3_df$percentage)
        ,ONBOARDING_A4        = ifelse(length(ONBOARDING_A4_df) == 0, "No Response", ONBOARDING_A4_df$percentage)
        ,ONBOARDING_A5        = ifelse(length(ONBOARDING_A5_df) == 0, "No Response", ONBOARDING_A5_df$percentage)
        ,ONBOARDING_A6        = ifelse(length(ONBOARDING_A6_df) == 0, "No Response", ONBOARDING_A6_df$percentage)
        ,ONBOARDING_A7        = ifelse(length(ONBOARDING_A7_df) == 0, "No Response", ONBOARDING_A7_df$percentage)
        ,ONBOARDING_A8        = ifelse(length(ONBOARDING_A8_df) == 0, "No Response", ONBOARDING_A8_df$percentage)
        ,MEMBERTRAINING_A1        = ifelse(length(MEMBERTRAINING_A1_df) == 0, "No Response", MEMBERTRAINING_A1_df$percentage)
        ,MEMBERTRAINING_A2        = ifelse(length(MEMBERTRAINING_A2_df) == 0, "No Response", MEMBERTRAINING_A2_df$percentage)
        ,MEMBERTRAINING_A3        = ifelse(length(MEMBERTRAINING_A3_df) == 0, "No Response", MEMBERTRAINING_A3_df$percentage)
        ,MEMBERTRAINING_A4        = ifelse(length(MEMBERTRAINING_A4_df) == 0, "No Response", MEMBERTRAINING_A4_df$percentage)
        ,MEMBERTRAINING_A5        = ifelse(length(MEMBERTRAINING_A5_df) == 0, "No Response", MEMBERTRAINING_A5_df$percentage)
        ,MEMBERTRAINING_A6        = ifelse(length(MEMBERTRAINING_A6_df) == 0, "No Response", MEMBERTRAINING_A6_df$percentage)
        ,MEMBERTRAINING_A7        = ifelse(length(MEMBERTRAINING_A7_df) == 0, "No Response", MEMBERTRAINING_A7_df$percentage)
        ,MEMBERTRAINING_A8        = ifelse(length(MEMBERTRAINING_A8_df) == 0, "No Response", MEMBERTRAINING_A8_df$percentage)
        ,MEMBERTRAINING_A9        = ifelse(length(MEMBERTRAINING_A9_df) == 0, "No Response", MEMBERTRAINING_A9_df$percentage)
        ,MEMBERTRAINING_A10        = ifelse(length(MEMBERTRAINING_A10_df) == 0, "No Response", MEMBERTRAINING_A10_df$percentage)
        ,MEMBERTRAINING_A11        = ifelse(length(MEMBERTRAINING_A11_df) == 0, "No Response", MEMBERTRAINING_A11_df$percentage)
        ,INCAMERAPROPORTION        = ifelse(length(INCAMERAPROPORTION_df) == 0, "No Response", INCAMERAPROPORTION_df$percentage)
        ,INCAMERAEXECUTIVES_A1       = ifelse(length(INCAMERAEXECUTIVES_A1_df) == 0, "No Response", INCAMERAEXECUTIVES_A1_df$percentage)
        ,INCAMERAEXECUTIVES_A2       = ifelse(length(INCAMERAEXECUTIVES_A2_df) == 0, "No Response", INCAMERAEXECUTIVES_A2_df$percentage)
        ,INCAMERAEXECUTIVES_A3       = ifelse(length(INCAMERAEXECUTIVES_A3_df) == 0, "No Response", INCAMERAEXECUTIVES_A3_df$percentage)
        ,INCAMERAEXECUTIVES_A4       = ifelse(length(INCAMERAEXECUTIVES_A4_df) == 0, "No Response", INCAMERAEXECUTIVES_A4_df$percentage)
        ,INCAMERAEXECUTIVES_A5       = ifelse(length(INCAMERAEXECUTIVES_A5_df) == 0, "No Response", INCAMERAEXECUTIVES_A5_df$percentage)
        ,AFINCAMERA_A1       = ifelse(length(AFINCAMERA_A1_df) == 0, "No Response", AFINCAMERA_A1_df$percentage)
        ,AFINCAMERA_A2       = ifelse(length(AFINCAMERA_A2_df) == 0, "No Response", AFINCAMERA_A2_df$percentage)
        ,AFINCAMERA_A3       = ifelse(length(AFINCAMERA_A3_df) == 0, "No Response", AFINCAMERA_A3_df$percentage)
        ,AFINCAMERA_A3       = ifelse(length(AFINCAMERA_A3_df) == 0, "No Response", AFINCAMERA_A3_df$percentage)
        ,GOVINCAMERA_A1       = ifelse(length(GOVINCAMERA_A1_df) == 0, "No Response", GOVINCAMERA_A1_df$percentage)
        ,HRINCAMERA_A1       = ifelse(length(HRINCAMERA_A1_df) == 0, "No Response", HRINCAMERA_A1_df$percentage)
        # ,AFMEETINGS         = formatC(AFMEETINGS, format="f", big.mark=",", digits = 2)
        # ,COMPENSATIONMEETING         = formatC(COMPENSATIONMEETING, format="f", big.mark=",", digits = 2)
        ,AFMEETINGS = AFMEETINGS_df$percentage
        ,COMPENSATIONMEETING = COMPENSATIONMEETING_df$percentage
        
      ) %>% mutate_all(~ifelse(is.nan(.), NA, .)) #%>% replace(is.na(.), "NA") 
    
  })
  
  
  peer_bp1_wave2_french<-reactive({
    if (nrow(peer_bp_wave2_french())==0) {
      
      df1<-as_tibble(t(peer_bp_wave2_french()), rownames = "Variable")
      V1<-"text"
      peer_bp1_wave2_french<-cbind(df1,V1)
    }else {
      peer_bp1_wave2_french <-  as_tibble(t(peer_bp_wave2_french()), rownames = "Variable")
    }
  })
  
  # peer_bp1_wave2_french <-  reactive({as_tibble(t(peer_bp_wave2_french()), rownames = "Variable")
  # }) 
  
  
  
  peer_bp2_wave2_french <- reactive({
    peer_bp1_wave2_french() %>% inner_join(bp_list_wave2_french, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })  
  
  
  
  bp_df1_wave2_french <- reactive({
    
    bp_df_wave2_french() %>% 
      left_join(peer_bp2_wave2_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  })  
  
  
  
  # bp_varlist_french <- reactive({
  #   df<-selectors_tbl_french[selected1_french(),1:2 ] # Variable, Category 
  #   bp_varlist_french <-df%>%
  #     filter(str_detect(Category,"Pratiques du conseil"))  
  # })
  
  # own_bp1_wave1_french<-own_bp1(x=data1_french, wave=1,text="Données non disponibles",orglist=bp_list_french)
  # own_bp_df_wave1_french<-own_org_profile2(ownorg=own_bp1_wave1_french(),orglist=bp_list_french, varlist=bp_varlist_french())
  # peer_bp_wave1_french<-peer_bp(x=data1_french,tb=peergroups_selected_Wave1_french(),orglist=bp_list_french)
  # bp_df1_wave1_french<-org_profile_df1(data1=own_bp_df_wave1_french(), data2=peer_bp_wave1_french())
  # 
  # own_bp1_wave2_french<-own_bp1(x=data1_french, wave=2,text="Données non disponibles",orglist=bp_list_french)
  # own_bp_df_wave2_french<-own_org_profile2(ownorg=own_bp1_wave2_french(),orglist=bp_list_french, varlist=bp_varlist_french())
  # peer_bp_wave2_french<-peer_bp(x=data1_french,tb=peergroups_selected_Wave2_french(),orglist=bp_list_french)
  # bp_df1_wave2_french<-org_profile_df1(data1=own_bp_df_wave2_french(), data2=peer_bp_wave2_french())
  # 
  # ~~~Compensation ~~~----  
  
  compen_list_wave1_french <- selectors_tbl_french %>%
    filter(str_detect(Category,"^Rémunération")) %>% 
    arrange(Order) %>%
    filter(is.na(Wave)==TRUE)
  compen_list_wave2_french <- selectors_tbl_french %>%
    filter(str_detect(Category,"^Rémunération")) %>% arrange(Order)
  
  own_compen1_wave1_french <- reactive({
    
    df <- data1_french %>% 
      filter(Wave==1) %>%
      
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(compen_list_wave1_french$Variable)) %>%
      
      mutate(
        MAXCOMPENSATIONFIXED = scales::dollar(MAXCOMPENSATIONFIXED),
        BOARDCOMPENSATION = scales::dollar(BOARDCOMPENSATION),
        
        DIRECTORCOMPENSATION_A1 = scales::dollar(DIRECTORCOMPENSATION_A1),
        DIRECTORCOMPENSATION_A2 = scales::dollar(DIRECTORCOMPENSATION_A2),
        DIRECTORCOMPENSATION_A3 = scales::dollar(DIRECTORCOMPENSATION_A3),
        DIRECTORCOMPENSATION_A4 = scales::dollar(DIRECTORCOMPENSATION_A4),
        
        TOTALCOMPENSATION_A2 = scales::dollar(TOTALCOMPENSATION_A2),
        TOTALCOMPENSATION_A3 = scales::dollar(TOTALCOMPENSATION_A3),
        TOTALCOMPENSATION_A4 = scales::dollar(TOTALCOMPENSATION_A4),
        TOTALCOMPENSATION_A5 = scales::dollar(TOTALCOMPENSATION_A5),
        
        REIMBURSEMENTDIRECTORCOSTS_A1 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A1),
        REIMBURSEMENTDIRECTORCOSTS_A2 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A2),
        REIMBURSEMENTDIRECTORCOSTS_A3 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A3),
        REIMBURSEMENTDIRECTORCOSTS_A4 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A4),
        REIMBURSEMENTDIRECTORCOSTS_A5 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A5),
        REIMBURSEMENTDIRECTORCOSTS_A6 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A6),
        REIMBURSEMENTDIRECTORCOSTS_A7 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A7),
        REIMBURSEMENTDIRECTORCOSTS_A8 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A8),
        REIMBURSEMENTDIRECTORCOSTS_A9 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A9),
        LASTREVIEW         = formatC(LASTREVIEW, format="f", big.mark=",", digits = 2)
        ,AFCHAIRCOMPENSATION = scales::dollar(AFCHAIRCOMPENSATION)
        ,AFCOMPENSATION = scales::dollar(AFCOMPENSATION)
        
      ) 
    
    
    #Transpose df 
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_compen1_wave1_french<-cbind(df1,V1)
    }else {
      own_compen1_wave1_french <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_compen1_wave1_french <-  as_tibble(t(df), rownames = "Variable")
    
  })   
  
  
  
  own_compen2_wave1_french <- reactive({
    own_compen1_wave1_french()%>% inner_join(compen_list_wave1_french, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation") 
    
  })     
  
  
  
  compen_df_wave1_french <- reactive({
    
    variables_selected_french() %>% filter(str_detect(Category,"^Rémunération")) %>%  
      inner_join(own_compen2_wave1_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
    
  }) 
  
  
  
  
  peer_compen_wave1_french <- reactive({
    
    df <-   data1_french %>%
      filter(Organization %in% peergroups_selected_Wave1_french()$Organization) %>%
      filter(Wave==1) %>%
      select(starts_with(compen_list_wave1_french$Variable))  
    MAXCOMPENSATION_df<-summarydata (df,"MAXCOMPENSATION")
    DISCLOSECOMPENSATION_df<-summarydata (df,"DISCLOSECOMPENSATION")
    COMPENSATIONREVIEW_df<-summarydata (df,"COMPENSATIONREVIEW")
    PAYDIRECTORS_df<-summarydata (df,"PAYDIRECTORS")
    BASEDONFCL_df<-summarydata (df,"BASEDONFCL")
    DIRECTORBENEFITS_A1_df<-summarydata (df,"DIRECTORBENEFITS_A1")
    DIRECTORBENEFITS_A2_df<-summarydata (df,"DIRECTORBENEFITS_A2")
    DIRECTORBENEFITS_A3_df<-summarydata (df,"DIRECTORBENEFITS_A3")
    DIRECTORBENEFITS_A4_df<-summarydata (df,"DIRECTORBENEFITS_A4")
    DIRECTORBENEFITS_A5_df<-summarydata (df,"DIRECTORBENEFITS_A5")
    DIRECTORBENEFITS_A6_df<-summarydata (df,"DIRECTORBENEFITS_A6")
    MAXCOMPENSATIONFIXED_df<-meandata (df,MAXCOMPENSATIONFIXED,1)
    MAXCOMPENSATIONPERCENT_df<-meandata (df,MAXCOMPENSATIONPERCENT,0)
    BOARDCOMPENSATION_df<-meandata (df,BOARDCOMPENSATION,1)
    DIRECTORCOMPENSATION_A1_df<-meandata (df,DIRECTORCOMPENSATION_A1,1)
    DIRECTORCOMPENSATION_A2_df<-meandata (df,DIRECTORCOMPENSATION_A2,1)
    DIRECTORCOMPENSATION_A3_df<-meandata (df,DIRECTORCOMPENSATION_A3,1)
    DIRECTORCOMPENSATION_A4_df<-meandata (df,DIRECTORCOMPENSATION_A4,1)
    TOTALCOMPENSATION_A2_df<-meandata (df,TOTALCOMPENSATION_A2,1)
    TOTALCOMPENSATION_A3_df<-meandata (df,TOTALCOMPENSATION_A3,1)
    TOTALCOMPENSATION_A4_df<-meandata (df,TOTALCOMPENSATION_A4,1)
    TOTALCOMPENSATION_A5_df<-meandata (df,TOTALCOMPENSATION_A5,1)
    REIMBURSEMENTDIRECTORCOSTS_A1_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A1,1)
    REIMBURSEMENTDIRECTORCOSTS_A2_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A2,1)
    REIMBURSEMENTDIRECTORCOSTS_A3_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A3,1)
    REIMBURSEMENTDIRECTORCOSTS_A4_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A4,1)
    REIMBURSEMENTDIRECTORCOSTS_A5_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A5,1)
    REIMBURSEMENTDIRECTORCOSTS_A6_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A6,1)
    REIMBURSEMENTDIRECTORCOSTS_A7_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A7,1)
    REIMBURSEMENTDIRECTORCOSTS_A8_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A8,1)
    REIMBURSEMENTDIRECTORCOSTS_A9_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A9,1)
    LASTREVIEW_df<-meandata (df,LASTREVIEW,0)
    AFCHAIRCOMPENSATION_df<-meandata (df,AFCHAIRCOMPENSATION,1)
    AFCOMPENSATION_df<-meandata (df,AFCOMPENSATION,1)
    peer_compen_wave1<-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
                
                
      ) %>%
      
      mutate(
        COMPENSATIONREVIEWO = NA
        
        ,LASTREVIEW         = LASTREVIEW_df$percentage
        ,MAXCOMPENSATION        = ifelse(length(MAXCOMPENSATION_df) == 0, "No Response", MAXCOMPENSATION_df$percentage)
        ,MAXCOMPENSATIONFIXED = MAXCOMPENSATIONFIXED_df$percentage
        ,BOARDCOMPENSATION = BOARDCOMPENSATION_df$percentage
        ,DISCLOSECOMPENSATION   = ifelse(length(DISCLOSECOMPENSATION_df) == 0, "No Response", DISCLOSECOMPENSATION_df$percentage)
        ,COMPENSATIONREVIEW   = ifelse(length(COMPENSATIONREVIEW_df) == 0, "No Response", COMPENSATIONREVIEW_df$percentage)
        ,PAYDIRECTORS        = ifelse(length(PAYDIRECTORS_df) == 0, "No Response", PAYDIRECTORS_df$percentage)
        ,BASEDONFCL   = ifelse(length(BASEDONFCL_df) == 0, "No Response", BASEDONFCL_df$percentage)
        
        ,DIRECTORCOMPENSATION_A1 = DIRECTORCOMPENSATION_A1_df$percentage
        ,DIRECTORCOMPENSATION_A2 = DIRECTORCOMPENSATION_A2_df$percentage
        ,DIRECTORCOMPENSATION_A3 = DIRECTORCOMPENSATION_A3_df$percentage
        ,DIRECTORCOMPENSATION_A4 = DIRECTORCOMPENSATION_A4_df$percentage
        
        ,TOTALCOMPENSATION_A2 = TOTALCOMPENSATION_A2_df$percentage
        ,TOTALCOMPENSATION_A3 = TOTALCOMPENSATION_A3_df$percentage
        ,TOTALCOMPENSATION_A4 = TOTALCOMPENSATION_A4_df$percentage
        ,TOTALCOMPENSATION_A5 = TOTALCOMPENSATION_A5_df$percentage
        
        ,REIMBURSEMENTDIRECTORCOSTS_A1 = REIMBURSEMENTDIRECTORCOSTS_A1_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A2 = REIMBURSEMENTDIRECTORCOSTS_A2_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A3 = REIMBURSEMENTDIRECTORCOSTS_A3_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A4 = REIMBURSEMENTDIRECTORCOSTS_A4_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A5 = REIMBURSEMENTDIRECTORCOSTS_A5_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A6 = REIMBURSEMENTDIRECTORCOSTS_A6_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A7 = REIMBURSEMENTDIRECTORCOSTS_A7_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A8 = REIMBURSEMENTDIRECTORCOSTS_A8_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A9 = REIMBURSEMENTDIRECTORCOSTS_A9_df$percentage
        
        ,DIRECTORBENEFITS_A1        = ifelse(length(DIRECTORBENEFITS_A1_df) == 0, "No Response", DIRECTORBENEFITS_A1_df$percentage)
        ,DIRECTORBENEFITS_A2   = ifelse(length(DIRECTORBENEFITS_A2_df) == 0, "No Response", DIRECTORBENEFITS_A2_df$percentage)
        ,DIRECTORBENEFITS_A3   = ifelse(length(DIRECTORBENEFITS_A3_df) == 0, "No Response", DIRECTORBENEFITS_A3_df$percentage)
        ,DIRECTORBENEFITS_A4   = ifelse(length(DIRECTORBENEFITS_A4_df) == 0, "No Response", DIRECTORBENEFITS_A4_df$percentage)
        ,DIRECTORBENEFITS_A5   = ifelse(length(DIRECTORBENEFITS_A5_df) == 0, "No Response", DIRECTORBENEFITS_A5_df$percentage)
        ,DIRECTORBENEFITS_A6   = ifelse(length(DIRECTORBENEFITS_A6_df) == 0, "No Response", DIRECTORBENEFITS_A6_df$percentage)
        ,AFCHAIRCOMPENSATION = AFCHAIRCOMPENSATION_df$percentage
        ,AFCOMPENSATION = AFCOMPENSATION_df$percentage
      )  %>% mutate_all(~ifelse(is.nan(.), NA, .))# %>% replace(is.na(.), "NA")    
    
    
    
  })
  
  
  
  peer_compen1_wave1_french<-reactive({
    if (nrow(peer_compen_wave1_french())==0) {
      
      df1<-as_tibble(t(peer_compen_wave1_french()), rownames = "Variable")
      V1<-"text"
      peer_compen1_wave1_french<-cbind(df1,V1)
    }else {
      peer_compen1_wave1_french <-  as_tibble(t(peer_compen_wave1_french()), rownames = "Variable")
    }
  })
  # peer_compen1_wave1_french <-  reactive({as_tibble(t(peer_compen_wave1_french()), rownames = "Variable")
  # }) 
  
  
  
  
  peer_compen2_wave1_french <- reactive({
    peer_compen1_wave1_french() %>% inner_join(compen_list_wave1_french, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })   
  
  
  
  
  compen_df1_wave1_french <- reactive({
    
    compen_df_wave1_french() %>% 
      left_join(peer_compen2_wave1_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  })      
  
  own_compen1_wave2_french <- reactive({
    
    df <- data1_french %>% 
      filter(Wave==2) %>%
      
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(compen_list_wave2_french$Variable)) %>%
      
      mutate(
        MAXCOMPENSATIONFIXED = scales::dollar(MAXCOMPENSATIONFIXED),
        BOARDCOMPENSATION = scales::dollar(BOARDCOMPENSATION),
        
        DIRECTORCOMPENSATION_A1 = scales::dollar(DIRECTORCOMPENSATION_A1),
        DIRECTORCOMPENSATION_A2 = scales::dollar(DIRECTORCOMPENSATION_A2),
        DIRECTORCOMPENSATION_A3 = scales::dollar(DIRECTORCOMPENSATION_A3),
        DIRECTORCOMPENSATION_A4 = scales::dollar(DIRECTORCOMPENSATION_A4),
        
        TOTALCOMPENSATION_A2 = scales::dollar(TOTALCOMPENSATION_A2),
        TOTALCOMPENSATION_A3 = scales::dollar(TOTALCOMPENSATION_A3),
        TOTALCOMPENSATION_A4 = scales::dollar(TOTALCOMPENSATION_A4),
        TOTALCOMPENSATION_A5 = scales::dollar(TOTALCOMPENSATION_A5),
        
        REIMBURSEMENTDIRECTORCOSTS_A1 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A1),
        REIMBURSEMENTDIRECTORCOSTS_A2 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A2),
        REIMBURSEMENTDIRECTORCOSTS_A3 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A3),
        REIMBURSEMENTDIRECTORCOSTS_A4 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A4),
        REIMBURSEMENTDIRECTORCOSTS_A5 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A5),
        REIMBURSEMENTDIRECTORCOSTS_A6 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A6),
        REIMBURSEMENTDIRECTORCOSTS_A7 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A7),
        REIMBURSEMENTDIRECTORCOSTS_A8 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A8),
        REIMBURSEMENTDIRECTORCOSTS_A9 = scales::dollar(REIMBURSEMENTDIRECTORCOSTS_A9),
        LASTREVIEW         = formatC(LASTREVIEW, format="f", big.mark=",", digits = 2)
        ,AFCHAIRCOMPENSATION = scales::dollar(AFCHAIRCOMPENSATION)
        ,AFCOMPENSATION = scales::dollar(AFCOMPENSATION)
        ,COMPENSATION_B1 = scales::dollar(COMPENSATION_B1)
        ,COMPENSATION_B2 = scales::dollar(COMPENSATION_B2)
        ,COMPENSATION_B3 = scales::dollar(COMPENSATION_B3)
        ,COMPENSATION_B4 = scales::dollar(COMPENSATION_B4)
        ,COMPENSATION_C1 = scales::dollar(COMPENSATION_C1)
        ,COMPENSATION_C2 = scales::dollar(COMPENSATION_C2)
        ,COMPENSATION_C3 = scales::dollar(COMPENSATION_C3)
        ,COMPENSATION_C4 = scales::dollar(COMPENSATION_C4)
        ,COMPENSATION_D1 = scales::dollar(COMPENSATION_D1)
        ,COMPENSATION_D2 = scales::dollar(COMPENSATION_D2)
        ,COMPENSATION_D3 = scales::dollar(COMPENSATION_D3)
        ,COMPENSATION_D4 = scales::dollar(COMPENSATION_D4)
        
      ) 
    
    
    #Transpose df 
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_compen1_wave2_french<-cbind(df1,V1)
    }else {
      own_compen1_wave2_french <-  as_tibble(t(df), rownames = "Variable")
    }
    
    # own_compen1_wave2_french <-  as_tibble(t(df), rownames = "Variable")
    
  })   
  
  
  
  own_compen2_wave2_french <- reactive({
    own_compen1_wave2_french()%>% inner_join(compen_list_wave2_french, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation") 
    
  })     
  
  
  
  compen_df_wave2_french <- reactive({
    
    variables_selected_french() %>% filter(str_detect(Category,"^Rémunération")) %>%  
      inner_join(own_compen2_wave2_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
    
  }) 
  
  
  
  
  peer_compen_wave2_french <- reactive({
    
    df <-   data1_french%>%
      filter(Organization %in% peergroups_selected_Wave2_french()$Organization) %>%
      filter(Wave==2) %>%
      select(starts_with(compen_list_wave2_french$Variable))  
    MAXCOMPENSATION_df<-summarydata (df,"MAXCOMPENSATION")
    DISCLOSECOMPENSATION_df<-summarydata (df,"DISCLOSECOMPENSATION")
    COMPENSATIONREVIEW_df<-summarydata (df,"COMPENSATIONREVIEW")
    PAYDIRECTORS_df<-summarydata (df,"PAYDIRECTORS")
    BASEDONFCL_df<-summarydata (df,"BASEDONFCL")
    DIRECTORBENEFITS_A1_df<-summarydata (df,"DIRECTORBENEFITS_A1")
    DIRECTORBENEFITS_A2_df<-summarydata (df,"DIRECTORBENEFITS_A2")
    DIRECTORBENEFITS_A3_df<-summarydata (df,"DIRECTORBENEFITS_A3")
    DIRECTORBENEFITS_A4_df<-summarydata (df,"DIRECTORBENEFITS_A4")
    DIRECTORBENEFITS_A5_df<-summarydata (df,"DIRECTORBENEFITS_A5")
    DIRECTORBENEFITS_A6_df<-summarydata (df,"DIRECTORBENEFITS_A6")
    MAXCOMPENSATIONFIXED_df<-meandata (df,MAXCOMPENSATIONFIXED,1)
    MAXCOMPENSATIONPERCENT_df<-meandata (df,MAXCOMPENSATIONPERCENT,0)
    BOARDCOMPENSATION_df<-meandata (df,BOARDCOMPENSATION,1)
    DIRECTORCOMPENSATION_A1_df<-meandata (df,DIRECTORCOMPENSATION_A1,1)
    DIRECTORCOMPENSATION_A2_df<-meandata (df,DIRECTORCOMPENSATION_A2,1)
    DIRECTORCOMPENSATION_A3_df<-meandata (df,DIRECTORCOMPENSATION_A3,1)
    DIRECTORCOMPENSATION_A4_df<-meandata (df,DIRECTORCOMPENSATION_A4,1)
    TOTALCOMPENSATION_A2_df<-meandata (df,TOTALCOMPENSATION_A2,1)
    TOTALCOMPENSATION_A3_df<-meandata (df,TOTALCOMPENSATION_A3,1)
    TOTALCOMPENSATION_A4_df<-meandata (df,TOTALCOMPENSATION_A4,1)
    TOTALCOMPENSATION_A5_df<-meandata (df,TOTALCOMPENSATION_A5,1)
    REIMBURSEMENTDIRECTORCOSTS_A1_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A1,1)
    REIMBURSEMENTDIRECTORCOSTS_A2_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A2,1)
    REIMBURSEMENTDIRECTORCOSTS_A3_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A3,1)
    REIMBURSEMENTDIRECTORCOSTS_A4_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A4,1)
    REIMBURSEMENTDIRECTORCOSTS_A5_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A5,1)
    REIMBURSEMENTDIRECTORCOSTS_A6_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A6,1)
    REIMBURSEMENTDIRECTORCOSTS_A7_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A7,1)
    REIMBURSEMENTDIRECTORCOSTS_A8_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A8,1)
    REIMBURSEMENTDIRECTORCOSTS_A9_df<-meandata (df,REIMBURSEMENTDIRECTORCOSTS_A9,1)
    COMPENSATION_B1_df<-meandata(df,COMPENSATION_B1,1)
    COMPENSATION_B2_df<-meandata(df,COMPENSATION_B2,1)
    COMPENSATION_B3_df<-meandata(df,COMPENSATION_B3,1)
    COMPENSATION_B4_df<-meandata(df,COMPENSATION_B4,1)
    COMPENSATION_C1_df<-meandata(df,COMPENSATION_C1,1)
    COMPENSATION_C2_df<-meandata(df,COMPENSATION_C2,1)
    COMPENSATION_C3_df<-meandata(df,COMPENSATION_C3,1)
    COMPENSATION_C4_df<-meandata(df,COMPENSATION_C4,1)
    COMPENSATION_D1_df<-meandata(df,COMPENSATION_D1,1)
    COMPENSATION_D2_df<-meandata(df,COMPENSATION_D2,1)
    COMPENSATION_D3_df<-meandata(df,COMPENSATION_D3,1)
    COMPENSATION_D4_df<-meandata(df,COMPENSATION_D4,1)
    LASTREVIEW_df<-meandata (df,LASTREVIEW,0)
    AFCHAIRCOMPENSATION_df<-meandata (df,AFCHAIRCOMPENSATION,1)
    AFCOMPENSATION_df<-meandata (df,AFCOMPENSATION,1)
    peer_compen_wave1<-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
                
                
      ) %>%
      
      mutate(
        COMPENSATIONREVIEWO = NA
        
        ,LASTREVIEW         = LASTREVIEW_df$percentage
        ,MAXCOMPENSATION        = ifelse(length(MAXCOMPENSATION_df) == 0, "No Response", MAXCOMPENSATION_df$percentage)
        ,MAXCOMPENSATIONFIXED = MAXCOMPENSATIONFIXED_df$percentage
        ,BOARDCOMPENSATION = BOARDCOMPENSATION_df$percentage
        ,DISCLOSECOMPENSATION   = ifelse(length(DISCLOSECOMPENSATION_df) == 0, "No Response", DISCLOSECOMPENSATION_df$percentage)
        ,COMPENSATIONREVIEW   = ifelse(length(COMPENSATIONREVIEW_df) == 0, "No Response", COMPENSATIONREVIEW_df$percentage)
        ,PAYDIRECTORS        = ifelse(length(PAYDIRECTORS_df) == 0, "No Response", PAYDIRECTORS_df$percentage)
        ,BASEDONFCL   = ifelse(length(BASEDONFCL_df) == 0, "No Response", BASEDONFCL_df$percentage)
        
        ,DIRECTORCOMPENSATION_A1 = DIRECTORCOMPENSATION_A1_df$percentage
        ,DIRECTORCOMPENSATION_A2 = DIRECTORCOMPENSATION_A2_df$percentage
        ,DIRECTORCOMPENSATION_A3 = DIRECTORCOMPENSATION_A3_df$percentage
        ,DIRECTORCOMPENSATION_A4 = DIRECTORCOMPENSATION_A4_df$percentage
        
        ,TOTALCOMPENSATION_A2 = TOTALCOMPENSATION_A2_df$percentage
        ,TOTALCOMPENSATION_A3 = TOTALCOMPENSATION_A3_df$percentage
        ,TOTALCOMPENSATION_A4 = TOTALCOMPENSATION_A4_df$percentage
        ,TOTALCOMPENSATION_A5 = TOTALCOMPENSATION_A5_df$percentage
        
        ,REIMBURSEMENTDIRECTORCOSTS_A1 = REIMBURSEMENTDIRECTORCOSTS_A1_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A2 = REIMBURSEMENTDIRECTORCOSTS_A2_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A3 = REIMBURSEMENTDIRECTORCOSTS_A3_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A4 = REIMBURSEMENTDIRECTORCOSTS_A4_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A5 = REIMBURSEMENTDIRECTORCOSTS_A5_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A6 = REIMBURSEMENTDIRECTORCOSTS_A6_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A7 = REIMBURSEMENTDIRECTORCOSTS_A7_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A8 = REIMBURSEMENTDIRECTORCOSTS_A8_df$percentage
        ,REIMBURSEMENTDIRECTORCOSTS_A9 = REIMBURSEMENTDIRECTORCOSTS_A9_df$percentage
        
        ,DIRECTORBENEFITS_A1        = ifelse(length(DIRECTORBENEFITS_A1_df) == 0, "No Response", DIRECTORBENEFITS_A1_df$percentage)
        ,DIRECTORBENEFITS_A2   = ifelse(length(DIRECTORBENEFITS_A2_df) == 0, "No Response", DIRECTORBENEFITS_A2_df$percentage)
        ,DIRECTORBENEFITS_A3   = ifelse(length(DIRECTORBENEFITS_A3_df) == 0, "No Response", DIRECTORBENEFITS_A3_df$percentage)
        ,DIRECTORBENEFITS_A4   = ifelse(length(DIRECTORBENEFITS_A4_df) == 0, "No Response", DIRECTORBENEFITS_A4_df$percentage)
        ,DIRECTORBENEFITS_A5   = ifelse(length(DIRECTORBENEFITS_A5_df) == 0, "No Response", DIRECTORBENEFITS_A5_df$percentage)
        ,DIRECTORBENEFITS_A6   = ifelse(length(DIRECTORBENEFITS_A6_df) == 0, "No Response", DIRECTORBENEFITS_A6_df$percentage)
        ,COMPENSATION_B1 = COMPENSATION_B1_df$percentage
        ,COMPENSATION_B2 = COMPENSATION_B2_df$percentage
        ,COMPENSATION_B3 = COMPENSATION_B3_df$percentage
        ,COMPENSATION_B4 = COMPENSATION_B4_df$percentage
        ,COMPENSATION_C1 = COMPENSATION_C1_df$percentage
        ,COMPENSATION_C2 = COMPENSATION_C2_df$percentage
        ,COMPENSATION_C3 = COMPENSATION_C3_df$percentage
        ,COMPENSATION_C4 = COMPENSATION_C4_df$percentage
        ,COMPENSATION_D1 = COMPENSATION_D1_df$percentage
        ,COMPENSATION_D2 = COMPENSATION_D2_df$percentage
        ,COMPENSATION_D3 = COMPENSATION_D3_df$percentage
        ,COMPENSATION_D4 = COMPENSATION_D4_df$percentage
        ,AFCHAIRCOMPENSATION = AFCHAIRCOMPENSATION_df$percentage
        ,AFCOMPENSATION = AFCOMPENSATION_df$percentage
      )  %>% mutate_all(~ifelse(is.nan(.), NA, .))# %>% replace(is.na(.), "NA")    
    
    
    
  })
  
  
  
  peer_compen1_wave2_french<-reactive({
    if (nrow(peer_compen_wave2_french())==0) {
      
      df1<-as_tibble(t(peer_compen_wave2_french()), rownames = "Variable")
      V1<-"text"
      peer_compen1_wave2_french<-cbind(df1,V1)
    }else {
      peer_compen1_wave2_french <-  as_tibble(t(peer_compen_wave2_french()), rownames = "Variable")
    }
  })
  # peer_compen1_wave2_french <-  reactive({as_tibble(t(peer_compen_wave2_french()), rownames = "Variable")
  # }) 
  
  
  
  
  peer_compen2_wave2_french <- reactive({
    peer_compen1_wave2_french() %>% inner_join(compen_list_wave2_french, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })   
  
  
  
  
  compen_df1_wave2_french <- reactive({
    
    compen_df_wave2_french() %>% 
      left_join(peer_compen2_wave2_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  })      
  
  
  
  # compen_varlist_french <- reactive({
  #   df<-selectors_tbl_french[selected1_french(),1:2 ] # Variable, Category 
  #   compen_varlist_french <-df%>%
  #     filter(str_detect(Category,"^Rémunération"))  
  # })
  # own_compen1_wave1_french<-own_compen1(x=data1_french, wave=1,text="Données non disponibles",orglist=compen_list_french)
  # own_compen_df_wave1_french<-own_org_profile2(ownorg=own_compen1_wave1_french(),orglist=compen_list_french, varlist=compen_varlist_french())
  # peer_compen_wave1_french<-peer_compen(x=data1,tb=peergroups_selected_Wave1_french(),orglist=compen_list_french)
  # compen_df1_wave1_french<-org_profile_df1(data1=own_compen_df_wave1_french(), data2=peer_compen_wave1_french())
  # 
  # own_compen1_wave2_french<-own_compen1(x=data1_french, wave=2,text="Données non disponibles",orglist=compen_list_french)
  # own_compen_df_wave2_french<-own_org_profile2(ownorg=own_compen1_wave2_french(),orglist=compen_list_french, varlist=compen_varlist_french())
  # peer_compen_wave2_french<-peer_compen(x=data1_french,tb=peergroups_selected_Wave2_french(),orglist=compen_list_french)
  # compen_df1_wave2_french<-org_profile_df1(data1=own_compen_df_wave2_french(), data2=peer_compen_wave2_french())
  # 
  # ~~~Democratic Process~~~----  
  tldel_list_wave1_french <- selectors_tbl_french %>%
    filter(str_detect(Category,"Processus démocratique")) %>% 
    arrange(Order) %>%
    filter(is.na(Wave)==TRUE)
  tldel_list_wave2_french <- selectors_tbl_french %>%
    filter(str_detect(Category,"Processus démocratique")) %>% arrange(Order)
  
  
  own_tldel1_wave1_french <- reactive({
    
    df <- data1_french %>% 
      filter(Wave==1) %>%
      
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(tldel_list_wave1_french$Variable)
      ) %>% mutate(
        
        MEMBERAGMATTENDANCE         = formatC(MEMBERAGMATTENDANCE, format="f", big.mark=",", digits = 2),
        DELEGAGMATTENDANCE         = formatC(DELEGAGMATTENDANCE, format="f", big.mark=",", digits = 2),
        ONLINEATTENDANCE         = formatC(ONLINEATTENDANCE, format="f", big.mark=",", digits = 2),
        ONLINEATTENDANCE_POSTC19         = formatC(ONLINEATTENDANCE_POSTC19, format="f", big.mark=",", digits = 2),
        LASTAGMDURATION         = formatC(LASTAGMDURATION, format="f", big.mark=",", digits = 2),
        ELECTIONSPAST10         = formatC(ELECTIONSPAST10, format="f", big.mark=",", digits = 2),
        CONTESTEDSEATS         = formatC(CONTESTEDSEATS, format="f", big.mark=",", digits = 2)
      ) 
    
    
    #Transpose df 
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_tldel1_wave1_french<-cbind(df1,V1)
    }else {
      own_tldel1_wave1_french <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_tldel1_wave1_french <-  as_tibble(t(df), rownames = "Variable")
    
  })   
  
  
  
  own_tldel2_wave1_french <- reactive({
    own_tldel1_wave1_french()%>% inner_join(tldel_list_wave1_french, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation")
    
  }) 
  
  
  
  
  tldel_df_wave1_french <- reactive({
    
    variables_selected_french() %>% filter(str_detect(Category,"Processus démocratique")) %>%  
      inner_join(own_tldel2_wave1_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
    
  })   
  
  
  
  peer_tldel_wave1_french <- reactive({
    
    df <-   data1_french %>%
      filter(Organization %in% peergroups_selected_Wave1_french()$Organization) %>%
      filter(Wave==1) %>%
      select(starts_with(tldel_list_wave1_french$Variable))  
    WHOATTENDAGM_df<-summarydatasort (df,"WHOATTENDAGM")
    HOWVOTEC1_df<-summarydata (df,"HOWVOTEC1")
    HOWVOTEC2_df<-summarydata (df,"HOWVOTEC2")
    HOWVOTEC3_df<-summarydata (df,"HOWVOTEC3")
    HOWVOTEC4_df<-summarydata (df,"HOWVOTEC4")
    HOWVOTEC5_df<-summarydata (df,"HOWVOTEC5")
    BOARDELECTEDMEMBERS_df<-summarydatasort (df,"BOARDELECTEDMEMBERS")
    STAGGEREDELECTIONS_df<-summarydata (df,"STAGGEREDELECTIONS")
    DEMOCRATICPRINCIPLE_df<-summarydata (df,"DEMOCRATICPRINCIPLE")
    OTHERDEMOCRATICPRACTICE_df<-summarydata (df,"OTHERDEMOCRATICPRACTICE")
    DIRECTORSELECTIONM1_df<-summarydatasort (df,"DIRECTORSELECTIONM1")
    DIRECTORSELECTIONM2_df<-summarydatasort (df,"DIRECTORSELECTIONM2")
    DIRECTORSELECTIONM3_df<-summarydatasort (df,"DIRECTORSELECTIONM3")
    DIRECTORSELECTIONM4_df<-summarydatasort (df,"DIRECTORSELECTIONM4")
    DIRECTORSELECTIONM5_df<-summarydatasort (df,"DIRECTORSELECTIONM5")
    MEMBERAGMATTENDANCE_df<-meandata (df,MEMBERAGMATTENDANCE,0)
    DELEGAGMATTENDANCE_df<-meandata (df,DELEGAGMATTENDANCE,0)
    ONLINEATTENDANCE_df<-meandata (df,ONLINEATTENDANCE,0)
    ONLINEATTENDANCE_POSTC19_df<-meandata (df,ONLINEATTENDANCE_POSTC19,0)
    LASTAGMDURATION_df<-meandata (df,LASTAGMDURATION,0)
    ELECTIONSPAST10_df<-meandata (df,ELECTIONSPAST10,0)
    CONTESTEDSEATS_df<-meandata (df,CONTESTEDSEATS,0)
    
    peer_tldel_wave1_french<-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
                
                
      ) %>%
      
      mutate(
        WHOATTENDAGM        = ifelse(length(WHOATTENDAGM_df) == 0, "No Response", WHOATTENDAGM_df$percentage)
        ,MEMBERAGMATTENDANCE         = MEMBERAGMATTENDANCE_df$percentage
        ,DELEGAGMATTENDANCE         = DELEGAGMATTENDANCE_df$percentage
        ,ONLINEATTENDANCE         = ONLINEATTENDANCE_df$percentage
        , ONLINEATTENDANCE_POSTC19         = ONLINEATTENDANCE_POSTC19_df$percentage
        #,HOWVOTEO = NA
        ,HOWVOTEC1        = ifelse(length(HOWVOTEC1_df) == 0, "No Response", HOWVOTEC1_df$percentage)
        ,HOWVOTEC2        = ifelse(length(HOWVOTEC2_df) == 0, "No Response", HOWVOTEC2_df$percentage)
        ,HOWVOTEC3        = ifelse(length(HOWVOTEC3_df) == 0, "No Response", HOWVOTEC3_df$percentage)
        ,HOWVOTEC4        = ifelse(length(HOWVOTEC4_df) == 0, "No Response", HOWVOTEC4_df$percentage)
        ,HOWVOTEC5        = ifelse(length(HOWVOTEC5_df) == 0, "No Response", HOWVOTEC5_df$percentage)
        ,LASTAGMDURATION         = LASTAGMDURATION_df$percentage
        ,BOARDELECTEDMEMBERS   = ifelse(length(BOARDELECTEDMEMBERS_df) == 0, "No Response", BOARDELECTEDMEMBERS_df$percentage)
        ,STAGGEREDELECTIONS   = ifelse(length(STAGGEREDELECTIONS_df) == 0, "No Response", STAGGEREDELECTIONS_df$percentage)
        ,ELECTIONSPAST10         = ELECTIONSPAST10_df$percentage
        ,CONTESTEDSEATS         = CONTESTEDSEATS_df$percentage
        ,DEMOCRATICPRINCIPLE        = ifelse(length(DEMOCRATICPRINCIPLE_df) == 0, "No Response", DEMOCRATICPRINCIPLE_df$percentage)
        ,OTHERDEMOCRATICPRACTICE   = ifelse(length(OTHERDEMOCRATICPRACTICE_df) == 0, "No Response", OTHERDEMOCRATICPRACTICE_df$percentage)
        ,DIRECTORSELECTIONM1   = ifelse(length(DIRECTORSELECTIONM1_df) == 0, "No Response", DIRECTORSELECTIONM1_df$percentage)
        ,DIRECTORSELECTIONM2   = ifelse(length(DIRECTORSELECTIONM2_df) == 0, "No Response", DIRECTORSELECTIONM2_df$percentage)
        ,DIRECTORSELECTIONM3   = ifelse(length(DIRECTORSELECTIONM3_df) == 0, "No Response", DIRECTORSELECTIONM3_df$percentage)
        ,DIRECTORSELECTIONM4   = ifelse(length(DIRECTORSELECTIONM4_df) == 0, "No Response", DIRECTORSELECTIONM4_df$percentage)
        ,DIRECTORSELECTIONM5   = ifelse(length(DIRECTORSELECTIONM5_df) == 0, "No Response", DIRECTORSELECTIONM5_df$percentage)
        
        
      ) %>% mutate_all(~ifelse(is.nan(.), NA, .)) #%>% replace(is.na(.), "NA") 
    
  })    
  
  peer_tldel1_wave1_french<-reactive({
    if (nrow(peer_tldel_wave1_french())==0) {
      
      df1<-as_tibble(t(peer_tldel_wave1_french()), rownames = "Variable")
      V1<-"text"
      peer_tldel1_wave1_french<-cbind(df1,V1)
    }else {
      peer_tldel1_wave1_french <-  as_tibble(t(peer_tldel_wave1_french()), rownames = "Variable")
    }
  })
  # peer_tldel1_wave1_french <-  reactive({as_tibble(t(peer_tldel_wave1_french()), rownames = "Variable")
  # }) 
  
  
  
  peer_tldel2_wave1_french <- reactive({
    peer_tldel1_wave1_french() %>% inner_join(tldel_list_wave1_french, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })  
  
  
  
  tldel_df1_wave1_french <- reactive({
    
    tldel_df_wave1_french() %>% 
      left_join(peer_tldel2_wave1_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  })      
  
  own_tldel1_wave2_french <- reactive({
    
    df <- data1_french %>% 
      filter(Wave==2) %>%
      
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(tldel_list_wave2_french$Variable)
      ) %>% mutate(
        
        MEMBERAGMATTENDANCE         = formatC(MEMBERAGMATTENDANCE, format="f", big.mark=",", digits = 2),
        DELEGAGMATTENDANCE         = formatC(DELEGAGMATTENDANCE, format="f", big.mark=",", digits = 2),
        ONLINEATTENDANCE         = formatC(ONLINEATTENDANCE, format="f", big.mark=",", digits = 2),
        ONLINEATTENDANCE_POSTC19         = formatC(ONLINEATTENDANCE_POSTC19, format="f", big.mark=",", digits = 2),
        LASTAGMDURATION         = formatC(LASTAGMDURATION, format="f", big.mark=",", digits = 2),
        ELECTIONSPAST10         = formatC(ELECTIONSPAST10, format="f", big.mark=",", digits = 2),
        CONTESTEDSEATS         = formatC(CONTESTEDSEATS, format="f", big.mark=",", digits = 2)
        
      ) 
    
    
    #Transpose df 
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_tldel1_wave2_french<-cbind(df1,V1)
    }else {
      own_tldel1_wave2_french <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_tldel1_wave2_french <-  as_tibble(t(df), rownames = "Variable")
    
  })   
  
  
  
  own_tldel2_wave2_french <- reactive({
    own_tldel1_wave2_french()%>% inner_join(tldel_list_wave2_french, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation")
    
  }) 
  
  
  
  
  tldel_df_wave2_french <- reactive({
    
    variables_selected_french() %>% filter(str_detect(Category,"Processus démocratique")) %>%  
      inner_join(own_tldel2_wave2_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
    
  })   
  
  
  
  peer_tldel_wave2_french <- reactive({
    
    df <-   data1_french %>%
      filter(Organization %in% peergroups_selected_Wave2_french()$Organization) %>%
      filter(Wave==2) %>%
      select(starts_with(tldel_list_wave2_french$Variable))  
    WHOATTENDAGM_df<-summarydatasort (df,"WHOATTENDAGM")
    HOWVOTEC1_df<-summarydata (df,"HOWVOTEC1")
    HOWVOTEC2_df<-summarydata (df,"HOWVOTEC2")
    HOWVOTEC3_df<-summarydata (df,"HOWVOTEC3")
    HOWVOTEC4_df<-summarydata (df,"HOWVOTEC4")
    HOWVOTEC5_df<-summarydata (df,"HOWVOTEC5")
    BOARDELECTEDMEMBERS_df<-summarydatasort (df,"BOARDELECTEDMEMBERS")
    STAGGEREDELECTIONS_df<-summarydata (df,"STAGGEREDELECTIONS")
    DEMOCRATICPRINCIPLE_df<-summarydata (df,"DEMOCRATICPRINCIPLE")
    OTHERDEMOCRATICPRACTICE_df<-summarydata (df,"OTHERDEMOCRATICPRACTICE")
    AGMPARTICIPATION_A1_df<-summarydata (df,"AGMPARTICIPATION_A1")
    AGMPARTICIPATION_A2_df<-summarydata (df,"AGMPARTICIPATION_A2")
    AGMPARTICIPATION_A3_df<-summarydata (df,"AGMPARTICIPATION_A3")
    AGMPARTICIPATION_A4_df<-summarydata (df,"AGMPARTICIPATION_A4")
    AGMPARTICIPATION_A5_df<-summarydata (df,"AGMPARTICIPATION_A5")
    AGMPARTICIPATION_A6_df<-summarydata (df,"AGMPARTICIPATION_A6")
    AGMPARTICIPATION_A7_df<-summarydata (df,"AGMPARTICIPATION_A7")
    AGMPARTICIPATION_A8_df<-summarydata (df,"AGMPARTICIPATION_A8")
    DIRECTORSELECTIONM1_df<-summarydatasort (df,"DIRECTORSELECTIONM1")
    DIRECTORSELECTIONM2_df<-summarydatasort (df,"DIRECTORSELECTIONM2")
    DIRECTORSELECTIONM3_df<-summarydatasort (df,"DIRECTORSELECTIONM3")
    DIRECTORSELECTIONM4_df<-summarydatasort (df,"DIRECTORSELECTIONM4")
    DIRECTORSELECTIONM5_df<-summarydatasort (df,"DIRECTORSELECTIONM5")
    MEMBERAGMATTENDANCE_df<-meandata (df,MEMBERAGMATTENDANCE,0)
    DELEGAGMATTENDANCE_df<-meandata (df,DELEGAGMATTENDANCE,0)
    ONLINEATTENDANCE_df<-meandata (df,ONLINEATTENDANCE,0)
    ONLINEATTENDANCE_POSTC19_df<-meandata (df,ONLINEATTENDANCE_POSTC19,0)
    LASTAGMDURATION_df<-meandata (df,LASTAGMDURATION,0)
    ELECTIONSPAST10_df<-meandata (df,ELECTIONSPAST10,0)
    CONTESTEDSEATS_df<-meandata (df,CONTESTEDSEATS,0)
    peer_tldel_wave2_french<-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
                
                
      ) %>%
      
      mutate(
        WHOATTENDAGM        = ifelse(length(WHOATTENDAGM_df) == 0, "No Response", WHOATTENDAGM_df$percentage)
        ,MEMBERAGMATTENDANCE         = MEMBERAGMATTENDANCE_df$percentage
        ,DELEGAGMATTENDANCE         = DELEGAGMATTENDANCE_df$percentage
        ,ONLINEATTENDANCE         = ONLINEATTENDANCE_df$percentage
        , ONLINEATTENDANCE_POSTC19         = ONLINEATTENDANCE_POSTC19_df$percentage
        #,HOWVOTEO = NA
        ,HOWVOTEC1        = ifelse(length(HOWVOTEC1_df) == 0, "No Response", HOWVOTEC1_df$percentage)
        ,HOWVOTEC2        = ifelse(length(HOWVOTEC2_df) == 0, "No Response", HOWVOTEC2_df$percentage)
        ,HOWVOTEC3        = ifelse(length(HOWVOTEC3_df) == 0, "No Response", HOWVOTEC3_df$percentage)
        ,HOWVOTEC4        = ifelse(length(HOWVOTEC4_df) == 0, "No Response", HOWVOTEC4_df$percentage)
        ,HOWVOTEC5        = ifelse(length(HOWVOTEC5_df) == 0, "No Response", HOWVOTEC5_df$percentage)
        ,LASTAGMDURATION         = LASTAGMDURATION_df$percentage
        ,BOARDELECTEDMEMBERS   = ifelse(length(BOARDELECTEDMEMBERS_df) == 0, "No Response", BOARDELECTEDMEMBERS_df$percentage)
        ,STAGGEREDELECTIONS   = ifelse(length(STAGGEREDELECTIONS_df) == 0, "No Response", STAGGEREDELECTIONS_df$percentage)
        ,ELECTIONSPAST10         = ELECTIONSPAST10_df$percentage
        ,CONTESTEDSEATS         = CONTESTEDSEATS_df$percentage
        ,DEMOCRATICPRINCIPLE        = ifelse(length(DEMOCRATICPRINCIPLE_df) == 0, "No Response", DEMOCRATICPRINCIPLE_df$percentage)
        ,OTHERDEMOCRATICPRACTICE   = ifelse(length(OTHERDEMOCRATICPRACTICE_df) == 0, "No Response", OTHERDEMOCRATICPRACTICE_df$percentage)
        ,AGMPARTICIPATION_A1   = ifelse(length(AGMPARTICIPATION_A1_df) == 0, "No Response", AGMPARTICIPATION_A1_df$percentage)
        ,AGMPARTICIPATION_A2   = ifelse(length(AGMPARTICIPATION_A2_df) == 0, "No Response", AGMPARTICIPATION_A2_df$percentage)
        ,AGMPARTICIPATION_A3   = ifelse(length(AGMPARTICIPATION_A3_df) == 0, "No Response", AGMPARTICIPATION_A3_df$percentage)
        ,AGMPARTICIPATION_A4   = ifelse(length(AGMPARTICIPATION_A4_df) == 0, "No Response", AGMPARTICIPATION_A4_df$percentage)
        ,AGMPARTICIPATION_A5   = ifelse(length(AGMPARTICIPATION_A5_df) == 0, "No Response", AGMPARTICIPATION_A5_df$percentage)
        ,AGMPARTICIPATION_A6   = ifelse(length(AGMPARTICIPATION_A6_df) == 0, "No Response", AGMPARTICIPATION_A6_df$percentage)
        ,AGMPARTICIPATION_A7   = ifelse(length(AGMPARTICIPATION_A7_df) == 0, "No Response", AGMPARTICIPATION_A7_df$percentage)
        ,AGMPARTICIPATION_A8   = ifelse(length(AGMPARTICIPATION_A8_df) == 0, "No Response", AGMPARTICIPATION_A8_df$percentage)
        ,DIRECTORSELECTIONM1   = ifelse(length(DIRECTORSELECTIONM1_df) == 0, "No Response", DIRECTORSELECTIONM1_df$percentage)
        ,DIRECTORSELECTIONM2   = ifelse(length(DIRECTORSELECTIONM2_df) == 0, "No Response", DIRECTORSELECTIONM2_df$percentage)
        ,DIRECTORSELECTIONM3   = ifelse(length(DIRECTORSELECTIONM3_df) == 0, "No Response", DIRECTORSELECTIONM3_df$percentage)
        ,DIRECTORSELECTIONM4   = ifelse(length(DIRECTORSELECTIONM4_df) == 0, "No Response", DIRECTORSELECTIONM4_df$percentage)
        ,DIRECTORSELECTIONM5   = ifelse(length(DIRECTORSELECTIONM5_df) == 0, "No Response", DIRECTORSELECTIONM5_df$percentage)
        
        
      ) %>% mutate_all(~ifelse(is.nan(.), NA, .)) #%>% replace(is.na(.), "NA") 
    
  })    
  
  peer_tldel1_wave2_french<-reactive({
    if (nrow(peer_tldel_wave2_french())==0) {
      
      df1<-as_tibble(t(peer_tldel_wave2_french()), rownames = "Variable")
      V1<-"text"
      peer_tldel1_wave2_french<-cbind(df1,V1)
    }else {
      peer_tldel1_wave2_french <-  as_tibble(t(peer_tldel_wave2_french()), rownames = "Variable")
    }
  })
  # peer_tldel1_wave2_french <-  reactive({as_tibble(t(peer_tldel_wave2_french()), rownames = "Variable")
  # }) 
  
  
  
  peer_tldel2_wave2_french <- reactive({
    peer_tldel1_wave2_french() %>% inner_join(tldel_list_wave2_french, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })  
  
  
  
  tldel_df1_wave2_french <- reactive({
    
    tldel_df_wave2_french() %>% 
      left_join(peer_tldel2_wave2_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  })      
  
  
  
  # tldel_varlist_french <- reactive({
  #   df<-selectors_tbl_french[selected1_french(),1:2 ] # Variable, Category 
  #   tldel_varlist_french <-df%>%
  #     filter(str_detect(Category,"Processus démocratique"))  
  # })
  # 
  # own_tldel1_wave1_french<-own_tldel1(x=data1_french, wave=1,text="Données non disponibles",orglist=tldel_list_french)
  # own_tldel_df_wave1_french<-own_org_profile2(ownorg=own_tldel1_wave1_french(),orglist=tldel_list_french, varlist=tldel_varlist_french())
  # peer_tldel_wave1_french<-peer_tldel(x=data1_french,tb=peergroups_selected_Wave1_french(),orglist=tldel_list_french)
  # tldel_df1_wave1_french<-org_profile_df1(data1=own_tldel_df_wave1_french(), data2=peer_tldel_wave1_french())
  # own_tldel1_wave2_french<-own_tldel1(x=data1_french, wave=2,text="Données non disponibles",orglist=tldel_list_french)
  # own_tldel_df_wave2_french<-own_org_profile2(ownorg=own_tldel1_wave2_french(),orglist=tldel_list_french, varlist=tldel_varlist_french())
  # peer_tldel_wave2_french<-peer_tldel(x=data1_french,tb=peergroups_selected_Wave2_french(),orglist=tldel_list_french)
  # tldel_df1_wave2_french<-org_profile_df1(data1=own_tldel_df_wave2_french(), data2=peer_tldel_wave2_french())
  # 
  # ~~~ Delegates~~~ ----  
  del_list_wave1_french <- selectors_tbl_french %>%
    filter(str_detect(Category,"^Délégués")) %>% 
    arrange(Order) %>%
    filter(is.na(Wave)==TRUE)
  del_list_wave2_french <- selectors_tbl_french %>%
    filter(str_detect(Category,"^Délégués")) %>% arrange(Order)
  own_del1_wave1_french <- reactive({
    
    df <- data1_french %>% 
      filter(Wave==1) %>%
      
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(del_list_wave1_french$Variable)
      ) %>% mutate(
        NUMBEROFDELEGATES         = formatC(NUMBEROFDELEGATES, format="f", big.mark=",", digits = 2),
        DELEGMEMBERS        = formatC(DELEGMEMBERS, format="f", big.mark=",", digits = 2),
        DELEGEMPLOYEES        = formatC(DELEGEMPLOYEES, format="f", big.mark=",", digits = 2),
        DELEGNONELECT       = formatC(DELEGNONELECT, format="f", big.mark=",", digits = 2),
        DELEGFEMALE         = formatC(DELEGFEMALE, format="f", big.mark=",", digits = 2),
        DELEGMINORITY        = formatC(DELEGMINORITY, format="f", big.mark=",", digits = 2),
        DELEGINDIGENOUS        = formatC(DELEGINDIGENOUS, format="f", big.mark=",", digits = 2),
        DELEGEXECUTIVE         = formatC(DELEGEXECUTIVE, format="f", big.mark=",", digits = 2),
        DELEGINDUSTRY        = formatC(DELEGINDUSTRY, format="f", big.mark=",", digits = 2),
        DELEGATECOMPENSATIONAMOUNT_A1         = scales::dollar(DELEGATECOMPENSATIONAMOUNT_A1),
        DELEGATECOMPENSATIONAMOUNT_A2          = scales::dollar(DELEGATECOMPENSATIONAMOUNT_A2),
        DELEGATECOMPENSATIONAMOUNT_A3          = scales::dollar(DELEGATECOMPENSATIONAMOUNT_A3),
        DELEGATECOMPENSATIONAMOUNT_A4          = scales::dollar(DELEGATECOMPENSATIONAMOUNT_A4),
        
        REIMBURSEMENTDELEGATES_A1          = scales::dollar(REIMBURSEMENTDELEGATES_A1),
        REIMBURSEMENTDELEGATES_A2          = scales::dollar(REIMBURSEMENTDELEGATES_A2),
        REIMBURSEMENTDELEGATES_A3          = scales::dollar(REIMBURSEMENTDELEGATES_A3),
        REIMBURSEMENTDELEGATES_A4          = scales::dollar(REIMBURSEMENTDELEGATES_A4),
        REIMBURSEMENTDELEGATES_A5          = scales::dollar(REIMBURSEMENTDELEGATES_A5),
        REIMBURSEMENTDELEGATES_A6          = scales::dollar(REIMBURSEMENTDELEGATES_A6),
        TOTALDELEGCOMPENSATION          = scales::dollar(TOTALDELEGCOMPENSATION),
        DELEGRUNREELECTION         = formatC(DELEGRUNREELECTION, format="f", big.mark=",", digits = 2),
        DELEGMAXIMUMSERVICE        = formatC(DELEGMAXIMUMSERVICE, format="f", big.mark=",", digits = 2),
        DELEGRETIREMENTAGE        = formatC(DELEGRETIREMENTAGE, format="f", big.mark=",", digits = 2)
        
      ) 
    
    
    #Transpose df 
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_del1_wave1_french<-cbind(df1,V1)
    }else {
      own_del1_wave1_french <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_del1_wave1_french <-  as_tibble(t(df), rownames = "Variable")
    
  })     
  
  
  own_del2_wave1_french <- reactive({
    own_del1_wave1_french()%>% inner_join(del_list_wave1_french, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation")
    
  }) 
  
  
  
  
  
  del_df_wave1_french <- reactive({
    
    variables_selected_french() %>% filter(str_detect(Category,"^Délégués")) %>%  
      inner_join(own_del2_wave1_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
    
  }) 
  
  
  
  
  peer_del_wave1_french <- reactive({
    
    df <-   data1_french %>% filter(Organization %in% peergroups_selected_Wave1_french()$Organization) %>%
      filter(Wave==1) %>%
      select(starts_with(del_list_wave1_french$Variable))
    
    DELEGATECOMPENSATION_df<-summarydata (df,"DELEGATECOMPENSATION")
    OTHERCOMPENSATION_df<-summarydata (df,"OTHERCOMPENSATION")
    REIMBURSEDELEGATESOTHER_df<-summarydata (df,"REIMBURSEDELEGATESOTHER")
    DELEGRETIREREQUIREMENT_df<-summarydata (df,"DELEGRETIREREQUIREMENT")
    DELEGATESPOUSETRAVEL_df<-summarydata (df,"DELEGATESPOUSETRAVEL")
    DELEGTERMLIMITS_df<-summarydata (df,"DELEGTERMLIMITS")
    TOTALDELEGLIMIT_df<-summarydata (df,"TOTALDELEGLIMIT")
    HOWVOTEDELEGC1_df<-summarydata (df,"HOWVOTEDELEGC1")
    HOWVOTEDELEGC2_df<-summarydata (df,"HOWVOTEDELEGC2")
    HOWVOTEDELEGC3_df<-summarydata (df,"HOWVOTEDELEGC3")
    HOWVOTEDELEGC4_df<-summarydata (df,"HOWVOTEDELEGC4")
    HOWVOTEDELEGC5_df<-summarydata (df,"HOWVOTEDELEGC5")
    NUMBEROFDELEGATES_df<-meandata (df,NUMBEROFDELEGATES,0)
    AGEOFDELEG_df<-meandata (df,AGEOFDELEG,0)
    HOWLONGDELEGSERVE_df<-meandata (df,HOWLONGDELEGSERVE,0)
    DELEGMEMBERS_df<-meandata (df,DELEGMEMBERS,0)
    DELEGEMPLOYEES_df<-meandata (df,DELEGEMPLOYEES,0)
    DELEGNONELECT_df<-meandata (df,DELEGNONELECT,0)
    DELEGFEMALE_df<-meandata (df,DELEGFEMALE,0)
    DELEGMINORITY_df<-meandata (df,DELEGMINORITY,0)
    DELEGINDIGENOUS_df<-meandata (df,DELEGINDIGENOUS,0)
    DELEGEXECUTIVE_df<-meandata (df,DELEGEXECUTIVE,0)
    DELEGINDUSTRY_df<-meandata (df,DELEGINDUSTRY,0)
    DELEGATECOMPENSATIONAMOUNT_A1_df<-meandata (df,DELEGATECOMPENSATIONAMOUNT_A1,1)
    DELEGATECOMPENSATIONAMOUNT_A2_df<-meandata (df,DELEGATECOMPENSATIONAMOUNT_A2,1)
    DELEGATECOMPENSATIONAMOUNT_A3_df<-meandata (df,DELEGATECOMPENSATIONAMOUNT_A3,1)
    DELEGATECOMPENSATIONAMOUNT_A4_df<-meandata (df,DELEGATECOMPENSATIONAMOUNT_A4,1)
    REIMBURSEMENTDELEGATES_A1_df<-meandata (df,REIMBURSEMENTDELEGATES_A1,1)
    REIMBURSEMENTDELEGATES_A2_df<-meandata (df,REIMBURSEMENTDELEGATES_A2,1)
    REIMBURSEMENTDELEGATES_A3_df<-meandata (df,REIMBURSEMENTDELEGATES_A3,1)
    REIMBURSEMENTDELEGATES_A4_df<-meandata (df,REIMBURSEMENTDELEGATES_A4,1)
    REIMBURSEMENTDELEGATES_A5_df<-meandata (df,REIMBURSEMENTDELEGATES_A5,1)
    REIMBURSEMENTDELEGATES_A6_df<-meandata (df,REIMBURSEMENTDELEGATES_A6,1)
    TOTALDELEGCOMPENSATION_df<-meandata (df,TOTALDELEGCOMPENSATION,1)
    DELEGRUNREELECTION_df<-meandata (df,DELEGRUNREELECTION,0)
    DELEGMAXIMUMSERVICE_df<-meandata (df,DELEGMAXIMUMSERVICE,0)
    DELEGRETIREMENTAGE_df<-meandata (df,DELEGRETIREMENTAGE,0)
    
    peer_del_wave1_french<-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
      ) %>%  
      
      
      mutate(
        NUMBEROFDELEGATES         = NUMBEROFDELEGATES_df$percentage,
        AGEOFDELEG                = AGEOFDELEG_df$percentage,
        HOWLONGDELEGSERVE         = HOWLONGDELEGSERVE_df$percentage,
        DELEGMEMBERS     = DELEGMEMBERS_df$percentage,
        DELEGEMPLOYEES = DELEGEMPLOYEES_df$percentage,
        DELEGNONELECT       = DELEGNONELECT_df$percentage,
        DELEGFEMALE         = DELEGFEMALE_df$percentage,
        DELEGMINORITY        = DELEGMINORITY_df$percentage,
        DELEGINDIGENOUS        = DELEGINDIGENOUS_df$percentage,
        DELEGEXECUTIVE         = DELEGEXECUTIVE_df$percentage,
        DELEGINDUSTRY        = DELEGINDUSTRY_df$percentage,
        DELEGATECOMPENSATION        = ifelse(length(DELEGATECOMPENSATION_df) == 0, "No Response", DELEGATECOMPENSATION_df$percentage),
        DELEGATECOMPENSATIONAMOUNT_A1         = DELEGATECOMPENSATIONAMOUNT_A1_df$percentage,
        DELEGATECOMPENSATIONAMOUNT_A2          = DELEGATECOMPENSATIONAMOUNT_A2_df$percentage,
        DELEGATECOMPENSATIONAMOUNT_A3          = DELEGATECOMPENSATIONAMOUNT_A3_df$percentage,
        DELEGATECOMPENSATIONAMOUNT_A4          = DELEGATECOMPENSATIONAMOUNT_A4_df$percentage,
        OTHERCOMPENSATION        = ifelse(length(OTHERCOMPENSATION_df) == 0, "No Response", OTHERCOMPENSATION_df$percentage),
        REIMBURSEMENTDELEGATES_A1          = REIMBURSEMENTDELEGATES_A1_df$percentage,
        REIMBURSEMENTDELEGATES_A2          = REIMBURSEMENTDELEGATES_A2_df$percentage,
        REIMBURSEMENTDELEGATES_A3          = REIMBURSEMENTDELEGATES_A3_df$percentage,
        REIMBURSEMENTDELEGATES_A4          = REIMBURSEMENTDELEGATES_A4_df$percentage,
        REIMBURSEMENTDELEGATES_A5          = REIMBURSEMENTDELEGATES_A5_df$percentage,
        REIMBURSEMENTDELEGATES_A6          = REIMBURSEMENTDELEGATES_A6_df$percentage,
        REIMBURSEDELEGATESOTHER        = ifelse(length(REIMBURSEDELEGATESOTHER_df) == 0, "No Response", REIMBURSEDELEGATESOTHER_df$percentage),
        DELEGATESPOUSETRAVEL        = ifelse(length(DELEGATESPOUSETRAVEL_df) == 0, "No Response", DELEGATESPOUSETRAVEL_df$percentage),
        TOTALDELEGCOMPENSATION          = TOTALDELEGCOMPENSATION_df$percentage,
        DELEGRUNREELECTION         = DELEGRUNREELECTION_df$percentage,
        DELEGMAXIMUMSERVICE        = DELEGMAXIMUMSERVICE_df$percentage,
        DELEGRETIREREQUIREMENT        = ifelse(length(DELEGRETIREREQUIREMENT_df) == 0, "No Response", DELEGRETIREREQUIREMENT_df$percentage),
        DELEGRETIREMENTAGE        = DELEGRETIREMENTAGE_df$percentage,
        # DELEGTERMLIMITS=NA
        DELEGTERMLIMITS        = ifelse(length(DELEGTERMLIMITS_df) == 0, "No Response", DELEGTERMLIMITS_df$percentage)
        ,TOTALDELEGLIMIT        = ifelse(length(TOTALDELEGLIMIT_df) == 0, "No Response", TOTALDELEGLIMIT_df$percentage),
        HOWVOTEDELEGC1        = ifelse(length(HOWVOTEDELEGC1_df) == 0, "No Response", HOWVOTEDELEGC1_df$percentage),
        HOWVOTEDELEGC2        = ifelse(length(HOWVOTEDELEGC2_df) == 0, "No Response", HOWVOTEDELEGC2_df$percentage),
        HOWVOTEDELEGC3        = ifelse(length(HOWVOTEDELEGC3_df) == 0, "No Response", HOWVOTEDELEGC3_df$percentage),
        HOWVOTEDELEGC4        = ifelse(length(HOWVOTEDELEGC4_df) == 0, "No Response", HOWVOTEDELEGC4_df$percentage),
        HOWVOTEDELEGC5        = ifelse(length(HOWVOTEDELEGC5_df) == 0, "No Response", HOWVOTEDELEGC5_df$percentage),
        
      )  %>% mutate_all(~ifelse(is.nan(.), NA, .)) %>% replace(is.na(.), "NA") 
    
  })
  
  peer_del1_wave1_french<-reactive({
    if (nrow(peer_del_wave1_french())==0) {
      
      df1<-as_tibble(t(peer_del_wave1_french()), rownames = "Variable")
      V1<-"text"
      peer_del1_wave1_french<-cbind(df1,V1)
    }else {
      peer_del1_wave1_french <-  as_tibble(t(peer_del_wave1_french()), rownames = "Variable")
    }
  })
  
  # peer_del1_wave1_french <-  reactive({as_tibble(t(peer_del_wave1_french()), rownames = "Variable")
  # })  
  
  
  
  peer_del2_wave1_french <- reactive({
    peer_del1_wave1_french() %>% inner_join(del_list_wave1_french, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })  
  
  
  
  
  
  del_df1_wave1_french <- reactive({
    
    del_df_wave1_french() %>% 
      left_join(peer_del2_wave1_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  })  
  
  own_del1_wave2_french <- reactive({
    
    df <- data1_french %>% 
      filter(Wave==2) %>%
      
      filter(Organization %in% reactive_values$organization) %>% 
      select(starts_with(del_list_wave2_french$Variable)
      ) %>% mutate(
        NUMBEROFDELEGATES         = formatC(NUMBEROFDELEGATES, format="f", big.mark=",", digits = 2),
        DELEGMEMBERS        = formatC(DELEGMEMBERS, format="f", big.mark=",", digits = 2),
        DELEGEMPLOYEES        = formatC(DELEGEMPLOYEES, format="f", big.mark=",", digits = 2),
        DELEGNONELECT       = formatC(DELEGNONELECT, format="f", big.mark=",", digits = 2),
        DELEGFEMALE         = formatC(DELEGFEMALE, format="f", big.mark=",", digits = 2),
        DELEGMINORITY        = formatC(DELEGMINORITY, format="f", big.mark=",", digits = 2),
        DELEGINDIGENOUS        = formatC(DELEGINDIGENOUS, format="f", big.mark=",", digits = 2),
        DELEGEXECUTIVE         = formatC(DELEGEXECUTIVE, format="f", big.mark=",", digits = 2),
        DELEGINDUSTRY        = formatC(DELEGINDUSTRY, format="f", big.mark=",", digits = 2),
        DELEGATECOMPENSATIONAMOUNT_A1         = scales::dollar(DELEGATECOMPENSATIONAMOUNT_A1),
        DELEGATECOMPENSATIONAMOUNT_A2          = scales::dollar(DELEGATECOMPENSATIONAMOUNT_A2),
        DELEGATECOMPENSATIONAMOUNT_A3          = scales::dollar(DELEGATECOMPENSATIONAMOUNT_A3),
        DELEGATECOMPENSATIONAMOUNT_A4          = scales::dollar(DELEGATECOMPENSATIONAMOUNT_A4),
        
        REIMBURSEMENTDELEGATES_A1          = scales::dollar(REIMBURSEMENTDELEGATES_A1),
        REIMBURSEMENTDELEGATES_A2          = scales::dollar(REIMBURSEMENTDELEGATES_A2),
        REIMBURSEMENTDELEGATES_A3          = scales::dollar(REIMBURSEMENTDELEGATES_A3),
        REIMBURSEMENTDELEGATES_A4          = scales::dollar(REIMBURSEMENTDELEGATES_A4),
        REIMBURSEMENTDELEGATES_A5          = scales::dollar(REIMBURSEMENTDELEGATES_A5),
        REIMBURSEMENTDELEGATES_A6          = scales::dollar(REIMBURSEMENTDELEGATES_A6),
        TOTALDELEGCOMPENSATION          = scales::dollar(TOTALDELEGCOMPENSATION),
        DELEGRUNREELECTION         = formatC(DELEGRUNREELECTION, format="f", big.mark=",", digits = 2),
        DELEGMAXIMUMSERVICE        = formatC(DELEGMAXIMUMSERVICE, format="f", big.mark=",", digits = 2),
        DELEGRETIREMENTAGE        = formatC(DELEGRETIREMENTAGE, format="f", big.mark=",", digits = 2)
        
      ) 
    
    
    #Transpose df 
    if (nrow(df)==0) {
      df1 <-  as_tibble(t(df), rownames = "Variable")
      V1<-"text"
      own_del1_wave2_french<-cbind(df1,V1)
    }else {
      own_del1_wave2_french <-  as_tibble(t(df), rownames = "Variable")
    }
    # own_del1_wave2_french <-  as_tibble(t(df), rownames = "Variable")
    
  })     
  
  
  own_del2_wave2_french <- reactive({
    own_del1_wave2_french()%>% inner_join(del_list_wave2_french, by = "Variable") %>% arrange(Order) %>% 
      select("Variable","Second_level_disaggregation", "Third_level_disaggregation", "V1") %>% 
      rename( "Your Organization"  = "V1",
              "Theme"              = "Second_level_disaggregation",     
              "Measure"            =  "Third_level_disaggregation")
    
  }) 
  
  
  
  
  
  del_df_wave2_french <- reactive({
    
    variables_selected_french() %>% filter(str_detect(Category,"^Délégués")) %>%  
      inner_join(own_del2_wave2(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Variable")
    
    
  }) 
  
  
  
  
  peer_del_wave2_french <- reactive({
    
    df <-   data1_french %>% filter(Organization %in% peergroups_selected_Wave2_french()$Organization) %>%
      filter(Wave==2) %>%
      select(starts_with(del_list_wave2_french$Variable))
    
    DELEGATECOMPENSATION_df<-summarydata (df,"DELEGATECOMPENSATION")
    OTHERCOMPENSATION_df<-summarydata (df,"OTHERCOMPENSATION")
    REIMBURSEDELEGATESOTHER_df<-summarydata (df,"REIMBURSEDELEGATESOTHER")
    DELEGRETIREREQUIREMENT_df<-summarydata (df,"DELEGRETIREREQUIREMENT")
    DELEGATESPOUSETRAVEL_df<-summarydata (df,"DELEGATESPOUSETRAVEL")
    DELEGTERMLIMITS_df<-summarydata (df,"DELEGTERMLIMITS")
    TOTALDELEGLIMIT_df<-summarydata (df,"TOTALDELEGLIMIT")
    HOWVOTEDELEGC1_df<-summarydata (df,"HOWVOTEDELEGC1")
    HOWVOTEDELEGC2_df<-summarydata (df,"HOWVOTEDELEGC2")
    HOWVOTEDELEGC3_df<-summarydata (df,"HOWVOTEDELEGC3")
    HOWVOTEDELEGC4_df<-summarydata (df,"HOWVOTEDELEGC4")
    HOWVOTEDELEGC5_df<-summarydata (df,"HOWVOTEDELEGC5")
    NUMBEROFDELEGATES_df<-meandata (df,NUMBEROFDELEGATES,0)
    AGEOFDELEG_df<-meandata (df,AGEOFDELEG,0)
    HOWLONGDELEGSERVE_df<-meandata (df,HOWLONGDELEGSERVE,0)
    DELEGMEMBERS_df<-meandata (df,DELEGMEMBERS,0)
    DELEGEMPLOYEES_df<-meandata (df,DELEGEMPLOYEES,0)
    DELEGNONELECT_df<-meandata (df,DELEGNONELECT,0)
    DELEGFEMALE_df<-meandata (df,DELEGFEMALE,0)
    DELEGMINORITY_df<-meandata (df,DELEGMINORITY,0)
    DELEGINDIGENOUS_df<-meandata (df,DELEGINDIGENOUS,0)
    DELEGEXECUTIVE_df<-meandata (df,DELEGEXECUTIVE,0)
    DELEGINDUSTRY_df<-meandata (df,DELEGINDUSTRY,0)
    DELEGATECOMPENSATIONAMOUNT_A1_df<-meandata (df,DELEGATECOMPENSATIONAMOUNT_A1,1)
    DELEGATECOMPENSATIONAMOUNT_A2_df<-meandata (df,DELEGATECOMPENSATIONAMOUNT_A2,1)
    DELEGATECOMPENSATIONAMOUNT_A3_df<-meandata (df,DELEGATECOMPENSATIONAMOUNT_A3,1)
    DELEGATECOMPENSATIONAMOUNT_A4_df<-meandata (df,DELEGATECOMPENSATIONAMOUNT_A4,1)
    REIMBURSEMENTDELEGATES_A1_df<-meandata (df,REIMBURSEMENTDELEGATES_A1,1)
    REIMBURSEMENTDELEGATES_A2_df<-meandata (df,REIMBURSEMENTDELEGATES_A2,1)
    REIMBURSEMENTDELEGATES_A3_df<-meandata (df,REIMBURSEMENTDELEGATES_A3,1)
    REIMBURSEMENTDELEGATES_A4_df<-meandata (df,REIMBURSEMENTDELEGATES_A4,1)
    REIMBURSEMENTDELEGATES_A5_df<-meandata (df,REIMBURSEMENTDELEGATES_A5,1)
    REIMBURSEMENTDELEGATES_A6_df<-meandata (df,REIMBURSEMENTDELEGATES_A6,1)
    TOTALDELEGCOMPENSATION_df<-meandata (df,TOTALDELEGCOMPENSATION,1)
    DELEGRUNREELECTION_df<-meandata (df,DELEGRUNREELECTION,0)
    DELEGMAXIMUMSERVICE_df<-meandata (df,DELEGMAXIMUMSERVICE,0)
    DELEGRETIREMENTAGE_df<-meandata (df,DELEGRETIREMENTAGE,0)
    peer_del_wave2_french<-  df %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
      ) %>%  
      
      
      mutate(
        NUMBEROFDELEGATES         = NUMBEROFDELEGATES_df$percentage,
        AGEOFDELEG                = AGEOFDELEG_df$percentage,
        HOWLONGDELEGSERVE         = HOWLONGDELEGSERVE_df$percentage,
        DELEGMEMBERS     =DELEGMEMBERS_df$percentage,
        DELEGEMPLOYEES = DELEGEMPLOYEES_df$percentage,
        DELEGNONELECT       = DELEGNONELECT_df$percentage,
        DELEGFEMALE         = DELEGFEMALE_df$percentage,
        DELEGMINORITY        = DELEGMINORITY_df$percentage,
        DELEGINDIGENOUS        = DELEGINDIGENOUS_df$percentage,
        DELEGEXECUTIVE         = DELEGEXECUTIVE_df$percentage,
        DELEGINDUSTRY        = DELEGINDUSTRY_df$percentage,
        DELEGATECOMPENSATION        = ifelse(length(DELEGATECOMPENSATION_df) == 0, "No Response", DELEGATECOMPENSATION_df$percentage),
        DELEGATECOMPENSATIONAMOUNT_A1         = DELEGATECOMPENSATIONAMOUNT_A1_df$percentage,
        DELEGATECOMPENSATIONAMOUNT_A2          = DELEGATECOMPENSATIONAMOUNT_A2_df$percentage,
        DELEGATECOMPENSATIONAMOUNT_A3          = DELEGATECOMPENSATIONAMOUNT_A3_df$percentage,
        DELEGATECOMPENSATIONAMOUNT_A4          = DELEGATECOMPENSATIONAMOUNT_A4_df$percentage,
        OTHERCOMPENSATION        = ifelse(length(OTHERCOMPENSATION_df) == 0, "No Response", OTHERCOMPENSATION_df$percentage),
        REIMBURSEMENTDELEGATES_A1          = REIMBURSEMENTDELEGATES_A1_df$percentage,
        REIMBURSEMENTDELEGATES_A2          = REIMBURSEMENTDELEGATES_A2_df$percentage,
        REIMBURSEMENTDELEGATES_A3          = REIMBURSEMENTDELEGATES_A3_df$percentage,
        REIMBURSEMENTDELEGATES_A4          = REIMBURSEMENTDELEGATES_A4_df$percentage,
        REIMBURSEMENTDELEGATES_A5          = REIMBURSEMENTDELEGATES_A5_df$percentage,
        REIMBURSEMENTDELEGATES_A6          = REIMBURSEMENTDELEGATES_A6_df$percentage,
        REIMBURSEDELEGATESOTHER        = ifelse(length(REIMBURSEDELEGATESOTHER_df) == 0, "No Response", REIMBURSEDELEGATESOTHER_df$percentage),
        DELEGATESPOUSETRAVEL        = ifelse(length(DELEGATESPOUSETRAVEL_df) == 0, "No Response", DELEGATESPOUSETRAVEL_df$percentage),
        TOTALDELEGCOMPENSATION          = TOTALDELEGCOMPENSATION_df$percentage,
        DELEGRUNREELECTION         = DELEGRUNREELECTION_df$percentage,
        DELEGMAXIMUMSERVICE        = DELEGMAXIMUMSERVICE_df$percentage,
        DELEGRETIREREQUIREMENT        = ifelse(length(DELEGRETIREREQUIREMENT_df) == 0, "No Response", DELEGRETIREREQUIREMENT_df$percentage),
        DELEGRETIREMENTAGE        = DELEGRETIREMENTAGE_df$percentage,
        DELEGTERMLIMITS        = ifelse(length(DELEGTERMLIMITS_df) == 0, "No Response", DELEGTERMLIMITS_df$percentage)
        ,TOTALDELEGLIMIT        = ifelse(length(TOTALDELEGLIMIT_df) == 0, "No Response", TOTALDELEGLIMIT_df$percentage),
        HOWVOTEDELEGC1        = ifelse(length(HOWVOTEDELEGC1_df) == 0, "No Response", HOWVOTEDELEGC1_df$percentage),
        HOWVOTEDELEGC2        = ifelse(length(HOWVOTEDELEGC2_df) == 0, "No Response", HOWVOTEDELEGC2_df$percentage),
        HOWVOTEDELEGC3        = ifelse(length(HOWVOTEDELEGC3_df) == 0, "No Response", HOWVOTEDELEGC3_df$percentage),
        HOWVOTEDELEGC4        = ifelse(length(HOWVOTEDELEGC4_df) == 0, "No Response", HOWVOTEDELEGC4_df$percentage),
        HOWVOTEDELEGC5        = ifelse(length(HOWVOTEDELEGC5_df) == 0, "No Response", HOWVOTEDELEGC5_df$percentage),
        
      )  %>% mutate_all(~ifelse(is.nan(.), NA, .)) %>% replace(is.na(.), "NA") 
    
  })
  
  
  peer_del1_wave2_french<-reactive({
    if (nrow(peer_del_wave2_french())==0) {
      
      df1<-as_tibble(t(peer_del_wave2_french()), rownames = "Variable")
      V1<-"text"
      peer_del1_wave2_french<-cbind(df1,V1)
    }else {
      peer_del1_wave2_french <-  as_tibble(t(peer_del_wave2_french()), rownames = "Variable")
    }
  })
  # peer_del1_wave2_french <-  reactive({as_tibble(t(peer_del_wave2_french()), rownames = "Variable")
  # })  
  
  
  
  peer_del2_wave2_french <- reactive({
    peer_del1_wave2_french() %>% inner_join(del_list_wave2_french, by = "Variable") %>% 
      select("Variable", 
             #"Label",
             "V1") %>% rename( "Peer Group" = "V1"
                               #,"Measure"             = "Label"
             )
  })  
  
  
  
  
  
  del_df1_wave2_french <- reactive({
    
    del_df_wave2_french() %>% 
      left_join(peer_del2_wave2_french(), by = "Variable") %>% 
      select("Theme","Measure", "Your Organization", "Peer Group")
    
  })  
  
  # del_varlist_french <- reactive({
  #   df<-selectors_tbl_french[selected1_french(),1:2 ] # Variable, Category 
  #   del_varlist_french <-df%>%
  #     filter(str_detect(Category,"^Délégués"))  
  # })
  # 
  # own_del1_wave1_french<-own_del1(x=data1_french, wave=1,text="data not available ",orglist=del_list_french)
  # own_del_df_wave1_french<-own_org_profile2(ownorg=own_del1_wave1_french(),orglist=del_list_french, varlist=del_varlist_french())
  # peer_del_wave1_french<-peer_del(x=data1_french,tb=peergroups_selected_Wave1_french(),orglist=del_list_french)
  # del_df1_wave1_french<-org_profile_df1(data1=own_del_df_wave1_french(), data2=peer_del_wave1_french())
  # own_del1_wave2_french<-own_del1(x=data1_french, wave=2,text="data not available ",orglist=del_list_french)
  # own_del_df_wave2_french<-own_org_profile2(ownorg=own_del1_wave2_french(),orglist=del_list_french, varlist=del_varlist_french())
  # peer_del_wave2_french<-peer_del(x=data1_french,tb=peergroups_selected_Wave2_french(),orglist=del_list_french)
  # del_df1_wave2_french<-org_profile_df1(data1=own_del_df_wave2_french(), data2=peer_del_wave2_french())
  # 
  # ~~~FULL TABLE French~~~ ---- 
  full_table_wave1_french <- reactive ({
    rbind(org_profile_df1_wave1_french(),ceo_df1_wave1_french(),boardcomp_df1_wave1_french(),bedi_df1_wave1_french(),cob_df1_wave1_french(),bp_df1_wave1_french(),compen_df1_wave1_french(),tldel_df1_wave1_french(),del_df1_wave1_french())
    
  })
  full_table_wave2_french <- reactive ({
    rbind(org_profile_df1_wave2_french(),ceo_df1_wave2_french(),boardcomp_df1_wave2_french(),bedi_df1_wave2_french(),cob_df1_wave2_french(),bp_df1_wave2_french(),compen_df1_wave2_french(),tldel_df1_wave2_french(),del_df1_wave2_french())
    
  })
  
  # Questionnaire table 
  survey_data<-function(x=data1_questionnaire, wave=1){
    survey_data <- reactive({
      
      df <- x %>% 
        filter(Wave==wave) %>%
        
        filter(Organization %in% reactive_values$organization)
      
      df1 <-  as_tibble(t(df), rownames = "Variable")
      df2<- questionnaire_tbl %>%
        left_join(df1, by = "Variable")
      df2 <- df2 %>% 
        mutate_if(is.character, ~replace_na(.,""))
      col_order <- c("eng", "fra", "V1","Variable")
      df2<-df2[, col_order]
      colnames(df2)[colnames(df2)=='V1'] <- reactive_values$organization
      # colnames(df2)[colnames(df2)=='fra'] <- "Registre du sondage"
      survey_data<-df2
      return(survey_data)
      
    })
  }
  survey_data_wave1<-survey_data(x=data1_questionnaire, wave=1)
  survey_data_wave2<-survey_data(x=data1_questionnaire, wave=2)
  survey_data_wave1_french<-survey_data(x=data1_questionnaire_french, wave=1)
  survey_data_wave2_french<-survey_data(x=data1_questionnaire_french, wave=2)
}
shinyApp(ui, server)



