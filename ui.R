customLogo <- shinyDashboardLogoDIY(
  
  boldText = "Covid"
  ,mainText = "Dashboard"
  ,textSize = 16
  ,badgeText = "Realtime"
  ,badgeTextColor = "white"
  #,badgeTextSize = 2
  ,badgeBackColor = "#1e2b37"
  #,badgeBorderRadius = 3
  
)

header <-   dashboardHeader( title= customLogo ) #titleWidth = 275

sidebar <-  dashboardSidebar(
  sidebarMenu(
    menuItem(text = "Summary", icon = icon("tachometer-alt"), tabName = "summary"),
    menuItem(text = "Top Country", icon = icon("viruses"), tabName = "top_n"),
    menuItem(text = "Trend",tabName = "trend",icon=icon("chart-line")),
    #menuItem(text = "Comparison", icon = icon("chart-bar"), tabName = "stat"),
    menuItem(text = "Data", icon = icon("table"), tabName = "data"),
    menuItem(text = "About",tabName = "about",icon=icon("comment-alt"))
  )
  
)

body <- dashboardBody(customTheme,
                      tabItems(
                        #Summary
                        tabItem(tabName = "summary", align = "center",
                              fluidPage(
                                fluidRow(
                                  valueBoxOutput("vbox1", width = 4),
                                  valueBoxOutput("vbox2", width = 4),
                                  valueBoxOutput("vbox3", width = 4),
                                  column(width=12,
                                         box(width=12, status = "danger",
                                             leafletOutput("Map", height = "600px")
                                          )
                                         )
                                  ),
                              )
                        ),
                        
                        tabItem(tabName = "trend",
                                fluidPage(
                                  fluidRow(
                                    box(
                                      solidHeader=T,
                                      collapsible = F,
                                      width=8,
                                      height = 475,
                                      tags$strong("Trend's Report"),
                                      plotlyOutput("trend_map",height=430)
                                      
                                    ),
                                    
                                    box(
                                      solidHeader=F,
                                      collapsible = F,
                                      width=4,
                                      pickerInput(
                                        inputId = "Country",
                                        label = "Select Country:",
                                        choices = unique(covid$Country),
                                        selected = "Indonesia",
                                        options = list(
                                          'actions-box' = TRUE
                                        ),
                                        choicesOpt = list( content = stringr::str_trunc(unique(covid$Country), width = 20)),
                                        
                                        multiple = TRUE
                                      )
                                      
                                    ),
                                    box(solidHeader=F,
                                        collapsible = F,
                                        width=4,height = 360,
                                        strong("Corona's Case Graph"),
                                        plotOutput("col_plot_1",height=320)
                                    )
                                    
                                    
                                    
                                    
                                  )
                                  
                                  
                                )
                                
                        ),
                        
                        #Data
                        tabItem(tabName = "data", 
                                
                                h2("COVID-19 Data"),
                                
                                br(),
                                
                                downloadButton(outputId = "download", label = "Download Data"),
                                
                                br(), 
                                
                                DT::dataTableOutput("data_table"),
                                
                                h4("Source: Center for Systems Science and Engineering (CSSE) at Johns Hopkins University")
                        ),
                        
                        #Top_Country
                        tabItem(tabName = "top_n",
                                fluidPage(
                                  fluidRow(
                                    column(width = 4,
                                           box(solidHeader=F,
                                               collapsible = F,
                                               width=12,
                                               height=425,
                                               sliderInput(inputId = "bins_top",
                                                           label = "Select Top:",
                                                           min = 5,max = 30,
                                                           step = 1,
                                                           value = 10
                                               ),
                                               radioButtons(inputId="radio_type_top",
                                                            label="Case Type:",
                                                            choices=c("Case","Vaccinated","Death"),
                                                            selected="Case",
                                                            inline=F),
                                               radioButtons(inputId="radio_asc_top",
                                                            label="Sorting:",
                                                            choices=c("Ascending","Descending"),
                                                            selected="Descending",
                                                            inline=F)
                                               
                                           )
                                           
                                    ),
                                    column(width = 8,
                                           box(solidHeader=F,
                                               collapsible = F,
                                               width=12,
                                               
                                               plotOutput("top_country_plot")
                                           )
                                    )
                                    
                                    
                                  )
                                  
                                )
                                
                        ),
                        
                        #About
                        tabItem(tabName = "about",
                                userBox(
                                  title = userDescription(
                                    title = "Ignatius Sarwo Edhi Wiyoto",
                                    subtitle = "Data Enthusiast",
                                    type = 2,
                                    image = "isew3.png",
                                  ),
              
                                  width = 9,
                                  "Hi! This project, Covid Dashboard, will help you to keep updated the information about Corona Virus arround the world.",
                                  "The realtime data was taken from", a("Systems Science and Engineering (CSSE) Johns Hopkins University.",href="https://github.com/CSSEGISandData/COVID-19/"),
                                  
                                  
                                  footer = ""
                                )
                        )
                        
                      )
)

                              
ui <- dashboardPage(
  #skin="red",
  header = header,
  body = body,
  sidebar =sidebar
)
