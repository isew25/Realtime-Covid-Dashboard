library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(maps)
library(rgdal)
library(treemapify)
library(shinyWidgets)

# data wrangling
library(dplyr)
library(tidyr)
library(lubridate)
library(reshape2)
library(zoo)
library(stringr)


# visualization
library(ggplot2)
library(plotly)
library(scales)
library(glue)
library(leaflet)
library(geojsonio)

rm(list=ls())

# Import Data from John Hopkins University

case_confirmed <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", 
                           check.names = F, stringsAsFactors = FALSE)

case_recover <- read.csv("https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_doses_admin_global.csv",
                         check.names = F, stringsAsFactors = FALSE) %>% select(-c(UID,iso2,iso3,code3,FIPS,Admin2,Combined_Key,Population))

renames <- c('Country/Region' = 'Country_Region', 'Province/State' = 'Province_State', Long= 'Long_')
case_recover <- dplyr::rename(case_recover, !!!renames) 


case_death <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
                       check.names = F, stringsAsFactors = FALSE)

# Data Cleansing
## Active Cases

case_confirmed <- case_confirmed %>% 
  pivot_longer(-c(`Province/State`, `Country/Region`, Lat, Long), 
               names_to = "Date", values_to = "Case") %>% 
  mutate(
    Date = mdy(Date)
  )

## Recovery
case_recover <- case_recover %>% 
  pivot_longer(-c(`Province/State`, `Country/Region`, Lat, Long), 
               names_to = "Date", values_to = "Recover") %>% 
  mutate(
    Date = ymd(Date)
  )

## Death
case_death <- case_death %>% 
  pivot_longer(-c(`Province/State`, `Country/Region`, Lat, Long), 
               names_to = "Date", values_to = "Death") %>% 
  mutate(
    Date = mdy(Date)
  )

## Merge/Join data.frame
covid <- case_confirmed %>% 
  full_join(case_recover) %>% 
  left_join(case_death) #%>% 
  #mutate(
  #  cfr = Death/Case,
  #  Active = Case-(Death-Recover)
  #) 

renames <- c(Country = 'Country/Region', State = 'Province/State')
covid <- dplyr::rename(covid, !!!renames) %>% 
  filter(!(Country %in% c("Diamond Princess", "MS Zaandam")))

covid[is.na(covid)] <- 0

covid_total_Recover <- covid %>% 
  select(Recover, Country) %>% 
  group_by(Country) %>% 
  summarise(Total = max(Recover))

covid_total_Death <- covid %>% 
  select(Death, Country) %>% 
  group_by(Country) %>% 
  summarise(Total = max(Death))

covid_total_Case  <- covid %>% 
  select(Case, Country) %>% 
  group_by(Country) %>% 
  summarise(Total = max(Case))

# Data from the latest date only

covid_update <- covid %>% 
  filter(Date == max(Date)) 

list1 <- c("Bahamas", "Burma", "Cabo Verde","Congo (Brazzaville)",
           "Congo (Kinshasa)","Cote d'Ivoire","Czechia","Eswatini",
           "Guinea-Bissau","Holy See","Korea, South","North Macedonia",
           "Serbia","Taiwan*","Tanzania","Timor-Leste","US", "West Bank and Gaza")

list2 <- c("The Bahamas","Myanmar","Cape Verde","Republic of Congo",
           "Democratic Republic of the Congo","Ivory Coast","Czech Republic",
           "Swaziland","Guinea Bissau","Vatican","South Korea","Macedonia",
           "Republic of Serbia", "Taiwan","United Republic of Tanzania", 
           "East Timor","United States of America", "Palestine")

for (i in 1:length(list1)){
  covid$Country[which(covid$Country == list1[i])] <- list2[i]
}

world_json <- geojson_read("countries.json", what = "sp")

# Theme for Visualization

### THEME
###Custome Style ####
customTheme <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(33,37,41)"
  ,primaryFontColor = "rgb(245,245,245)"
  ,infoFontColor = "rgb(245,245,245)"
  ,successFontColor = "rgb(33,37,41)"
  ,warningFontColor = "rgb(33,37,41)"
  ,dangerFontColor = "rgb(33,37,41)"
  ,bodyBackColor = "rgb(255,255,255)"
  
  ### header
  ,logoBackColor = "rgb(23,103,124)"
  
  ,headerButtonBackColor = "rgb(238,238,238)"
  ,headerButtonIconColor = "rgb(75,75,75)"
  ,headerButtonBackColorHover = "rgb(210,210,210)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,headerBackColor = "rgb(238,238,238)"
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "2px 2px 2px"
  
  
  ### sidebar
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(45,105,131)"
    ,colorMiddle = "rgb(45,105,131)"
    ,colorEnd = "rgb(45,105,131)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "inherit"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = ""
  ,sidebarShadowColor = "0px 0px 0px"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(255,255,255)"
  ,sidebarSearchIconColor = "rgb(44,62,80)"
  ,sidebarSearchBorderColor = "rgb(255,255,255)"
  
  ,sidebarTabTextColor = "rgb(255,255,255)"
  ,sidebarTabTextSize = 14
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = 0
  
  ,sidebarTabBackColorSelected = "rgb(30,43,55)"
  ,sidebarTabTextColorSelected = "rgb(24,188,156)"
  ,sidebarTabRadiusSelected = "0px"
  
  ,sidebarTabBackColorHover = "rgb(44,62,80)"
  ,sidebarTabTextColorHover = "rgb(24,188,156)"
  ,sidebarTabBorderStyleHover = "none"
  ,sidebarTabBorderColorHover = "none"
  ,sidebarTabBorderWidthHover = 0
  ,sidebarTabRadiusHover = "0px"
  
  ### boxes
  ,boxBackColor = "rgb(233, 241, 245)"
  ,boxBorderRadius = 0
  ,boxShadowSize = "0px 0px 0px"
  ,boxShadowColor = ""
  ,boxTitleSize = 19
  ,boxDefaultColor = "rgb(50,150,175)" #(52,152,219) (30, 43, 55)
  ,boxPrimaryColor = "rgb(44,62,80)"
  ,boxInfoColor = "rgb(52,152,219)"
  ,boxSuccessColor = "rgb(24, 188, 156)"
  ,boxWarningColor = "rgb(243,156,18)"
  ,boxDangerColor = "rgb(231,76,60)"
  
  ,tabBoxTabColor = "rgb(44,62,80)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(24, 188, 156)"
  ,tabBoxTabTextColorSelected = "rgb(255, 255, 255)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgb(255,255,255)"
  ,tabBoxBorderRadius = 10
  
  ### inputs
  ,buttonBackColor = "rgb(44,62,80)"
  ,buttonTextColor = "rgb(255,255,255)"
  ,buttonBorderColor = "rgb(44,62,80)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(30,43,55)"
  ,buttonTextColorHover = "rgb(255,255,255)"
  ,buttonBorderColorHover = "rgb(30,43,55)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(206,212,218)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(255,255,255)"
  ,textboxBorderColorSelect = "rgb(89,126,162)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(236,240,241)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
  
)

### THEME
###Custome Style ####
customTheme <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(33,37,41)"
  ,primaryFontColor = "rgb(245,245,245)"
  ,infoFontColor = "rgb(245,245,245)"
  ,successFontColor = "rgb(33,37,41)"
  ,warningFontColor = "rgb(33,37,41)"
  ,dangerFontColor = "rgb(33,37,41)"
  ,bodyBackColor = "rgb(255,255,255)"
  
  ### header
  ,logoBackColor = "rgb(23,103,124)"
  
  ,headerButtonBackColor = "rgb(238,238,238)"
  ,headerButtonIconColor = "rgb(75,75,75)"
  ,headerButtonBackColorHover = "rgb(210,210,210)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,headerBackColor = "rgb(238,238,238)"
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "2px 2px 2px"
  
  
  ### sidebar
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(20,97,117)"
    ,colorMiddle = "rgb(56,161,187)"
    ,colorEnd = "rgb(3,22,56)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "inherit"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = ""
  ,sidebarShadowColor = "0px 0px 0px"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(255,255,255)"
  ,sidebarSearchIconColor = "rgb(44,62,80)"
  ,sidebarSearchBorderColor = "rgb(255,255,255)"
  
  ,sidebarTabTextColor = "rgb(255,255,255)"
  ,sidebarTabTextSize = 14
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = 0
  
  ,sidebarTabBackColorSelected = "rgb(30,43,55)"
  ,sidebarTabTextColorSelected = "rgb(24,188,156)"
  ,sidebarTabRadiusSelected = "0px"
  
  ,sidebarTabBackColorHover = "rgb(44,62,80)"
  ,sidebarTabTextColorHover = "rgb(24,188,156)"
  ,sidebarTabBorderStyleHover = "none"
  ,sidebarTabBorderColorHover = "none"
  ,sidebarTabBorderWidthHover = 0
  ,sidebarTabRadiusHover = "0px"
  
  ### boxes
  ,boxBackColor = "rgb(233, 241, 245)"
  ,boxBorderRadius = 0
  ,boxShadowSize = "0px 0px 0px"
  ,boxShadowColor = ""
  ,boxTitleSize = 19
  ,boxDefaultColor = "rgb(50,150,175)" #(52,152,219) (30, 43, 55)
  ,boxPrimaryColor = "rgb(44,62,80)"
  ,boxInfoColor = "rgb(52,152,219)"
  ,boxSuccessColor = "rgb(24, 188, 156)"
  ,boxWarningColor = "rgb(243,156,18)"
  ,boxDangerColor = "rgb(231,76,60)"
  
  ,tabBoxTabColor = "rgb(44,62,80)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(24, 188, 156)"
  ,tabBoxTabTextColorSelected = "rgb(255, 255, 255)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgb(255,255,255)"
  ,tabBoxBorderRadius = 10
  
  ### inputs
  ,buttonBackColor = "rgb(44,62,80)"
  ,buttonTextColor = "rgb(255,255,255)"
  ,buttonBorderColor = "rgb(44,62,80)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(30,43,55)"
  ,buttonTextColorHover = "rgb(255,255,255)"
  ,buttonBorderColorHover = "rgb(30,43,55)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(206,212,218)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(255,255,255)"
  ,textboxBorderColorSelect = "rgb(89,126,162)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(236,240,241)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
  
)


source("ui.R")
source("server.R")

shinyApp(ui = ui,server = server)




