# Define server logic required to draw a histogram

server <- function(input, output) {
    
    # Value Box
    output$vbox1 <- renderValueBox({ valueBox(subtitle = "Total Cases", 
                                              color = "red",
                                              value = tags$p(number(sum(covid_update$Case, na.rm = T), accuracy = 1, big.mark = ","), style = "font-size: 80%;"),
                                              icon = icon("virus"), 
                                              width = 15)})
    output$vbox2 <- renderValueBox({ valueBox(subtitle = "Total Vaccine Doses", 
                                              color = "green",
                                              value = tags$p(number(sum(covid_update$Recover, na.rm = T), accuracy = 1, big.mark = ","), style = "font-size: 80%;"),
                                              icon = icon("plus-square"),
                                              width = 15)})
    output$vbox3 <- renderValueBox({ valueBox(subtitle = "Total Deaths", 
                                              color = "black",
                                              value = tags$p(number(sum(covid_update$Death, na.rm = T), accuracy = 1, big.mark = ","), style = "font-size: 80%;"),
                                              icon = icon("book-dead"),
                                              width = 15)})
    
    
    
    # Tree map
    output$tree_map <- renderPlotly({
        
        df_tree_death <- covid %>% 
            drop_na(Case, Recover, Death, Active) %>% 
            group_by(Country) %>% summarise(Death = max(Death))
        
        df_tree_case <- covid %>% 
            drop_na(Case, Recover, Death, Active) %>% 
            group_by(Country) %>% summarise(Case = max(Case))
        
        df_tree_recover <- covid %>% 
            drop_na(Case, Recover, Death, Active) %>% 
            group_by(Country) %>% summarise(Recover = max(Recover))
        
        #df_tree_active <- covid %>% 
        #    drop_na(Case, Recover, Death, Active) %>% 
        #    group_by(Country) %>% summarise(Active = max(Case)-(max(Death)+max(Recover)))
        
        
        plot_tree <- plot_ly(
            data = df_tree_case ,
            type= "treemap",
            values = ~Case,
            labels= ~Country,
            parents=  "Confirmed Case",
            domain = list(column=0),
            name = "Confirmed Cases",
            textinfo="label+value+percent parent"
        ) %>%
            plotly::add_trace(
                data = df_tree_recover,
                type= "treemap",
                values = ~Recover,
                labels= ~ Country,
                parents=  "Recovered",
                domain = list(column=1),
                name = "Recovered",
                textinfo="label+value+percent parent"
            ) %>%
            plotly::add_trace(
                data = df_tree_death,
                type= "treemap",
                values = ~Death,
                labels= ~ Country,
                parents=  "Death",
                domain = list(column=2),
                name = "Death",
                textinfo="label+value+percent parent") %>%
            layout(plot_bgcolor='transparent', paper_bgcolor='transparent', 
                   modebar=list(bgcolor='transparent', color='blue', activecolor='green')) %>%
            plotly::layout(grid=list(columns=3, rows=1))
    })
    
    
    
    
    # Stat
    # Bar Chart
    output$p1 <- renderPlotly({
        
        p1 <- covid %>%
            filter(Date == max(input$DatesMerge2), Country %in% c(input$country1, input$country2)) %>%
            group_by(Country) %>%
            summarise(Case = max(Case), Death = max(Death), Recover = max(Recover), Active = max(Case)-(max(Death)+max(Recover)))
        p1 <- melt(data = p1, id.vars = c("Country"), measure.vars = c("Case", "Death", "Active", "Recover"))
        
        p1 <- ggplot(p1) +
              geom_bar(aes(x = variable, y = value, fill = Country), 
                     stat="identity", position = "dodge", width = 0.7) +
            labs(
                 x = NULL,
                 y = "Number of Cases",
                 color = NULL
            ) 
        

        ggplotly(p1, tooltip = "text")
    })
    
    output$p2 <- renderPlotly({
        
        p2 <- covid %>%
            select(Date, Country, rlang::sym(input$Case_Type2)) %>%
            filter(Date <= input$DatesMerge2, Country %in% c(input$country1, input$country2)) %>% 
            ggplot(aes(Date, !!rlang::sym(input$Case_Type2), color = Country, group = Country,
                       
                       text = glue("Country : {Country}
                                     Date : {Date}
                                     ")
            )) +
            scale_y_continuous(labels = number_format(big.mark = ",")) +
            scale_x_date(date_breaks = "1 month",
                         labels = date_format(format = "%b")
            ) +
            geom_line() +
            labs(
                 x = NULL,
                 y = "Number of Cases",
                 color = NULL
            ) 
        
        ggplotly(p2, tooltip = "text")
    })
    
    # Leaflet
    output$Map <- renderLeaflet({
        
        T <- rlang::sym('Case')

        
        p2 <- covid %>% 
            filter(Date == max(Date)) %>% 
            group_by(Country) %>% 
            summarise(T = sum(!!T))
        
        p3 <- covid %>% 
            filter(Date == max(Date)) %>% 
            group_by(Country) %>% 
            summarise(T = sum(!!T))
        
        
        world_json@data$T <- p2$T[match(world_json@data$ADMIN,p2$Country)]
        
        
        pal <- colorNumeric(
            palette = "Blues",
            domain = p2$T, n=9)
        
        qpal <- colorQuantile("Blues", p3$T, n = 9)
        
        content_popup <- paste(sep = " ",
                               "Country :", world_json@data$ADMIN,"<br>",  
                               paste(sep = " ", 'Case', ":"), world_json@data$T) 
        
        leaflet(world_json) %>%
            addPolygons(stroke = FALSE, 
                        smoothFactor = 0.2, 
                        fillOpacity = 1,
                        color = ~qpal(world_json@data$T), 
                        label = paste0("Country: ", world_json@data$ADMIN),
                        popup = content_popup) %>%
            addLegend("topleft", 
                      pal = pal,
                      values = p2$T,
                      title = paste(sep = " ", 'Cases', ":"),
                      opacity = 1)
    })
    
    
    
    # Full Data
    output$data_table <- DT::renderDataTable({
        
        DT::datatable(covid, 
                      options = list(scrollX = T))
    })
    
    # Download
    output$download <- downloadHandler( 
        filename = function() {paste("data-", Sys.Date(), ".csv", sep="")},
        content = function(file) {
            write.csv(covid, file, row.names = F)
        })
    
    
    
    
    ####### PAGE TOP-N COUNTRY ############as
    output$top_country_plot <- renderPlot({
      
      T <- rlang::sym(input$radio_type_top)
      
      if (input$radio_type_top=="Case"){
          corona_per_region_top <- covid %>%  filter(Date == max(Date)) %>% group_by(Country) %>% summarise(Case = sum(!!T)) %>% arrange(-Case)
      }
      
      else if (input$radio_type_top=="Vaccinated"){
          corona_per_region_top <- covid %>%  filter(Date == max(Date)) %>% group_by(Country) %>% summarise(Vaccinated = sum(Recover)) %>% arrange(-Vaccinated)
      }
      
      else{
          corona_per_region_top <- covid %>%  filter(Date == max(Date)) %>% group_by(Country) %>% summarise(Death = sum(!!T)) %>% arrange(-Death)
      }
      
      if (input$radio_asc_top=="Descending"){
        corona_per_region_top <- corona_per_region_top %>% head(input$bins_top)
        plot_2 <- corona_per_region_top %>%
          ggplot(mapping = aes(x = reorder(Country,get(input$radio_type_top)), y = get(input$radio_type_top)))
      }
      else{
        corona_per_region_top <- corona_per_region_top %>% tail(input$bins_top)
        plot_2 <- corona_per_region_top %>%
          ggplot(mapping = aes(x = reorder(Country,-get(input$radio_type_top)), y = get(input$radio_type_top)))
        
      }
      
      
      #plot_2 <- corona_per_region_top %>%
      plot_2 <- plot_2+geom_col(aes(fill=get(input$radio_type_top))) +
        coord_flip() +
        geom_label(aes(label=comma(get(input$radio_type_top))),size=3,colour="black")+ #,nudge_y = 4 nudge_y = 0.1
        scale_fill_gradient(low = ifelse(input$radio_type_top == "Case", "#004b7a", ifelse(input$radio_type_top == "Vaccinated", "orange", ifelse(input$radio_type_top == "Death", "#00542d", "#590b0b"))),
                            high = ifelse(input$radio_type_top == "Case", "#004b7a", ifelse(input$radio_type_top == "Vaccinated", "orange", ifelse(input$radio_type_top == "Death", "#00542d", "#590b0b"))))+ #004b7a
        
        #scale_fill_manual("red")+
        #scale_color_brewer()+
        #scale_color_viridis_d()+
        
        labs(title = glue("Top {input$bins_top} Country where Corona Virus Spread"),
             caption = "22 Jan to 20 Dec 2020",
             x="", y="",
             fill="Total Case")+
        scale_y_continuous(labels = scales::comma)+
        
        theme(legend.position = "none" )
      
      plot_2
      
      
      
    })
    
    ####### PAGE TREND ############
    output$trend_map <- renderPlotly({
      
      #validate(
      #  need(input$Country != "", "Please fill at least one country.")
      #)
      C <- melt(data = covid %>% rename(Vaccinated=Recover), id.vars = c("Country",'Date'), measure.vars = c("Case", "Death", "Vaccinated"))
      
      C$Month_Yr <- as.yearmon(C$Date)
      
      C <- C %>% filter(Country %in% input$Country) %>% group_by(Month_Yr,variable) %>% slice(which.max(value))
      
      
      
      plot_2 <- C %>% ggplot(aes(x=Date, y=value,group=variable,text = glue("Total {str_to_title(variable)} in {Month_Yr} : <b>{comma(value)}</b>")))+
        geom_line(lwd=0.4,aes(col=variable))+ #colour="steelblue"+
        #theme(axis.text.x = element_text(angle=90,hjust = 1))+
        #scale_x_date(date_labels = "%b")+
        labs(x = "",
             y = "")+
        scale_color_manual(values=c("#fabc20", "#c41d00", "#1d6604"))+ #("#999999", "#E69F00", "#56B4E9")
        scale_y_continuous(labels = scales::comma)+
        #scale_x_datetime(date_labels = "%m %Y") +
        theme(text = element_text(family = "Arial"),
              strip.background = element_blank(),
              panel.border = element_blank(),
              panel.background = element_rect(fill = "#e9f1f5",
                                              colour = "#e9f1f5",
                                              size = 0.5, linetype = "dotted"),
              legend.title = element_blank()
              #legend.box.margin = margin(116, 6, 6, 6)
              #legend.position="None"
              
              #legend.position = c(1.95, 2.95)
              #legend.position="bottom", legend.box = "horizontal"
              #legend.position = "left"
              #legend.background = element_rect(colour = "red",fill="red")
              
        )
      
      #+theme_replace()   #theme_stata()
      
      ggplotly <- ggplotly(plot_2,tooltip="text") %>%  config(displayModeBar = T)
      
      
    })
    
    
    output$col_plot_1 <- renderPlot({
      C <- melt(data = covid %>% rename(Vaccinated=Recover), id.vars = c("Country",'Date'), measure.vars = c("Case", "Death", "Vaccinated"))
      
      C$Month_Yr <- as.yearmon(C$Date)
      
      C <- C %>% filter(Country %in% input$Country) %>% group_by(Month_Yr,variable) %>% slice(which.max(value))
      
      C$variable <- factor(C$variable, levels = c("Case","Vaccinated", "Death")) #reorder label
      
      plot_2 <-  C %>% group_by(variable) %>% summarise(jumlah=sum(value)) %>%
        
        ggplot( mapping = aes(x = variable, y = jumlah)) +
        # geom_jitter(aes(size = jumlah),
        #             col = "black",
        #             alpha = 0.2) + # transparency
        geom_col(mapping = aes(fill = variable )) +
        theme_minimal() + # mengatur tema
        theme(legend.position = "none") + # mengatur tema
        labs(x="",
             y="",
             caption = "") +
        geom_label(aes(label=comma(jumlah)),size=3)+
        scale_fill_manual(values=c("#fabc20", "#1d6604","#c41d00"))+
        scale_y_continuous(labels = scales::comma)
      #scale_fill_brewer(palette = "Set1")
      
      plot_2
      
      
      
    })
    
}