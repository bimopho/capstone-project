library(shiny)
library(tidyverse)
library(countrycode)

server <- function(input, output, session) {
  
  ##### 1. Preparing dataset #####
  # Gunakan reactive, karena reactive akan otomatis melakukan caching

happinessIndex_df2015 <- read_csv('2015.csv') 
happinessIndex_df2015$iso_code <- countrycode(sourcevar = happinessIndex_df2015$`Country`, origin = 'country.name', destination = 'iso3c')

  
happinessIndex_df2016 <- read_csv('2016.csv') 
happinessIndex_df2016$iso_code <- countrycode(sourcevar = happinessIndex_df2016$`Country`, origin = 'country.name', destination = 'iso3c')

  
happinessIndex_df2017 <- read_csv('2017.csv') 
happinessIndex_df2017$iso_code <- countrycode(sourcevar = happinessIndex_df2017$`Country`, origin = 'country.name', destination = 'iso3c')

  

happinessIndex_df2018 <- read_csv('2018.csv') 
happinessIndex_df2018$iso_code <- countrycode(sourcevar = happinessIndex_df2018$`Country or region`, origin = 'country.name', destination = 'iso3c')

  

happinessIndex_df <- read_csv('2019.csv') 
happinessIndex_df$iso_code <- countrycode(sourcevar = happinessIndex_df$`Country or region`, origin = 'country.name', destination = 'iso3c')

  ##### rendering output #####
  output$max_gdp_per_capita <- renderText({
    x <- happinessIndex_df %>% slice_max(`GDP per capita`)
    
    format(x$`Country or region`, nsmall=0, big.mark=".")
  })
  
  output$max_healthy_life_expectancy <- renderText({
    x <- happinessIndex_df %>% slice_max(`Healthy life expectancy`)
    
    format(x$`Country or region`, nsmall=0, big.mark=".")
  })
  
  output$max_social_support <- renderText({
    x <- happinessIndex_df %>% slice_max(`Social support`)
    
    format(x$`Country or region`, nsmall=0, big.mark=".")
  })
  
  output$max_freedom <- renderText({
    x <- happinessIndex_df %>% slice_max(`Freedom to make life choices`)
    
    format(x$`Country or region`, nsmall=0, big.mark=".")
  })
  
  output$total_covid_cases <- renderText({
    format(sum(latest_covid_df()$total_cases, na.rm = T), nsmall=0, big.mark=".")
  })
  
  output$total_covid_death <- renderText({
    format(sum(latest_covid_df()$total_deaths, na.rm = T), nsmall=0, big.mark=".")
  })
  
  output$sum_people_fully_vaccinated <- renderText({
    x <- vaccinations_df() %>% 
      drop_na(people_fully_vaccinated) %>% 
      group_by(location) %>% 
      filter(date == max(date)) %>% 
      summarize(sum_people_fully_vaccinated = sum(people_fully_vaccinated)) %>%
      # group_by() %>%
      summarize(sum_people_fully_vaccinated = sum(sum_people_fully_vaccinated))
    
    format(x$sum_people_fully_vaccinated, nsmall=0, big.mark=".")
  })
  
  output$sum_total_vaccinations <- renderText({
    x <- vaccinations_df() %>% 
      drop_na(total_vaccinations) %>% 
      group_by(location) %>% 
      filter(date == max(date)) %>% 
      summarize(sum_total_vaccinations = sum(total_vaccinations)) %>% 
      # group_by() %>% 
      summarize(sum_total_vaccinations = sum(sum_total_vaccinations))
    
    format(x$sum_total_vaccinations, nsmall=0, big.mark=".")
  })
  
  output$slider_year <- renderUI({
    data <- vaccinations_df() %>%
      drop_na(total_vaccinations)
    
    sliderInput(
      "slider_year", 
      "", 
      min=min(data$date), 
      max=max(data$date), 
      value=max(data$date), 
      animate=animationOptions(loop = TRUE, interval = 1000)
    )
  })
  
  output$slider_n <- renderUI({
    data <- happinessIndex_df
      
    sliderInput(
      "slider_n", 
      "", 
      min=min(2015), 
      max=max(2019), 
      value=max(2019), 
      animate=animationOptions(loop = TRUE, interval = 1000)
    )
  })
  
  output$plot_overall_rank <- renderPlotly({
    if (input$slider_n == 2019){
      data <- happinessIndex_df %>% 
        arrange(`Overall rank`) %>%
        select(`Country or region`, `Score`) %>%
        slice_head(n = 10) %>%
        plot_ly(
          x = ~`Country or region`, 
          y = ~Score,
          text = ~Score,
          mode = "lines+markers"
        ) %>% 
        layout(
          height = 300,
          paper_bgcolor = '#222327', 
          plot_bgcolor = '#222327', 
          font = list(
            color = '#bdbdbd'
          )
        ) %>% 
        
        highlight(on = "plotly_click", off = "plotly_doubleclick")
    } else if(input$slider_n == 2018){
      data <- happinessIndex_df2018() %>% 
        arrange(`Overall rank`) %>%
        select(`Country or region`, `Score`) %>%
        slice_head(n = 10) %>%
        plot_ly(
          x = ~`Country or region`, 
          y = ~Score,
          text = ~Score,
          mode = "lines+markers"
        ) %>% 
        layout(
          height = 300,
          paper_bgcolor = '#222327', 
          plot_bgcolor = '#222327', 
          font = list(
            color = '#bdbdbd'
          )
        ) %>% 
        
        highlight(on = "plotly_click", off = "plotly_doubleclick")
    } else if(input$slider_n == 2017){
      data <- happinessIndex_df2017() %>% 
        arrange(`Happiness.Rank`) %>%
        select(`Country`, `Happiness.Score`) %>%
        slice_head(n = 10) %>%
        plot_ly(
          x = ~`Country`, 
          y = ~`Happiness.Score`,
          text = ~`Happiness.Score`,
          mode = "lines+markers"
        ) %>% 
        layout(
          height = 300,
          paper_bgcolor = '#222327', 
          plot_bgcolor = '#222327', 
          font = list(
            color = '#bdbdbd'
          )
        ) %>% 
        
        highlight(on = "plotly_click", off = "plotly_doubleclick")
    } else if(input$slider_n == 2016){
      data <- happinessIndex_df2016() %>% 
        arrange(`Happiness Rank`) %>%
        select(`Country`, `Happiness Score`) %>%
        slice_head(n = 10) %>%
        plot_ly(
          x = ~`Country`, 
          y = ~`Happiness Score`,
          text = ~`Happiness Score`,
          mode = "lines+markers"
        ) %>% 
        layout(
          height = 300,
          paper_bgcolor = '#222327', 
          plot_bgcolor = '#222327', 
          font = list(
            color = '#bdbdbd'
          )
        ) %>% 
        
        highlight(on = "plotly_click", off = "plotly_doubleclick")
    } else if (input$slider_n == 2015){
      data <- happinessIndex_df2015() %>% 
        arrange(`Happiness Rank`) %>%
        select(`Country`, `Happiness Score`) %>%
        slice_head(n = 10) %>%
        plot_ly(
          x = ~`Country`, 
          y = ~`Happiness Score`,
          text = ~`Happiness Score`,
          mode = "lines+markers"
        ) %>% 
        layout(
          height = 300,
          paper_bgcolor = '#222327', 
          plot_bgcolor = '#222327', 
          font = list(
            color = '#bdbdbd'
          )
        ) %>% 
        
        highlight(on = "plotly_click", off = "plotly_doubleclick")
    }
  })
  
  output$peta_happiness <- renderPlotly({
    if (input$slider_n == 2019){
      data <- happinessIndex_df
      
      # filter(vaccines == event_data("plotly_click", source="vaccines")$x) %>%
      
      data$hover <- with(
        data, paste(
          `Country or region`, '<br>', 
          "Overall rank", `Overall rank`, "<br>",
          "Score", Score, "<br>",
          "GDP per capita", `GDP per capita`, "<br>"
          # "Vaccines", vaccines
        ))
      
      data %>% highlight_key(~iso_code) %>%
        plot_ly(
          source="peta_happiness", 
          type='choropleth', 
          locations=~iso_code, 
          z=~`Overall rank`, 
          # text=~country, 
          text=~`Country or region`,
          colorscale="Viridis"
        ) %>% 
        # https://plotly.com/python/templates/
        layout(
          paper_bgcolor = '#27293d', 
          font = list(
            color = '#bdbdbd'
          ),
          geo = list(
            showframe = FALSE,
            showcoastlines = FALSE,
            bgcolor = '#27293d'
          )
        ) %>%
        highlight(on = "plotly_click", off = "plotly_doubleclick")
    } else if (input$slider_n == 2018){
      data <- happinessIndex_df2018
      
      # filter(vaccines == event_data("plotly_click", source="vaccines")$x) %>%
      
      data$hover <- with(
        data, paste(
          `Country or region`, '<br>', 
          "Overall rank", `Overall rank`, "<br>",
          "Score", Score, "<br>",
          "GDP per capita", `GDP per capita`, "<br>"
          # "Vaccines", vaccines
        ))
      
      data %>% highlight_key(~iso_code) %>%
        plot_ly(
          source="peta_happiness", 
          type='choropleth', 
          locations=~iso_code, 
          z=~`Overall rank`, 
          # text=~country, 
          text=~`Country or region`,
          colorscale="Viridis"
        ) %>% 
        # https://plotly.com/python/templates/
        layout(
          paper_bgcolor = '#27293d', 
          font = list(
            color = '#bdbdbd'
          ),
          geo = list(
            showframe = FALSE,
            showcoastlines = FALSE,
            bgcolor = '#27293d'
          )
        ) %>%
        highlight(on = "plotly_click", off = "plotly_doubleclick")
    }else if(input$slider_n == 2017){
      data <- happinessIndex_df2017
      
      # filter(vaccines == event_data("plotly_click", source="vaccines")$x) %>%
      
      data$hover <- with(
        data, paste(
          `Country`, '<br>', 
          "Happiness.Rank", `Happiness.Rank`, "<br>",
          "Happiness.Score", Happiness.Score, "<br>",
          "GDP per capita", `Economy..GDP.per.Capita.`, "<br>"
          # "Vaccines", vaccines
        ))
      
      data %>% highlight_key(~iso_code) %>%
        plot_ly(
          source="peta_happiness", 
          type='choropleth', 
          locations=~iso_code, 
          z=~`Happiness.Rank`, 
          # text=~country, 
          text=~`Country`,
          colorscale="Viridis"
        ) %>% 
        # https://plotly.com/python/templates/
        layout(
          paper_bgcolor = '#27293d', 
          font = list(
            color = '#bdbdbd'
          ),
          geo = list(
            showframe = FALSE,
            showcoastlines = FALSE,
            bgcolor = '#27293d'
          )
        ) %>%
        highlight(on = "plotly_click", off = "plotly_doubleclick")
    } else if(input$slider_n == 2016){
      data <- happinessIndex_df2016
      
      # filter(vaccines == event_data("plotly_click", source="vaccines")$x) %>%
      
      data$hover <- with(
        data, paste(
          `Country`, '<br>', 
          "Overall rank", `Happiness Rank`, "<br>",
          "Score", `Happiness Score`, "<br>",
          "GDP per capita", `Economy (GDP per Capita)`, "<br>"
          # "Vaccines", vaccines
        ))
      
      data %>% highlight_key(~iso_code) %>%
        plot_ly(
          source="peta_happiness", 
          type='choropleth', 
          locations=~iso_code, 
          z=~`Happiness Rank`, 
          # text=~country, 
          text=~`Country`,
          colorscale="Viridis"
        ) %>% 
        # https://plotly.com/python/templates/
        layout(
          paper_bgcolor = '#27293d', 
          font = list(
            color = '#bdbdbd'
          ),
          geo = list(
            showframe = FALSE,
            showcoastlines = FALSE,
            bgcolor = '#27293d'
          )
        ) %>%
        highlight(on = "plotly_click", off = "plotly_doubleclick")
    } else if (input$slider_n == 2015){
      data <- happinessIndex_df2015
      
      # filter(vaccines == event_data("plotly_click", source="vaccines")$x) %>%
      
      data$hover <- with(
        data, paste(
          `Country`, '<br>', 
          "Overall rank", `Happiness Rank`, "<br>",
          "Score", `Happiness Score`, "<br>",
          "GDP per capita", `Economy (GDP per Capita)`, "<br>"
          # "Vaccines", vaccines
        ))
      
      data %>% highlight_key(~iso_code) %>%
        plot_ly(
          source="peta_happiness", 
          type='choropleth', 
          locations=~iso_code, 
          z=~`Overall rank`, 
          # text=~country, 
          text=~`Country or region`,
          colorscale="Viridis"
        ) %>% 
        # https://plotly.com/python/templates/
        layout(
          paper_bgcolor = '#27293d', 
          font = list(
            color = '#bdbdbd'
          ),
          geo = list(
            showframe = FALSE,
            showcoastlines = FALSE,
            bgcolor = '#27293d'
          )
        ) %>%
        highlight(on = "plotly_click", off = "plotly_doubleclick")
    }
  })
  
  output$bar_happiness <- renderPlotly({
    data <- happinessIndex_df %>% 
      arrange(`Overall rank`) %>%
      select(`Country or region`, `Score`) %>%
      slice_head(n = 10) %>%
      plot_ly(
        x = ~`Country or region`, 
        y = ~Score,
        title = "Happiness Score",
        text = ~Score,
        mode = "lines+markers"
      ) %>% 
      layout(
        height = 300,
        paper_bgcolor = '#222327', 
        plot_bgcolor = '#222327', 
        font = list(
          color = '#bdbdbd'
        )
      ) %>% 
      
      highlight(on = "plotly_click", off = "plotly_doubleclick")
  })
  
  output$sum_progress_people_vaccinated_daily <- renderPlotly({
    vaccinations_df %>%
      drop_na(people_vaccinated) %>%
      group_by(date) %>%
      summarize(sum_people_vaccinated = sum(people_vaccinated)) %>%
      highlight_key(~date) %>%
      plot_ly(
        x = ~date, 
        y = ~sum_people_vaccinated,
        mode = "lines+markers"
      ) %>% 
      layout(
        height = 200,
        paper_bgcolor = '#222327', 
        plot_bgcolor = '#222327', 
        font = list(
          color = '#bdbdbd'
        )
      ) %>% 
      # add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines+markers')  %>% 
      highlight(on = "plotly_click", off = "plotly_doubleclick")
  })
  
}