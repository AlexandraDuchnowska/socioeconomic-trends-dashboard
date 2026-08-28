packages <- c(
  "shiny", "plotly", "dplyr", "readxl", 
  "scales", "ggplot2", "tidyr", "reshape2"
)
library(shiny)
library(plotly)
library(dplyr)
library(readxl)
library(scales)
library(ggplot2)
library(tidyr)
library(reshape2)
####################

data_path <- "your path"
data <- read_excel(data_path)

data$Year <- as.numeric(data$Year)

indicators <- c(
  "Life expectancy at birth, total (years)",
  "GDP per capita (constant 2015 US$)",
  "Fertility rate, total (births per woman)",
  "Mortality rate, infant (per 1,000 live births)",
  "Current health expenditure per capita (current US$)",
  "School enrollment, secondary (% gross)",
  "School enrollment, tertiary (% gross)",
  "Carbon dioxide (CO2) emissions excluding LULUCF per capita (t CO2e/capita)",
  "People using at least basic sanitation services (% of population)"
)

for(col in indicators){
  if(col %in% names(data)){
    data[[col]] <- as.numeric(data[[col]])
  }
}

##########
ui <- fluidPage(
  titlePanel("Socioeconomic and Environmental Trends (2000–2023): A Panel Data Analysis of Selected Countries"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "countries", "Select Countries:",
        choices = unique(data$Country),
        selected = c("Switzerland", "Poland", "China"),
        multiple = TRUE
      ),
      sliderInput(
        "year_range", "Select Year Range:",
        min = 2000, max = 2023, value = c(2010, 2020), sep = ""
      ),
      checkboxInput("log_scale", "Log scale for GDP axis", value = TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Life Expectancy Over Time", plotlyOutput("lifePlot")),
        tabPanel("Animated GDP vs Life Expectancy Bubble", plotlyOutput("bubblePlot")),
        tabPanel("Animated Education vs GDP Bubble", plotlyOutput("eduGDPPlot")),
        tabPanel("Health Expenditure vs Life Expectancy", plotlyOutput("healthPlot")),
        tabPanel("GDP vs CO₂ Emissions", plotlyOutput("co2Plot")),
        tabPanel("Fertility vs Infant Mortality", plotlyOutput("fertilityPlot")),
        tabPanel("Secondary Education vs Life Expectancy", plotlyOutput("secondaryPlot")),
        tabPanel("Tertiary Education vs Life Expectancy", plotlyOutput("educationPlot")),
        tabPanel("Sanitation vs Life Expectancy", plotlyOutput("sanitationPlot")),
        tabPanel("Correlation Heatmap", plotlyOutput("corrPlot"))
      )
    )
  )
)

###########
server <- function(input, output) {
  
  filtered_data <- reactive({
    data %>%
      filter(
        Country %in% input$countries,
        Year >= input$year_range[1],
        Year <= input$year_range[2]
      )
  })
  
#Life Expectancy Over Time
  output$lifePlot <- renderPlotly({
    df <- filtered_data()
    p <- ggplot(df, aes(x = Year, y = `Life expectancy at birth, total (years)`, color = Country)) +
      geom_line(size = 1.2) + geom_point(size = 1.8) +
      labs(title = "Life Expectancy Over Time", x = "Year", y = "Life Expectancy (years)") +
      theme_minimal(base_size = 14)
    ggplotly(p)
  })
  
#Animated GDP vs Life Expectancy Bubble
  output$bubblePlot <- renderPlotly({
    df <- filtered_data()
    plot_ly(
      df,
      x = ~`GDP per capita (constant 2015 US$)`,
      y = ~`Life expectancy at birth, total (years)`,
      size = ~`Current health expenditure per capita (current US$)`,
      color = ~Country,
      frame = ~Year,
      text = ~paste(
        "Country:", Country,
        "<br>Year:", Year,
        "<br>GDP per capita:", round(`GDP per capita (constant 2015 US$)`),
        "<br>Life Expectancy:", round(`Life expectancy at birth, total (years)`, 1)
      ),
      hoverinfo = "text",
      type = "scatter",
      mode = "markers",
      sizes = c(10, 60)
    ) %>%
      layout(
        title = "Animated GDP vs Life Expectancy (2000–2023)",
        xaxis = list(title = "GDP per capita (constant 2015 US$)", type = ifelse(input$log_scale, "log", "linear")),
        yaxis = list(title = "Life Expectancy (years)"),
        showlegend = TRUE
      )
  })
  
#Animated Education vs GDP Bubble
  output$eduGDPPlot <- renderPlotly({
    df <- filtered_data()
    plot_ly(
      df,
      x = ~`School enrollment, secondary (% gross)`,
      y = ~`GDP per capita (constant 2015 US$)`,
      size = ~`Current health expenditure per capita (current US$)`,
      color = ~Country,
      frame = ~Year,
      text = ~paste(
        "Country:", Country,
        "<br>Year:", Year,
        "<br>Secondary Education (%):", round(`School enrollment, secondary (% gross)`, 1),
        "<br>GDP per capita:", round(`GDP per capita (constant 2015 US$)`),
        "<br>Health Exp.:", round(`Current health expenditure per capita (current US$)`)
      ),
      hoverinfo = "text",
      type = "scatter",
      mode = "markers",
      sizes = c(10, 60)
    ) %>%
      layout(
        title = "Animated Secondary Education vs GDP (2000–2023)",
        xaxis = list(title = "Secondary Education Enrollment (%)"),
        yaxis = list(title = "GDP per capita (constant 2015 US$)", type = ifelse(input$log_scale, "log", "linear")),
        showlegend = TRUE
      )
  })
  
#Health Expenditure vs Life Expectancy
  output$healthPlot <- renderPlotly({
    df <- filtered_data()
    p <- ggplot(df, aes(
      x = `Current health expenditure per capita (current US$)`,
      y = `Life expectancy at birth, total (years)`,
      color = Country
    )) +
      geom_point(alpha = 0.8) +
      labs(x = "Health expenditure per capita (US$)", y = "Life Expectancy", title = "Health Expenditure vs Life Expectancy") +
      theme_minimal()
    ggplotly(p)
  })
  
#GDP vs CO₂ Emissions
  output$co2Plot <- renderPlotly({
    df <- filtered_data()
    p <- ggplot(df, aes(
      x = `GDP per capita (constant 2015 US$)`,
      y = `Carbon dioxide (CO2) emissions excluding LULUCF per capita (t CO2e/capita)`,
      color = Country
    )) +
      geom_point(alpha = 0.8) +
      labs(title = "GDP vs CO₂ Emissions", x = "GDP per capita", y = "CO₂ emissions per capita") +
      theme_minimal()
    if (input$log_scale) p <- p + scale_x_log10(labels = comma)
    ggplotly(p)
  })
  
#Fertility vs Infant Mortality
  output$fertilityPlot <- renderPlotly({
    df <- filtered_data()
    p <- ggplot(df, aes(
      x = `Fertility rate, total (births per woman)`,
      y = `Mortality rate, infant (per 1,000 live births)`,
      color = Country
    )) +
      geom_point(alpha = 0.7) +
      geom_smooth(se = FALSE, method = "lm", color = "gray70", size = 0.5) +
      labs(title = "Fertility vs Infant Mortality") +
      theme_minimal()
    ggplotly(p)
  })
  
#Secondary Education vs Life Expectancy
  output$secondaryPlot <- renderPlotly({
    df <- filtered_data()
    p <- ggplot(df, aes(
      x = `School enrollment, secondary (% gross)`,
      y = `Life expectancy at birth, total (years)`,
      color = Country
    )) +
      geom_point(alpha = 0.8) +
      geom_smooth(se = FALSE, method = "lm", color = "gray70", size = 0.5) +
      labs(title = "Secondary Education vs Life Expectancy", x = "Secondary Enrollment (%)", y = "Life Expectancy (years)") +
      theme_minimal()
    ggplotly(p)
  })
  
#Tertiary Education vs Life Expectancy
  output$educationPlot <- renderPlotly({
    df <- filtered_data()
    p <- ggplot(df, aes(
      x = `School enrollment, tertiary (% gross)`,
      y = `Life expectancy at birth, total (years)`,
      color = Country
    )) +
      geom_point(alpha = 0.8) +
      labs(title = "Tertiary Education vs Life Expectancy", x = "Tertiary Enrollment (%)", y = "Life Expectancy") +
      theme_minimal()
    ggplotly(p)
  })
  
#Sanitation vs Life Expectancy
  output$sanitationPlot <- renderPlotly({
    df <- filtered_data()
    p <- ggplot(df, aes(
      x = `People using at least basic sanitation services (% of population)`,
      y = `Life expectancy at birth, total (years)`,
      color = Country
    )) +
      geom_point(alpha = 0.8) +
      labs(title = "Sanitation vs Life Expectancy", x = "Sanitation Services (%)", y = "Life Expectancy") +
      theme_minimal()
    ggplotly(p)
  })
  
#Correlation Heatmap
  output$corrPlot <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0)
    cols <- indicators[indicators %in% names(df)]
    df_num <- df %>% select(all_of(cols))
    if (ncol(df_num) < 2) {
      return(plotly_empty() %>% layout(title = "Not enough data for correlation heatmap"))
    }
    
    corr_matrix <- cor(df_num, use = "pairwise.complete.obs")
    corr_matrix[is.na(corr_matrix)] <- 0
    
    hover_text <- matrix("", nrow = nrow(corr_matrix), ncol = ncol(corr_matrix))
    rown <- rownames(corr_matrix); coln <- colnames(corr_matrix)
    for (i in seq_len(nrow(corr_matrix))) {
      for (j in seq_len(ncol(corr_matrix))) {
        hover_text[i, j] <- paste0(
          "<b>", rown[i], "</b> vs <b>", coln[j], "</b><br>",
          "Correlation: ", sprintf("%.2f", corr_matrix[i, j])
        )
      }
    }
    
    plot_ly(
      x = coln, y = rown, z = corr_matrix,
      text = hover_text, hoverinfo = "text",
      type = "heatmap",
      colorscale = list(list(0, "blue"), list(0.5, "white"), list(1, "red")),
      zmin = -1, zmax = 1,
      colorbar = list(
        title = list(text = "<b>Correlation</b>", font = list(size = 14)),
        tickvals = seq(-1, 1, 0.5),
        ticktext = sprintf("%.1f", seq(-1, 1, 0.5)),
        tickfont = list(size = 12),
        len = 0.8,
        thickness = 25,
        x = 1.05
      )
    ) %>%
      layout(
        title = list(text = "Correlation Heatmap of Indicators", font = list(size = 13)),
        xaxis = list(tickangle = -45, tickfont = list(size = 8)),
        yaxis = list(autorange = "reversed", tickfont = list(size = 8))
      )
  })
  
}

#####################################
shinyApp(ui = ui, server = server)  #
#####################################