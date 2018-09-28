library(dplyr)
library(shiny)
library(ggplot2)
library(plotly)
library(tidyr)
library(readr)

variablelist = c("AIDS estimated deaths (UNAIDS estimates) " = "aids", 
                 #"Current health expenditure (% of GDP)" = "health.expen.gdp", 
                 #"Current health expenditure per capita (current US$)" = "health.expen.capita",
                 "Death rate, crude (per 1,000 people)" = "death.rate",
                 #"Domestic general government health expenditure (% of current health expenditure)" = "domestic.expen.curr.health.expen",
                 #"Domestic general government health expenditure (% of GDP)" = "domestic.expen.gdp",
                 #"Domestic general government health expenditure per capita (current US$)" = "domestic.expen.capita",
                 #"External health expenditure (% of current health expenditure)" = "external.expen.curr.health.expen",
                 #"External health expenditure per capita (current US$)" = "external.expen.capita",
                 "Life expectancy at birth, total (years)" = "life.expect.total",
                 "Life expectancy at birth, female (years)" = "life.expect.female",
                 "Life expectancy at birth, male (years)" = "life.expect.male",
                 "Lifetime risk of maternal death (%)" = "risk.maternal.death",
                 "Mortality rate, adult, female (per 1,000 female adults)" = "mortality.rate.female",
                 "Mortality rate, adult, male (per 1,000 male adults)" = "mortality.rate.male",
                 "Mortality rate, infant (per 1,000 live births)" = "mortality.rate.infant",
                 "Number of people who are undernourished" = "undernourished",
                 "Out-of-pocket expenditure per capita (current US$)" = "out.of.pocket.expen"
)


dataframevariables = c("AIDS estimated deaths (UNAIDS estimates)",
                       "Current health expenditure (% of GDP)",
                       "Current health expenditure per capita (current US$)",
                       "Death rate, crude (per 1,000 people)",
                       "Domestic general government health \n expenditure (% of current health expenditure)",
                       "Domestic general government health \n expenditure (% of GDP)",
                       "Domestic general government health \n expenditure per capita (current US$)",
                       "External health expenditure \n(% of current health expenditure)",
                       "External health expenditure per capita (current US$)",
                       "Life expectancy at birth, total (years)",
                       "Life expectancy at birth, female (years)",
                       "Life expectancy at birth, male (years)",
                       "Lifetime risk of maternal death (%)",
                       "Mortality rate, adult, female (per 1,000 female adults)",
                       "Mortality rate, adult, male (per 1,000 male adults)",
                       "Mortality rate, infant (per 1,000 live births)",
                       "Number of people who are undernourished",
                       "Out-of-pocket expenditure per capita (current US$)")

names(dataframevariables) = c("aids",
                       "health.expen.gdp",
                       "health.expen.capita",
                       "death.rate",
                       "domestic.expen.curr.health.expen",
                       "domestic.expen.gdp",
                       "domestic.expen.capita",
                       "external.expen.curr.health.expen",
                       "external.expen.capita",
                       "life.expect.total",
                       "life.expect.female",
                       "life.expect.male",
                       "risk.maternal.death",
                       "mortality.rate.female",
                       "mortality.rate.male",
                       "mortality.rate.infant",
                       "undernourished",
                       "out.of.pocket.expen")


#Merge gdp per capita data
gdppercapita <- read_delim("gdppercapita.csv", "\t", escape_double = FALSE, trim_ws = TRUE)

cleanedgdppercapita = gdppercapita %>%
  select(`Country Name`, '2000','2001','2002','2003','2004','2005','2006',
         '2007','2008','2009','2010','2011','2012','2013',
         '2014','2015') %>%
  rename(Country = `Country Name`)

cleanedgdppercapita = cleanedgdppercapita[complete.cases(cleanedgdppercapita), ]


#Select Data Based on GDP Per Capita

selectedDataPerCapita = function(healthvariable){
  
  varstring = deparse(substitute(healthvariable))

  meltedHealthVariable = gather(healthvariable, Year, healthvariable, `2000`:`2015`)
  #remove 2016/2017
  meltedHealthVariable$`2016` = NULL
  meltedHealthVariable$`2017` = NULL
  
  healthexpcapita = gather(health.expen.capita, Year, healthexpenditurepercapita, `2000`:`2015`)
  gdppercapita = gather(cleanedgdppercapita, Year, gdppercapita, `2000`:`2015`)
  #remove 2016/2017
  gdppercapita$`2016` = NULL
  gdppercapita$`2017` = NULL
  
  population = gather(pop.total, Year, population, `2000`:`2015`)
  #remove 2016/2017
  population$`2016` = NULL
  population$`2017` = NULL
  
  mergeHealthCapita = merge(meltedHealthVariable,  healthexpcapita, by = c("Year", "Country"), all.x = TRUE)
  
  mergeHealthCapitaGDP = merge(mergeHealthCapita, gdppercapita, by = c("Year", "Country"), all.x = TRUE)
  finaldata = merge(mergeHealthCapitaGDP, population, by = c("Year", "Country"), all.x = TRUE)
  
  #Categorize countries by income level
  finaldata$category[finaldata$gdppercapita < 1005] = "Low-income"
  finaldata$category[finaldata$gdppercapita <= 3955 & finaldata$gdppercapita >= 1006] = "Lower-middle income"
  finaldata$category[finaldata$gdppercapita <= 12235 & finaldata$gdppercapita >= 3956] = "Upper-middle income"
  finaldata$category[finaldata$gdppercapita > 12235] = "High-income"
  
  return(na.omit(finaldata))
}


#Select data based on percentage of GDP

selectedDataGDP = function(healthvariable){
  
  varstring = deparse(substitute(healthvariable))
  
  meltedHealthVariable = gather(healthvariable, Year, healthvariable, `2000`:`2015`)
  #remove 2016/2017
  meltedHealthVariable$`2016` = NULL
  meltedHealthVariable$`2017` = NULL
  
  healthexpgdp = gather(health.expen.gdp, Year, healthexpendituregdp, `2000`:`2015`)
  gdppercapita = gather(cleanedgdppercapita, Year, gdppercapita, `2000`:`2015`)
  #remove 2016/2017
  gdppercapita$`2016` = NULL
  gdppercapita$`2017` = NULL
  
  population = gather(pop.total, Year, population, `2000`:`2015`)
  #remove 2016/2017
  population$`2016` = NULL
  population$`2017` = NULL
  
  mergeHealthCapita = merge(meltedHealthVariable,  healthexpgdp, by = c("Year", "Country"), all.x = TRUE)
  
  mergeHealthCapitaGDP = merge(mergeHealthCapita, gdppercapita, by = c("Year", "Country"), all.x = TRUE)
  finaldata = merge(mergeHealthCapitaGDP, population, by = c("Year", "Country"), all.x = TRUE)
  
  #Categorize countries by income level
  finaldata$category[finaldata$gdppercapita < 1005] = "Low-income"
  finaldata$category[finaldata$gdppercapita <= 3955 & finaldata$gdppercapita >= 1006] = "Lower-middle income"
  finaldata$category[finaldata$gdppercapita <= 12235 & finaldata$gdppercapita >= 3956] = "Upper-middle income"
  finaldata$category[finaldata$gdppercapita > 12235] = "High-income"
  
  return(na.omit(finaldata))
}




# Define UI for application
ui = fluidPage(
  
  # Application title
  titlePanel("Global Health Expenditures and Outcomes"),
  
  sidebarPanel(
    selectInput("variable", "Choose a variable:",
                choices = variablelist)
  ),
  mainPanel(
    plotlyOutput("healthyearplotpercapita"),
    plotlyOutput("healthyearplotgdp")
  )
)

server = function(input, output) {
  
  output$healthyearplotpercapita = renderPlotly({
    graphdata = selectedDataPerCapita(eval(parse(text=input$variable)))
    plot_ly(graphdata, x = ~healthexpenditurepercapita, y = ~healthvariable, color = ~category,
            text = ~Country, frame = ~Year,mode = 'markers', type = 'scatter', ids = ~Country
    ) %>%
      layout(legend = list(x = 100, y = 0.9) ,xaxis = list(title = "Current health expenditure per capita (current US$)"),
             yaxis = list(title = dataframevariables[input$variable], titlefont = list(size = 12))) 
  })
  
  output$healthyearplotgdp = renderPlotly({
    graphdata = selectedDataGDP(eval(parse(text=input$variable)))
    plot_ly(graphdata, x = ~healthexpendituregdp, y = ~healthvariable, color = ~category,
            text = ~Country, frame = ~Year,mode = 'markers', type = 'scatter', ids = ~Country
    ) %>%
      layout(legend = list(x = 100, y = 0.9) ,xaxis = list(title = "Current health expenditure (% of GDP)"),
             yaxis = list(title = dataframevariables[input$variable], titlefont = list(size = 12)))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
