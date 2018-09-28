variablelist = c("AIDS estimated deaths (UNAIDS estimates) " = "aids", 
                 "Current health expenditure (% of GDP)" = "health.expen.gdp", 
                 "Current health expenditure per capita (current US$)" = "health.expen.capita",
                 "Death rate, crude (per 1,000 people)" = "death.rate",
                 "Domestic general government health expenditure (% of current health expenditure)" = "domestic.expen.curr.health.expen",
                 "Domestic general government health expenditure (% of GDP)" = "domestic.expen.gdp",
                 "Domestic general government health expenditure per capita (current US$)" = "domestic.expen.capita",
                 "External health expenditure (% of current health expenditure)" = "external.expen.curr.health.expen",
                 "External health expenditure per capita (current US$)" = "external.expen.capita",
                 "Life expectancy at birth, female (years)" = "life.expect.female",
                 "Life expectancy at birth, male (years)" = "life.expect.male",
                 "Lifetime risk of maternal death (%)" = "risk.maternal.death",
                 "Mortality rate, adult, female (per 1,000 female adults)" = "mortality.rate.female",
                 "Mortality rate, adult, male (per 1,000 male adults)" = "mortality.rate.male",
                 "Mortality rate, infant (per 1,000 live births)" = "mortality.rate.infant",
                 "Number of people who are undernourished" = "undernourished",
                 "Out-of-pocket expenditure per capita (current US$)" = "out.of.pocket.expen"
)

library(dplyr)
library(shiny)
library(ggplot2)
library(plotly)
library(tidyr)
library(readr)


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



#test20 = selectedDataPerCapita(aids)


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
  titlePanel("Global Health Data"),
  
  sidebarPanel(
    selectInput("variable", "Choose a variable:",
                choices = variablelist)
    # sliderInput("year", "Year: ", min = 2000, max = 2015, ticks = FALSE, sep = "", step = 1, value = 2000,
    #             animate = animationOptions(interval = 1500, loop = TRUE))
  ),
  mainPanel(
    plotlyOutput("healthyearplotpercapita"),
    plotlyOutput("healthyearplotgdp")
  )
)

server = function(input, output) {
  
  # output$healthyearplotpercapita = renderPlotly({
  #   graphdata = selectedDataPerCapita(toString(input$year), eval(parse(text=input$variable)))
  #   p = ggplot(graphdata,aes(x = healthexpenditureyear, y = healthvariableyear, color = category)) + geom_point(aes(size = populationvariable)) +
  #     scale_size_continuous(range = c(1, 10)) +
  #     xlab("Current health expenditure per capita (current US$)") +
  #     ylab(toString(input$variable))
  #   ggplotly(p) %>% layout(height = 800, width = 800)
  #   
  # })
  
  # output$healthyearplotpercapita = renderPlotly({
  #   graphdata = selectedDataPerCapita(toString(input$year), eval(parse(text=input$variable)))
  #   plot_ly(graphdata, x = ~healthexpenditureyear, y = ~healthvariableyear, color = ~category,
  #           text = ~Country)%>%
  #     layout(legend = list(x = 100, y = 0.9), xaxis = list(title = "Current health expenditure per capita (current US$)"))
  #   
  # })
  
  # output$healthyearplotgdp = renderPlotly({
  #   graphdata = selectedDataGDP(toString(input$year), eval(parse(text=input$variable)))
  #   q = ggplot(graphdata,aes(x = healthexpenditureyear, y = healthvariableyear, color = category)) + geom_point(aes(size = populationvariable)) +
  #     scale_size_continuous(range = c(1, 10)) +
  #     xlab("Current health expenditure (% of GDP)") +
  #     ylab(toString(input$variable))
  #   ggplotly(q)
  # 
  # 
  # })
  
  output$healthyearplotpercapita = renderPlotly({
    #print(eval(parse(text=input$variable)))
    print(typeof(input$variable))
    graphdata = selectedDataPerCapita(eval(parse(text=input$variable)))
    #print(graphdata)
    plot_ly(graphdata, x = ~healthexpenditurepercapita, y = ~healthvariable, color = ~category,
            text = ~Country, frame = ~Year,mode = 'markers', type = 'scatter'
    ) %>%
      layout(legend = list(x = 100, y = 0.9) ,xaxis = list(title = "Current health expenditure per capita (current US$)"),
             yaxis = list(title = input$variable)) 
  })
  
  output$healthyearplotgdp = renderPlotly({
    graphdata = selectedDataGDP(eval(parse(text=input$variable)))
    plot_ly(graphdata, x = ~healthexpendituregdp, y = ~healthvariable, color = ~category,
            text = ~Country, frame = ~Year,mode = 'markers', type = 'scatter'
    ) %>%
      layout(legend = list(x = 100, y = 0.9) ,xaxis = list(title = "Current health expenditure (% of GDP)"),
             yaxis = list(title = input$variable))
  })
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
