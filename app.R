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
                 

                
                    
yearlist = c("2000" = 2000, "2001" = 2001, "2002" = 2002, "2003" = 2003,
             "2004" = 2004, "2005" = 2005, "2006" = 2006, "2007" = 2007,
             "2008" = 2008, "2009" = 2009, "2010" = 2010, "2011" = 2011,
             "2012" = 2012, "2013" = 2013, "2014" = 2014, "2015" = 2015)


library(dplyr)
library(shiny)


selectedData = function(year, healthvariable) {
  healthexpenditureyear = health.expen.capita %>%
    select(Country, year) %>%
    rename(healthexpenditureyear = year)
  
  healthvariableyear = healthvariable %>%
    select(Country, year) %>%
    rename(healthvariableyear = year)

  mergeddata <- merge(healthexpenditureyear, healthvariableyear, by ="Country" , all.x = TRUE)

  namergedata = na.omit(mergeddata)
}


# Define UI for application
ui = fluidPage(
   
   # Application title
   titlePanel("Global Health Data"),
   
     sidebarPanel(
       selectInput("variable", "Choose a variable:",
                   choices = variablelist),
       sliderInput("year", "Year: ", min = 2000, max = 2015, ticks = FALSE, sep = "", step = 1, value = 2000)
     ),
     mainPanel(
       plotOutput("healthyearplot")
     )
   )

server = function(input, output) {
   output$healthyearplot = renderPlot({
     graphdata = selectedData(toString(input$year), eval(parse(text=input$variable)))
     ggplot(graphdata,aes(x = healthexpenditureyear, y = healthvariableyear)) + geom_point()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)



