# Data cleaning for Case Study 2
library(dplyr)
rawdata <- read.csv("Data_Extract_From_Health_Nutrition_and_Population_Statistics.csv")
rawdata <- rawdata %>% select(-c(X1967, X1968, X1969, X1970, X1971, X1972, X1982, 
                                 X1983, X1984, X1985, X1986, X1987, X1990, X1991, X1992, X1993,
                                 X1994, X1995, X1996, X1997, X1998, X1999))

# AIDS estimated deaths (UNAIDS estimates): 
# Remove years with no data -> Remove countries with no data
aids <- rawdata %>% 
  filter(Series.Name == 'AIDS estimated deaths (UNAIDS estimates)') %>%
  select(-c(Series.Name))
aids <- aids[complete.cases(aids), ]
names(aids) <- c('Country', 2000:2017)
save(aids, file = "AIDS-death.RData")

# Current health expenditure (% of GDP)
# Remove years with no data -> Remove countries with no data
health.expen.gdp <- rawdata %>% 
  filter(Series.Name == 'Current health expenditure (% of GDP)') %>%
  select(-c(Series.Name, X2016, X2017))
health.expen.gdp <- health.expen.gdp[complete.cases(health.expen.gdp), ]
names(health.expen.gdp) <- c('Country', 2000:2015)
save(health.expen.gdp, file = "health-expenditure-gdp.RData")

# Current health expenditure per capita (current US$) 
# Remove years with no data -> Remove countries with no data
health.expen.capita <- rawdata %>% 
  filter(Series.Name == 'Current health expenditure per capita (current US$)') %>%
  select(-c(Series.Name, X2016, X2017))
health.expen.capita <- health.expen.capita[complete.cases(health.expen.capita), ]
names(health.expen.capita) <- c('Country', 2000:2015)
save(health.expen.capita, file = "health-expenditure-capita.RData")

# Death rate, crude (per 1,000 people)
# Remove years with no data -> Remove countries with no data
death.rate <- rawdata %>% 
  filter(Series.Name == 'Death rate, crude (per 1,000 people)') %>%
  select(-c(Series.Name, X2017))
death.rate <- death.rate[complete.cases(death.rate), ]
names(death.rate) <- c('Country', 2000:2016)
save(death.rate, file = "death-rate.RData")

# Domestic general government health expenditure (% of current health expenditure)
domestic.expen.curr.health.expen <- rawdata %>% 
  filter(Series.Name == 'Domestic general government health expenditure (% of current health expenditure)') %>%
  select(-c(Series.Name, X2016, X2017))
domestic.expen.curr.health.expen <- domestic.expen.curr.health.expen[complete.cases(domestic.expen.curr.health.expen), ]
names(domestic.expen.curr.health.expen) <- c('Country', 2000:2015)
save(domestic.expen.curr.health.expen, file = 'domestic-expen-%curr-health-expen.RData')

# Domestic general government health expenditure (% of GDP)
domestic.expen.gdp <- rawdata %>% 
  filter(Series.Name == 'Domestic general government health expenditure (% of GDP)') %>%
  select(-c(Series.Name, X2016, X2017))
domestic.expen.gdp <- domestic.expen.gdp[complete.cases(domestic.expen.gdp), ]
names(domestic.expen.gdp) <- c('Country', 2000:2015)
save(domestic.expen.gdp, file = 'domestic-expen-%gdp.RData')

# Domestic general government health expenditure per capita (current US$)
domestic.expen.capita <- rawdata %>% 
  filter(Series.Name == 'Domestic general government health expenditure per capita (current US$)') %>% 
  select(-c(Series.Name, X2016, X2017))
domestic.expen.capita <- domestic.expen.capita[complete.cases(domestic.expen.capita), ]
names(domestic.expen.capita) <- c('Country', 2000:2015)
save(domestic.expen.capita, file = 'domestic-expen-capita.RData')

# External health expenditure (% of current health expenditure)
external.expen.curr.health.expen <- rawdata %>% 
  filter(Series.Name == 'External health expenditure (% of current health expenditure)') %>% 
  select(-c(Series.Name, X2016, X2017))
external.expen.curr.health.expen <- external.expen.curr.health.expen[complete.cases(external.expen.curr.health.expen), ]
names(external.expen.curr.health.expen) <- c('Country', 2000:2015)
save(external.expen.curr.health.expen, file = 'external-expen-%curr-health-expen.RData')

# External health expenditure per capita (current US$) 
external.expen.capita <- rawdata %>% 
  filter(Series.Name == 'External health expenditure per capita (current US$)') %>% 
  select(-c(Series.Name, X2016, X2017))
external.expen.capita <- external.expen.capita[complete.cases(external.expen.capita), ]
names(external.expen.capita) <- c('Country', 2000:2015)
save(external.expen.capita, file = 'external-expen-capita.RData')

# Life expectancy at birth, female (years)  
life.expect.female <- rawdata %>% 
  filter(Series.Name == 'Life expectancy at birth, female (years)') %>% 
  select(-c(Series.Name, X2017))
life.expect.female <- life.expect.female[complete.cases(life.expect.female), ]
names(life.expect.female) <- c('Country', 2000:2016)
save(life.expect.female, file = 'life-expectancy-female.RData')

# Life expectancy at birth, male (years)
life.expect.male <- rawdata %>% 
  filter(Series.Name == 'Life expectancy at birth, male (years)') %>% 
  select(-c(Series.Name, X2017))
life.expect.male <- life.expect.male[complete.cases(life.expect.male), ]
names(life.expect.male) <- c('Country', 2000:2016)
save(life.expect.male, file = 'life-expectancy-male.RData')

# Lifetime risk of maternal death (%)
risk.maternal.death <- rawdata %>% 
  filter(Series.Name == 'Lifetime risk of maternal death (%)') %>% 
  select(-c(Series.Name, X2016, X2017))
risk.maternal.death <- risk.maternal.death[complete.cases(risk.maternal.death), ]
names(risk.maternal.death) <- c('Country', 2000:2015)
save(risk.maternal.death, file = 'risk-maternal-death.RData')

# Mortality rate, adult, female (per 1,000 female adults)
mortality.rate.female <- rawdata %>% 
  filter(Series.Name == 'Mortality rate, adult, female (per 1,000 female adults)') %>% 
  select(-c(Series.Name, X2017))
mortality.rate.female <- mortality.rate.female[complete.cases(mortality.rate.female), ]
names(mortality.rate.female) <- c('Country', 2000:2016)
save(mortality.rate.female, file = 'mortality-rate-female.RData')

# Mortality rate, adult, male (per 1,000 male adults)
mortality.rate.male <- rawdata %>% 
  filter(Series.Name == 'Mortality rate, adult, male (per 1,000 male adults)') %>% 
  select(-c(Series.Name, X2017))
mortality.rate.male <- mortality.rate.male[complete.cases(mortality.rate.male), ]
names(mortality.rate.male) <- c('Country', 2000:2016)
save(mortality.rate.male, file = 'mortality-rate-male.RData')

# Mortality rate, infant (per 1,000 live births)
mortality.rate.infant <- rawdata %>% 
  filter(Series.Name == 'Mortality rate, infant (per 1,000 live births)') %>% 
  select(-(Series.Name))
mortality.rate.infant <- mortality.rate.infant[complete.cases(mortality.rate.infant), ]
names(mortality.rate.infant) <- c('Country', 2000:2017)
save(mortality.rate.infant, file = 'mortality-rate-infant.RData')

# Number of people who are undernourished
undernourished <- rawdata %>% 
  filter(Series.Name == 'Number of people who are undernourished') %>% 
  select(-c(Series.Name, X2016, X2017))
undernourished <- undernourished[complete.cases(undernourished), ]
names(undernourished) <- c('Country', 2000:2015)
save(undernourished, file = 'undernourished.RData')

# Out-of-pocket expenditure per capita (current US$) 
out.of.pocket.expen <- rawdata %>% 
  filter(Series.Name == 'Out-of-pocket expenditure per capita (current US$)') %>% 
  select(-c(Series.Name, X2016, X2017))
out.of.pocket.expen <- out.of.pocket.expen[complete.cases(out.of.pocket.expen),]
names(out.of.pocket.expen) <- c('Country', 2000:2015)
save(out.of.pocket.expen, file = 'out-of-pocket-expen.RData')

# Population, total
pop.total <- rawdata %>% 
  filter(Series.Name == 'Population, total') %>% 
  select(-c(Series.Name))
names(pop.total) <- c('Country', 2000:2017)
save(pop.total, file = 'pop-total.RData')

# Life Expectancy total
life.expect.total <- read.csv("Life Expectancy Total.csv")
names(life.expect.total) <- c('Country', 2000:2016)
save(life.expect.total, file = 'life.expect.total.RData')
  
# Private Expenditure 
private.expend <- read.csv("Private expenditure.csv")
private.expend <- private.expend[complete.cases(private.expend),]
names(private.expend) <- c('Country', 2000:2015)
save(private.expend, file = 'private.expend.RData')


