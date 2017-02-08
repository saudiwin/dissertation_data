# Create Tunisia-specific charts and plots from World Bank data

require(readr)
require(tidyr)
require(dplyr)
require(ggplot2)

all_data <- read_csv(file = 'world_bank/tunisia_all_wb.csv',skip=4)
all_data <- all_data %>% gather(year,measurement,`1960`:`2016`) %>% 
  select(-X62,-`Country Code`) %>% rename(indicator_name=`Indicator Name`,
                                          country_name=`Country Name`,
                                          indicator_code=`Indicator Code`)

count_miss <- all_data %>% group_by(indicator_name) %>% summarize(num_avail=sum(!is.na(measurement)))

all_data %>% filter(indicator_name=="Bank capital to assets ratio (%)",!is.na(measurement)) %>% 
  ggplot(aes(x=as.numeric(year),y=measurement)) + geom_line() + theme_minimal() + xlab("") + 
  ylab('Percentage Non-performing Loans')

all_data %>% filter(indicator_name=='Bank nonperforming loans to total gross loans (%)',!is.na(measurement)) %>% 
  ggplot(aes(x=as.numeric(year),y=measurement)) + geom_line() + theme_minimal() + xlab("") + 
  ylab('Percentage Non-performing Loans to Good Loans')

all_data %>% filter(indicator_name=='Borrowers from commercial banks (per 1,000 adults)',!is.na(measurement)) %>% 
  ggplot(aes(x=as.numeric(year),y=measurement)) + geom_line() + theme_minimal() + xlab("") + 
  ylab('% Borrowers from Commercial Banks')

#Central government debt, total (% of GDP)

all_data %>% filter(indicator_name=='Central government debt, total (% of GDP)',!is.na(measurement)) %>% 
  ggplot(aes(x=as.numeric(year),y=measurement)) + geom_line() + theme_minimal() + xlab("") + 
  ylab('% GDP of Government Debt')

#Cereal yield (kg per hectare)

all_data %>% filter(indicator_name=='Cereal yield (kg per hectare)',!is.na(measurement)) %>% 
  ggplot(aes(x=as.numeric(year),y=measurement)) + geom_line() + theme_minimal() + xlab("") + 
  ylab('Cereal yield (kg per hectare)') + stat_smooth()

#"Cost of business start-up procedures (% of GNI per capita)" 14

all_data %>% filter(indicator_name=='Cost of business start-up procedures (% of GNI per capita)',!is.na(measurement)) %>% 
  ggplot(aes(x=as.numeric(year),y=measurement)) + geom_line() + theme_minimal() + xlab("") + 
  ylab('Cost of business start-up procedures (% of GNI per capita')

#"Cost to export (US$ per container)" 10 & "Cost to import (US$ per container)" 10

all_data %>% filter(indicator_name %in% c('Cost to export (US$ per container)',
                                          'Cost to import (US$ per container)'),!is.na(measurement)) %>% 
  ggplot(aes(x=as.numeric(year),y=measurement,colour=indicator_name)) + geom_line() + theme_minimal() + xlab("") + 
  ylab('US $ per container')

#Crop production index (2004-2006 = 100)

all_data %>% filter(indicator_name=='Crop production index (2004-2006 = 100)',!is.na(measurement)) %>% 
  ggplot(aes(x=as.numeric(year),y=measurement)) + geom_line() + theme_minimal() + xlab("") + 
  ylab('Crop production index (2004-2006 = 100)') + stat_smooth()

#Domestic credit provided by financial sector (% of GDP)

all_data %>% filter(indicator_name=='Domestic credit provided by financial sector (% of GDP)',!is.na(measurement)) %>% 
  ggplot(aes(x=as.numeric(year),y=measurement)) + geom_line() + theme_minimal() + xlab("") + 
  ylab('Domestic Credit % GDP') + stat_smooth()

#Employment in agriculture (% of total employment)

all_data %>% filter(indicator_name=='Employment in agriculture (% of total employment)',!is.na(measurement)) %>% 
  ggplot(aes(x=as.numeric(year),y=measurement)) + geom_line() + theme_minimal() + xlab("") + 
  ylab('Employment in agriculture (% of total employment)')

#Add in crop production

all_data %>% filter(indicator_name %in% c('Employment in agriculture (% of total employment)',
                                          'Cereal yield (kg per hectare)'),!is.na(measurement)) %>% 
  ggplot(aes(x=as.numeric(year),y=measurement)) + geom_line(colour='blue') + theme_minimal() + xlab("") + 
  facet_wrap(~indicator_name,scales='free_y') + xlim(1980,2016) + ylab("")

#Look at employment in industry, services & ag together

all_data %>% filter(indicator_name %in% c('Employment in agriculture (% of total employment)',
                                          'Employment in services (% of total employment)',
                                          'Employment in industry (% of total employment)'),!is.na(measurement)) %>% 
  mutate(indicator_name=factor(indicator_name,labels=c('Agriculture','Industry','Services'))) %>% 
  ggplot(aes(x=as.numeric(year),y=measurement)) + geom_line(colour='blue') + theme_minimal() + xlab("") + 
  facet_wrap(~indicator_name) + xlim(1980,2016) + ylab("Percent Employed")