library(dplyr)
library(ggplot2)
library(reshape2)
library(ggplot2)
library(stringr)
vera_df = read.csv('incarceration_trends.csv')


### Summary Values
prison_pop_ratio = vera_df %>% 
  select(year, state, total_pop, total_prison_pop) %>%
  filter(year == 2016) %>% 
  na.omit() %>% 
  group_by(state) %>% 
  summarize(prison_pop_to_total_pop_ratio = mean(total_prison_pop / total_pop)) %>% 
  arrange(desc(prison_pop_to_total_pop_ratio))
  
black_prison_pop_change = vera_df %>% 
  select(year, black_prison_pop) %>%
  filter(year >= 2006, year <= 2016) %>% 
  group_by(year) %>% 
  na.omit() %>% 
  summarize(black_prison_pop = sum(black_prison_pop))
black_prison_pop_change = black_prison_pop_change[black_prison_pop_change$year==2016, "black_prison_pop"] - black_prison_pop_change[black_prison_pop$year==2006, "black_prison_pop"]

aapi_prison_high = vera_df %>% 
  select(year, state, aapi_prison_pop) %>%
  filter(year == 2016) %>% 
  na.omit() %>% 
  group_by(state) %>% 
  summarise(aapi_prison_pop = sum(aapi_prison_pop)) %>% 
  filter(aapi_prison_pop == max(aapi_prison_pop)) %>% 
  pull(state)

white_jail_low = vera_df %>% 
  select(year, state, white_jail_pop) %>%
  filter(year == 2016) %>% 
  na.omit() %>% 
  group_by(state) %>% 
  summarise(white_jail_pop = sum(white_jail_pop)) %>% 
  filter(white_jail_pop == min(white_jail_pop)) %>% 
  pull(state)
  
highest_state_prison_pop = vera_df %>% 
  select(year, state, total_prison_pop) %>% 
  filter(year == 2016) %>% 
  na.omit() %>% 
  group_by(state) %>% 
  summarise(total_prison_pop = sum(total_prison_pop)) %>% 
  filter(total_prison_pop == max(total_prison_pop)) %>% 
  pull(state)
  
### Trend Over Time Chart 1
race_jail_trends = vera_df %>% 
  select(year, state, white_prison_pop, aapi_prison_pop, black_prison_pop, latinx_prison_pop,
         other_race_prison_pop) %>% 
  filter(year >= 2006, year <= 2016, state == 'WA') %>% 
  na.omit() %>% 
  group_by(year) %>%
  summarize(white_jail_pop = sum(white_prison_pop), aapi_jail_pop = sum(aapi_prison_pop),
            black_jail_pop = sum(black_prison_pop), latinx_jail_pop = sum(latinx_prison_pop),
            other_race_jail_pop = sum(other_race_prison_pop)) %>% 
  melt(id.vars = 'year', variable.name = 'prison_pop_by_race') %>% rename(percentage = value, bar_chart_by_year = year)

chart1 = race_jail_trends %>% ggplot(aes(y=percentage, x=bar_chart_by_year, fill=prison_pop_by_race)) + 
  geom_bar(position='fill', stat="identity") +
  labs(title = 'Washington State Prison Race Distribution 2006 ~ 2016')

### Variable Comparison Chart 2
top_3_pop_rate = vera_df %>% 
  select(year, state, white_jail_pop_rate, aapi_jail_pop_rate, latinx_jail_pop_rate,
         black_jail_pop_rate, native_jail_pop_rate) %>% 
  filter(state == 'NJ' | state == 'NY' | state == 'MA') %>% 
  filter(year == 2016) %>%
  group_by(state) %>% 
  summarize(white_jail_pop_rate = mean(white_jail_pop_rate),
            aapi_jail_pop_rate =  mean(aapi_jail_pop_rate),
            latinx_jail_pop_rate = mean(latinx_jail_pop_rate),
            black_jail_pop_rate = mean(black_jail_pop_rate),
            native_jail_pop_rate = mean(native_jail_pop_rate))
top_3_pop_rate = rbind(rep(1300, 5), rep(0, 5), top_3_pop_rate)
rownames(top_3_pop_rate) = top_3_pop_rate$state

colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )

chart2 = top_3_pop_rate %>% select(-state) %>% radarchart(axistype=4 , 
    pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1, lty=3,
    cglcol="grey", cglty=3, axislabcol="grey", caxislabels=seq(0,1300,5), cglwd=1.0,
    vlcex=0.8, title='2016 Jail Population Rate by Race in MA, NJ, & NY')
legend(x=0.7, y=1, legend = c('MA', 'NJ', 'NY'), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)

### Map Chart 3
gis_county_data = map_data('county') %>% 
  rename(county_name = subregion)

county_2016_df = vera_df %>% 
  filter(year == 2016) %>% 
  select(state, county_name, total_prison_pop, black_prison_pop) %>% 
  na.omit() %>% 
  summarize(state, county_name, ratio = black_prison_pop/total_prison_pop)


county_2016_df$county_name = str_remove_all(county_2016_df$county_name, ' County') %>% 
  str_to_lower()

county_2016_df = left_join(gis_county_data, county_2016_df, by='county_name')

chart3 = ggplot() +
  geom_polygon(county_2016_df, mapping = aes(x=long, y=lat, group=group, fill=ratio), color='black', size=.1) +
  scale_fill_continuous(low='white', high='red') +
  labs(title = '2016 U.S. County Prisons Black Pop to Total Pop Ratio', fill = 'Ratio')
