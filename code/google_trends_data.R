
### Google Trends data collection using gtrendsR
# https://cran.r-project.org/web/packages/gtrendsR/gtrendsR.pdf 

## Keyword example: "locuri de munca anglia"
# ----------------------------------------------

install.packages('gtrendsR')
library(gtrendsR)
library(reshape2)

#chosen keyword(s)
keyword = c("locuri de munca anglia")

#chosen geographic area: RO = Romania
geo_area = c('RO')

#chosen time period: 2012 - 2021
time_period = ("2012-01-01 2021-06-08")

#preferred channel: web
channel='web'

# Use gtrendsR to collect the data
Sys.setenv(LANG = "en", TZ = 'GMT')
trend_data = gtrends(keyword, gprop = channel, geo = geo_area, time = time_period)
trend_over_time = trend_data$interest_over_time
head(trend_over_time)

plot <- ggplot(data = trend_over_time, aes(x = date, y = hits, group = keyword, col = keyword))+
          geom_smooth(span = 0.5,se = FALSE) + xlab('Time') + ylab('Relative Interest') + 
          theme_bw() + theme(legend.title = element_blank(), legend.position = "bottom",
            legend.text = element_text(size = 12)) + ggtitle("Google Search Volume")
plot

# Set each year individually
year_2012 = "2012-01-01 2012-12-31"
year_2013 = "2013-01-01 2013-12-31"
year_2014 = "2014-01-01 2014-12-31"
year_2015 = "2015-01-01 2015-12-31"
year_2016 = "2016-01-01 2016-12-31"
year_2017 = "2017-01-01 2017-12-31"
year_2018 = "2018-01-01 2018-12-31"
year_2019 = "2019-01-01 2019-12-31"
year_2020 = "2020-01-01 2020-12-31"
year_2021 = "2021-01-01 2021-06-08"
year_all  = "2012-01-01 2021-06-08"

years = c(year_2012, year_2013, year_2014, year_2015, year_2016, year_2017, year_2018, year_2019, year_2020, year_2021, year_all)
years_names = c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021" "all")

# Download the trend over time for the keyword and save to a CSV
counter = 0
for(word in keyword){
  counter = counter + 1
  print(paste(counter, length(keyword), word))
  dir.create(word)
  for( i in c(1:length(years))){
    year = years[i]
    df = gtrends(word, time = year, geo = "RO")
    write.csv2(df$interest_over_time, file = paste(word, "/", word, "_interest_over_time_", years_names[i], ".csv", sep = ""))
  }
}