library(zoo, warn.conflicts = FALSE)
unloadNamespace('zoo')
library(MASS)

read.org <- function(org) {
  data(activity)
  startdate <- '2012-04-30'
  psfk <- subset(activity, Organization == org & Date >= startdate)
  psfk$Organization <- NULL
  data.frame(date = psfk[-1,'Date'],
             organization = org,
             zoo::rollapply(psfk[-1], 2, function(x) x[2]-x[1]))
}

read.four.orgs <- function() rbind(read.org('PSFK.com'),
                                   read.org('Daily Bruin'),
                                   read.org('The State Press'),
                                   read.org('The Duke Chronicle'))

features <- function(df) {
  df$weekend <- weekdays(df$date) == 'Saturday' | weekdays(df$date) == 'Sunday'

  data(seasons)
  for (i in rownames(seasons)) {
    selector <- df$date >= seasons[i,'start'] & df$date <= seasons[i,'end']
    df[selector,'summer'] <- seasons[i,'summer']
    df[selector,'season'] <- i
  }
 #df$season <- factor(df$season) 

  df
}
