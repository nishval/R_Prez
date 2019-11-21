# Quality Alloys Business Case

# import the excel
library(readxl)
# weekly visits
weekly_visits_df <- read_excel("Web Analytics Case Student Spreadsheet.xls", 
                                                     sheet = "Weekly Visits", range = "A5:H71")
View(weekly_visits_df)
# financials
financial_df <- read_excel("Web Analytics Case Student Spreadsheet.xls", 
                                                     sheet = "Financials", range = "A5:E71")
View(financial_df)
# lbs
lbs_df <- read_excel("Web Analytics Case Student Spreadsheet.xls", 
                                                     sheet = "Lbs. Sold", range = "A5:B295")
View(lbs_df)


# Checking the data for cleaning and massaging
summary(weekly_visits_df)

summary(financial_df)

summary(lbs_df)

# change the format for week in lbs_df to char from date format
lbs_df$Week <- as.character(lbs_df$Week)


# data looks good, no null values

#data can be split into its time periods
initial <- weekly_visits_df$`Week (2008-2009)`[1:14]
pre_promo <- weekly_visits_df$`Week (2008-2009)`[15:35]
promo <- weekly_visits_df$`Week (2008-2009)`[36:52]
post_promo <- weekly_visits_df$`Week (2008-2009)`[53:66]


# Descriptive statistics for each variable
# Mean, Median, Std_dev

stats <- function(x){
  mean <- mean(x)
  median <- median(x)
  std <- sd(x)
  max <- max(x)
  min <- min(x)
  lst <- c(mean,median,std,max,min)
  out <- cat('Mean: ',mean, 'Median: ',median, 'SD: ',std, 'Max: ',max, 'Min: ',min)
  return(out)
}


# Promotion analysis
# column chart with the intervals
bar <-ggplot(financial_df, aes(`Week (2008-2009)`, Revenue))
bar + stat_summary(fun.y = mean, geom = "bar") + labs(x = "Week", y = "Revenue") + theme(axis.text.x = element_text(angle = 90)) + scale_x_discrete(breaks= as.numeric(levels(financial_df$`Week (2008-2009)`)))

# revenue over different periods
int_rev = 0
for (i in 1:14){
  int_rev = int_rev + financial_df$Revenue[i]
}
print(int_rev)


pre_rev = 0
for (i in 15:35){
  pre_rev = pre_rev + financial_df$Revenue[i]
}
print(pre_rev)

pro_rev = 0
for (i in 36:52){
  pro_rev = pro_rev + financial_df$Revenue[i]
}
print(pro_rev)


post_rev = 0
for (i in 53:66){
  post_rev = post_rev + financial_df$Revenue[i]
}
print(post_rev)

lst <- c(int_rev,pre_rev,pro_rev,post_rev)

pie(lst, labels = names(lst) <- c('Int','Pre','Promo','Post'))
