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


install.packages('plotly')
library(plotly)
library(ggplot2)
# Question 8

# Summary of lbs
summary(lbs_df$`Lbs. Sold`)

# Histogram for lbs sold
ggplot(data=lbs_df, aes(`Lbs. Sold`))+geom_histogram(bins = 20)


# Linear Model for Profit vs Lbs Sold
profit_model <- lm(Profit ~ `Lbs. Sold`+`Inquiries`, data = financial_df)
summary(profit_model)

# If Lbs Sold more than overall mean, denote as success
# Mean of Lbs. Sold
lbs_mean <- mean(financial_df$`Lbs. Sold`)
# Impute success/fail column
for (i in 1:nrow(financial_df)) {
  if (financial_df$`Lbs. Sold`[i] > 1.5*lbs_mean) {
    financial_df$succ[i] <- 1
  }
  else{
    financial_df$succ[i] <- 0
  }
}
# logistic model for lbs sold
lbs_mod <- glm(succ ~ Inquiries + Profit, `Lbs. Sold`, data = financial_df, family = 'binomial')
summary(lbs_mod)


financial_df <- merge(financial_df, weekly_visits_df$Visits)

names(financial_df)[names(financial_df) == "y"] <- "visits"


succ_mod <- glm(succ ~ Revenue + visits, data = financial_df, family = 'binomial', maxit = 100)
summary(succ_mod)

exp(-1.201e-18) 


# Scatter for Rev vs Visits
ggplot(data = financial_df, aes(Revenue, `Lbs. Sold`)) + geom_point() + geom_smooth(method = "lm", se=FALSE, formula = y ~ x)
rev_lbs_cor <- cor(financial_df$Revenue,financial_df$`Lbs. Sold`)

# Linear Model
rev_mod <- lm(`Lbs. Sold` ~ Revenue, data = financial_df)
summary(rev_mod)


#Question 5 and 6
#call ggplot2 for visualisation
library(ggplot2)

#Linear relationship between revenue and Lbs. Sold 
#as well as Revenue and Visits and their correlation
scatter_corr <- function(x,Revenue){
  
  #Relationship between x and y
  plot(x, Revenue)
  abline(lm(Revenue ~ x))
  
  #Correlation between x and y
  corr <- cor(x,Revenue)
  print(corr)
}

#call the scatter_corr function
scatter_corr(financial_df$`Lbs. Sold`, financial_df$Revenue)
scatter_corr(weekly_visits_df$Visits, financial_df$Revenue)

#install GGally to show the correlation matrix of Financials
install.packages('GGally')
library(GGally)

#visual correlation in Financials
#With focus on Revenue and Lbs. Sold
ggcorr(financial_df)


#summary values for Lbs. Sold Worksheet
summary(lbs_df)


#Question 8a,b,c
#Histogram for Lbs. Sold
boxplot(lbs_df$`Lbs. Sold`, horizontal = TRUE)
hist(lbs_df$`Lbs. Sold`, col = 'gray')

####Answer: Although the histogram appears to be bellshaped
#it is not symmetrical. Implying it has outliers to the right
