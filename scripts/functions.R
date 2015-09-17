
# names this as a code chunk so I can run it from knitr
## @knitr variablesXY

#require("car")
require("rmarkdown")
require("knitr")
require("grid")
require("xlsx")
require("tframe")
require("tframePlus")
require("lubridate")
require("stringr")
require("scales")
require("zoo")
require("xts")
if (!require(seasonal)) {
  install.packages("seasonal")
  require(seasonal)
}
Sys.setenv(X13_PATH = "C:/Aran Installed/x13as")
#checkX13()
require("forecast")
require("car")
require("reshape2")
require("ggplot2")
require("tidyr")
require("plyr") #Hadley said if you load plyr first it should be fine
require("dplyr")
require("lazyeval")
require("broom")
require("assertthat")
library("readxl")


#############################
#
# sets a theme
#
#require(grid)

theme_jack <- function (base_size = 12, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(                                  #text=element_text(family="Lato Light", size=14),
      panel.grid.major.x=element_blank(),
      panel.grid.minor.x=element_blank(),
      panel.grid.minor.y=element_blank(),
      panel.grid.major.y=element_line(colour="#ECECEC", size=0.5, linetype=1),
      axis.ticks.y=element_blank(),
      panel.background=element_blank(),
      legend.title=element_blank(),
      legend.key=element_rect(fill="white", colour = "white"),
      legend.key.size=unit(1.5, "cm"),
      legend.text=element_text(size=16),
      axis.title=element_text(size=10),
      axis.text=element_text(color="black",size=13)
    )
}

####################
#
# sets up a way to recode a variable, similar to a lookup table approach
# copied from following link
# http://susanejohnston.wordpress.com/2012/10/01/find-and-replace-in-
# r-part-2-how-to-recode-many-values-simultaneously/

recoder_func <- function(data, oldvalue, newvalue) {
  # convert any factors to characters
  if (is.factor(data))     data     <- as.character(data)
  if (is.factor(oldvalue)) oldvalue <- as.character(oldvalue)
  if (is.factor(newvalue)) newvalue <- as.character(newvalue)
  
  # create the return vector
  newvec <- data
  # put recoded values into the correct position in the return vector
  for (i in unique(oldvalue)) newvec[data == i] <- newvalue[oldvalue == i]
  newvec
}


##########################
#
# converts units to millions, taking a column of a dataframe as input

units_millions <- function(col) {
  col/1000000
}


##########################
#
# seasonally adjust a series 
# arguments series
# output a data frame containing the seasonal factor and the seasonally adjusted series


seasonal_ad <- function (x,
                        meffects = c("const", "easter[8]", "thank[5]"), 
                        qeffects = c("const", "easter[8]")) {
  #stores the name
  holdn <- names(x)
  print(holdn)
  # trims the NAs from the series
      # I commented this out while doing the host work because it was causing the 
      # seasonal factors to be shifted, and not aligned with the proper dates
      # I didn't have this issue before I guess because things all tended to 
      # start at the same date
  # x <- na.trim(x)
  # this series y is used in the output, just outputs the original series
  y <- x
  y <- xts(y)
  
  # http://stackoverflow.com/questions/15393749/get-frequency-for-ts-from-and-xts-for-x12
  freq <- switch(periodicity(x)$scale,
                 daily=365,
                 weekly=52,
                 monthly=12,
                 quarterly=4,
                 yearly=1)
  plt_start <- as.POSIXlt(start(x))
  start <- c(plt_start$year+1900,plt_start$mon+1)
  print(start)
  
  # creates a time series object using start date and frequency
  # declared it as a global object by using <<- because I couldn't figure out
  # how to handle environments. It seems like the issue I was having is that
  # I define the seasonal_ad function, but then when it tries to run the 
  # seas function within that it is referrring to a different environment 
  # and can't seem to find the object that I want to give as an argument to 
  # seas. This is a temporary fix. Long term I should figure out how to handle
  # so that I'm not defining a global object from within the function, but 
  # should be fine for now.
  temp_seasonal_a <<- ts(as.numeric(x), start=start, frequency=freq)
  
  print(head(temp_seasonal_a))
  print(str(temp_seasonal_a))
  print(freq)
  if (freq == '12') regressvar <<- meffects
  if (freq == '4') regressvar <<- qeffects
  print(regressvar)
  print("checking")
  print(head(temp_seasonal_a))
  print(str(temp_seasonal_a))
  
  mp <- seas(temp_seasonal_a,
             transform.function = "log",
             regression.aictest = NULL,
             regression.variables = regressvar, #c("const", "easter[8]", "thank[3]"),
             identify.diff = c(0, 1),
             identify.sdiff = c(0, 1),
             forecast.maxlead = 30, # extends 30 quarters ahead
             x11.appendfcst = "yes", # appends the forecast of the seasonal factors
             dir = "output_data/" 
  )
  #inspect(mp)
  # removes series that is no longer needed
  # doesn't seem to work, maybe because I don't understand environments
  # rm(temp_seasonal_a)
  
  # grabs the seasonally adjusted series
  tempdata_sa <- series(mp, c("d11")) # seasonally adjusted series
  tempdata_sf <- series(mp, c("d16")) # seasonal factors
  tempdata_fct <- series(mp, "forecast.forecasts") # forecast of nonseasonally adjusted series
  tempdata_irreg <- series(mp, c("d13")) # final irregular component
  
  # creates xts objects
  tempdata_sa <- as.xts(tempdata_sa)
  tempdata_sf <- as.xts(tempdata_sf)
  # in the following, we just want the forecast series, not the ci bounds
  # I had to do in two steps, I'm not sure why
  tempdata_fct <- as.xts(tempdata_fct) 
  tempdata_fct <- as.xts(tempdata_fct$forecast) 
  tempdata_irreg <- as.xts(tempdata_irreg)
  
  # names the objects
  names(tempdata_sa) <- paste(holdn,"_sa",sep="") 
  names(tempdata_sf) <- paste(holdn,"_sf",sep="") 
  names(tempdata_fct) <- paste(holdn,"_fct",sep="") 
  names(tempdata_irreg) <- paste(holdn,"_irreg",sep="") 
  
  # merges the adjusted series onto the existing xts object with the unadjusted
  # series
  out_sa <- merge(y, tempdata_sa, tempdata_sf, tempdata_fct, tempdata_irreg)
  return(out_sa)
}


##########################
#
# skip seasonal adjustment but still output series that are 
# the same format as what would be exported by the seasonal 
# adjustment function
# in other words copy the unadjusted series as the
# seasonally adjusted, create seasonal factors equal to 1
# and create a temporary fct series


skip_seasonal_ad <- function (x) {

  # stores the name
  holdn <- names(x)
  print(holdn)
  # trims the NAs from the series
  x <- na.trim(x)
  # sets up a variable with the end of the historical data
  # and then the start of the forecast in the month after
  end <- end(x)
  library(lubridate)
  d <- ymd(end) 
  d <- d + months(1)
  startd <- as.Date(d)
  startd
  
  # this series y is used in the output, just outputs the original series
  y <- x
  
  tempdata_sa <- y
  # seasonal factor is unadjusted series divided by adjusted
  # though in this case that's just 1
  tempdata_sf <- y/y

  # just out of habit, forecast the seasonally adjusted series
  # which is just the nsa series anyway
  tempdata_fct <- forecast(tempdata_sa,h=30)$mean
  plot(tempdata_fct)
  head(tempdata_fct)
  tail(y)
  # start forecast in the month after
  temp2 <- zooreg(1:30, start = as.yearmon(startd), frequency = 12)
  temp3 <- as.Date(index(temp2))
  temp4 <- xts(tempdata_fct, temp3)
  head(temp4)
  tail(temp4)
  tempdata_fct <- rbind(y, temp4)
  plot(tempdata_fct)
  # converts the forecast to an nsa version (even though it's the same)
  tempdata_fct <- tempdata_fct * tempdata_sf
  
  # creates xts objects
  tempdata_sa <- as.xts(tempdata_sa)
  tempdata_sf <- as.xts(tempdata_sf)
  tempdata_fct <- as.xts(tempdata_fct)
  # names the objects
  names(tempdata_sa) <- paste(holdn,"_sa",sep="") 
  names(tempdata_sf) <- paste(holdn,"_sf",sep="") 
  names(tempdata_fct) <- paste(holdn,"_fct",sep="") 
  
  # merges the adjusted series onto the existing xts object with the unadjusted
  # series
  out_sa <- merge(y, tempdata_sa, tempdata_sf, tempdata_fct)
  
return(out_sa)
}

#########################
#
# aggregates, or converts, from monthly to quarterly
#
# I based this on the as.quarterly function that is in
# the tframePlus package
# I set it up so you have to give a type of aggregation
# it works for type=sum or type=mean
# The ts line is basically converting a series from xts
# to ts, because the ts works as an input to as.quarterly
# One reason that I believe I needed to set this up was
# that I want to be able to run it accross columns of an xts
# object, which one can't typically do with apply.
# also, the aggregate.zoo function will sum up the months
# to a quarterly value even if the last month is missing, for 
# example:
#tempc_m <- opcl_m$totusoprms
#tempc_q <- aggregate(tempc_m, as.yearqtr, sum)
#tempc_q <- xts(tempc_q)
# so this is what I came up with
m_to_q=function(x, type){
  out_q <- as.quarterly(
    ts(as.numeric(x), frequency = 12, start = c(year(start(x)), month(start(x)))), 
    FUN=type,
    # changed the following to FALSE. sometimes the dataframe will have some 
    # series that start earlier than others. If I did TRUE, I think some were
    # being dropped, leading to errors when used in a vapply, where I had to 
    # specify the length of what would come back, as it wasn't the same for all
    # so I changed this to FALSE, and then in the vapply I used ceiling to round
    # to include the last quarter
    na.rm=FALSE)
  return(out_q)
}
# as an example of using this function is the steps I had in 
# load_str_openclose, which are as follows
# start <- as.yearqtr((start(opcl_m)))
#h <- zooreg(vapply(opcl_m, m_to_q, FUN.VALUE = 
#                     numeric(floor(nrow(opcl_m)/3)), 
#                   type="sum"), start=start, frequency=4)
#opcl_q <- xts(h)
#indexClass(opcl_q) <- c("Date")
#

# a couple examples of as.quarterly for reference
# z <- ts(1:10, start = c(1999,2), frequency=4)
# z
# as.annually(z) 
# as.annually(z, na.rm=TRUE)
# 
# z <- ts(1:30, start = c(1999,2), frequency=12)
# z
# as.annually(z) 
# as.annually(z, na.rm=TRUE)
# as.quarterly(z) 
# as.quarterly(z, na.rm=TRUE)

# similar for conversion to annual
q_to_a=function(x, type){
  a_a <- as.annually(
    ts(as.numeric(x), frequency = 4, start = c(year(start(x)), month(start(x)))), 
    FUN=type,
    na.rm=FALSE)
  return(a_a)
}

#######################
#
# takes a quarterly xts object with multiple columns and converts to annual 
# need to give the type of conversion as argument, for example could do
# b <- q_to_a_xts(suma, type="sum")
# this function uses the q_to_a function that I've defined above, but then
# applys it to all of the columns of a xts object
q_to_a_xts=function(x, type){
start <- as.Date((start(x)))
start <- as.numeric(format(start(x), "%Y"))
h <- zooreg(vapply(x, q_to_a, FUN.VALUE = 
                     numeric(floor(nrow(x)/4)), 
                   type=type),  start=start, frequency=1)
# at this point it has a four digit year as the index
# but I wanted to format as a date with the start date of the year
h <- zooreg(h, order.by=as.Date(paste(index(h),"-01-01", sep="")))
h <- xts(h)
  return(h)
}

#######################
#
# creating indexes

# creates an index of a quarterly xts or maybe zoo series
index_q=function(x, index_year){
  start_index <- as.Date(paste(index_year,"-01-01",sep=""))
  end_index <- as.Date(paste(index_year,"-10-01",sep=""))
  temp_mean <- mean(window(x, start=start_index, end=end_index))
  x_index <- (x / temp_mean)*100
  return(x_index)
}

# runs across a quarterly xts object to create an index of each series
index_q_xts=function(x, index_year){
  start <- as.Date((start(x)))
  h <- zooreg(vapply(x, index_q, FUN.VALUE = 
                       numeric(nrow(x)), 
                     index_year=index_year))  
  h <- zooreg(h, order.by=index(x))
  h <- xts(h)
  return(h)
}

# creates an index of a single quarterly series that is in a melted dataframe
index_q_melted=function(x, index_year){
  x_index <- x %>%
    spread(variable, value) %>% 
    read.zoo(drop=FALSE) %>% 
    xts() %>%
    index_q(index_year=index_year) %>%
    data.frame() %>%
    as.matrix() %>%
    melt() %>%
    rename(date=Var1, variable=Var2, value=value)
    x_index$date <- as.Date(x_index$date)
    return(x_index)
}


# creates an index of a _monthly_ xts or maybe zoo series
index_m=function(x, index_year){
  start_index <- as.Date(paste(index_year,"-01-01",sep=""))
  end_index <- as.Date(paste(index_year,"-12-01",sep=""))
  temp_mean <- mean(window(x, start=start_index, end=end_index))
  x_index <- (x / temp_mean)*100
  return(x_index)
}

# creates an index of a single monthly series that is in a melted dataframe
index_m_melted=function(x, index_year){
  x_index <- x %>%
     spread(variable, value) %>% 
     read.zoo(drop=FALSE) %>% 
     xts()  %>%
     index_m(index_year=index_year) %>%
     data.frame() %>%
     as.matrix() %>%
     melt() %>%
  rename(date=Var1, variable=Var2, value=value)
  x_index$date <- as.Date(x_index$date)
  return(x_index)
}



#######
#
# function takes a data frame of monthly str data
# and returns a list containing a monthly data frame
# and a quarterly data frame

load_str <- function(load_m){
  
  # spreads into a tidy format with
  # tidyr and then calculates the occupancy and revpar series
  # first needs to go from xts to dataframe
  # b1 <- data.frame(date=time(lodus_m), lodus_m) %>% 
  b1 <- load_m %>% 
    # creates column called segvar that contains the column names, and one next to 
    # it with the values, dropping the time column
    gather(segvar, value, -date, na.rm = FALSE) %>%
    # in the following the ^ means anything not in the list
    # with the list being all characters and numbers
    # so it separates segvar into two colums using sep
    
    # in August 2015 I changed the following line to the one below it
    # which separates based on the last occurance of an underscore
    # I changed as part of the host data, in which I had a few underscores in my
    # series name and wanted to split on the last one, I hope this works generally
    # also changed quarterly below
    # separate(segvar, c("seg", "variable"), sep = "[^[:alnum:]]+") %>%
    separate(segvar, c("seg", "variable"), sep = "_(?!.*_)", extra="merge") %>%
     
    # keeps seg as a column and spreads variable into multiple columns containing
    # the values
    spread(variable,value) %>%
    # days_in_month is a function I borrowed. leap_impact=0 ignores leap year
    # this uses transform to create a new column where the new column is
    # created by using sapply on the date column to apply the days_in_month
    # function with the leap_impact argument set to 0
    transform(days = sapply(date, days_in_month,leap_impact=0)) %>%
    # adds several new calculated columns
    mutate(occ = demt / supt) %>%
    mutate(revpar = rmrevt / supt) %>%
    mutate(adr = rmrevt / demt) %>%
    # converts several concepts to millions
    mutate(supt = supt / 1000000) %>%
    mutate(demt = demt / 1000000) %>%
    mutate(rmrevt = rmrevt / 1000000) %>%
    mutate(demd = demt / days) %>%
    mutate(supd = supt / days) 
  
  load_m <- b1
  
  #############################
  #
  # creates quarterly by summing monthly
  #
  
  # get it ready to convert
  # takes it from a tidy format and melts it creating a dataframe with the
  # following columns (date, seg, variable, value), and then creates the unique
  # variable names and then reads into a zoo object spliting on the 
  # second column
  m_z <- load_m %>%
    select(-occ, -adr, -revpar, -demd, -supd) %>%
    melt(id=c("date","seg"), na.rm=FALSE) %>%
    mutate(variable = paste(seg, "_", variable, sep='')) %>%
    select(-seg) %>%
    read.zoo(split = 2) 
  
  # convert to quarterly
  # I couldn't use apply because the object is 
  # a xts, not a dataframe, see 
  # http://codereview.stackexchange.com/questions/39180/best-way-to-apply-across-an-xts-object
  
  # sets up the start of the index that will be used for the quarterly object
  # uses vapply to essentially run an apply across the xts object because
  # apply doesn't work on an xts object
  # for vapply we need to give the expected length in FUN.VALUE and a
  # start date and quarterly frequency
  # The function that I'm applying to each column is m_to_q, which I wrote, the type="sum"
  # is giving the type of aggregation to use in it
  
  # as a temp fix, I shortened a_mz to end at the end of a quarter. 
  # I need to come up with a better fix. The issue was that the 
  # function was expecting something length 111, but getting 112. 
  # might be an issue because I changed na.rm to FALSE in the 
  # m_to_q function because it was causing issues for the Mexico
  # series that were different lengths. So I put that back to na.rm=TRUE and it worked
  
  
  # head(raw_str_us)
  # head(tempa)
  # tempa <- read.zoo(raw_str_us)
  # head(tempa)
  # tempd <- tempa$totus_demt
  # str(tempd)
  # tail(tempd)
  # tempd2 <- m_to_q(tempd,type=sum)
  # tempd2 <- zoo(tempd2)
  # tail(tempd2)
  # str(tempd2)
  # 
  # nrow(tempd2)
  # nrow(tempa)/3
  # ceiling(nrow(tempa)/3)
  # start <- as.yearqtr((start(tempa)))
  # 
  # temp_q <- zooreg(vapply(tempa, m_to_q, FUN.VALUE = 
  #                           numeric(ceiling(nrow(tempa)/3)), 
  #                         type="sum"), start=start, frequency=4)
  # head(temp_q)
  # tail(temp_q)
  
  
  start <- as.yearqtr((start(m_z)))
  load_q <- zooreg(vapply(m_z, m_to_q, FUN.VALUE = 
                            numeric(ceiling(nrow(m_z)/3)), 
                          type="sum"), start=start, frequency=4)
  head(load_q)
  
  # turn into a data frame with a date column
  load_q <- data.frame(date=time(load_q), load_q) 
  load_q$date <- as.Date(load_q$date)
  row.names(load_q) <- NULL
  
  # goes into tidy format and then adds some calculated series
  b1q <- load_q %>% 
    # creates column called segvar that contains the column names, and one next to 
    # it with the values, dropping the time column
    gather(segvar, value, -date, na.rm = FALSE) %>%
    
    # in August 2015 I changed the following line to the one below it
    # which separates based on the last occurance of an underscore
    # I changed as part of the host data, in which I had a few underscores in my
    # series name and wanted to split on the last one, I hope this works generally
    # also changed quarterly below
    # separate(segvar, c("seg", "variable"), sep = "[^[:alnum:]]+") %>%
    separate(segvar, c("seg", "variable"), sep = "_(?!.*_)", extra="merge") %>%

    # keeps seg as a column and spreads variable into multiple columns containing
    # the values
    spread(variable,value) %>%
    # adds several new calculated columns
    mutate(occ = demt / supt) %>%
    mutate(revpar = rmrevt / supt) %>%
    mutate(adr = rmrevt / demt) %>%
    mutate(demd = demt / days) %>%
    mutate(supd = supt / days) 
  
  load_q <- b1q
  
  #############################
  #
  # puts back to wide format
  #
  
  # puts it back into a wide data frame, with one column for each series
  # days is a series for each segment/market\
  load_m <- load_m %>%
    melt(id=c("date","seg"), na.rm=FALSE) %>%
    mutate(variable = paste(seg, "_", variable, sep='')) %>%
    select(-seg) %>%
    spread(variable,value)
  # if instead I had wanted a zoo object, I could have done
  #read.zoo(split = 2) 
  
  # converts to xts from dataframe
  #lodus_m <- lodus_m %>%
  #  read.zoo() %>%
  #  as.xts
  
  # puts it back into a wide zoo object, with one column for each series
  # days is a series for each segment/market\
  load_q <- load_q %>%
    melt(id=c("date","seg"), na.rm=FALSE) %>%
    mutate(variable = paste(seg, "_", variable, sep='')) %>%
    select(-seg) %>%
    spread(variable,value)
  # if instead I had wanted a zz object, I could have done
  #read.zoo(split = 2
  
  return(list(load_m,load_q))
}


#######################
#
# function takes a monthly data frame with monthly data and seasonal factors
# and creates monthly sa

create_sa_str_m <- function(str_m){
  
  # following converts to a tidy format, uses seasonal factors to calculate sa
  # series, then converts back to a wide dataframe
  str_m <- str_m %>% 
    # creates column called segvar that contains the column names, and one next to 
    # it with the values, dropping the time column
    gather(segvar, value, -date, na.rm = FALSE) %>%
    # in the following the ^ means anything not in the list
    # with the list being all characters and numbers
    # so it separates segvar into two colums using sep
    # it separates on the _, as long as it's not followed by sf
    # the not followed piece uses a Negative Lookahead from
    # http://www.regular-expressions.info/lookaround.html
    separate(segvar, c("seg", "variable"), sep = "_(?!sf)") %>%
    # keeps seg as a column and spreads variable into multiple columns containing
    # the values
    spread(variable,value) %>%
    mutate(occ_sa = occ / occ_sf) %>%
    mutate(revpar_sa = revpar / revpar_sf) %>%
    mutate(adr_sa = adr / adr_sf) %>%
    mutate(demd_sa = demd / demd_sf) %>%
    mutate(supd_sa = supd / supd_sf) %>%
    mutate(demar_sa = demd_sa * 365) %>% # creates demand at an annual rate
    # puts it back into a wide data frame, with one column for each series
    # days is a series for each segment/market\
    melt(id=c("date","seg"), na.rm=FALSE) %>%
    mutate(variable = paste(seg, "_", variable, sep='')) %>%
    select(-seg) %>%
    spread(variable,value)
  # if instead I had wanted an xts object, I could have done
  #read.zoo(split = 2) %>%
  #xts()
  return(str_m)
}

#######################
#
# function takes a monthly data frame with monthly data and seasonal factors
# and creates monthly sa

create_sa_str_q <- function(str_q){
  
  # create quarterly sa from seasonal factors
  # following converts to a tidy format, uses seasonal factors to calculate sa
  # series, then converts back to a wide dataframe
  str_q <- str_q %>% 
    # creates column called segvar that contains the column names, and one next to 
    # it with the values, dropping the time column
    gather(segvar, value, -date, na.rm = FALSE) %>%
    # in the following the ^ means anything not in the list
    # with the list being all characters and numbers
    # so it separates segvar into two colums using sep
    # it separates on the _, as long as it's not followed by sf
    # the not followed piece uses a Negative Lookahead from
    # http://www.regular-expressions.info/lookaround.html
    separate(segvar, c("seg", "variable"), sep = "_(?!sf)") %>%
    # keeps seg as a column and spreads variable into multiple columns containing
    # the values
    spread(variable,value) %>%
    mutate(occ_sa = occ / occ_sf) %>%
    mutate(revpar_sa = revpar / revpar_sf) %>%
    mutate(adr_sa = adr / adr_sf) %>%
    mutate(demd_sa = demd / demd_sf) %>%
    mutate(supd_sa = supd / supd_sf) %>%
    mutate(demar_sa = demd_sa * 365) %>% # creates demand at an annual rate
    # puts it back into a wide data frame, with one column for each series
    # days is a series for each segment/market\
    melt(id=c("date","seg"), na.rm=FALSE) %>%
    mutate(variable = paste(seg, "_", variable, sep='')) %>%
    select(-seg) %>%
    spread(variable,value)
  # if instead I had wanted an xts object, I could have done
  #read.zoo(split = 2) %>%
  #xts()
  return(str_q)
}


######
#
# my thought process on this function had originally been that 
# it would start with a dataframe that had a single underscore
# between the segment code and the variable code. So all segment
# and geographic info would be in the segment code. But then I got 
# into the Host work, and realized that it was potentially useful 
# to have additional segment, geography, country information 
# in the mneomonic, separated by underscores. But I couldn't 
# figure out how to write a regular express than would just
# break of the lodging variable concept. What I could do was
# break up the other stuff until I was left with the lodging concept. 
# But that pointed to the idea of staying flexible on the set up 
# of the dataframe going into this function. So it is more generalized.
# Also, I realized that the monthly and quarterly were doing the same
# thing, so I combined them. I left the monthly and quarterly ones
# there, but I could get rid of them once I bring the US lodfor 
# process onto the same footing.

create_sa_str <- function(df){
  
  # create either monthly or quarterly sa from seasonal factors
  # requires as an input a dataframe with lodging variables separate
  # any number of geographic or segment columns is fine
  df <- df %>% 
    mutate(occ_sa = occ / occ_sf) %>%
    mutate(revpar_sa = revpar / revpar_sf) %>%
    mutate(adr_sa = adr / adr_sf) %>%
    mutate(demd_sa = demd / demd_sf) %>%
    mutate(supd_sa = supd / supd_sf) %>%
    mutate(demar_sa = demd_sa * 365) # creates demand at an annual rate
  return(df)
}


#######
#
# function takes a data frame of monthly str data
# and returns a set of monthly seasonal factors, including future months


seas_factors_m <- function(str_m, dont_m_cols){
  
  # gets ready to create monthly seasonal factors
  print("get ready to create monthly seasonal factors")
  
  # used to drop concepts that shouldn't be seasonally adjusted
  # but I removed it I put it into the actual script
  toadj_m <- str_m 
  #   toadj_m <- select(str_m, 
  #                     -ends_with("_days"), 
  #                     -ends_with("_demt"), 
  #                     -ends_with("_rmrevt"), 
  #                     -ends_with("_supt"))
 
  # drop series that have given errors when seasonally adjusting in past
  #dont_m_cols <- c("anaheim_supd|dallas_supd|detroit_supd|neworleans_supd|oahu_supd|sanfrancisco_supd|tampa_supd")
  #dont_m_cols <- c("totus_demd|totus_occ|totus_revpar|totus_supd|anaheim_supd|neworleans_supd|oahu_supd|sanfranchisco_supd|tampa_supd")
  
  # put the matching series into a dataframe
  dontadj_m <- toadj_m %>% 
    select(date, matches(dont_m_cols))
  
  # if there is anything in addition to the date column in the dontadj_m data frame, then 
  if (ncol(dontadj_m) > 1) {
    # also, removes the matching series from the full dataframe
    toadj_m  <- toadj_m  %>%
      select(-matches(dont_m_cols))
    print("there are series being excluded from adjustment")
  } else {
    print("nothing in dataframe being excluded from adjustment")
  }
  #########
  #
  # create monthly seasonal factors
  #
  
  head(toadj_m)
  print("line37")
  
  # convert to xts
  toadj_m <- toadj_m %>%
    read.zoo(drop=FALSE) %>%
    xts()
  
  # creates a blank object with just a dummy series
  # this is used to hold the output of seasonal adjustment
  sf_m <- xts(order.by=index(toadj_m))
  sf_m <- merge(sf_m, dummy=1)
  
  # monthly seasonal adj of all columns in xts object
  # I would have used vapply, but hard to know how many rows in output
  for (i in 1:ncol(toadj_m)){
    current_series <- colnames(toadj_m[,i])
    print(paste("starting monthly adjustment of", current_series, sep=" "))
    temphold <- seasonal_ad(toadj_m[,i], 
                            meffects =  c("const", "easter[8]", "thank[5]"))
    sf_m <- merge(sf_m, temphold)  
  }
  
  print("line56")
  
  
  # turn into a data frame with a date column
  sf_m <- data.frame(date=time(sf_m), sf_m) 
  sf_m$date <- as.Date(sf_m$date)
  row.names(sf_m) <- NULL
  # full copy of seasonally adjusted, factors, and irreg 
  full_sf_m <- select(sf_m, -dummy)
  # takes just the seasonal factors
  sf_m <- sf_m %>%
    select(date, ends_with("_sf"))
  
  #########
  #
  # create seasonal factors for those that were specifically not adjusted
  
  temp_a <- sf_m %>%
    read.zoo(drop=FALSE) %>%
    xts()
  tempc <- index(temp_a)
  head(tempc)
  
  print("line76")
  # create a matrix of ones with sufficient number of columns
  m <- matrix(rep(1), nrow = nrow(sf_m), ncol = ncol(dontadj_m)) %>%
    data.frame()
  # get column names, add sf to names for seasonal factor
  colnames(m) <- paste(colnames(dontadj_m),"_sf", sep="")
  # drop the date_sf column
  m <- select(m, -date_sf)
  # create dataframe with date as first column
  sf_m_dontadj <- cbind(tempc, m) %>% 
    dplyr::rename(date = tempc)
  
  print("line87")
  ########
  #
  # combines seasonal factors from adjustments and skipped
  #
  
  sf_m <- merge(sf_m, sf_m_dontadj, by= "date")
  
  return(sf_m)
}


#######
#
# function takes a data frame of quarterly str data
# and returns a set of quarterly seasonal factors, including future months


seas_factors_q <- function(str_q, dont_q_cols){
  
  # gets ready to create quarterly seasonal factors
  print("get ready to create quarterly seasonal factors")
  
  # used to drop concepts that shouldn't be seasonally adjusted
  # but I removed it I put it into the actual script
  toadj_q <- str_q 

#   toadj_q <- select(str_q, 
#                     -ends_with("_days"), 
#                     -ends_with("_demt"), 
#                     -ends_with("_rmrevt"), 
#                     -ends_with("_supt"))
   
  # drop series that have given errors when seasonally adjusting in past
  #dont_q_cols <- c("anaheim_supd|neworleans_supd|oahu_supd|sanfranchisco_supd|tampa_supd")
  #dont_q_cols <- c("totus_demd|totus_occ|totus_revpar|totus_supd|anaheim_supd|neworleans_supd|oahu_supd|sanfranchisco_supd|tampa_supd")
  
  # put the matching series into a dataframe
  dontadj_q <- toadj_q %>% 
    select(date, matches(dont_q_cols))
  
  # if there is anything in addition to the date column in the dontadj_m data frame, then 
  if (ncol(dontadj_q) > 1) {
    # also, removes the matching series from the full dataframe
    toadj_q  <- toadj_q  %>%
      select(-matches(dont_q_cols))
    print("there are series being excluded from adjustment")
  } else {
    print("nothing in dataframe being excluded from adjustment")
  }
  
  #########
  #
  # create quarterly seasonal factors
  #
  
  # convert to xts
  toadj_q <- toadj_q %>%
    read.zoo(drop=FALSE) %>%
    xts()
  
  # creates a blank object with just a dummy series
  # this is used to hold the output of seasonal adjustment
  sf_q <- xts(order.by=index(toadj_q))
  sf_q <- merge(sf_q, dummy=1)
  
  # quarterly seasonal adj of all columns in xts object
  # I would have used vapply, but hard to know how many rows in output
  for (i in 1:ncol(toadj_q)){
    current_series <- colnames(toadj_q[,i])
    print(paste("starting quarterly adjustment of", current_series, sep=" "))
    # take out thanksgiving variable for quarterly
    temphold <- seasonal_ad(toadj_q[,i], 
                            qeffects =  c("const", "easter[8]"))
    sf_q <- merge(sf_q, temphold)  
  }
  
  # turn into a data frame with a date column
  sf_q <- data.frame(date=time(sf_q), sf_q) 
  sf_q$date <- as.Date(sf_q$date)
  row.names(sf_q) <- NULL
  # full copy of seasonally adjusted, factors, and irreg 
  full_sf_q <- select(sf_q, -dummy)
  # takes just the seasonal factors
  sf_q <- sf_q %>%
    select(date, ends_with("_sf"))
  
  #########
  #
  # create seasonal factors for those that were specifically not adjusted
  
  temp_a <- sf_q %>%
    read.zoo(drop=FALSE) %>%
    xts()
  tempc <- index(temp_a)
  head(tempc)
  
  # create a matrix of ones with sufficient number of columns
  m <- matrix(rep(1), nrow = nrow(sf_q), ncol = ncol(dontadj_q)) %>%
    data.frame()
  # get column names, add sf to names for seasonal factor
  colnames(m) <- paste(colnames(dontadj_q),"_sf", sep="")
  # drop the date_sf column
  m <- select(m, -date_sf)
  # create dataframe with date as first column
  sf_q_dontadj <- cbind(tempc, m) %>% 
    dplyr::rename(date = tempc)
  
  ########
  #
  # combines seasonal factors from adjustments and skipped
  #
  
  sf_q <- merge(sf_q, sf_q_dontadj, by= "date")
  
  return(sf_q)
}


########################
#
# functions for plots
#

# adds the title
plot_title_1=function(plot, grtitle, footnote){
  grobframe <- arrangeGrob(plot,
    main = textGrob(grtitle, x=0, hjust=0, vjust=0.6, 
                    gp = gpar(fontsize=16, fontface="bold")),
    sub = textGrob(footnote, x=0, hjust=0, vjust=0.1, 
                    gp = gpar(fontface="plain", fontsize=7)))
  grid.newpage() # basic command to create a new page of output
  grid.draw(grobframe)
  # these worked but didn't improve things much I thought
  #ggsave(grobframe,file="whatever.png",compression="lzw",height=5.5,width=9,dpi=1000,units="in")
  #ggsave(grobframe,file="whatever.png",height=5.5,width=9,dpi=900,units="in")
}

plot_title_2=function(plot, grtitle, footnote, filename){
  grobframe <- arrangeGrob(plot, ncol=1, nrow=1,
                           main = textGrob(grtitle, x=0, hjust=0, vjust=0.6, 
                                           gp = gpar(fontsize=16, fontface="bold")),
                           sub = textGrob(footnote, x=0, hjust=0, vjust=0.1, 
                                          gp = gpar(fontface="plain", fontsize=7)))
  grid.newpage() # basic command to create a new page of output
  grid.draw(grobframe)
  # these worked but didn't improve things much I thought
  #ggsave(grobframe,file="whatever.tiff",compression="lzw",height=5.5,width=9,dpi=1000,units="in")
  #ggsave(grobframe,file="whatever.emf",height=5.7,width=9,dpi=800,units="in")
  ggsave(grobframe,file=filename,height=5.7,width=9,dpi=800,units="in")
}

plot_title_3=function(plot, grtitle, footnote){
  grobframe <- arrangeGrob(plot,heights=unit(c(5,.5), c("in", "in")),
                           top = textGrob(grtitle, x=0, hjust=0, vjust=0.6, 
                                           gp = gpar(fontsize=16, fontface="bold")),
                           sub = textGrob(footnote, x=0, hjust=0, vjust=0.1, 
                                          gp = gpar(fontface="plain", fontsize=7)))
  grid.newpage() # basic command to create a new page of output
  grid.draw(grobframe)
  # these worked but didn't improve things much I thought
  #ggsave(grobframe,file="whatever.png",compression="lzw",height=5.5,width=9,dpi=1000,units="in")
  #ggsave(grobframe,file="whatever.png",height=5.5,width=9,dpi=900,units="in")
}


# combines two plots together (e.g. employment and GDP)
# to get this function to work I needed to modify ggsave using the following line
# this is because I've used ggplot_gtable, and so the pieces aren't ggplot plot
# elements, and that's what's expected in ggsave. Based on:
#http://stackoverflow.com/questions/18406991/saving-a-graph-with-ggsave-after-using-ggplot-build-and-ggplot-gtable
ggsave <- ggplot2::ggsave; body(ggsave) <- body(ggplot2::ggsave)[-2]
plot_title_two1=function(p1, p2, grtitle, footnote, filename){
  gp1<- ggplot_gtable(ggplot_build(p1))
  gp2<- ggplot_gtable(ggplot_build(p2))
  maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3])
  gp1$widths[2:3] <- maxWidth
  gp2$widths[2:3] <- maxWidth
  
  grobframe <- arrangeGrob(gp2, gp1, ncol=1, nrow=2,
                           main = textGrob(grtitle, x=0, hjust=0, vjust=0.6, 
                                           gp = gpar(fontsize=16, fontface="bold")),
                           sub = textGrob(footnote, x=0, hjust=0, vjust=0.1, 
                                          gp = gpar(fontface="plain", fontsize=7)))
  grid.newpage() # basic command to create a new page of output
  grid.draw(grobframe)
  ggsave(grobframe,file=filename,height=5.7,width=9,dpi=800,units="in")
}
########
#
# CAGR calculations

# Compounded Annual Rate of Change
# (Ending Value / Initial Value) ^ ( 1 / # of periods) - 1)
calc_cagr <- function(x, n){
  (x/lag(x,n))^(1/n)-1}

# Compounded Annual Rate of Change
# (((x(t)/x(t-1)) ** (n_obs_per_yr)) - 1) * 100
calc_annualized <- function(x, p) {
  (x/lag(x,1))^(p)-1}

##########
#
# function to check whether all intended geographies are 
# included in a dataframe of series set up with var_geo 
# format series names

# input a dataframe that is set up for eviews, for example,
# with series names as column headers and dates in a date column
# arguments: check_for = vector of codes to check for, such as based on 
# geo_for_eviews

check_vargeo <- function(x, check_for) {
  all_series_1 <- colnames(x)[2:ncol(x)]
  
  temp_a1 <- x %>%
    gather(vargeo, value, -date) %>%
    # separates based on regular expression that finds last occurance of _
    separate(vargeo, c("var", "area_sh"), sep = "_(?!.*_)", extra="merge") %>%
    select(var) %>%
    distinct(var) 
  
  temp_a2 <- expand.grid(x=temp_a1$var, y=unique(check_for)) %>%
    mutate(z = paste(x, y, sep="_")) %>%
    select(z) 
  temp_a2 <- as.character(temp_a2$z)
  
  # all elements of the first vector without matching elements in the second
  # so taking temp_a2 as all combinations of the variables in the dataframe
  # with the areas in the check_for list. And then seeing if there are any
  # that aren't already in the dataframe. 
  miss_series <- setdiff(temp_a2, all_series_1)
  if(length(miss_series)>0){
    # creates a dataframe with missing series as the column names
    df1 <- matrix(,nrow = 1, ncol = length(miss_series)) %>%
      data.frame()
    colnames(df1) <- miss_series
    # replace the 1's with NA
    df2 <- as.data.frame(lapply(df1, function(y) as.numeric(gsub("1", NA, y))))
    # creates a zoo object using the dates from the input data frame, 
    # but then converts back to a dataframe
    df3 <- zoo(df2, order.by=x$date) %>%
      data.frame(date=index(.),.) 
    # joins the dataframe of NAs for all missing series onto the input dataframe
    b <- left_join(x, df3, by = c("date" = "date"))
  }  else 
    # if there is nothing in the missing series list, just return the original x
    b <- x
  output <- list(b, miss_series)
}



####################
#
# ending that might help with issue in knitr

## @knitr plotXY

####################
#
# function to return days in month
# I modified so you can set leap_impact equal 0 to help with str data

days_in_month <- function(d = Sys.Date(), leap_impact=1){
  
  m = substr((as.character(d)),6,7)              # month number as string
  y = as.numeric(substr((as.character(d)),1,4))  # year number as numeric
  
  # Quick check for leap year
  leap = 0
  if ((y %% 4 == 0 & y %% 100 != 0) | y %% 400 == 0){leap = leap_impact}
  
  # Return the number of days in the month
  return(switch(m,
                '01' = 31,
                '02' = 28 + leap,
                '03' = 31,
                '04' = 30,
                '05' = 31,
                '06' = 30,
                '07' = 31,
                '08' = 31,
                '09' = 30,
                '10' = 31,
                '11' = 30,
                '12' = 31))
}
#days_in_month("2014-11-25")
