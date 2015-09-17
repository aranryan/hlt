
#######
#
# define functions

# here is a simple function I use to convert from adr_sa to adrsa, for 
# example

convert_sa_vars <- function(x) {
  x <- x %>%
  gather(variable, value, -date) %>%
    # replace the sa terms
    mutate(variable = gsub("_demd_sa$", "_demdsa", variable)) %>%
    mutate(variable = gsub("_demar_sa$", "_demarsa", variable)) %>%
    mutate(variable = gsub("_supd_sa$", "_supdsa", variable)) %>%
    mutate(variable = gsub("_occ_sa$", "_occsa", variable)) %>%
    mutate(variable = gsub("_adr_sa$", "_adrsa", variable)) %>%
    mutate(variable = gsub("_revpar_sa$", "_revparsa", variable)) %>%
    # do it for sf
    mutate(variable = gsub("_demd_sf$", "_demdsf", variable)) %>%
    mutate(variable = gsub("_demar_sf$", "_demarsf", variable)) %>%
    mutate(variable = gsub("_supd_sf$", "_supdsf", variable)) %>%
    mutate(variable = gsub("_occ_sf$", "_occsf", variable)) %>%
    mutate(variable = gsub("_adr_sf$", "_adrsf", variable)) %>%
    mutate(variable = gsub("_revpar_sf$", "_revparsf", variable)) %>%
    #spread it back out
  spread(variable, value)
}


###############
#
# China

load("output_data/raw_str_hlt_chn.Rdata")
# runs the load_str function to create monthly and quarterly data
temp_str_chn <- load_str(raw_str_hlt_chn)

# these two data frames are the working data frames and become the outputs
str_chn_m <- temp_str_chn[[1]]
str_chn_q <- temp_str_chn[[2]]

# loads seasonal factors
load("output_data/str_hlt_chn_m_factors.Rdata")
load("output_data/str_hlt_chn_q_factors.Rdata")

# create monthly sa from seasonal factors using a function
str_chn_m1 <- merge(str_chn_m, str_hlt_chn_m_factors, all=TRUE)
out_str_chn_m <- create_sa_str_m(str_chn_m1)

# create quarterly sa from seasonal factors using a function
str_chn_q1 <- merge(str_chn_q, str_hlt_chn_q_factors, all=TRUE)
out_str_chn_q <- create_sa_str_q(str_chn_q1)

# I decided I wanted to try to do the China analysis without having the
# _ before the sa and sf. At some point I'll fix the create_sa_str_q 
# function, but here I just used a simple function defined above to 
# gsub things.

# monthly
out_str_chn_m <- convert_sa_vars(out_str_chn_m)
# quarterly
out_str_chn_q <- convert_sa_vars(out_str_chn_q)

#####################
#
# adds str_days to the monthly and quarterly objects
# this is similar to the original us script, with
# a few "us" changed to "chn". Then also changed totus_strdays to 
# just strdays. Could write this to be a function.

# add a str_days series with the number of days in each month and quarter
# there are already series such as totus_days, but they don't go into 
# the future
b_m <- as.data.frame(out_str_chn_m$date )
colnames(b_m) <- c("date")
b_m <- b_m %>% 
  transform(strdays = sapply(date, days_in_month,leap_impact=0)) %>%
  read.zoo(drop=FALSE) 

# for quarterly, we need to to extend to the end of the quarterly series
# so summing the monthly dataframe doesn't work. But then the days_in_month
# function isn't giving a quarterly answer just days in the first month
# of the quarter. So solution was to create a monthly series that extended
# the full length, and then sum that.

# creates newm as a monthly zoo object with number of days for full
# length of quarterly
temp <- read.zoo(out_str_chn_q)
starta <- start(temp)
enda <- end(temp)
rm(temp)
newm <- zoo(1,seq(starta,enda,by="month"))
newma <- index(newm)
newm_df<- as.data.frame(newm)
newm_df$date <- rownames(newm_df)
newm_df <- newm_df %>%
   transform(strdays = sapply(date, days_in_month,leap_impact=0)) %>%
   select(-newm)
newm <- read.zoo(newm_df, drop=FALSE) 
#sums zoo object to quarterly
start <- as.yearqtr((start(newm)))
b_q <- zooreg(vapply(newm, m_to_q, FUN.VALUE = 
                       numeric(ceiling(nrow(newm)/3)), 
                     type="sum"), start=start, frequency=4)

b_m <- as.data.frame(b_m)
b_m <- cbind(date = rownames(b_m), b_m)
b_m$date <- as.Date(b_m$date)

b_q <- data.frame(date=time(b_q), b_q) 
b_q$date <- as.Date(b_q$date)
row.names(b_q) <- NULL

out_str_chn_m <- merge(out_str_chn_m, b_m)
out_str_chn_q <- merge(out_str_chn_q, b_q)
colnames(out_str_chn_m)
colnames(out_str_chn_q)

################
#
# creates output files
# 

out_str_chn_m <- out_str_chn_m %>%
  read.zoo() %>%
  xts()
  
out_str_chn_q <- out_str_chn_q %>%
  read.zoo() %>%
  xts()

colnames(out_str_chn_q)

#########################
#
# writing outputs
#

# writes csv versions of the output files
write.zoo(out_str_chn_m, file="output_data/out_str_chn_m.csv", sep=",")
write.zoo(out_str_chn_q, file="output_data/out_str_chn_q.csv", sep=",")

# saves Rdata versions of the output files
save(out_str_chn_m, file="output_data/out_str_chn_m.Rdata")
save(out_str_chn_q, file="output_data/out_str_chn_q.Rdata")

