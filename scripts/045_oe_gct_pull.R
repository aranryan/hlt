

# adapted from fred project to run with China metros

# imports list of MSAs
manual_cw_oegct_msa <- read.csv("input_data/manual_cw_oegct_msa.csv", head=TRUE, colClasses="character") 

# reads in an Excel file using Hadley's readxl and saves as Rdata
temp_a <- read_excel("input_data/GCT - HLT - China_2015_09_16.xlsx", sheet=2)

################
#
# formats data and appends area codes

str(temp_a)
tail(temp_a)
# removes columns that are all NA
temp_b <- Filter(function(x)!all(is.na(x)), temp_a)
# formats column names
colnames(temp_b) <- colnames(temp_b) %>%
  make.names() %>%
  tolower() %>%
  gsub("\\.", "\\_", .)

# When gathering variables, we need to provide the name of the new key-value 
# columns to create. The first argument, is the name of the key column, which 
# is the name of the variable defined by the values of the column headings. In 
# this case, it's date. The second argument is the name of the value column, in
# this case it's value.

temp_c <- temp_b %>%
  gather(date, value, x2000:x2025) %>%
  mutate(date = gsub("x", "", date)) %>%
  # uses as.yearqtr to convert to class yearqtr, then applies as.Date
  mutate(date = paste(date, "-01-01", sep="")) %>%
  mutate(date = as.Date(date)) %>%
  rename(oe_location = location) %>%
  rename(oegct_location_code = location_code) %>%
  rename(oe_code = indicator_code) %>%
  mutate(oe_code = tolower(oe_code)) %>%
  rename(origin = region_country)


# joins on area_code_sh
temp_d <- temp_c %>%
  left_join(., manual_cw_oegct_msa, by=c("oegct_location_code" = "oegct_location_code")) %>%
  filter(! is.na(area_sh)) %>%
  select(date, area_sh, oe_code, indicator, origin, value)

# show indicator codes
a <- temp_d$oe_code
a <- unique(a)
a

###########
#
# Extract a file to save with all of the annual data at this point.
# This is before slimming down and converting some totals to quarterly.
# Left this code alone in creating Hilton file

out_gct_full_a <- temp_d %>%
  select(date, area_sh, oe_code, value) %>%
  # short mnuemonics
  mutate(oe_code = ifelse(oe_code == "tourrstay", "vr", 
                   ifelse(oe_code == "tourfstay", "vf",
                   ifelse(oe_code == "rnightstot", "rn", 
                   ifelse(oe_code == "fnightstot", "rnf", oe_code))))) %>%
  mutate(oe_code = gsub("nights", "rnf", oe_code)) %>%
  mutate(oe_code = gsub("visit", "vf", oe_code)) %>%
  # calculating a few sub-totals. If done for more variables, this
  # could be done in a cleaner format by splitting the oe_code and
  # assigning sub-total regions, and then summing groups. This is a 
  # short cut for visits
  spread(oe_code, value) %>%
  # Latin America and Caribbean
  mutate(vflatc = vfcar + vfcam + vfsam) %>%
  # Europe
  mutate(vfeur = vfweu + vfeeu) %>%
  # Asia
  mutate(vfasi = vfnea + vfsea + vfsas) %>%
  gather(oe_code, value, -date, -area_sh) %>%
  spread(date, value)

# saves Rdata version
save(out_gct_full_a, file=paste("output_data/out_gct_full_a.Rdata", sep=""))
write.csv(out_gct_full_a, "output_data/out_gct_full_a.csv", row.names=FALSE)

###########
#
# I didn't try to figure out how to parse the various origin descriptions
# at this point. I just filtered to keep only the total visits and nights
to_keep <- c("vr", "vf", "rn", "rnf")
out_gct_a <- out_gct_full_a %>%
  gather(date, value, -area_sh, -oe_code) %>%
  # put in order
  select(date, area_sh, oe_code, value) %>%
  filter(oe_code %in% to_keep) %>%
  mutate(date = as.Date(date)) %>%
  # spread to tidy format
  spread(oe_code, value)

##############
#
#  converts to quarterly

# reads to zoo
temp_q_1 <- out_gct_a %>%
  gather(variable, value, -date, -area_sh) %>%
  mutate(vargeo = paste(variable, area_sh, sep="_")) %>%
  select(date, vargeo, value) %>%
  spread(vargeo, value) %>%
  # because it complained about a bad entry when going to zoo
  data.frame() %>%
  read.zoo(regular=TRUE)

autoplot(temp_q_1$vf_beijichn)
autoplot(temp_q_1$vf_chgduchn)
autoplot(temp_q_1$vf_gngzhchn)
autoplot(temp_q_1$vf_shangchn)


# works for one using na.spline function in zoo
a <- na.spline(temp_q_1$vf_beijichn, xout = seq(start(temp_q_1$vf_beijichn), 
    end(temp_q_1$vf_beijichn), by = "quarter"))
autoplot(a)

temp_q_2 <- temp_q_1 %>%
  # uses the na.spline function in zoo to convert from annual to quarterly
  # from the start of the zoo object to the end
  lapply(., FUN=na.spline, xout = seq(start(.), end(.), by = "quarter")) %>%
  do.call("merge", .) %>%
  data.frame(date=time(.), .) %>%
  read.zoo

autoplot(temp_q_2$vf_chgduchn)
autoplot(temp_q_2$vf_gngzhchn)
autoplot(temp_q_2$vf_shangchn)

###########
#
# since the quarterly data doesn't have seasonality, it just puts it into 
# a tidy format 

out_gct_q <- temp_q_2 %>%
  data.frame(date=time(.), .) %>%
  gather(variable, value, -date) %>%
  separate(variable, c("var", "area_sh"), sep = "_", extra="merge") %>%
  spread(var, value) 

######################3
#
# saving results

# saves Rdata version
save(out_gct_a, file=paste("output_data/out_gct_a.Rdata", sep=""))
save(out_gct_q, file=paste("output_data/out_gct_q.Rdata", sep=""))
