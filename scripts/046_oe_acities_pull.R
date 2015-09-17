
# imports manual crosswalk with oe location codes
colc <- rep("character", 5)
manual_cw_oe_msa <- read.csv("input_data/manual_cw_oeac_msa.csv", head=TRUE, colClasses=colc)


# reads in an OE Asian Cities csv file using Hadley's readxl and saves as Rdata
temp_a <- read_excel("input_data/Asian cities - HLT - China_2015_09_16.xlsx", sheet=2)

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
  gather(date, value, x2000:x2030) %>%
  mutate(date = gsub("x", "", date)) %>%
  mutate(date = as.Date(paste(date, "-01-01", sep=""))) %>%
  rename(oeac_location = location) %>%
  rename(oeac_loc_code = location_code) %>%
  rename(oe_code = indicator_code)

unique(temp_c$oeac_location)


# joins on area_code_sh
manual_cw_oe_msa <- manual_cw_oe_msa %>%
  select(-oeac_location)
temp_d <- temp_c %>%
  left_join(., manual_cw_oe_msa, by=c("oeac_loc_code" = "oeac_loc_code")) %>%
  mutate(area_sh = ifelse(oeac_location == "China", "chn", area_sh)) %>%
  select(date, area_sh, oe_code, value)

# changes some oe_codes to be consistent with the NA cities codes
temp_e <- temp_d %>%
  mutate(oe_code = gsub("GDPLCC", "gdp", oe_code))

# show indicator codes
unique(temp_e$oe_code)


# select only a few series to keep, convert to zoo
temp_f <- temp_e %>%
  # filter based on whether the contents are in a vector
  filter(oe_code %in% c("gdp")) %>%
  # get ready to convert to xts
  mutate(vargeo = paste(oe_code, area_sh, sep="_")) %>%
  select(date, vargeo, value) %>%
  spread(vargeo, value) %>%
  # because it complained about a bad entry when going to zoo
  data.frame() %>%
  read.zoo(regular=TRUE) %>%
  xts()

# rename the China GDP series so that I can remember it came from Asian cities
temp_f <- temp_f %>%
  data.frame(date=time(.), .) %>%
  rename(gdpac_chn = gdp_chn) %>%
  read.zoo %>%
  xts()

out_acities_a <- temp_f %>%
  data.frame(date=time(.), .) %>%
  gather(vargeo, value, -date) %>%
  separate(vargeo, c("var", "area_sh"), sep = "_", extra="merge") %>%
  spread(var, value) 

#############
#
# convert to quarterly

# next uses na.spline to convert to quarterly
# Note: In the Global Model, China GDP is four quarterly values that sum 
# to an annual total. This is similar for US GDP. So BEA presents quarterly
# data at an annualized rate. But OE Global Model has it as quarterly
# values that sum to an annual value. 
# The following approach seems to put the annual value as the first quarter
# and then fills in the remaining quarter. I searched but can't figure out the 
# alternatives right now. 

temp_q <- temp_f %>%
  # uses the na.spline function in zoo to convert from annual to quarterly
  # from the start of the zoo object to the end
  # I think this is putting the annual value as the first quarter, and then filling
  # in the remaining quarters
  lapply(., FUN=na.spline, xout = seq(start(.), end(.), by = "quarter")) %>%
  do.call("merge", .) %>%
  data.frame(date=time(.), .) %>%
  read.zoo %>%
  xts()

colnames(temp_q)
autoplot(diff(log(temp_q$gdpac_chn)))
autoplot(diff(log(temp_q$gdp_beijichn)))
autoplot(diff(log(temp_q$gdp_gngzhchn)))
autoplot(diff(log(temp_q$gdp_shangchn)))

# spread to tidy format
out_acities_q <- temp_q %>%
  data.frame(date=time(.), .) %>%
  gather(vargeo, value, -date) %>%
  separate(vargeo, c("var", "area_sh"), sep = "_", extra="merge") %>%
  spread(var, value) 

# convert GDP from annual rate to quarterly value. Isn't quite perfect, but
# does take the quarterly values down to a quarter of what they were. Since they
# were based on annual data filled in, this is helpful in staying consistent with
# the OE global model approach to GDP.
out_acities_q <- out_acities_q %>%
  mutate(gdp = gdp/4) %>%
  mutate(gdpac = gdpac /4)

######################3
#
# saving results

# saves Rdata version
save(out_acities_a, file=paste("output_data/out_acities_a.Rdata", sep=""))
save(out_acities_q, file=paste("output_data/out_acities_q.Rdata", sep=""))
