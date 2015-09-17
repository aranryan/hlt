# oe tdm pull
Tourism Economics  
September 17, 2015  


Setup

```r
source('~/Project/R projects/hlt/scripts/functions.R')
```


As an input, this expects an Excel file.

```r
# when kniting from the button, I either needed the full path or to add "../" 
# in front of each

# imports manual crosswalk with oe location codes
colc <- rep("character", 4)
manual_cw <- read.csv("../input_data/manual_cw_oetdm_country.csv", head=TRUE, colClasses=colc)

# reads in an OE Asian Cities csv file using Hadley's readxl and saves as Rdata
temp_a <- read_excel("../input_data/TDM - HLT - China_2015_09_17.xlsx", sheet=2)
```



```r
################
#
# formats data and appends area codes

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
  gather(date, value, x1988:x2025) %>%
  mutate(date = gsub("x", "", date)) %>%
  mutate(date = as.Date(paste(date, "-01-01", sep=""))) %>%
  mutate(indicator_code = tolower(indicator_code))

unique(temp_c$oegc_location)
```

```
## NULL
```

```r
# are any of the indicator codes duplicated, zero for none (if dataframe
# has only one country that is)
anyDuplicated(temp_b$indicator_code)
```

```
## [1] 0
```

```r
# joins on area_code_sh
temp_d <- temp_c %>%
  left_join(., manual_cw, by=c("location_code" = "location_code")) %>%
  select(date, country_abb, indicator_code, value)

# show indicator codes
unique(temp_d$indicator_code)
```

```
##  [1] "tourf"        "tourfstay"    "tourfday"     "tourfcru"    
##  [5] "tourfhol"     "tourfbiz"     "tourfoth"     "fnightstot"  
##  [9] "fnightshotel" "fguests"      "avestayf"     "rnightstot"  
## [13] "rnightshotel" "rguests"      "avestayr"
```



```r
# select only a few series to keep, convert to zoo
temp_f <- temp_d %>%
  # filter based on whether the contents are in a vector
  #filter(oe_code %in% c("gdp")) %>%
  # get ready to convert to xts
  mutate(vargeo = paste(indicator_code, country_abb, sep="_")) %>%
  select(date, vargeo, value) %>%
  spread(vargeo, value) %>%
  # because it complained about a bad entry when going to zoo
  data.frame() %>%
  read.zoo(regular=TRUE) %>%
  xts()

# spread to tidy format
out_tdm_a <- temp_f %>%
  data.frame(date=time(.), .) %>%
  gather(vargeo, value, -date) %>%
  separate(vargeo, c("var", "area_sh"), sep = "_", extra="merge") %>%
  spread(var, value) 
```


```r
##########
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

temp_h <- temp_f %>%
  # uses the na.spline function in zoo to convert from annual to quarterly
  # from the start of the zoo object to the end
  # I think this is putting the annual value as the first quarter, and then filling
  # in the remaining quarters
  lapply(., FUN=na.spline, xout = seq(start(.), end(.), by = "quarter", method = "hyman")) %>%
  do.call("merge", .) %>%
  data.frame(date=time(.), .) %>%
  read.zoo %>%
  xts()
```



###A few plots

```
##  [1] "avestayf_chn"     "avestayr_chn"     "fguests_chn"     
##  [4] "fnightshotel_chn" "fnightstot_chn"   "rguests_chn"     
##  [7] "rnightshotel_chn" "rnightstot_chn"   "tourf_chn"       
## [10] "tourfbiz_chn"     "tourfcru_chn"     "tourfday_chn"    
## [13] "tourfhol_chn"     "tourfoth_chn"     "tourfstay_chn"
```

```
## Warning in log(temp_h$tourf_chn): NaNs produced
```

```
## Warning: Removed 1 rows containing missing values (geom_path).
```

![](048_oe_tdm_pull_files/figure-html/plots-1.png) ![](048_oe_tdm_pull_files/figure-html/plots-2.png) 

```
## Warning: Removed 1 rows containing missing values (geom_path).
```

![](048_oe_tdm_pull_files/figure-html/plots-3.png) ![](048_oe_tdm_pull_files/figure-html/plots-4.png) 

```
## Warning: Removed 1 rows containing missing values (geom_path).
```

![](048_oe_tdm_pull_files/figure-html/plots-5.png) ![](048_oe_tdm_pull_files/figure-html/plots-6.png) 

```
## Warning: Removed 1 rows containing missing values (geom_path).
```

![](048_oe_tdm_pull_files/figure-html/plots-7.png) ![](048_oe_tdm_pull_files/figure-html/plots-8.png) 

```
## Warning: Removed 1 rows containing missing values (geom_path).
```

![](048_oe_tdm_pull_files/figure-html/plots-9.png) 


```r
# spread to tidy format
out_tdm_q <- temp_h %>%
  data.frame(date=time(.), .) %>%
  gather(vargeo, value, -date) %>%
  separate(vargeo, c("var", "area_sh"), sep = "_", extra="merge") %>%
  spread(var, value) 
```


###Writing out files

```r
# writes csv versions of the output files
write.zoo(out_tdm_a, file="../output_data/out_tdm_a.csv", sep=",")
write.zoo(out_tdm_q, file="../output_data/out_tdm_q.csv", sep=",")
# saves Rdata versions of the output files
save(out_tdm_a, file="../output_data/out_tdm_a.Rdata")
save(out_tdm_q, file="../output_data/out_tdm_q.Rdata")
```

