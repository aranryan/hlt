# create hist
Tourism Economics  
Thursday, October 16, 2014  

Setup

```r
source('~/Project/R projects/hlt/scripts/functions.R')
```

Creates a us historical databank. Combines the STR data with selected macro data and calculates a few series

```r
fpath <- c("~/Project/R projects/hlt/") 
#macro data
  load(paste(fpath,"output_data/out_macro_q.Rdata", sep=""))
# str data
  load(paste(fpath,"output_data/out_str_chn_q.Rdata", sep="")) 
  load(paste(fpath,"output_data/out_str_chn_m.Rdata", sep=""))
# GCT data
  load(paste(fpath,"output_data/out_gct_a.Rdata", sep="")) 
  load(paste(fpath,"output_data/out_gct_q.Rdata", sep="")) 
# Asia Cities data
  load(paste(fpath,"output_data/out_acities_a.Rdata", sep="")) 
  load(paste(fpath,"output_data/out_acities_q.Rdata", sep="")) 
# Global Cities data
  load(paste(fpath,"output_data/out_gcities_a.Rdata", sep="")) 
  load(paste(fpath,"output_data/out_gcities_q.Rdata", sep="")) 
# TDM data
  load(paste(fpath,"output_data/out_tdm_a.Rdata", sep="")) 
  load(paste(fpath,"output_data/out_tdm_q.Rdata", sep=""))
```

Decided to move the segment information from being bundled with the geography, to be part of the lodging variable. I guess the reason is that then I would have a clean geo column. 


```r
str_q <- out_str_chn_q %>%
    data.frame(date=time(.), .) %>%
    gather(variable, value, -date) %>%
    # filter to drop the str_days variable
    filter(variable != "strdays") %>%
    # separate variable column
    separate(variable, c("seg", "variable"), 3) %>%
    separate(variable, c("geo", "variable"), sep="_") %>%
    # combine the seg and variable column
    mutate(variable = paste0(seg, variable)) %>%
    # create a single variable mnuemonic
    mutate(variable = paste(geo, variable, sep="_")) %>%
    select(-seg, -geo) %>%
    spread(variable, value) %>%
    read.zoo %>%
    as.xts
```


Calculate some real lodging series.

```r
# to start, bring all series from the macro databank
# temp <- out_macro_q %>%
#   data.frame(date=time(.), .) %>%
#   read.zoo %>%
#   as.xts
# head(temp)

# merges dataframes. the all.=TRUE piece ensures all the rows
# in the first dataframe are included
#hist_q <- merge(temp, str_q, all=TRUE) 

  
######################
#
# converts some series to real
#

# first index the personal cons price deflator to average 100 in 2014
chn_pcindex <- index_q(out_macro_q$chn_pc, index_year=2014)
names(chn_pcindex) <- "chn_pcindex"
str_q <- merge(str_q, chn_pcindex)
autoplot(str_q$chn_pcindex)
```

![](050_create_hist_files/figure-html/calc real-1.png) 

```r
# select the series that contain adr or revpar and convert to real
# the way this works is that matches works on a regular expression
# I wrote a regular expression that is taking columns with names that
# end with the text shown
# for reference on writing regular expressions, see
# http://www.regular-expressions.info/quickstart.html
real_df <- data.frame(str_q) %>%
  select(matches("adr$|adrsa$|revpar$|revparsa$")) %>% 
  mutate_each(funs(ind = ( . / chn_pcindex)*100))
# adds on a time index to get it back to xts
temp <- data.frame(date=time(str_q)) 
real_df <- cbind(temp,real_df)
real <- read.zoo(real_df)
real <- xts(real)

# renames series 
tempnames <- names(real)
tempnames <- paste(tempnames,"r",sep="")
tempnames
```

```
##  [1] "beijichn_upaadrr"      "beijichn_upaadrsar"   
##  [3] "beijichn_uparevparr"   "beijichn_uparevparsar"
##  [5] "chgduchn_upaadrr"      "chgduchn_upaadrsar"   
##  [7] "chgduchn_uparevparr"   "chgduchn_uparevparsar"
##  [9] "chn_upaadrr"           "chn_upaadrsar"        
## [11] "chn_uparevparr"        "chn_uparevparsar"     
## [13] "gngzhchn_upaadrr"      "gngzhchn_upaadrsar"   
## [15] "gngzhchn_uparevparr"   "gngzhchn_uparevparsar"
## [17] "sanyachn_upaadrr"      "sanyachn_upaadrsar"   
## [19] "sanyachn_uparevparr"   "sanyachn_uparevparsar"
## [21] "shangchn_upaadrr"      "shangchn_upaadrsar"   
## [23] "shangchn_uparevparr"   "shangchn_uparevparsar"
```

```r
names(real) <- tempnames
rm(tempnames)

autoplot(window(real$chn_upaadrsar, start="2000-01-01", end="2015-10-01"))
```

```
## Warning: Removed 38 rows containing missing values (geom_path).
```

![](050_create_hist_files/figure-html/calc real-2.png) 

```r
autoplot(window(str_q$chn_upaadrsa, start="2000-01-01", end="2015-10-01"))
```

```
## Warning: Removed 38 rows containing missing values (geom_path).
```

![](050_create_hist_files/figure-html/calc real-3.png) 

```r
# merges onto ushist_q
str_q <- merge(str_q, real)
autoplot(window(str_q$chn_upaadrsar, start="2000-01-01", end="2015-10-01"))
```

```
## Warning: Removed 38 rows containing missing values (geom_path).
```

![](050_create_hist_files/figure-html/calc real-4.png) 

Looking at what's in quarterly databank

```r
# which segments or markets are in the data frame, just for observation
# not used anywhere
a <- grep(pattern="demt$", colnames(str_q), value=TRUE)
a
```

```
## [1] "beijichn_upademt" "chgduchn_upademt" "chn_upademt"     
## [4] "gngzhchn_upademt" "sanyachn_upademt" "shangchn_upademt"
```

```r
a <- gsub(pattern="demt$",replacement="",a)
a
```

```
## [1] "beijichn_upa" "chgduchn_upa" "chn_upa"      "gngzhchn_upa"
## [5] "sanyachn_upa" "shangchn_upa"
```

```r
b <- grep(pattern="chn_", colnames(str_q), value=TRUE)
b
```

```
##   [1] "beijichn_upaadr"       "beijichn_upaadrsa"    
##   [3] "beijichn_upaadrsf"     "beijichn_upadays"     
##   [5] "beijichn_upademarsa"   "beijichn_upademd"     
##   [7] "beijichn_upademdsa"    "beijichn_upademdsf"   
##   [9] "beijichn_upademt"      "beijichn_upaocc"      
##  [11] "beijichn_upaoccsa"     "beijichn_upaoccsf"    
##  [13] "beijichn_uparevpar"    "beijichn_uparevparsa" 
##  [15] "beijichn_uparevparsf"  "beijichn_uparmrevt"   
##  [17] "beijichn_upasupd"      "beijichn_upasupdsa"   
##  [19] "beijichn_upasupdsf"    "beijichn_upasupt"     
##  [21] "chgduchn_upaadr"       "chgduchn_upaadrsa"    
##  [23] "chgduchn_upaadrsf"     "chgduchn_upadays"     
##  [25] "chgduchn_upademarsa"   "chgduchn_upademd"     
##  [27] "chgduchn_upademdsa"    "chgduchn_upademdsf"   
##  [29] "chgduchn_upademt"      "chgduchn_upaocc"      
##  [31] "chgduchn_upaoccsa"     "chgduchn_upaoccsf"    
##  [33] "chgduchn_uparevpar"    "chgduchn_uparevparsa" 
##  [35] "chgduchn_uparevparsf"  "chgduchn_uparmrevt"   
##  [37] "chgduchn_upasupd"      "chgduchn_upasupdsa"   
##  [39] "chgduchn_upasupdsf"    "chgduchn_upasupt"     
##  [41] "chn_upaadr"            "chn_upaadrsa"         
##  [43] "chn_upaadrsf"          "chn_upadays"          
##  [45] "chn_upademarsa"        "chn_upademd"          
##  [47] "chn_upademdsa"         "chn_upademdsf"        
##  [49] "chn_upademt"           "chn_upaocc"           
##  [51] "chn_upaoccsa"          "chn_upaoccsf"         
##  [53] "chn_uparevpar"         "chn_uparevparsa"      
##  [55] "chn_uparevparsf"       "chn_uparmrevt"        
##  [57] "chn_upasupd"           "chn_upasupdsa"        
##  [59] "chn_upasupdsf"         "chn_upasupt"          
##  [61] "gngzhchn_upaadr"       "gngzhchn_upaadrsa"    
##  [63] "gngzhchn_upaadrsf"     "gngzhchn_upadays"     
##  [65] "gngzhchn_upademarsa"   "gngzhchn_upademd"     
##  [67] "gngzhchn_upademdsa"    "gngzhchn_upademdsf"   
##  [69] "gngzhchn_upademt"      "gngzhchn_upaocc"      
##  [71] "gngzhchn_upaoccsa"     "gngzhchn_upaoccsf"    
##  [73] "gngzhchn_uparevpar"    "gngzhchn_uparevparsa" 
##  [75] "gngzhchn_uparevparsf"  "gngzhchn_uparmrevt"   
##  [77] "gngzhchn_upasupd"      "gngzhchn_upasupdsa"   
##  [79] "gngzhchn_upasupdsf"    "gngzhchn_upasupt"     
##  [81] "sanyachn_upaadr"       "sanyachn_upaadrsa"    
##  [83] "sanyachn_upaadrsf"     "sanyachn_upadays"     
##  [85] "sanyachn_upademarsa"   "sanyachn_upademd"     
##  [87] "sanyachn_upademdsa"    "sanyachn_upademdsf"   
##  [89] "sanyachn_upademt"      "sanyachn_upaocc"      
##  [91] "sanyachn_upaoccsa"     "sanyachn_upaoccsf"    
##  [93] "sanyachn_uparevpar"    "sanyachn_uparevparsa" 
##  [95] "sanyachn_uparevparsf"  "sanyachn_uparmrevt"   
##  [97] "sanyachn_upasupd"      "sanyachn_upasupdsa"   
##  [99] "sanyachn_upasupdsf"    "sanyachn_upasupt"     
## [101] "shangchn_upaadr"       "shangchn_upaadrsa"    
## [103] "shangchn_upaadrsf"     "shangchn_upadays"     
## [105] "shangchn_upademarsa"   "shangchn_upademd"     
## [107] "shangchn_upademdsa"    "shangchn_upademdsf"   
## [109] "shangchn_upademt"      "shangchn_upaocc"      
## [111] "shangchn_upaoccsa"     "shangchn_upaoccsf"    
## [113] "shangchn_uparevpar"    "shangchn_uparevparsa" 
## [115] "shangchn_uparevparsf"  "shangchn_uparmrevt"   
## [117] "shangchn_upasupd"      "shangchn_upasupdsa"   
## [119] "shangchn_upasupdsf"    "shangchn_upasupt"     
## [121] "chn_pcindex"           "beijichn_upaadrr"     
## [123] "beijichn_upaadrsar"    "beijichn_uparevparr"  
## [125] "beijichn_uparevparsar" "chgduchn_upaadrr"     
## [127] "chgduchn_upaadrsar"    "chgduchn_uparevparr"  
## [129] "chgduchn_uparevparsar" "chn_upaadrr"          
## [131] "chn_upaadrsar"         "chn_uparevparr"       
## [133] "chn_uparevparsar"      "gngzhchn_upaadrr"     
## [135] "gngzhchn_upaadrsar"    "gngzhchn_uparevparr"  
## [137] "gngzhchn_uparevparsar" "sanyachn_upaadrr"     
## [139] "sanyachn_upaadrsar"    "sanyachn_uparevparr"  
## [141] "sanyachn_uparevparsar" "shangchn_upaadrr"     
## [143] "shangchn_upaadrsar"    "shangchn_uparevparr"  
## [145] "shangchn_uparevparsar"
```

Creating quarterly dataframe

```r
str_qt <- str_q %>%
  data.frame(date=time(.), .) %>%
  gather(variable, value, -date) %>%
  separate(variable, c("area_sh", "variable"), sep = "_") %>%
  spread(variable, value)

# The challenge here is that I have a lodging dataframe, with a mix of cities
# and a country. Then I have a macro dataframe with just China. And then 
# I have some city dataframes that have both, and then I have the tdm
# dataframe with just China. Do I try to put them all together? Why?
# As part of the challenge, GDP for example, is in several of the dataframes
# so when I joined, I started ending up with several columns of GDP.
# My initial approach was to be careful. I was going to set up dataframes
# of what I'm taking from each source. With the goal of taking each concept
# only once for each geography, and then I'd figure out how to combine them

# Then I realized an approach of adding a source column to each source, 
# thencombining everything, and then filtering 
# to drop certain items, or keep only certain items, and then checking 
# what I had in the end. 

# take everything from the tidy str dataframe
take_str_q <- str_qt %>%
    gather(variable, value, -date, -area_sh) %>%
  mutate(src = "str")

# take everything from the macro dataframe
take_macro_q <- out_macro_q %>%
  data.frame(date=time(.), .) %>%
  gather(variable, value, -date) %>%
  separate(variable, c("area_sh", "variable"), sep = "_") %>%
  mutate(src = "macro")

# take GDP for some cities from Asia cities
take_acities_q <- out_acities_q %>%
  select(date, area_sh, gdp) %>%
  gather(variable, value, -date, -area_sh) %>%
  mutate(src = "acities")

# take GDP for other cities form Global cities
take_gcities_q <- out_gcities_q %>%
  select(date, area_sh, gdp) %>%
  gather(variable, value, -date, -area_sh) %>%
  mutate(src = "gcities")

# take everything from tdm
take_tdm_q <- out_tdm_q %>%
  gather(variable, value, -date, -area_sh) %>%
  mutate(src = "tdm")

# combine everything
temp_a <- bind_rows(take_str_q, take_macro_q, take_acities_q, take_gcities_q, take_tdm_q) %>%
  filter(!is.na(value)) 

# filter to remove certain rows that I know are being duplicated based on my sources
temp_b <- temp_a %>%
  # sort of amazing, but this gives me all records that don't meet
  # the condition of having gcities as a source and not being in the 
  # list. So of those with gcities as a source, I'm only keeping
  # chgduchn
  filter(!(src == "gcities" & !area_sh %in% c("chgduchn")))
  # the following was evidently not necessary
  #filter(!(src == "acities" & !area_sh %in% c("beijichn", "gngzhchn", "shangchn"))) %>%

# as long as check_dup is empty there are evidently no duplicates
# based partly on the following link, I removed the negative 
#https://www.quora.com/How-do-I-remove-entire-rows-of-duplicates-based-on-two-different-columns-in-a-R-dataframe
check_dup <- temp_b[duplicated(temp_b[,c('date', 'area_sh', 'variable')]),]


hist_q <- temp_b %>%
  select(-src) %>%
  spread(date, value) %>%
  mutate(indicator_code = paste(area_sh, variable, sep = "_")) %>%
  select(area_sh, variable, indicator_code, everything())
```




Create annual STR data

```r
# put quarterly dataframe back into a zoo form
temp_q <- hist_q %>%
  select(-area_sh, -variable) %>%
  gather(date, value, -indicator_code) %>%
  spread(indicator_code, value) %>%
  data.frame() %>%
  read.zoo(regular=TRUE, drop=FALSE) %>%
  as.xts

###########
#
# start by creating an annual version of lodging data

# select series that should be converted to annual by summing
# I wrote a regular expression that is looking for certain text strings
# for reference on writing regular expressions, see
# http://www.regular-expressions.info/quickstart.html
suma <- data.frame(temp_q) %>%
  select(matches("demt$|supt$|rmrevt$")) %>%
  as.xts()

# not sure why I needed this. Maybe it didn't like that it ended on a first quarter
suma <- window(suma, start = as.POSIXct("1980-01-01"), end = as.POSIXct("2029-10-01"))

# this function is one I defined, it converts all the columns in 
# an xts object to annual. Must be an xts object to start with
suma <- q_to_a_xts(suma, type="sum")

# takes the summed data and spreads it into a tidy format with
# tidyr and then calculates the occupancy and revpar series
# first needs to go from xts to dataframe
tb2 <- data.frame(date=time(suma), suma)%>% 
  # creates column called segvar that contains the column names, and one next to 
  # it with the values, dropping the time column
  gather(segvar, value, -date, na.rm = FALSE) %>%
  # in the following the ^ means anything not in the list
  # with the list being all characters and numbers
  # so it separates segvar into two colums using sep
  separate(segvar, c("area_sh", "variable"), sep = "[^[:alnum:]]+") %>%
  # added for China, separates off the chain scale segment
  separate(variable, c("seg", "variable"), sep = 3) %>%
  # keeps seg as a column and spreads variable into multiple columns containing
  # containint the values
  spread(variable,value) %>%
  # adds new calculated column
  mutate(occ = demt / supt) %>%
  # adds another column
  mutate(revpar = rmrevt / supt) %>%
  mutate(adr = rmrevt / demt)

# takes it from a tidy format, recreates the variable mnuemonic,
# and then creates the unique
# variable names and then reads into a zoo object spliting on the 
# second column
str_at  <- tb2 %>%
  gather(variable, value, -date, -area_sh,-seg) %>%
  mutate(variable = paste0(seg, variable)) %>%
  select(-seg)
```

Create annual databank

```r
# take everything from the tidy str dataframe
take_str_a <- str_at %>%
  mutate(src = "str") %>%
  mutate(variable = as.character(variable))

# take everything from the macro dataframe - doesn't exist would require work
# take_macro_a <- out_macro_a %>%
#   data.frame(date=time(.), .) %>%
#   gather(variable, value, -date) %>%
#   separate(variable, c("area_sh", "variable"), sep = "_") %>%
#   mutate(src = "macro")

# take GDP for some cities from Asia cities
take_acities_a <- out_acities_a %>%
  select(date, area_sh, gdp) %>%
  gather(variable, value, -date, -area_sh) %>%
  mutate(src = "acities") %>%
  mutate(variable = as.character(variable))

# take GDP for other cities form Global cities
take_gcities_a <- out_gcities_a %>%
  select(date, area_sh, gdp) %>%
  gather(variable, value, -date, -area_sh) %>%
  mutate(src = "gcities") %>%
  mutate(variable = as.character(variable))

# take everything from tdm
take_tdm_a <- out_tdm_a %>%
  gather(variable, value, -date, -area_sh) %>%
  mutate(src = "tdm") %>%
  mutate(variable = as.character(variable))

# combine everything
temp_a <- bind_rows(take_str_a, take_acities_a, take_gcities_a, take_tdm_a) %>%
  filter(!is.na(value)) 


# filter to remove certain rows that I know are being duplicated based on my sources
temp_b <- temp_a %>%
  # sort of amazing, but this gives me all records that don't meet
  # the condition of having gcities as a source and not being in the 
  # list. So of those with gcities as a source, I'm only keeping
  # chgduchn
  filter(!(src == "gcities" & !area_sh %in% c("chgduchn")))
  # the following was evidently not necessary
  #filter(!(src == "acities" & !area_sh %in% c("beijichn", "gngzhchn", "shangchn"))) %>%

# as long as check_dup is empty there are evidently no duplicates
# based partly on the following link, I removed the negative 
#https://www.quora.com/How-do-I-remove-entire-rows-of-duplicates-based-on-two-different-columns-in-a-R-dataframe
check_dup <- temp_b[duplicated(temp_b[,c('date', 'area_sh', 'variable')]),]

hist_a <- temp_b %>%
  select(-src) %>%
  spread(date, value) %>%
  mutate(indicator_code = paste(area_sh, variable, sep = "_")) %>%
  select(area_sh, variable, indicator_code, everything())
```

Creating monthly historical databank

```r
# not that much that needs to be done
hist_m <- out_str_chn_m
```

### Writing outputs

