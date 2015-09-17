# load oef macro
Tourism Economics  
Wednesday, October 15, 2014  


Setup

```r
source('~/Project/R projects/hlt/scripts/functions.R')
```


As an input, this expects a csv file. This csv file can be created using the
select file that I've set up and the OE macro model.


```r
#sets up to later shorten data based on current date 
cur_year <- year(Sys.time())
end_year <- cur_year +15
end_year <- round(end_year,-1) -1
end_date <- paste(end_year,"-10-01",sep="")
```



```r
fpath <- c("~/Project/R projects/hlt/")

# when kniting from the button, I either needed the full path or to add "../" 
# in front of each

fname <- c("../input_data/HLTCHN_OEF_MACRO_2015_09_15.csv")
# the check.names piece fixes the issueof the column names coming in with
# quotes and spaces due to the Oxford file format that is visible when 
# you open the csv file in notepad
temp <- read.csv(fname, header=TRUE, sep=",", check.names=FALSE) 
# puts column names into lower case
names(temp) <- tolower(names(temp))
# trims leading and trailing whitespace
names(temp) <- str_trim(names(temp))
colnames(temp)
```

```
##  [1] "dates"       "cn_pc"       "cn_rrx"      "cn_rxd"      "cn_rxdppp"  
##  [6] "cn_pgdp"     "cn_pgdpser"  "cn_gdpsshot" "cn_gdp&"     "cn_gdpppp"  
## [11] "cn_gdp"      "cn_gdpsa"    "cn_gdpser&"  "cn_gdpser"   "cn_pedy"    
## [16] "cn_ipnr"     "cn_if"       "cn_penw"     "cn_rrxd"     "us_pgdp"    
## [21] "us_c"        "wd_gdp$&"    "wd_gdpppp"   "wd_gdp"
```

```r
# changes $ and & in the wd_gdp$& column name
col_temp <- colnames(temp) %>%
  gsub("\\$&", "nusd", .) %>%
  gsub("\\&", "n", .)
colnames(temp) <- col_temp
colnames(temp)
```

```
##  [1] "dates"       "cn_pc"       "cn_rrx"      "cn_rxd"      "cn_rxdppp"  
##  [6] "cn_pgdp"     "cn_pgdpser"  "cn_gdpsshot" "cn_gdpn"     "cn_gdpppp"  
## [11] "cn_gdp"      "cn_gdpsa"    "cn_gdpsern"  "cn_gdpser"   "cn_pedy"    
## [16] "cn_ipnr"     "cn_if"       "cn_penw"     "cn_rrxd"     "us_pgdp"    
## [21] "us_c"        "wd_gdpnusd"  "wd_gdpppp"   "wd_gdp"
```

```r
# works on the date column to get into a date format
# more difficult than I would have liked
temp <- temp %>%
  rename(date = dates) %>%
  mutate(date = gsub("01$", "-01-01", date)) %>%
  mutate(date = gsub("02$", "-04-01", date)) %>%
  mutate(date = gsub("03$", "-07-01", date)) %>%
  mutate(date = gsub("04$", "-10-01", date)) %>%
  mutate(date = as.Date(date))

temp_2 <- temp %>%
  gather(geovar, value, -date) %>%
  mutate(geovar = as.character(geovar)) %>%
  # I think this separates on the first occurance of the underscore, 
  # not all occurances
  separate(geovar, c("geo", "variable"), sep = "\\_", extra="merge") 

print(unique(temp_2$variable))
```

```
##  [1] "pc"       "rrx"      "rxd"      "rxdppp"   "pgdp"     "pgdpser" 
##  [7] "gdpsshot" "gdpn"     "gdpppp"   "gdp"      "gdpsa"    "gdpsern" 
## [13] "gdpser"   "pedy"     "ipnr"     "if"       "penw"     "rrxd"    
## [19] "c"        "gdpnusd"
```

```r
macro_q <- temp_2 %>%
  # renames "if" to "ifix" because r doesn't like "if" as a variable name
  mutate(variable = ifelse(variable == "if", "ifix", variable)) %>%
  # also, I didn't like wpo_wti as a variable name, because it has the 
  # underscore, which is distracting, and may cause trouble
  #mutate(variable = ifelse(variable == "wpo_wti", "wpowti", variable)) %>%
  # changes codes for canada...
  mutate(geo = ifelse(geo == "ca", "can", geo)) %>% # canada
  mutate(geo = ifelse(geo == "mx", "mex", geo)) %>% # mexico
  mutate(geo = ifelse(geo == "cn", "chn", geo)) %>% # china
  # recombine the geo and var
  mutate(geovar = paste(geo, variable, sep="_")) %>%
  select(-geo, -variable) %>%
  spread(geovar, value) %>%
  read.zoo() %>%
  xts(frequency=4)

# shortens data based on end date established at start of script
macro_q <- window(macro_q, end = end_date)
```

###A few plots
![](040_load_macro_files/figure-html/plots-1.png) ![](040_load_macro_files/figure-html/plots-2.png) ![](040_load_macro_files/figure-html/plots-3.png) ![](040_load_macro_files/figure-html/plots-4.png) ![](040_load_macro_files/figure-html/plots-5.png) ![](040_load_macro_files/figure-html/plots-6.png) ![](040_load_macro_files/figure-html/plots-7.png) ![](040_load_macro_files/figure-html/plots-8.png) 

```
## Warning: Removed 1 rows containing missing values (geom_path).
```

![](040_load_macro_files/figure-html/plots-9.png) 

```
## Warning: Removed 4 rows containing missing values (geom_path).
```

![](040_load_macro_files/figure-html/plots-10.png) 

```
## Warning: Removed 4 rows containing missing values (geom_path).
```

![](040_load_macro_files/figure-html/plots-11.png) 

```
## Warning: Removed 4 rows containing missing values (geom_path).
```

![](040_load_macro_files/figure-html/plots-12.png) 

```
##  num [1:81] NA NA 1170 1182 1218 ...
```

###Writing out files

```r
# writes csv versions of the output files
write.zoo(out_macro_q, file="../output_data/out_macro_q.csv", sep=",")
# saves Rdata versions of the output files
save(out_macro_q, file="../output_data/out_macro_q.Rdata")
```

