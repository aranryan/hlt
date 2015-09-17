
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
#Sys.setenv(X13_PATH = "C:/Aran Installed/x13as")
#checkX13()
require("forecast")
require("car")
require("reshape2")
require("ggplot2")
require("tidyr")
require("plyr") #Hadley said if you load plyr first it should be fine
require("dplyr")
