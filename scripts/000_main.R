
# functions file loads functions that should be available at 
# at any point. If I clear the workspace, I need to run fuctions again
# before running the next program

# I have a couple auxillary function files
# load_str just contains a function
# also seasonal_factors contains functions to create monthly and 
# quarterly seasonal factors

source("scripts/setup.R")
source("scripts/functions.R")

# read raw STR monthly files
# results saved in Rdata file (dataframe)
rmarkdown::render('scripts/010_read_raw_str_chn_hlt.Rmd')

# create monthl and quarterly seasonal factors, only run if necessary
# starts with raw monthly data, no need for a quarterly file
# results saved in Rdata files (dataframe)
#source("scripts/015_create_seasonal.R")

# preps the STR data, including creating seasonal adjusted from factors
# results saved in Rdata (xts) and csv files 
source("scripts/020_prep_str.R")

# reads macro source file
# results saved in Rdata (xts) and csv files
rmarkdown::render('scripts/040_load_macro.Rmd')

# pulls GCT data
source("scripts/045_oe_gct_pull.R")

# pulls Asia Cities data
source("scripts/046_oe_acities_pull.R")

# pulls Global Cities data
source("scripts/047_oe_gcities_pull.R")

# pulls TDM data
rmarkdown::render('scripts/048_oe_tdm_pull.Rmd')

# create the hist file
rmarkdown::render('scripts/050_create_hist.Rmd')


# runs simple forecast 
#source("scripts/060_simple_forecast.R")

# reads in the current forecast 
source("scripts/070_read_usfor.R")

# creates a Rdata file with Fred data used in graphs
source("scripts/080_pull_fred_data.R")

# compiles some groups of top 25 markets
source("scripts/090_compile_top25.R")

# runs the markdown file
rmarkdown::render("scripts/100_us_overview_graphs.Rmd") 

# run several experiements with graph resolution
source("scripts/101_graph_resolution_test.R")

# creates an eviews output for host analysis
source("scripts/110_out_to_eviews.R")
source("scripts/120_build_lodhstbk.R")
