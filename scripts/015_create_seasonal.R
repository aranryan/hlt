
load("output_data/raw_str_hlt_chn.Rdata")

# Begins the same as the load_str step, using the load_str function
# So it can work with the raw monthly data, because it runs the full
# load_str process on it and then proceeds from there.
temp_str <- load_str(raw_str_hlt_chn)

# these two data frames are the working data frames
str_m <- temp_str[[1]]
str_q <- temp_str[[2]]

# drops series that aren't going to be adjusted
str_m <- select(str_m, 
                -ends_with("_days"), 
                -ends_with("_demt"), 
                -ends_with("_rmrevt"), 
                -ends_with("_supt"))
dont_m_cols <- c("upachgduchn_supd|upasanyachn_supd")

str_q <- select(str_q, 
                -ends_with("_days"), 
                -ends_with("_demt"), 
                -ends_with("_rmrevt"), 
                -ends_with("_supt"))

dont_q_cols <- c("temp_supd")

# creates seasonal factors and saves as Rdata files
# monthly
str_hlt_chn_m_factors <- seas_factors_m(str_m, dont_m_cols)
save(str_hlt_chn_m_factors, file="output_data/str_hlt_chn_m_factors.Rdata")
# quarterly
str_hlt_chn_q_factors <- seas_factors_q(str_q, dont_q_cols)
save(str_hlt_chn_q_factors, file="output_data/str_hlt_chn_q_factors.Rdata")
