library(dplyr)
library(magrittr)
library(ggplot2)
library(zoo)
library(gridExtra)
library(data.table)
library(plotly)

## importing excel file excel
# wd <- c("W:/Projects In Action/40567 - Bazan Retrofit (BRR)/12. O&M/Aviv/5. Process/1.BRR Performance/2.Raw_data/2021/8'th report_29.09.2021 to 25.11.2021")
# setwd(wd)
# file.list <- list.files(pattern='*.xlsx', recursive = TRUE)
# df.list <- lapply(file.list,read_excel)
# df <- bind_rows(df.list)

## importing excel file CSV
wd <- c("C:/Users/Shayl/Desktop/My projects/DATA/data")
setwd(wd)
file_names <- dir(pattern = "*Analogs")
df <- do.call(rbind, lapply(file_names, read.csv, header = TRUE, sep = ",", quote = "'"))

# df <- sample_frac(df, size = .1)
# df <- df[!duplicated(df),]
# df$TimeStamp <- as.POSIXct(df$TimeStamp)
# df$year <- as.numeric(format(as.Date(df$TimeStamp), "%Y"))

# df <- sample_frac(df, size = .6)
# df <- df[!duplicated(df),]
# df$TimeStamp <- as.POSIXct(df$TimeStamp)



#### renaming ####

# renaming RO pass1
df <- df %>%
  rename(RO_PASS1_CARTRIDGE_DP_CULC. = DPT_3401_CALC_VAL0) %>%
  rename(RO_PASS1A_PRODUCT_FLOW = FIT_3502_01A_FL_VAL0) %>%
  rename(RO_PASS1B_PRODUCT_FLOW = FIT_3502_01B_FL_VAL0) %>%
  rename(RO_PASS1C_PRODUCT_FLOW = FIT_3502_01C_FL_VAL0) %>%
  rename(RO_PASS1D_PRODUCT_FLOW = FIT_3502_01D_FL_VAL0) %>%
  rename(RO_PASS1E_PRODUCT_FLOW = FIT_3502_01E_FL_VAL0) %>%
  rename(RO_PASS1A_CONC._FLOW = FIT_3502_02A_FL_VAL0) %>%
  rename(RO_PASS1B_CONC._FLOW = FIT_3502_02B_FL_VAL0) %>%
  rename(RO_PASS1C_CONC._FLOW =FIT_3502_02C_FL_VAL0) %>%
  rename(RO_PASS1D_CONC._FLOW = FIT_3502_02D_FL_VAL0) %>%
  rename(RO_PASS1E_CONC._FLOW = FIT_3502_02E_FL_VAL0) %>%
  rename(RO_PASS1A_2nd_STAGE_PRODUCT_FLOW = FIT_3502_03A_FL_VAL0) %>%
  rename(RO_PASS1B_2nd_STAGE_PRODUCT_FLOW = FIT_3502_03B_FL_VAL0) %>%
  rename(RO_PASS1C_2nd_STAGE_PRODUCT_FLOW = FIT_3502_03C_FL_VAL0) %>%
  rename(RO_PASS1D_2nd_STAGE_PRODUCT_FLOW = FIT_3502_03D_FL_VAL0) %>%
  rename(RO_PASS1E_2nd_STAGE_PRODUCT_FLOW = FIT_3502_03E_FL_VAL0) %>%
  rename(RO_PASS1_CARTRIDGE_INLET_PRESSURE = PIT_3401_01_VAL0) %>%
  rename(RO_PASS1_CARTRIDGE_OUTLET_PRESSURE = PIT_3401_02_VAL0) %>%
  rename(RO_PASS1A_INLET_PRESSURE = PIT_3502_01A_VAL0) %>%
  rename(RO_PASS1B_INLET_PRESSURE = PIT_3502_01B_VAL0) %>%
  rename(RO_PASS1C_INLET_PRESSURE = PIT_3502_01C_VAL0) %>%
  rename(RO_PASS1D_INLET_PRESSURE = PIT_3502_01D_VAL0) %>%
  rename(RO_PASS1E_INLET_PRESSURE = PIT_3502_01E_VAL0) %>%
  rename(RO_PASS1A_STAGE1_CONC._PRESSURE_TO_TURBO = PIT_3502_02A_VAL0) %>%
  rename(RO_PASS1B_STAGE1_CONC._PRESSURE_TO_TURBO = PIT_3502_02B_VAL0) %>%
  rename(RO_PASS1C_STAGE1_CONC._PRESSURE_TO_TURBO = PIT_3502_02C_VAL0) %>%
  rename(RO_PASS1D_STAGE1_CONC._PRESSURE_TO_TURBO = PIT_3502_02D_VAL0) %>%
  rename(RO_PASS1E_STAGE1_CONC._PRESSURE_TO_TURBO = PIT_3502_02E_VAL0) %>%
  rename(RO_PASS1A_STAGE2_INLET_PRESSURE_TURBO_OUTLET = PIT_3502_03A_VAL0) %>%
  rename(RO_PASS1B_STAGE2_INLET_PRESSURE_TURBO_OUTLET = PIT_3502_03B_VAL0) %>%
  rename(RO_PASS1C_STAGE2_INLET_PRESSURE_TURBO_OUTLET = PIT_3502_03C_VAL0) %>%
  rename(RO_PASS1D_STAGE2_INLET_PRESSURE_TURBO_OUTLET = PIT_3502_03D_VAL0) %>%
  rename(RO_PASS1E_STAGE2_INLET_PRESSURE_TURBO_OUTLET = PIT_3502_03E_VAL0) %>%
  rename(RO_PASS1A_STAGE2_CONC._PRESSURE_TO_TURBO = PIT_3502_04A_VAL0) %>%
  rename(RO_PASS1B_STAGE2_CONC._PRESSURE_TO_TURBO = PIT_3502_04B_VAL0) %>%
  rename(RO_PASS1C_STAGE2_CONC._PRESSURE_TO_TURBO = PIT_3502_04C_VAL0) %>%
  rename(RO_PASS1D_STAGE2_CONC._PRESSURE_TO_TURBO = PIT_3502_04D_VAL0) %>%
  rename(RO_PASS1E_STAGE2_CONC._PRESSURE_TO_TURBO = PIT_3502_04E_VAL0) %>%
  rename(RO_PASS1A_TURBO_LP_OUTLET = PIT_3502_05A_VAL0) %>%
  rename(RO_PASS1B_TURBO_LP_OUTLET = PIT_3502_05B_VAL0) %>%
  rename(RO_PASS1C_TURBO_LP_OUTLET = PIT_3502_05C_VAL0) %>%
  rename(RO_PASS1D_TURBO_LP_OUTLET = PIT_3502_05D_VAL0) %>%
  rename(RO_PASS1E_TURBO_LP_OUTLET = PIT_3502_05E_VAL0) %>%
  rename(RO_PASS1_FEED_ORP = AIT_3401_02_OR_VAL0) %>%
  rename(RO_PASS1A_PRODUCT_COND. = AIT_3502_01A_C01_VAL0) %>%
  rename(RO_PASS1A_CONC._pH  = AIT_3502_01A_H01_VAL0) %>%
  rename(RO_PASS1B_PRODUCT_COND. = AIT_3502_01B_C01_VAL0) %>%
  rename(RO_PASS1B_CONC._pH  = AIT_3502_01B_H01_VAL0) %>%
  rename(RO_PASS1C_PRODUCT_COND. = AIT_3502_01C_C01_VAL0) %>%
  rename(RO_PASS1C_CONC._pH  = AIT_3502_01C_H01_VAL0) %>%
  rename(RO_PASS1D_PRODUCT_COND. = AIT_3502_01D_C01_VAL0) %>%
  rename(RO_PASS1D_CONC._pH  = AIT_3502_01D_H01_VAL0) %>%
  rename(RO_PASS1E_PRODUCT_COND. = AIT_3502_01E_C01_VAL0) %>%
  rename(RO_PASS1E_CONC._pH  = AIT_3502_01E_H01_VAL0) %>%
  rename(RO_PASS1_CARTRIDGE_INLET_COND. = CIT_3401_01_C01_VAL0) %>%
  rename(RO_PASS1_CARTRIDGE_INLET_TEMP. = CIT_3401_01_T01_VAL0)

df <- df %>%
  mutate(Total_Pass1_Product_Flow = RO_PASS1A_PRODUCT_FLOW +
           RO_PASS1B_PRODUCT_FLOW +
           RO_PASS1C_PRODUCT_FLOW +
           RO_PASS1D_PRODUCT_FLOW +
           RO_PASS1E_PRODUCT_FLOW)

df <- df %>%
  mutate(RO_PASS1A_PRODUCT_PRESSURE = ifelse(Total_Pass1_Product_Flow < 200, -0.32, 0.00001*Total_Pass1_Product_Flow^2 - 0.0036*Total_Pass1_Product_Flow + 1.3367)) %>%
  mutate(RO_PASS1B_PRODUCT_PRESSURE = ifelse(Total_Pass1_Product_Flow < 200, 0.8534, 0.000009*Total_Pass1_Product_Flow^2 - 0.0029*Total_Pass1_Product_Flow + 1.0734)) %>%
  mutate(RO_PASS1C_PRODUCT_PRESSURE = ifelse(Total_Pass1_Product_Flow < 200, 0.6185, 0.000004*Total_Pass1_Product_Flow^2 + 0.0002*Total_Pass1_Product_Flow + 0.4185)) %>%
  mutate(RO_PASS1D_PRODUCT_PRESSURE = ifelse(Total_Pass1_Product_Flow < 200, 0.817, 0.000002*Total_Pass1_Product_Flow^2 + 0.0014*Total_Pass1_Product_Flow + 0.457)) %>%
  mutate(RO_PASS1E_PRODUCT_PRESSURE = ifelse(Total_Pass1_Product_Flow < 200, 0.7553, 0.000006*Total_Pass1_Product_Flow^2 - 0.0019*Total_Pass1_Product_Flow + 0.8953))


df <- df %>%
  mutate(RO_Pass1A_Feed_Flow = df$RO_PASS1A_PRODUCT_FLOW + df$RO_PASS1A_CONC._FLOW) %>%
  mutate(RO_Pass1B_Feed_Flow = df$RO_PASS1B_PRODUCT_FLOW + df$RO_PASS1B_CONC._FLOW) %>%
  mutate(RO_Pass1C_Feed_Flow = df$RO_PASS1C_PRODUCT_FLOW + df$RO_PASS1C_CONC._FLOW) %>%
  mutate(RO_Pass1D_Feed_Flow = df$RO_PASS1D_PRODUCT_FLOW + df$RO_PASS1D_CONC._FLOW) %>%
  mutate(RO_Pass1E_Feed_Flow = df$RO_PASS1E_PRODUCT_FLOW + df$RO_PASS1E_CONC._FLOW)

df <- df %>%
  mutate(RO_Pass1A_Stage1_Product_Flow = df$RO_PASS1A_PRODUCT_FLOW - df$RO_PASS1A_2nd_STAGE_PRODUCT_FLOW) %>%
  mutate(RO_Pass1B_Stage1_Product_Flow = df$RO_PASS1B_PRODUCT_FLOW - df$RO_PASS1B_2nd_STAGE_PRODUCT_FLOW) %>%
  mutate(RO_Pass1C_Stage1_Product_Flow = df$RO_PASS1C_PRODUCT_FLOW - df$RO_PASS1C_2nd_STAGE_PRODUCT_FLOW) %>%
  mutate(RO_Pass1D_Stage1_Product_Flow = df$RO_PASS1D_PRODUCT_FLOW - df$RO_PASS1D_2nd_STAGE_PRODUCT_FLOW) %>%
  mutate(RO_Pass1E_Stage1_Product_Flow = df$RO_PASS1E_PRODUCT_FLOW - df$RO_PASS1E_2nd_STAGE_PRODUCT_FLOW)

df <- df %>%
  mutate(RO_Pass1A_Stage2_Feed_Flow = df$RO_Pass1A_Feed_Flow - df$RO_Pass1A_Stage1_Product_Flow) %>%
  mutate(RO_Pass1B_Stage2_Feed_Flow = df$RO_Pass1B_Feed_Flow - df$RO_Pass1B_Stage1_Product_Flow) %>%
  mutate(RO_Pass1C_Stage2_Feed_Flow = df$RO_Pass1C_Feed_Flow - df$RO_Pass1C_Stage1_Product_Flow) %>%
  mutate(RO_Pass1D_Stage2_Feed_Flow = df$RO_Pass1D_Feed_Flow - df$RO_Pass1D_Stage1_Product_Flow) %>%
  mutate(RO_Pass1E_Stage2_Feed_Flow = df$RO_Pass1E_Feed_Flow - df$RO_Pass1E_Stage1_Product_Flow)
         
         
         
# renaming RO pass2
df <- df %>%
  rename(RO_PASS2_FEED_pH  = AIT_4401_01_H01_VAL0) %>%
  rename(RO_PASS2_CARTRIDGE_DP_CULC. = DPT_4401_CALC_VAL0) %>%
  rename(RO_PASS2_CARTRIDGE_OUTLET_COND. = CIT_4401_01_C01_VAL0) %>%
  rename(RO_PASS2_CARTRIDGE_OUTLET_TEMP. = CIT_4401_01_T01_VAL0) %>%
  rename(RO_PASS2A_PRODUCT_COND. = CIT_4502_01_C01_VAL0) %>%
  rename(RO_PASS2B_PRODUCT_COND. = CIT_4502_01_C02_VAL0) %>%
  rename(RO_PASS2C_PRODUCT_COND. = CIT_4502_01_C03_VAL0) %>%
  rename(RO_PASS2A_FEED_FLOW = FIT_4501_01A_FL_VAL0) %>%
  rename(RO_PASS2B_FEED_FLOW = FIT_4501_01B_FL_VAL0) %>%
  rename(RO_PASS2C_FEED_FLOW = FIT_4501_01C_FL_VAL0) %>%
  rename(RO_PASS2A_CONC._FLOW = FIT_4502_01A_FL_VAL0) %>%
  rename(RO_PASS2B_CONC._FLOW = FIT_4502_01B_FL_VAL0) %>%
  rename(RO_PASS2C_CONC._FLOW = FIT_4502_01C_FL_VAL0) %>%
  rename(RO_PASS2A_CULCULATED_PRODUCT_FLOW = FIT_4501_01A_PROD_VAL0) %>%
  rename(RO_PASS2B_CULCULATED_PRODUCT_FLOW = FIT_4501_01B_PROD_VAL0) %>%
  rename(RO_PASS2C_CULCULATED_PRODUCT_FLOW = FIT_4501_01C_PROD_VAL0) %>%
  rename(RO_PASS2_CARTRIDGE_INLET_PRESSURE = PIT_4401_01_VAL0) %>%
  rename(RO_PASS2_CARTRIDGE_OUTLET_PRESSURE = PIT_4401_02_VAL0) %>%
  rename(RO_PASS2A_FEED_PRESSURE = PIT_4502_01A_VAL0) %>%
  rename(RO_PASS2B_FEED_PRESSURE = PIT_4502_01B_VAL0) %>%
  rename(RO_PASS2C_FEED_PRESSURE = PIT_4502_01C_VAL0) %>%
  rename(RO_PASS2A_STAGE1_CONC._PRESSURE = PIT_4502_02A_VAL0) %>%
  rename(RO_PASS2B_STAGE1_CONC._PRESSURE = PIT_4502_02B_VAL0) %>%
  rename(RO_PASS2C_STAGE1_CONC._PRESSURE = PIT_4502_02C_VAL0) %>%
  rename(RO_PASS2A_STAGE2_CONC._PRESSURE = PIT_4502_03A_VAL0) %>%
  rename(RO_PASS2B_STAGE2_CONC._PRESSURE = PIT_4502_03B_VAL0) %>%
  rename(RO_PASS2C_STAGE2_CONC._PRESSURE = PIT_4502_03C_VAL0) %>%
  rename(RO_PASS2A_CONC._PRESSURE = PIT_4502_04A_VAL0) %>%
  rename(RO_PASS2B_CONC._PRESSURE = PIT_4502_04B_VAL0) %>%
  rename(RO_PASS2C_CONC._PRESSURE = PIT_4502_04C_VAL0) %>%
  rename(RO_PASS2A_PRODUCT_PRESSURE = PIT_4502_05A_VAL0) %>%
  rename(RO_PASS2B_PRODUCT_PRESSURE = PIT_4502_05B_VAL0) %>%
  rename(RO_PASS2C_PRODUCT_PRESSURE = PIT_4502_05C_VAL0) %>%
  rename(RO_PASS2_TOTAL_PRODUCT_PRESSURE = PIT_4701_01_VAL0)





#### end ####

## RO analysis

#### RO Parameters ####
RO1_recovery_factor <- 0.6
Membrain_per_PV <- 7
Tempratur_compensation <- 0.025
RO1_Concentration_factor <- log(1/(1-RO1_recovery_factor))/RO1_recovery_factor
RO1_Concentration_Polaritazation <- exp(0.75*2*RO1_recovery_factor/(2-RO1_recovery_factor))^(1/Membrain_per_PV)
RO1_Active_area <- 37 # m^2
RO1_Active_area_stage2 <- RO1_Active_area*35 #m^2
RO1_Active_area_stage1 <- RO1_Active_area*Membrain_per_PV #m^2

RO2_recovery_factor <- 0.9
RO2_Concentration_factor <- log(1/(1-RO2_recovery_factor))/RO2_recovery_factor
RO2_Concentration_Polaritazation <- exp(0.75*2*RO2_recovery_factor/(2-RO2_recovery_factor))^(1/Membrain_per_PV)
RO2_Active_area <- 37.15 # m^2
RO2_Active_area_stage3 <- RO2_Active_area*Membrain_per_PV*2 #m^2
RO2_Active_area_stage2 <- RO2_Active_area*Membrain_per_PV*4 #m^2
RO2_Active_area_stage1 <- RO2_Active_area*Membrain_per_PV*9 #m^2
RO2_total_active_area  <- sum(RO2_Active_area_stage1,RO2_Active_area_stage2,RO2_Active_area_stage3)
#### end ####

#### data manipulation for RO pass1 and 2 analysis ####

# recovery
RO_recovery <- function(RO) {
  data.frame(
    pass1A_1st_stage_recovery = ifelse(df$RO_PASS1A_PRODUCT_FLOW > 50,(RO$RO_PASS1A_PRODUCT_FLOW - RO$RO_PASS1A_2nd_STAGE_PRODUCT_FLOW)/(RO$RO_PASS1A_CONC._FLOW + RO$RO_PASS1A_PRODUCT_FLOW),NA),
    pass1B_1st_stage_recovery = ifelse(df$RO_PASS1B_PRODUCT_FLOW > 50,(RO$RO_PASS1B_PRODUCT_FLOW - RO$RO_PASS1B_2nd_STAGE_PRODUCT_FLOW)/(RO$RO_PASS1B_CONC._FLOW + RO$RO_PASS1B_PRODUCT_FLOW),NA),
    pass1C_1st_stage_recovery = ifelse(df$RO_PASS1C_PRODUCT_FLOW > 50,(RO$RO_PASS1C_PRODUCT_FLOW - RO$RO_PASS1C_2nd_STAGE_PRODUCT_FLOW)/(RO$RO_PASS1C_CONC._FLOW + RO$RO_PASS1C_PRODUCT_FLOW),NA),
    pass1D_1st_stage_recovery = ifelse(df$RO_PASS1D_PRODUCT_FLOW > 50,(RO$RO_PASS1D_PRODUCT_FLOW - RO$RO_PASS1D_2nd_STAGE_PRODUCT_FLOW)/(RO$RO_PASS1D_CONC._FLOW + RO$RO_PASS1D_PRODUCT_FLOW),NA),
    pass1E_1st_stage_recovery = ifelse(df$RO_PASS1E_PRODUCT_FLOW > 50,(RO$RO_PASS1E_PRODUCT_FLOW - RO$RO_PASS1E_2nd_STAGE_PRODUCT_FLOW)/(RO$RO_PASS1E_CONC._FLOW + RO$RO_PASS1E_PRODUCT_FLOW),NA),
    pass1A_2nd_stage_recovery = ifelse(df$RO_PASS1A_PRODUCT_FLOW > 50,RO$RO_PASS1A_2nd_STAGE_PRODUCT_FLOW/(RO$RO_PASS1A_CONC._FLOW - RO$RO_PASS1A_2nd_STAGE_PRODUCT_FLOW),NA),
    pass1B_2nd_stage_recovery = ifelse(df$RO_PASS1B_PRODUCT_FLOW > 50,RO$RO_PASS1B_2nd_STAGE_PRODUCT_FLOW/(RO$RO_PASS1B_CONC._FLOW - RO$RO_PASS1B_2nd_STAGE_PRODUCT_FLOW),NA),
    pass1C_2nd_stage_recovery = ifelse(df$RO_PASS1C_PRODUCT_FLOW > 50,RO$RO_PASS1C_2nd_STAGE_PRODUCT_FLOW/(RO$RO_PASS1C_CONC._FLOW - RO$RO_PASS1C_2nd_STAGE_PRODUCT_FLOW),NA),
    pass1D_2nd_stage_recovery = ifelse(df$RO_PASS1D_PRODUCT_FLOW > 50,RO$RO_PASS1D_2nd_STAGE_PRODUCT_FLOW/(RO$RO_PASS1D_CONC._FLOW - RO$RO_PASS1D_2nd_STAGE_PRODUCT_FLOW),NA),
    pass1E_2nd_stage_recovery = ifelse(df$RO_PASS1E_PRODUCT_FLOW > 50,RO$RO_PASS1E_2nd_STAGE_PRODUCT_FLOW/(RO$RO_PASS1E_CONC._FLOW - RO$RO_PASS1E_2nd_STAGE_PRODUCT_FLOW),NA),
    pass1A_recovery           = ifelse(df$RO_PASS1A_PRODUCT_FLOW > 50,RO$RO_PASS1A_PRODUCT_FLOW/(RO$RO_PASS1A_PRODUCT_FLOW + RO$RO_PASS1A_CONC._FLOW),NA),
    pass1B_recovery           = ifelse(df$RO_PASS1B_PRODUCT_FLOW > 50,RO$RO_PASS1B_PRODUCT_FLOW/(RO$RO_PASS1B_PRODUCT_FLOW + RO$RO_PASS1B_CONC._FLOW),NA),
    pass1C_recovery           = ifelse(df$RO_PASS1C_PRODUCT_FLOW > 50,RO$RO_PASS1C_PRODUCT_FLOW/(RO$RO_PASS1C_PRODUCT_FLOW + RO$RO_PASS1C_CONC._FLOW),NA),
    pass1D_recovery           = ifelse(df$RO_PASS1D_PRODUCT_FLOW > 50,RO$RO_PASS1D_PRODUCT_FLOW/(RO$RO_PASS1D_PRODUCT_FLOW + RO$RO_PASS1D_CONC._FLOW),NA),
    pass1E_recovery           = ifelse(df$RO_PASS1E_PRODUCT_FLOW > 50,RO$RO_PASS1E_PRODUCT_FLOW/(RO$RO_PASS1E_PRODUCT_FLOW + RO$RO_PASS1E_CONC._FLOW),NA),
    Pass2A_recovery           = RO$RO_PASS2A_CULCULATED_PRODUCT_FLOW/RO$RO_PASS2A_FEED_FLOW,
    Pass2B_recovery           = RO$RO_PASS2B_CULCULATED_PRODUCT_FLOW/RO$RO_PASS2B_FEED_FLOW,
    Pass2C_recovery           = RO$RO_PASS2C_CULCULATED_PRODUCT_FLOW/RO$RO_PASS2C_FEED_FLOW
  )
}

# DP pass1
RO_DP <- function(RO) {
  data.frame(
    DP_Pass1_Cartridge  = RO$RO_PASS1_CARTRIDGE_INLET_PRESSURE - RO$RO_PASS1_CARTRIDGE_OUTLET_PRESSURE,
    DP_pass1A_1st_stage = ifelse(df$RO_PASS1A_PRODUCT_FLOW > 50,RO$RO_PASS1A_INLET_PRESSURE - RO$RO_PASS1A_STAGE1_CONC._PRESSURE_TO_TURBO,NA),
    DP_pass1A_2nd_stage = ifelse(df$RO_PASS1A_PRODUCT_FLOW > 50,RO$RO_PASS1A_STAGE2_INLET_PRESSURE_TURBO_OUTLET - RO$RO_PASS1A_STAGE2_CONC._PRESSURE_TO_TURBO,NA),
    DP_pass1B_1st_stage = ifelse(df$RO_PASS1B_PRODUCT_FLOW > 50,RO$RO_PASS1B_INLET_PRESSURE - RO$RO_PASS1B_STAGE1_CONC._PRESSURE_TO_TURBO,NA),
    DP_pass1B_2nd_stage = ifelse(df$RO_PASS1B_PRODUCT_FLOW > 50,RO$RO_PASS1B_STAGE2_INLET_PRESSURE_TURBO_OUTLET - RO$RO_PASS1B_STAGE2_CONC._PRESSURE_TO_TURBO,NA),
    DP_pass1C_1st_stage = ifelse(df$RO_PASS1C_PRODUCT_FLOW > 50,RO$RO_PASS1C_INLET_PRESSURE - RO$RO_PASS1C_STAGE1_CONC._PRESSURE_TO_TURBO,NA),
    DP_pass1C_2nd_stage = ifelse(df$RO_PASS1C_PRODUCT_FLOW > 50,RO$RO_PASS1C_STAGE2_INLET_PRESSURE_TURBO_OUTLET - RO$RO_PASS1C_STAGE2_CONC._PRESSURE_TO_TURBO,NA),
    DP_pass1D_1st_stage = ifelse(df$RO_PASS1D_PRODUCT_FLOW > 50,RO$RO_PASS1D_INLET_PRESSURE - RO$RO_PASS1D_STAGE1_CONC._PRESSURE_TO_TURBO,NA),
    DP_pass1D_2nd_stage = ifelse(df$RO_PASS1D_PRODUCT_FLOW > 50,RO$RO_PASS1D_STAGE2_INLET_PRESSURE_TURBO_OUTLET - RO$RO_PASS1D_STAGE2_CONC._PRESSURE_TO_TURBO,NA),
    DP_pass1E_1st_stage = ifelse(df$RO_PASS1E_PRODUCT_FLOW > 50,RO$RO_PASS1E_INLET_PRESSURE - RO$RO_PASS1E_STAGE1_CONC._PRESSURE_TO_TURBO,NA),
    DP_pass1E_2nd_stage = ifelse(df$RO_PASS1E_PRODUCT_FLOW > 50,RO$RO_PASS1E_STAGE2_INLET_PRESSURE_TURBO_OUTLET - RO$RO_PASS1E_STAGE2_CONC._PRESSURE_TO_TURBO,NA),
    DP_pass2A           = RO$RO_PASS2A_FEED_PRESSURE - RO$RO_PASS2A_CONC._PRESSURE,
    DP_pass2B           = RO$RO_PASS2B_FEED_PRESSURE - RO$RO_PASS2B_CONC._PRESSURE,
    DP_pass2C           = RO$RO_PASS2C_FEED_PRESSURE - RO$RO_PASS2C_CONC._PRESSURE
  )
}

RO_Norm._DP <- function(RO,df) {
  data.frame(
    Norm_DP_pass1A_1st_stage = RO$DP_pass1A_1st_stage*((110^1.2)/(df$RO_PASS1A_PRODUCT_FLOW + df$RO_PASS1A_CONC._FLOW)^1.2),
    Norm_DP_pass1B_1st_stage = RO$DP_pass1B_1st_stage*((110^1.2)/(df$RO_PASS1B_PRODUCT_FLOW + df$RO_PASS1B_CONC._FLOW)^1.2),
    Norm_DP_pass1C_1st_stage = RO$DP_pass1C_1st_stage*((110^1.2)/(df$RO_PASS1C_PRODUCT_FLOW + df$RO_PASS1C_CONC._FLOW)^1.2),
    Norm_DP_pass1D_1st_stage = RO$DP_pass1D_1st_stage*((110^1.2)/(df$RO_PASS1D_PRODUCT_FLOW + df$RO_PASS1D_CONC._FLOW)^1.2),
    Norm_DP_pass1E_1st_stage = RO$DP_pass1E_1st_stage*((110^1.2)/(df$RO_PASS1E_PRODUCT_FLOW + df$RO_PASS1E_CONC._FLOW)^1.2),
    Norm_DP_pass1A_2st_stage = RO$DP_pass1A_2nd_stage*((65^1.2)/(df$RO_PASS1A_CONC._FLOW + df$RO_PASS1A_2nd_STAGE_PRODUCT_FLOW)^1.2),
    Norm_DP_pass1B_2st_stage = RO$DP_pass1B_2nd_stage*((65^1.2)/(df$RO_PASS1B_CONC._FLOW + df$RO_PASS1B_2nd_STAGE_PRODUCT_FLOW)^1.2),
    Norm_DP_pass1C_2st_stage = RO$DP_pass1C_2nd_stage*((65^1.2)/(df$RO_PASS1C_CONC._FLOW + df$RO_PASS1C_2nd_STAGE_PRODUCT_FLOW)^1.2),
    Norm_DP_pass1D_2st_stage = RO$DP_pass1D_2nd_stage*((65^1.2)/(df$RO_PASS1D_CONC._FLOW + df$RO_PASS1D_2nd_STAGE_PRODUCT_FLOW)^1.2),
    Norm_DP_pass1E_2st_stage = RO$DP_pass1E_2nd_stage*((65^1.2)/(df$RO_PASS1E_CONC._FLOW + df$RO_PASS1E_2nd_STAGE_PRODUCT_FLOW)^1.2),
    Norm_DP_pass2A           = RO$DP_pass2A*((103^1.2)/rowMeans(df[c("RO_PASS2A_FEED_FLOW","RO_PASS2A_CONC._FLOW")])^1.2),
    Norm_DP_pass2B           = RO$DP_pass2B*((103^1.2)/rowMeans(df[c("RO_PASS2B_FEED_FLOW","RO_PASS2B_CONC._FLOW")])^1.2),
    Norm_DP_pass2C           = RO$DP_pass2C*((103^1.2)/rowMeans(df[c("RO_PASS2C_FEED_FLOW","RO_PASS2C_CONC._FLOW")])^1.2)
  )
}

# Concentration factor
RO_CF <- function(RO){
  data.frame(
    CF_pass1A = ifelse(RO$pass1A_recovery > 0, log(1/RO$pass1A_recovery)/RO$pass1A_recovery,NA),
    CF_pass1B = ifelse(RO$pass1B_recovery > 0, log(1/RO$pass1B_recovery)/RO$pass1B_recovery,NA),
    CF_pass1C = ifelse(RO$pass1C_recovery > 0, log(1/RO$pass1C_recovery)/RO$pass1C_recovery,NA),
    CF_pass1D = ifelse(RO$pass1D_recovery > 0, log(1/RO$pass1D_recovery)/RO$pass1D_recovery,NA),
    CF_pass1E = ifelse(RO$pass1E_recovery > 0, log(1/RO$pass1E_recovery)/RO$pass1E_recovery,NA),
    CF_pass2A = ifelse(RO$Pass2A_recovery > 0, log(1/RO$Pass2A_recovery)/RO$Pass2A_recovery,NA),
    CF_pass2B = ifelse(RO$Pass2B_recovery > 0, log(1/RO$Pass2B_recovery)/RO$Pass2B_recovery,NA),
    CF_pass2C = ifelse(RO$Pass2C_recovery > 0, log(1/RO$Pass2C_recovery)/RO$Pass2C_recovery,NA),
    CF_pass1A_1st_stage = ifelse(RO$pass1A_1st_stage_recovery > 0, log(1/RO$pass1A_1st_stage_recovery)/RO$pass1A_1st_stage_recovery,NA),
    CF_pass1B_1st_stage = ifelse(RO$pass1B_1st_stage_recovery > 0, log(1/RO$pass1B_1st_stage_recovery)/RO$pass1B_1st_stage_recovery,NA),
    CF_pass1C_1st_stage = ifelse(RO$pass1C_1st_stage_recovery > 0, log(1/RO$pass1C_1st_stage_recovery)/RO$pass1C_1st_stage_recovery,NA),
    CF_pass1D_1st_stage = ifelse(RO$pass1D_1st_stage_recovery > 0, log(1/RO$pass1D_1st_stage_recovery)/RO$pass1D_1st_stage_recovery,NA),
    CF_pass1E_1st_stage = ifelse(RO$pass1E_1st_stage_recovery > 0, log(1/RO$pass1E_1st_stage_recovery)/RO$pass1E_1st_stage_recovery,NA),
    CF_pass1A_2nd_stage = ifelse(RO$pass1A_2nd_stage_recovery > 0, log(1/RO$pass1A_2nd_stage_recovery)/RO$pass1A_2nd_stage_recovery,NA),
    CF_pass1B_2nd_stage = ifelse(RO$pass1B_2nd_stage_recovery > 0, log(1/RO$pass1B_2nd_stage_recovery)/RO$pass1B_2nd_stage_recovery,NA),
    CF_pass1C_2nd_stage = ifelse(RO$pass1C_2nd_stage_recovery > 0, log(1/RO$pass1C_2nd_stage_recovery)/RO$pass1C_2nd_stage_recovery,NA),
    CF_pass1D_2nd_stage = ifelse(RO$pass1D_2nd_stage_recovery > 0, log(1/RO$pass1D_2nd_stage_recovery)/RO$pass1D_2nd_stage_recovery,NA),
    CF_pass1E_2nd_stage = ifelse(RO$pass1E_2nd_stage_recovery > 0, log(1/RO$pass1E_2nd_stage_recovery)/RO$pass1E_2nd_stage_recovery,NA)
  )
}  


# TDS after concentration 
RO_TDS <- function(RO,df){
  data.frame(
    TDS_pass1A_1st_stage = RO$CF_pass1A_1st_stage*df$RO_PASS1_CARTRIDGE_INLET_COND.*0.56,
    TDS_pass1B_1st_stage = RO$CF_pass1B_1st_stage*df$RO_PASS1_CARTRIDGE_INLET_COND.*0.56,
    TDS_pass1C_1st_stage = RO$CF_pass1C_1st_stage*df$RO_PASS1_CARTRIDGE_INLET_COND.*0.56,
    TDS_pass1D_1st_stage = RO$CF_pass1D_1st_stage*df$RO_PASS1_CARTRIDGE_INLET_COND.*0.56,
    TDS_pass1E_1st_stage = RO$CF_pass1E_1st_stage*df$RO_PASS1_CARTRIDGE_INLET_COND.*0.56,
    TDS_pass1A_2nd_stage = (df$RO_PASS1_CARTRIDGE_INLET_COND.*0.56/(1-RO$pass1A_1st_stage_recovery))*RO$CF_pass1A_2nd_stage,
    TDS_pass1B_2nd_stage = (df$RO_PASS1_CARTRIDGE_INLET_COND.*0.56/(1-RO$pass1B_1st_stage_recovery))*RO$CF_pass1B_2nd_stage,
    TDS_pass1C_2nd_stage = (df$RO_PASS1_CARTRIDGE_INLET_COND.*0.56/(1-RO$pass1C_1st_stage_recovery))*RO$CF_pass1C_2nd_stage,
    TDS_pass1D_2nd_stage = (df$RO_PASS1_CARTRIDGE_INLET_COND.*0.56/(1-RO$pass1D_1st_stage_recovery))*RO$CF_pass1D_2nd_stage,
    TDS_pass1E_2nd_stage = (df$RO_PASS1_CARTRIDGE_INLET_COND.*0.56/(1-RO$pass1E_1st_stage_recovery))*RO$CF_pass1E_2nd_stage,
    TDS_pass2A           = df$RO_PASS2_CARTRIDGE_OUTLET_COND.*0.56*RO$CF_pass2A,
    TDS_pass2B           = df$RO_PASS2_CARTRIDGE_OUTLET_COND.*0.56*RO$CF_pass2B,
    TDS_pass2C           = df$RO_PASS2_CARTRIDGE_OUTLET_COND.*0.56*RO$CF_pass2C,
    TDS_pass1A           = df$RO_PASS1_CARTRIDGE_INLET_COND.*0.56*RO$CF_pass1A,
    TDS_pass1B           = df$RO_PASS1_CARTRIDGE_INLET_COND.*0.56*RO$CF_pass1B,
    TDS_pass1C           = df$RO_PASS1_CARTRIDGE_INLET_COND.*0.56*RO$CF_pass1C,
    TDS_pass1D           = df$RO_PASS1_CARTRIDGE_INLET_COND.*0.56*RO$CF_pass1D,
    TDS_pass1E           = df$RO_PASS1_CARTRIDGE_INLET_COND.*0.56*RO$CF_pass1E
  )
}


# Osmotic Pressure
RO_OP <- function(RO,df){
  data.frame(
    OP_pass1A_1st_stage =  (0.0385*RO$TDS_pass1A_1st_stage*(273+df$RO_PASS1_CARTRIDGE_INLET_TEMP.))/((1000-RO$TDS_pass1A_1st_stage/1000))/14.5,
    OP_pass1B_1st_stage =  (0.0385*RO$TDS_pass1B_1st_stage*(273+df$RO_PASS1_CARTRIDGE_INLET_TEMP.))/((1000-RO$TDS_pass1B_1st_stage/1000))/14.5,
    OP_pass1C_1st_stage =  (0.0385*RO$TDS_pass1C_1st_stage*(273+df$RO_PASS1_CARTRIDGE_INLET_TEMP.))/((1000-RO$TDS_pass1C_1st_stage/1000))/14.5,
    OP_pass1D_1st_stage =  (0.0385*RO$TDS_pass1D_1st_stage*(273+df$RO_PASS1_CARTRIDGE_INLET_TEMP.))/((1000-RO$TDS_pass1D_1st_stage/1000))/14.5,
    OP_pass1E_1st_stage =  (0.0385*RO$TDS_pass1E_1st_stage*(273+df$RO_PASS1_CARTRIDGE_INLET_TEMP.))/((1000-RO$TDS_pass1E_1st_stage/1000))/14.5,
    OP_pass1A_2nd_stage =  (0.0385*RO$TDS_pass1A_2nd_stage*(273+df$RO_PASS1_CARTRIDGE_INLET_TEMP.))/((1000-RO$TDS_pass1A_2nd_stage/1000))/14.5,
    OP_pass1B_2nd_stage =  (0.0385*RO$TDS_pass1B_2nd_stage*(273+df$RO_PASS1_CARTRIDGE_INLET_TEMP.))/((1000-RO$TDS_pass1B_2nd_stage/1000))/14.5,
    OP_pass1C_2nd_stage =  (0.0385*RO$TDS_pass1C_2nd_stage*(273+df$RO_PASS1_CARTRIDGE_INLET_TEMP.))/((1000-RO$TDS_pass1C_2nd_stage/1000))/14.5,
    OP_pass1D_2nd_stage =  (0.0385*RO$TDS_pass1D_2nd_stage*(273+df$RO_PASS1_CARTRIDGE_INLET_TEMP.))/((1000-RO$TDS_pass1D_2nd_stage/1000))/14.5,
    OP_pass1E_2nd_stage =  (0.0385*RO$TDS_pass1E_2nd_stage*(273+df$RO_PASS1_CARTRIDGE_INLET_TEMP.))/((1000-RO$TDS_pass1E_2nd_stage/1000))/14.5,
    OP_pass1A           =  (0.0385*RO$TDS_pass1A*(273+df$RO_PASS1_CARTRIDGE_INLET_TEMP.))/((1000-RO$TDS_pass1A/1000))/14.5,
    OP_pass1B           =  (0.0385*RO$TDS_pass1B*(273+df$RO_PASS1_CARTRIDGE_INLET_TEMP.))/((1000-RO$TDS_pass1B/1000))/14.5,
    OP_pass1C           =  (0.0385*RO$TDS_pass1C*(273+df$RO_PASS1_CARTRIDGE_INLET_TEMP.))/((1000-RO$TDS_pass1C/1000))/14.5,
    OP_pass1D           =  (0.0385*RO$TDS_pass1D*(273+df$RO_PASS1_CARTRIDGE_INLET_TEMP.))/((1000-RO$TDS_pass1D/1000))/14.5,
    OP_pass1E           =  (0.0385*RO$TDS_pass1E*(273+df$RO_PASS1_CARTRIDGE_INLET_TEMP.))/((1000-RO$TDS_pass1E/1000))/14.5,
    OP_pass2A           =  (0.0385*RO$TDS_pass2A*(273+df$RO_PASS2_CARTRIDGE_OUTLET_TEMP.))/((1000-RO$TDS_pass2A/1000))/14.5,
    OP_pass2B           =  (0.0385*RO$TDS_pass2B*(273+df$RO_PASS2_CARTRIDGE_OUTLET_TEMP.))/((1000-RO$TDS_pass2B/1000))/14.5,
    OP_pass2C           =  (0.0385*RO$TDS_pass2C*(273+df$RO_PASS2_CARTRIDGE_OUTLET_TEMP.))/((1000-RO$TDS_pass2C/1000))/14.5
  )
}

# normalized permeability
RO_norm_perm <- function(RO,df,tp,Aas1,Aas2,tae2){
  data.frame(
    NPer_pass1A_1st_stage = ifelse(df$RO_PASS1A_STAGE1_CONC._PRESSURE_TO_TURBO > 0 & df$RO_PASS1A_INLET_PRESSURE > 0, ((((df$RO_PASS1A_PRODUCT_FLOW - df$RO_PASS1A_2nd_STAGE_PRODUCT_FLOW)*1000)/Aas1)/(rowMeans(df[c("RO_PASS1A_STAGE1_CONC._PRESSURE_TO_TURBO","RO_PASS1A_INLET_PRESSURE")])-df$RO_PASS1A_PRODUCT_PRESSURE-RO$OP_pass1A_1st_stage))/(1+tp*(df$RO_PASS1_CARTRIDGE_INLET_TEMP.-25)),NA),
    NPer_pass1B_1st_stage = ifelse(df$RO_PASS1B_STAGE1_CONC._PRESSURE_TO_TURBO > 0 & df$RO_PASS1B_INLET_PRESSURE > 0, ((((df$RO_PASS1B_PRODUCT_FLOW - df$RO_PASS1B_2nd_STAGE_PRODUCT_FLOW)*1000)/Aas1)/(rowMeans(df[c("RO_PASS1B_STAGE1_CONC._PRESSURE_TO_TURBO","RO_PASS1B_INLET_PRESSURE")])-df$RO_PASS1B_PRODUCT_PRESSURE-RO$OP_pass1B_1st_stage))/(1+tp*(df$RO_PASS1_CARTRIDGE_INLET_TEMP.-25)),NA),
    NPer_pass1C_1st_stage = ifelse(df$RO_PASS1C_STAGE1_CONC._PRESSURE_TO_TURBO > 0 & df$RO_PASS1C_INLET_PRESSURE > 0, ((((df$RO_PASS1C_PRODUCT_FLOW - df$RO_PASS1C_2nd_STAGE_PRODUCT_FLOW)*1000)/Aas1)/(rowMeans(df[c("RO_PASS1C_STAGE1_CONC._PRESSURE_TO_TURBO","RO_PASS1C_INLET_PRESSURE")])-df$RO_PASS1C_PRODUCT_PRESSURE-RO$OP_pass1C_1st_stage))/(1+tp*(df$RO_PASS1_CARTRIDGE_INLET_TEMP.-25)),NA),
    NPer_pass1D_1st_stage = ifelse(df$RO_PASS1D_STAGE1_CONC._PRESSURE_TO_TURBO > 0 & df$RO_PASS1D_INLET_PRESSURE > 0, ((((df$RO_PASS1D_PRODUCT_FLOW - df$RO_PASS1D_2nd_STAGE_PRODUCT_FLOW)*1000)/Aas1)/(rowMeans(df[c("RO_PASS1D_STAGE1_CONC._PRESSURE_TO_TURBO","RO_PASS1D_INLET_PRESSURE")])-df$RO_PASS1D_PRODUCT_PRESSURE-RO$OP_pass1D_1st_stage))/(1+tp*(df$RO_PASS1_CARTRIDGE_INLET_TEMP.-25)),NA),
    NPer_pass1E_1st_stage = ifelse(df$RO_PASS1E_STAGE1_CONC._PRESSURE_TO_TURBO > 0 & df$RO_PASS1E_INLET_PRESSURE > 0, ((((df$RO_PASS1E_PRODUCT_FLOW - df$RO_PASS1E_2nd_STAGE_PRODUCT_FLOW)*1000)/Aas1)/(rowMeans(df[c("RO_PASS1E_STAGE1_CONC._PRESSURE_TO_TURBO","RO_PASS1E_INLET_PRESSURE")])-df$RO_PASS1E_PRODUCT_PRESSURE-RO$OP_pass1E_1st_stage))/(1+tp*(df$RO_PASS1_CARTRIDGE_INLET_TEMP.-25)),NA),
    NPer_pass1A_2nd_stage = ifelse(df$RO_PASS1A_STAGE2_INLET_PRESSURE_TURBO_OUTLET > 0 & df$RO_PASS1A_STAGE2_CONC._PRESSURE_TO_TURBO > 0, (df$RO_PASS1A_2nd_STAGE_PRODUCT_FLOW*1000)/((Aas2*(rowMeans(df[c("RO_PASS1A_STAGE2_INLET_PRESSURE_TURBO_OUTLET","RO_PASS1A_STAGE2_CONC._PRESSURE_TO_TURBO")])-df$RO_PASS1A_PRODUCT_PRESSURE-RO$OP_pass1A_2nd_stage))*(1+tp*(df$RO_PASS1_CARTRIDGE_INLET_TEMP.-25))),NA),
    NPer_pass1B_2nd_stage = ifelse(df$RO_PASS1B_STAGE2_INLET_PRESSURE_TURBO_OUTLET > 0 & df$RO_PASS1B_STAGE2_CONC._PRESSURE_TO_TURBO > 0, (df$RO_PASS1B_2nd_STAGE_PRODUCT_FLOW*1000)/((Aas2*(rowMeans(df[c("RO_PASS1B_STAGE2_INLET_PRESSURE_TURBO_OUTLET","RO_PASS1B_STAGE2_CONC._PRESSURE_TO_TURBO")])-df$RO_PASS1B_PRODUCT_PRESSURE-RO$OP_pass1B_2nd_stage))*(1+tp*(df$RO_PASS1_CARTRIDGE_INLET_TEMP.-25))),NA),
    NPer_pass1C_2nd_stage = ifelse(df$RO_PASS1C_STAGE2_INLET_PRESSURE_TURBO_OUTLET > 0 & df$RO_PASS1C_STAGE2_CONC._PRESSURE_TO_TURBO > 0, (df$RO_PASS1C_2nd_STAGE_PRODUCT_FLOW*1000)/((Aas2*(rowMeans(df[c("RO_PASS1C_STAGE2_INLET_PRESSURE_TURBO_OUTLET","RO_PASS1C_STAGE2_CONC._PRESSURE_TO_TURBO")])-df$RO_PASS1C_PRODUCT_PRESSURE-RO$OP_pass1C_2nd_stage))*(1+tp*(df$RO_PASS1_CARTRIDGE_INLET_TEMP.-25))),NA),
    NPer_pass1D_2nd_stage = ifelse(df$RO_PASS1D_STAGE2_INLET_PRESSURE_TURBO_OUTLET > 0 & df$RO_PASS1D_STAGE2_CONC._PRESSURE_TO_TURBO > 0, (df$RO_PASS1D_2nd_STAGE_PRODUCT_FLOW*1000)/((Aas2*(rowMeans(df[c("RO_PASS1D_STAGE2_INLET_PRESSURE_TURBO_OUTLET","RO_PASS1D_STAGE2_CONC._PRESSURE_TO_TURBO")])-df$RO_PASS1D_PRODUCT_PRESSURE-RO$OP_pass1D_2nd_stage))*(1+tp*(df$RO_PASS1_CARTRIDGE_INLET_TEMP.-25))),NA),
    NPer_pass1E_2nd_stage = ifelse(df$RO_PASS1E_STAGE2_INLET_PRESSURE_TURBO_OUTLET > 0 & df$RO_PASS1E_STAGE2_CONC._PRESSURE_TO_TURBO > 0, (df$RO_PASS1E_2nd_STAGE_PRODUCT_FLOW*1000)/((Aas2*(rowMeans(df[c("RO_PASS1E_STAGE2_INLET_PRESSURE_TURBO_OUTLET","RO_PASS1E_STAGE2_CONC._PRESSURE_TO_TURBO")])-df$RO_PASS1E_PRODUCT_PRESSURE-RO$OP_pass1E_2nd_stage))*(1+tp*(df$RO_PASS1_CARTRIDGE_INLET_TEMP.-25))),NA),
    NPer_pass2A           = ifelse(df$RO_PASS2A_FEED_PRESSURE >0 & df$RO_PASS2A_STAGE1_CONC._PRESSURE > 0 & df$RO_PASS2A_STAGE2_CONC._PRESSURE > 0 & df$RO_PASS2A_CONC._PRESSURE > 0, (df$RO_PASS2A_CULCULATED_PRODUCT_FLOW*1000)/(tae2*(rowMeans(df[c("RO_PASS2A_FEED_PRESSURE","RO_PASS2A_STAGE1_CONC._PRESSURE","RO_PASS2A_STAGE2_CONC._PRESSURE","RO_PASS2A_CONC._PRESSURE")])-df$RO_PASS2A_PRODUCT_PRESSURE)*(1+tp*(df$RO_PASS2_CARTRIDGE_OUTLET_TEMP.-25))),NA),
    NPer_pass2B           = ifelse(df$RO_PASS2B_FEED_PRESSURE >0 & df$RO_PASS2B_STAGE1_CONC._PRESSURE > 0 & df$RO_PASS2B_STAGE2_CONC._PRESSURE > 0 & df$RO_PASS2B_CONC._PRESSURE > 0, (df$RO_PASS2B_CULCULATED_PRODUCT_FLOW*1000)/(tae2*(rowMeans(df[c("RO_PASS2B_FEED_PRESSURE","RO_PASS2B_STAGE1_CONC._PRESSURE","RO_PASS2B_STAGE2_CONC._PRESSURE","RO_PASS2B_CONC._PRESSURE")])-df$RO_PASS2B_PRODUCT_PRESSURE)*(1+tp*(df$RO_PASS2_CARTRIDGE_OUTLET_TEMP.-25))),NA),
    NPer_pass2C           = ifelse(df$RO_PASS2C_FEED_PRESSURE >0 & df$RO_PASS2C_STAGE1_CONC._PRESSURE > 0 & df$RO_PASS2C_STAGE2_CONC._PRESSURE > 0 & df$RO_PASS2C_CONC._PRESSURE > 0, (df$RO_PASS2C_CULCULATED_PRODUCT_FLOW*1000)/(tae2*(rowMeans(df[c("RO_PASS2C_FEED_PRESSURE","RO_PASS2C_STAGE1_CONC._PRESSURE","RO_PASS2C_STAGE2_CONC._PRESSURE","RO_PASS2C_CONC._PRESSURE")])-df$RO_PASS2C_PRODUCT_PRESSURE)*(1+tp*(df$RO_PASS2_CARTRIDGE_OUTLET_TEMP.-25))),NA)
  )
}

#### end ####

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

RO_analysis <- bind_cols(RO_recovery(df), RO_DP(df))
RO_analysis <- bind_cols(RO_analysis, RO_CF(RO_analysis))
RO_analysis <- bind_cols(RO_analysis, RO_TDS(RO_analysis,df))
RO_analysis <- bind_cols(RO_analysis, RO_OP(RO_analysis,df))
RO_analysis <- bind_cols(RO_analysis, RO_Norm._DP(RO_analysis,df))
RO_analysis <- bind_cols(RO_analysis, RO_norm_perm(
 RO_analysis,df,Tempratur_compensation,RO1_Active_area_stage1,RO1_Active_area_stage2,RO2_total_active_area))
RO_analysis = as.data.frame(apply(RO_analysis, 2, remove_outliers ))
RO_analysis <- RO_analysis %>%
  mutate(TimeStamp = as.POSIXct(df$TimeStamp)) #%>%
  #mutate(year = df$year)

RO_analysis <- na.locf(RO_analysis)

####  plots ####

wd <- c("C:/Users/Shayl/Desktop/My projects/Plots")
setwd(wd)

#### DP pass1 1st stage ####
DP_pass1_1st_stage <- 
  ggplot(data = RO_analysis,
         aes(x = TimeStamp),
         size = 1) +
  geom_smooth(aes(y = Norm_DP_pass1A_1st_stage, color = "RO A"), size = 2) +
  geom_smooth(aes(y = Norm_DP_pass1B_1st_stage, color = "RO B"), size = 2) +
  geom_smooth(aes(y = Norm_DP_pass1C_1st_stage, color = "RO C"), size = 2) +
  geom_smooth(aes(y = Norm_DP_pass1D_1st_stage, color = "RO D"), size = 2) +
  geom_smooth(aes(y = Norm_DP_pass1E_1st_stage, color = "RO E"), size = 2) +
  labs(x = 'Time',
       y = "Nornalized DP [bar/lmh]",
       title = "DP pass1 stage1") + 
  scale_colour_manual(name = 'RO unit', 
                      values =c('RO A'='red','RO B'='green','RO C'='blue','RO D'='orange','RO E'='black'),
                      labels = c('RO_A','RO_B','RO_C','RO_D','RO_E'))
# ggplotly(DP_pass1_1st_stage) %>%
#   layout(hovermode = FALSE)

#### end ####

#### DP pass1 2nd stage ####
DP_pass1_2st_stage <- 
    ggplot(data = RO_analysis,
               aes(x = TimeStamp),
                 size = 0.5) +
  geom_smooth(aes(y = Norm_DP_pass1A_2st_stage, color = "RO A"), size = 2) +
  geom_smooth(aes(y = Norm_DP_pass1B_2st_stage, color = "RO B"), size = 2) +
  geom_smooth(aes(y = Norm_DP_pass1C_2st_stage, color = "RO C"), size = 2) +
  geom_smooth(aes(y = Norm_DP_pass1D_2st_stage, color = "RO D"), size = 2) +
  geom_smooth(aes(y = Norm_DP_pass1E_2st_stage, color = "RO E"), size = 2) +
    labs(x = 'Time',
         y = "Nornalized DP [bar/lmh]",
         title = "DP pass1 stage2") + 
  scale_colour_manual(name = 'RO unit', 
                      values =c('RO A'='red','RO B'='green','RO C'='blue','RO D'='orange','RO E'='black'),
                      labels = c('RO_A','RO_B','RO_C','RO_D','RO_E'))
# ggplotly(DP_pass1_2st_stage) %>%
#   layout(hovermode = FALSE)




#### Permability pass1 1st stage
Prem_pass1_1st_stage <- 
  ggplot(data = RO_analysis,
         aes(x = TimeStamp),
         size = .5) +
  geom_smooth(aes(y = NPer_pass1A_1st_stage, color = "RO A"), size = 2) +
  geom_smooth(aes(y = NPer_pass1B_1st_stage, color = "RO B"), size = 2) +
  geom_smooth(aes(y = NPer_pass1C_1st_stage, color = "RO C"), size = 2) +
  geom_smooth(aes(y = NPer_pass1D_1st_stage, color = "RO D"), size = 2) +
  geom_smooth(aes(y = NPer_pass1E_1st_stage, color = "RO E"), size = 2) +
  ylim(15,30) +
  labs(x = 'Time',
       y = "Nornalized Permabeality",
       title = "Permabeality pass1 stage1") + 
  scale_colour_manual(name = 'RO unit', 
                      values =c('RO A'='red','RO B'='green','RO C'='blue','RO D'='orange','RO E'='black'),
                      labels = c('RO_A','RO_B','RO_C','RO_D','RO_E'))



#### Permability pass1 2nd stage
Perm_pass1_2nd_stage <- 
  ggplot(data = RO_analysis,
         aes(x = TimeStamp),
         size = .5) +
  geom_smooth(aes(y = NPer_pass1A_2nd_stage, color = "RO A"), size = 2) +
  geom_smooth(aes(y = NPer_pass1B_2nd_stage, color = "RO B"), size = 2) +
  geom_smooth(aes(y = NPer_pass1C_2nd_stage, color = "RO C"), size = 2) +
  geom_smooth(aes(y = NPer_pass1D_2nd_stage, color = "RO D"), size = 2) +
  geom_smooth(aes(y = NPer_pass1E_2nd_stage, color = "RO E"), size = 2) +
  ylim(1,2.5) +
  labs(x = 'Time',
       y = "Nornalized Permabeality",
       title = "Permabeality pass1 stage2") + 
  scale_colour_manual(name = 'RO unit', 
                      values =c('RO A'='red','RO B'='green','RO C'='blue','RO D'='orange','RO E'='black'),
                      labels = c('RO_A','RO_B','RO_C','RO_D','RO_E'))

P = grid.arrange(Prem_pass1_1st_stage, Perm_pass1_2nd_stage, ncol=1)
ggsave(P, file=paste0("RO_Perm_", format(Sys.time(), "%d-%b-%Y"), ".png"))
#### end ####


#### DP plot ####
DP_pass1_2st_stage <- plot_ly(RO_analysis, x = ~TimeStamp)
DP_pass1_2st_stage <- DP_pass1_2st_stage %>% add_lines(y = ~Norm_DP_pass1A_2st_stage, name = "RO A")
DP_pass1_2st_stage <- DP_pass1_2st_stage %>% add_lines(y = ~Norm_DP_pass1B_2st_stage, name = "RO B")
DP_pass1_2st_stage <- DP_pass1_2st_stage %>% add_lines(y = ~Norm_DP_pass1C_2st_stage, name = "RO C")
DP_pass1_2st_stage <- DP_pass1_2st_stage %>% add_lines(y = ~Norm_DP_pass1D_2st_stage, name = "RO D")
DP_pass1_2st_stage <- DP_pass1_2st_stage %>% add_lines(y = ~Norm_DP_pass1E_2st_stage, name = "RO E")
DP_pass1_2st_stage <- DP_pass1_2st_stage %>% layout(
  legend=list(title=list(text='<b> RO unit </b>')),
  title = "DP pass 1 2nd stage",
  xaxis = list(
    rangeselector = list(
      buttons = list(
        list(
          count = 1,
          label = "1 mo",
          step = "month",
          stepmode = "backward"),
        list(
          count = 3,
          label = "3 mo",
          step = "month",
          stepmode = "backward"),
        list(
          count = 1,
          label = "YTD",
          step = "year",
          stepmode = "todate"),
        list(step = "all"))),
    
    rangeslider = list(type = "date")),
  yaxis = list(title = "DP [bar]"))

htmlwidgets::saveWidget(as_widget(DP_pass1_2st_stage), paste0("DP 1st stage ", format(Sys.time(), "%d-%b-%Y"), ".html"))


DP_pass1_1st_stage <- plot_ly(RO_analysis, x = ~TimeStamp)
DP_pass1_1st_stage <- DP_pass1_1st_stage %>% add_lines(y = ~Norm_DP_pass1A_1st_stage, name = "RO A")
DP_pass1_1st_stage <- DP_pass1_1st_stage %>% add_lines(y = ~Norm_DP_pass1B_1st_stage, name = "RO B")
DP_pass1_1st_stage <- DP_pass1_1st_stage %>% add_lines(y = ~Norm_DP_pass1C_1st_stage, name = "RO C")
DP_pass1_1st_stage <- DP_pass1_1st_stage %>% add_lines(y = ~Norm_DP_pass1D_1st_stage, name = "RO D")
DP_pass1_1st_stage <- DP_pass1_1st_stage %>% add_lines(y = ~Norm_DP_pass1E_1st_stage, name = "RO E")
DP_pass1_1st_stage <- DP_pass1_1st_stage %>% layout(
  legend=list(title=list(text='<b> RO unit </b>')),
  title = "DP pass 1 1st stage",
  xaxis = list(
    rangeselector = list(
      buttons = list(
        list(
          count = 1,
          label = "1 mo",
          step = "month",
          stepmode = "backward"),
        list(
          count = 3,
          label = "3 mo",
          step = "month",
          stepmode = "backward"),
        list(
          count = 1,
          label = "YTD",
          step = "year",
          stepmode = "todate"),
        list(step = "all"))),
    
    rangeslider = list(type = "date")),
  yaxis = list(title = "DP [bar]"))

htmlwidgets::saveWidget(as_widget(DP_pass1_2st_stage), paste0("DP 2nd stage ", format(Sys.time(), "%d-%b-%Y"), ".html"))

#### end ####

#### permeability plots ####
DP_pass1_2st_stage <- plot_ly(RO_analysis, x = ~TimeStamp)
DP_pass1_2st_stage <- DP_pass1_2st_stage %>% add_lines(y = ~Norm_DP_pass1A_2st_stage, name = "RO A")
DP_pass1_2st_stage <- DP_pass1_2st_stage %>% add_lines(y = ~Norm_DP_pass1B_2st_stage, name = "RO B")
DP_pass1_2st_stage <- DP_pass1_2st_stage %>% add_lines(y = ~Norm_DP_pass1C_2st_stage, name = "RO C")
DP_pass1_2st_stage <- DP_pass1_2st_stage %>% add_lines(y = ~Norm_DP_pass1D_2st_stage, name = "RO D")
DP_pass1_2st_stage <- DP_pass1_2st_stage %>% add_lines(y = ~Norm_DP_pass1E_2st_stage, name = "RO E")
DP_pass1_2st_stage <- DP_pass1_2st_stage %>% layout(
  legend=list(title=list(text='<b> RO unit </b>')),
  title = "DP pass 1 2nd stage",
  xaxis = list(
    rangeselector = list(
      buttons = list(
        list(
          count = 1,
          label = "1 mo",
          step = "month",
          stepmode = "backward"),
        list(
          count = 3,
          label = "3 mo",
          step = "month",
          stepmode = "backward"),
        list(
          count = 1,
          label = "YTD",
          step = "year",
          stepmode = "todate"),
        list(step = "all"))),
    
    rangeslider = list(type = "date")),
  yaxis = list(title = "DP [bar]"))

htmlwidgets::saveWidget(as_widget(DP_pass1_2st_stage), paste0("DP 1st stage ", format(Sys.time(), "%d-%b-%Y"), ".html"))


DP_pass1_1st_stage <- plot_ly(RO_analysis, x = ~TimeStamp)
DP_pass1_1st_stage <- DP_pass1_1st_stage %>% add_lines(y = ~Norm_DP_pass1A_1st_stage, name = "RO A")
DP_pass1_1st_stage <- DP_pass1_1st_stage %>% add_lines(y = ~Norm_DP_pass1B_1st_stage, name = "RO B")
DP_pass1_1st_stage <- DP_pass1_1st_stage %>% add_lines(y = ~Norm_DP_pass1C_1st_stage, name = "RO C")
DP_pass1_1st_stage <- DP_pass1_1st_stage %>% add_lines(y = ~Norm_DP_pass1D_1st_stage, name = "RO D")
DP_pass1_1st_stage <- DP_pass1_1st_stage %>% add_lines(y = ~Norm_DP_pass1E_1st_stage, name = "RO E")
DP_pass1_1st_stage <- DP_pass1_1st_stage %>% layout(
  legend=list(title=list(text='<b> RO unit </b>')),
  title = "DP pass 1 1st stage",
  xaxis = list(
    rangeselector = list(
      buttons = list(
        list(
          count = 1,
          label = "1 mo",
          step = "month",
          stepmode = "backward"),
        list(
          count = 3,
          label = "3 mo",
          step = "month",
          stepmode = "backward"),
        list(
          count = 1,
          label = "YTD",
          step = "year",
          stepmode = "todate"),
        list(step = "all"))),
    
    rangeslider = list(type = "date")),
  yaxis = list(title = "DP [bar]"))

#### end ####

htmlwidgets::saveWidget(as_widget(DP_pass1_2st_stage), paste0("DP 2nd stage ", format(Sys.time(), "%d-%b-%Y"), ".html"))

#### end ####
