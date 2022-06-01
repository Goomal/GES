library(dplyr)
library(magrittr)
library(ggplot2)
library(gridExtra)

# importing excel file
## import and bind csv files
wd <- c("C:/Users/Bazan_Lab/Desktop/My projects/UF_perm/06.10.2021 to 30.10.2021")
setwd(wd)
file_names <- dir(wd)
df <- do.call(rbind, lapply(file_names, read.csv, header = TRUE, sep = ",", quote = "'"))
df <- df[!duplicated(df),]
df$TimeStamp <- as.POSIXct(df$TimeStamp)

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
  mutate(RO_PASS1A_PRODUCT_PRESSURE = ifelse(Total_Pass1_Product_Flow < 200,-0.32,0.00001*Total_Pass1_Product_Flow^2 - 0.0036*Total_Pass1_Product_Flow + 1.3367)) %>%
  mutate(RO_PASS1B_PRODUCT_PRESSURE = ifelse(Total_Pass1_Product_Flow < 200,0.8534,0.000009*Total_Pass1_Product_Flow^2 - 0.0029*Total_Pass1_Product_Flow + 1.0734)) %>%
  mutate(RO_PASS1C_PRODUCT_PRESSURE = ifelse(Total_Pass1_Product_Flow < 200,0.6185,0.000004*Total_Pass1_Product_Flow^2 + 0.0002*Total_Pass1_Product_Flow + 0.4185)) %>%
  mutate(RO_PASS1D_PRODUCT_PRESSURE = ifelse(Total_Pass1_Product_Flow < 200,0.817,0.000002*Total_Pass1_Product_Flow^2 + 0.0014*Total_Pass1_Product_Flow + 0.457)) %>%
  mutate(RO_PASS1E_PRODUCT_PRESSURE = ifelse(Total_Pass1_Product_Flow < 200,0.7553,0.000006*Total_Pass1_Product_Flow^2 - 0.0019*Total_Pass1_Product_Flow + 0.8953))

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



# renaming UF
df <- df %>%
  rename(UF_Feed_tank_TEMP = TIT_2402_01_T01_VAL0) %>%
  rename(UF_A_filtration_TMP = UF_A_TMP_124_VAL0) %>%
  rename(UF_A_filtration_Perm. = UF_A_PERM_124_VAL0) %>%
  rename(UF_A_filtration_flux = UF_A_FLUX_124_VAL0) %>%
  rename(UF_A_filtration = UF_A_TIME_124_VAL0) %>%
  rename(UF_A_filtraion_end_TMP = UF_A_TMP_132_VAL0) %>%
  rename(UF_A_filtraion_end_Perm. = UF_A_PERM_132_VAL0) %>%
  rename(UF_A_filtraion_end_flux = UF_A_FLUX_132_VAL0) %>%
  rename(UF_A_filtraion_end  = UF_A_TIME_132_VAL0) %>%
  rename(UF_A_BW_1_TMP = UF_A_TMP_138_VAL0) %>%
  rename(UF_A_BW_2_TMP = UF_A_TMP_144_VAL0) %>%
  rename(UF_A_BW_3_TMP = UF_A_TMP_150_VAL0) %>%
  rename(UF_A_BW_flow = UF_A_BW_FLOW_144_VAL0) %>%
  rename(UF_BW_A2_TIME = UF_A_TIME_144_VAL0) %>%
  rename(UF_B_filtration_TMP = UF_B_TMP_124_VAL0) %>%
  rename(UF_B_filtration_Perm. = UF_B_PERM_124_VAL0) %>%
  rename(UF_B_filtration_flux = UF_B_FLUX_124_VAL0) %>%
  rename(UF_B_filtration = UF_B_TIME_124_VAL0) %>%
  rename(UF_B_filtraion_end_TMP = UF_B_TMP_132_VAL0) %>%
  rename(UF_B_filtraion_end_Perm. = UF_B_PERM_132_VAL0) %>%
  rename(UF_B_filtraion_end_flux = UF_B_FLUX_132_VAL0) %>%
  rename(UF_B_filtraion_end = UF_B_TIME_132_VAL0) %>%
  rename(UF_B_BW_1_TMP = UF_B_TMP_138_VAL0) %>%
  rename(UF_B_BW_2_TMP = UF_B_TMP_144_VAL0) %>%
  rename(UF_B_BW_3_TMP = UF_B_TMP_150_VAL0) %>%
  rename(UF_B_BW_4_TMP = UF_B_TMP_156_VAL0) %>%
  rename(UF_B_BW_flow = UF_B_BW_FLOW_144_VAL0) %>%
  rename(UF_BW_B2_TIME = UF_B_TIME_144_VAL0) %>%
  rename(UF_C_filtration_TMP = UF_C_TMP_124_VAL0) %>%
  rename(UF_C_filtration_Perm. = UF_C_PERM_124_VAL0) %>%
  rename(UF_C_filtration_flux = UF_C_FLUX_124_VAL0) %>%
  rename(UF_C_filtration = UF_C_TIME_124_VAL0) %>%
  rename(UF_C_filtraion_end_TMP = UF_C_TMP_132_VAL0) %>%
  rename(UF_C_filtraion_end_Perm. = UF_C_PERM_132_VAL0) %>%
  rename(UF_C_filtraion_end_flux = UF_C_FLUX_132_VAL0) %>%
  rename(UF_C_filtraion_end = UF_C_TIME_132_VAL0) %>%
  rename(UF_C_BW_1_TMP = UF_C_TMP_138_VAL0) %>%
  rename(UF_C_BW_2_TMP = UF_C_TMP_144_VAL0) %>%
  rename(UF_C_BW_3_TMP = UF_C_TMP_150_VAL0) %>%
  rename(UF_C_BW_4_TMP = UF_C_TMP_156_VAL0) %>%
  rename(UF_C_BW_flow = UF_C_BW_FLOW_144_VAL0) %>%
  rename(UF_BW_C2_TIME = UF_C_TIME_144_VAL0) %>%
  rename(UF_D_filtration_TMP = UF_D_TMP_124_VAL0) %>%
  rename(UF_D_filtration_Perm. = UF_D_PERM_124_VAL0) %>%
  rename(UF_D_filtration_flux = UF_D_FLUX_124_VAL0) %>%
  rename(UF_D_filtration = UF_D_TIME_124_VAL0) %>%
  rename(UF_D_filtraion_end_TMP = UF_D_TMP_132_VAL0) %>%
  rename(UF_D_filtraion_end_Perm. = UF_D_PERM_132_VAL0) %>%
  rename(UF_D_filtraion_end_flux = UF_D_FLUX_132_VAL0) %>%
  rename(UF_D_filtraion_end = UF_D_TIME_132_VAL0) %>%
  rename(UF_D_BW_1_TMP = UF_D_TMP_138_VAL0) %>%
  rename(UF_D_BW_2_TMP = UF_D_TMP_144_VAL0) %>%
  rename(UF_D_BW_3_TMP = UF_D_TMP_150_VAL0) %>%
  rename(UF_D_BW_flow = UF_D_BW_FLOW_144_VAL0) %>%
  rename(UF_BW_D2_TIME = UF_D_TIME_144_VAL0)

# renaming IO
df <- df %>%
  rename(TN_BAZAN = AIT_0901_01_NO1_VAL0) %>%
  rename(TN_CAOL = AIT_0901_01_NO2_VAL0) %>%
  rename(TN_EFFLUENT = AIT_0901_01_NO3_VAL0) %>%
  rename(BAZAN_TOC_MR = AIT_0901_01_OC1_VAL0) %>%
  rename(CAOL_TOC_MR = AIT_0901_01_OC2_VAL0) %>%
  rename(EFFLUENT_TOC_MR = AIT_0901_01_OC3_VAL0) %>%
  rename(BAZAN_NH4 = AIT_0901_02_NH1_VAL0) %>%
  rename(EFFLUENT_NH4 = AIT_0901_02_NH2_VAL0) %>%
  rename(CAOL_NH4 = AIT_0901_02_NH3_VAL0) %>%
  rename(EFFLUENT_TO_KISHON_COND. = AIT_0901_06_C11_VAL0) %>%
  rename(WW_CAOL_COND. = AIT_0901_06_C21_VAL0) %>%
  rename(WW_BAZAN_COND. = AIT_0901_06_C31_VAL0) %>%
  rename(EFFLUENT_TO_KISHON_TEMP. = AIT_0901_06_T11_VAL0) %>%
  rename(WW_CAOL_TEMP. = AIT_0901_06_T21_VAL0) %>%
  rename(WW_BAZAN_TEMP. = AIT_0901_06_T31_VAL0) %>%
  rename(EFFLUENT_TO_KISHON_TUBIDITY = AIT_0901_11_TR1_VAL0) %>%
  rename(WW_BAZAN_TURBIDITY = AIT_0901_31_CAOL_VAL0) %>%
  rename(WW_CAOL_TURBIDITY = AIT_0901_31_TR1_VAL0) %>%
  rename(EFFLUENT_TO_KISHON_DO = AIT_0902_01_O13_VAL0) %>%
  rename(WW_CAOL_DO = AIT_0902_01_O23_VAL0) %>%
  rename(WW_BAZAN_DO = AIT_0902_01_O33_VAL0) %>%
  rename(EFFLUENT_TO_KISHON_pH = AIT_0902_02_H12_VAL0) %>%
  rename(WW_CAOL_pH = AIT_0902_02_H22_VAL0) %>%
  rename(BAZAN_pH = AIT_0902_02_H32_VAL0) %>%
  rename(UF_BLEND_STREAM_AFTER_Na2CO3_COND. = AIT_3603_01_C01_VAL0) %>%
  rename(UF_BLEND_STREAM_AFTER_Na2CO3_pH = AIT_3603_01_H01_VAL0) %>%
  rename(STABILIZED_WATER_TO_CLIENT_COND. = AIT_3604_02_C01_VAL0) %>%
  rename(STABILIZED_WATER_TO_CLIENT_pH  = AIT_3604_02_H01_VAL0) %>%
  rename(NON_STAB_TO_CLIENT_COND. = CIT_3604_01_C01_VAL0) %>%
  rename(BAZAN_INLET_FLOW = FIT_1001_01_FL_VAL0) %>%
  rename(CAOL_INLET_FLOW = FIT_1001_02_FL_VAL0) %>%
  rename(Feed_by_pass = FIT_1001_03_FL_VAL0) %>%
  rename(EFFLUENT_TO_KISHON_FLOW = FIT_1801_01_FL_VAL0) %>%
  rename(RO_PASS1_PRODUCT_TO_STABILIZATION_TANK_FLOW = FIT_3603_01_FL_VAL0) %>%
  rename(UF_PRODUCT_TO_STABILIZATION_TANK_FLOW = FIT_3603_02_FL_VAL0) %>%
  rename(NON_STABILIZED_WATER_TO_CLIENT_FLOW = FIT_3604_01_FL_VAL0) %>%
  rename(STABILIZED_WATER_TO_CLIENT_FLOW = FIT_3604_02_FL_VAL0) %>%
  rename(INLET_PRESSURE_BAZAN = PIT_1001_01_VAL0) %>%
  rename(INLET_PRESSURE_CAOL = PIT_1001_02_VAL0 ) %>%
  rename(NON_STAB._TO_CLIENT_PRESSURE = PIT_3604_01_VAL0) %>%
  rename(STAB._TO_CLIENT_PRESSURE = PIT_3604_02_VAL0)

# renaming EDI
df <- df %>%
  rename(EDI_PRODUCT_SI = AIT_0901_04_SI1_VAL0) %>%
  rename(EDI_PRODUCT_TOC_LR = AIT_0901_05_OC1_VAL0) %>%
  rename(EDI_FEED_TOC_LR = AIT_0901_05_OC2_VAL0) %>%
  rename(EDI_B_PRODUCT_COND. = CIT_4702_01B_C01_VAL0) %>%
  rename(EDI_B_PRODUCT_TEMP. = CIT_4702_01B_T01_VAL0) %>%
  rename(EDI_C_PRODUCT_COND. = CIT_4702_01C_C01_VAL0) %>%
  rename(EDI_C_PRODUCT_TEMP. = CIT_4702_01C_T01_VAL0) %>%
  rename(EDI_TO_CLIENT_COND. = CIT_4703_01_C01_VAL0) %>%
  rename(EDI_B_PRODUCT_FLOW = FIT_4702_01B_FL_VAL0) %>%
  rename(EDI_C_PRODUCT_FLOW = FIT_4702_01C_FL_VAL0) %>%
  rename(EDI_B_CONC._FLOW = FIT_4702_02B_FL_VAL0) %>%
  rename(EDI_C_CONC._FLOW = FIT_4702_02C_FL_VAL0) %>%
  rename(EDI_PRODUCT_TO_CLIENT_FLOW = FIT_4703_01_FL_VAL0) %>%
  rename(EDI_B_INLET_PRODUCT_PRESSURE = PIT_4702_01B_VAL0) %>%
  rename(EDI_C_INLET_PRODUCT_PRESSURE = PIT_4702_01C_VAL0) %>%
  rename(EDI_B_OUTLET_PRODUCT_PRESSURE = PIT_4702_02B_VAL0) %>%
  rename(EDI_C_OUTLET_PRODUCT_PRESSURE = PIT_4702_02C_VAL0) %>%
  rename(EDI_B_INLET_CONC._PRESSURE = PIT_4702_03B_VAL0) %>%
  rename(EDI_C_INLET_CONC._PRESSURE = PIT_4702_03C_VAL0) %>%
  rename(EDI_B_OUTLET_CONC._PRESSURE = PIT_4702_04B_VAL0) %>%
  rename(EDI_C_OUTLET_CONC._PRESSURE = PIT_4702_04C_VAL0) %>%
  rename(EDI_PRODUCT_PUMPS_INLET_PRESSURE = PIT_4703_01_VAL0) %>%
  rename(EDI_PRODUCT_PUMPS_OUTLET_PRESSURE = PIT_4703_02_VAL0) %>%
  rename(EDI_B_RECTIFIER1_ADC_READBACK = R1_4702_01B_A_VAL0) %>%
  rename(EDI_B_RECTIFIER1_VDC_READBACK = R1_4702_01B_V_VAL0) %>%
  rename(EDI_C_RECTIFIER1_ADC_READBACK = R1_4702_01C_A_VAL0) %>%
  rename(EDI_C_RECTIFIER1_VDC_READBACK = R1_4702_01C_V_VAL0) %>%
  rename(EDI_B_RECTIFIER2_ADC_READBACK = R1_4702_02B_A_VAL0) %>%
  rename(EDI_B_RECTIFIER2_VDC_READBACK = R1_4702_02B_V_VAL0) %>%
  rename(EDI_C_RECTIFIER2_ADC_READBACK = R1_4702_02C_A_VAL0) %>%
  rename(EDI_C_RECTIFIER2_VDC_READBACK = R1_4702_02C_V_VAL0) %>%
  rename(EDI_A_PRODUCT_COND. = CIT_4702_01A_C01_VAL0) %>%
  rename(EDI_A_PRODUCT_TEMP. = CIT_4702_01A_T01_VAL0) %>%
  rename(EDI_A_PRODUCT_FLOW = FIT_4702_01A_FL_VAL0) %>%
  rename(EDI_A_CONC._FLOW = FIT_4702_02A_FL_VAL0) %>%
  rename(EDI_A_INLET_PRODUCT_PRESSURE = PIT_4702_01A_VAL0) %>%
  rename(EDI_A_INLET_Conc._PRESSURE = PIT_4702_02A_VAL0) %>%
  rename(EDI_A_INLET_CONC._PRESSURE = PIT_4702_03A_VAL0) %>%
  rename(EDI_A_OUTLET_CONC._PRESSURE = PIT_4702_04A_VAL0) %>%
  rename(EDI_A_INLET_Electrode_PRESSURE = PIT_4702_05A_VAL0) %>%
  rename(EDI_A_OUTLET_Electrode._PRESSURE = PIT_4702_06A_VAL0) %>%
  rename(EDI_A_RECTIFIER_ADC_READBACK = R1_4702_01A_A_VAL0) %>%
  rename(EDI_A_RECTIFIER_VDC_READBACK = R1_4702_01A_V_VAL0)

#### end



#### Filtration end permeability normalization ####

## Function for normalized permeability
Norm_FEP <- function(UF){
  data_frame(
    A_FEP = UF$UF_A_filtraion_end_Perm. * 2/3,
    B_FEP = UF$UF_B_filtraion_end_Perm. * 3/4,
    C_FEP = UF$UF_C_filtraion_end_Perm. * 3/4,
    D_FEP = UF$UF_D_filtraion_end_Perm. * 2/3
  )
}


## prepare data for graphs
UF_analysis <- bind_cols(Norm_FEP(df), TimeStamp = df$TimeStamp)
## sample 20% of data
UF_analysis <- sample_frac(UF_analysis, .2)

#### end

#### UF flow graphs ####

# Smooth combined 
colors <- c("UF A" = "Green", "UF B" = "Red", "UF C" = "Blue", "UF D" = "Orange")

UF_trend <- ggplot(UF_analysis, aes(x = TimeStamp)) +
  geom_smooth(data = UF_analysis, aes(y = A_FEP, colour = "UF A"), size = 1.5 ) + 
  geom_smooth(data = UF_analysis, aes(y = B_FEP, colour = "UF B"), size = 1.5) +
  geom_smooth(data = UF_analysis, aes(y = C_FEP, colour = "UF C"), size = 1.5) +
  geom_smooth(data = UF_analysis, aes(y = D_FEP, colour = "UF D"), size = 1.5) +
  labs(x="Time",
       y="Permability [lmh/barg]",
       color = "Legend") +
  scale_color_manual(values = colors) +
  ggtitle("UF permability trend") +
  theme(plot.title = element_text(hjust=0.5))


# Points separated
UFA <- ggplot(UF_analysis, aes(x = TimeStamp)) +
  geom_point(data = UF_analysis, aes(y = A_FEP), size = 1.5, colour = "green" ) +
  labs(x="Time",
       y="Permability [lmh/barg]") +
  ggtitle("UF A") +
  ylim(50,250) +
  theme(plot.title = element_text(hjust=0.5), legend.position = "none")

UFB <- ggplot(UF_analysis, aes(x = TimeStamp)) +
  geom_point(data = UF_analysis, aes(y = B_FEP), size = 1.5, colour = "red" ) +
  labs(x="Time",
       y="Permability [lmh/barg]") +
  ggtitle("UF B") +
  ylim(50,250) +
  theme(plot.title = element_text(hjust=0.5), legend.position = "none")

UFC <- ggplot(UF_analysis, aes(x = TimeStamp)) +
  geom_point(data = UF_analysis, aes(y = C_FEP), size = 1.5 , colour = "blue" ) +
  labs(x="Time",
       y="Permability [lmh/barg]") +
  ggtitle("UF C") +
  ylim(50,250) +
  theme(plot.title = element_text(hjust=0.5), legend.position = "none")

UFD <- ggplot(UF_analysis, aes(x = TimeStamp)) +
  geom_point(data = UF_analysis, aes(y = D_FEP), size = 1.5, colour = "orange" ) +
  labs(x="Time",
       y="Permability [lmh/barg]") +
  ggtitle("UF D") +
  ylim(50,250) +
  theme(plot.title = element_text(hjust=0.5), legend.position = "none")


# Grid show all combined
grid.arrange(UFA, UFB, UFC, UFD, UF_trend, nrow = 3)

#### end
