library(tidyverse)
library(gridExtra)
library(plotly)
library(lubridate)

## import and bind csv files
wd <- c("C:/Users/Shayl/Desktop/My projects/DATA/data 2023")
setwd(wd)
file_names <- dir(pattern = "*__1Sec")

dataset <- do.call(rbind, lapply(file_names, read.csv, header = TRUE, sep = ",", quote = "'"))

# fix data

dataset$UFA_STEP_POINTER_VAL0 <- as.character(dataset$UFA_STEP_POINTER_VAL0)
dataset$UFB_STEP_POINTER_VAL0 <- as.character(dataset$UFB_STEP_POINTER_VAL0)
dataset$UFC_STEP_POINTER_VAL0 <- as.character(dataset$UFC_STEP_POINTER_VAL0)
dataset$UFD_STEP_POINTER_VAL0 <- as.character(dataset$UFD_STEP_POINTER_VAL0)
dataset$TimeStamp <- as.POSIXct(dataset$TimeStamp)


steps <- as.character(c("138","144","150","156"))

data_test = dataset

#### flow and pressure check ####

data_flow <- data_test %>% 
  mutate(Datetime = date(TimeStamp)) %>%
  group_by(Datetime,Pointer = UFD_STEP_POINTER_VAL0) %>% 
  summarize(Flow = mean(FIT_2406_01_FL_VAL0), PIT_mean = mean(PIT_2403_03D_VAL0),PIT_max = max(PIT_2403_03D_VAL0))

data_flow <- data_flow[data_flow$Pointer %in% steps,]


#### end ####


#### pressure ####

df_PIT <- 
  data.frame(
    A = data_test$PIT_2403_03A_VAL0,
    B = data_test$PIT_2403_03B_VAL0,
    C = data_test$PIT_2403_03C_VAL0,
    D = data_test$PIT_2403_03D_VAL0
  )

df_PIT <- stack(df_PIT)
colnames(df_PIT)[colnames(df_PIT) == 'values'] <- 'PIT'

df_steps <- 
  data.frame(
    A = data_test$UFA_STEP_POINTER_VAL0,
    B = data_test$UFB_STEP_POINTER_VAL0,
    C = data_test$UFC_STEP_POINTER_VAL0,
    D = data_test$UFD_STEP_POINTER_VAL0
  )
df_steps <- stack(df_steps)
df_steps <- df_steps$values


df_bw <- data.frame(df_PIT,cor_steps = df_steps)
df_bw <- df_bw[df_bw$cor_steps %in% steps,]

df_bw %>%
  summarise(Avg=mean(PIT))

#### end ####

#### pressure box plot ####
BW_pressure <- 
  ggplot(data = df_bw, aes(x = cor_steps, y = PIT, colour = ind)) +
  geom_boxplot() +
  labs(x = 'step',
       y = "pressure [bar]",
       title = "pressure by stage")

ggplotly(BW_pressure)


df %>%
  group_by(cor_steps) %>%
  count()

#### end ####


#### data Flow plot####

plot_ly(data_flow, x = ~Datetime, y = ~Flow,
        color = ~Pointer, mode = 'line')

plot_ly(data_flow, x = ~Datetime, y = ~PIT_max,
        color = ~Pointer, mode = 'line')

plot_ly(data_flow, x = ~Datetime, y = ~PIT_mean,
        color = ~Pointer, mode = 'line')

#### end ####
