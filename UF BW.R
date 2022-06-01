library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(plotly)

## import and bind csv files
wd <- c("C:/Users/Bazan_Lab/Desktop/My projects/UF BW/12.10.2021")
setwd(wd)
file_names <- dir(wd)
dataset <- do.call(rbind, lapply(file_names, read.csv, header = TRUE, sep = ",", quote = "'"))

# fix data

dataset$UFA_STEP_POINTER_VAL0 <- as.character(dataset$UFA_STEP_POINTER_VAL0)
dataset$UFB_STEP_POINTER_VAL0 <- as.character(dataset$UFB_STEP_POINTER_VAL0)
dataset$UFC_STEP_POINTER_VAL0 <- as.character(dataset$UFC_STEP_POINTER_VAL0)
dataset$UFD_STEP_POINTER_VAL0 <- as.character(dataset$UFD_STEP_POINTER_VAL0)

steps <- as.character(c("138","144","150","156"))

df_PIT <- 
  data.frame(
    A = dataset$PIT_2403_03A_VAL0,
    B = dataset$PIT_2403_03B_VAL0,
    C = dataset$PIT_2403_03C_VAL0,
    D = dataset$PIT_2403_03D_VAL0
  )

df_PIT <- stack(df_PIT)
colnames(df_PIT)[colnames(df_PIT) == 'values'] <- 'PIT'

df_steps <- 
  data.frame(
    A = dataset$UFA_STEP_POINTER_VAL0,
    B = dataset$UFB_STEP_POINTER_VAL0,
    C = dataset$UFC_STEP_POINTER_VAL0,
    D = dataset$UFD_STEP_POINTER_VAL0
  )
df_steps <- stack(df_steps)
df_steps <- df_steps$values


df <- data.frame(df_PIT,cor_steps = df_steps)
df <- df[df$cor_steps %in% steps,]


#### plot ####
BW_pressure <- 
  ggplot(data = df, aes(x = cor_steps, y = PIT, colour = ind)) +
  geom_boxplot() +
  labs(x = 'step',
       y = "pressure [bar]",
       title = "pressure by stage")

ggplotly(BW_pressure)


df %>%
  group_by(cor_steps) %>%
  count()
