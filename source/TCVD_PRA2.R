## ----setup, include=FALSE-------------------------------------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.align = 'center',
  fig.pos = "H",
  tidy.opts = list(width.cutoff = 60),
  tidy = FALSE
  )

kable_setup <- function(df){
  df %>% kable( "latex", booktabs = T) %>%
  kable_styling(latex_options = c(
    "striped",
    "HOLD_position",
    "scale_down"))
}


## ----libraries, message=FALSE, include=FALSE------------------------------------------------------------
if(!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if(!require('tidyr')) install.packages('tidyr'); library('tidyr')
if(!require('dplyr')) install.packages('dplyr'); library('dplyr')
if(!require('kableExtra')) install.packages('kableExtra'); library('kableExtra')
if(!require('gridExtra')) install.packages('gridExtra'); library('gridExtra')
if(!require('formatR')) install.packages('formatR'); library('formatR')
if(!require('devtools')) install.packages('devtools'); library('devtools')
if(!require('kaggler')) devtools::install_github("ldurazo/kaggler")
if(!require('ggcorrplot')) install.packages('ggcorrplot'); if(!require('car')) install.packages('car'); library('car')
if(!require('patchwork')) install.packages('patchwork'); library('patchwork')



## ----download_data, message= FALSE, warning=FALSE-------------------------------------------------------
#https://medium.com/mcd-unison/how-to-use-kaggle-api-to-download-datasets-in-r-312179c7a99c

library(readr)
library(kaggler)
kgl_auth(creds_file = 'kaggle.json')
response <- kgl_datasets_download_all(owner_dataset =
            "rashikrahmanpritom/heart-attack-analysis-prediction-dataset")

download.file(response[["url"]], "data/temp.zip", mode="wb")
unzip_result <- unzip("data/temp.zip", exdir = "data/", overwrite = TRUE)
unzip_result
heart_attack_data <- read_csv("data/heart.csv")
o2_saturation_data <- read.csv("data/o2Saturation.csv",header=F)

rm(response)


## -------------------------------------------------------------------------------------------------------
head(heart_attack_data) %>% 
  kable_setup %>% 
  kable_paper(full_width = F)%>% 
  column_spec(c(3,7,14), width = "2 cm") %>%
  column_spec(c(1,5,9,12,13), width = "0.8 cm") %>%
  row_spec(0,bold=TRUE)


## -------------------------------------------------------------------------------------------------------
# Data types

data <- heart_attack_data %>% 
  mutate(
    sex = factor(sex, levels=c(1,0),labels = c("male","female")),
    cp = factor(cp,
                levels=c(0,1,2,3),
                labels=c("typical angina","atypical angina","non-anginal pain","asymptomatic")),
    fbs = factor(fbs,levels=c(0,1),labels = c(F,T)),
    restecg = factor(restecg,levels = c(0,1,2),
                     labels = c("normal","ST-T wave abnormality","left ventricular hypertrophy")),
    exng = factor(exng),
    slp = factor(slp,levels = c(0,1,2),
                 labels = c("unsloping","flat","downsloping")),
    caa = factor(caa),
    thall = factor(thall, 
                   levels = c(1,2,3),
                   labels = c("fixed defect","normal","reversable defect")),
    output = factor(output,levels=c(0,1),
                    labels = c("less chance of heart attack","more chance of heart attack"))
  )


## -------------------------------------------------------------------------------------------------------
# Summary
summary(data) 

# Duplicates
data %>% 
  unique() %>% 
  nrow()

data <- data %>% 
  unique()



## ---- echo = FALSE--------------------------------------------------------------------------------------
## Numerical data
data_num <- data %>% 
  select_if(is.numeric)

# Histograms
data_num %>% 
pivot_longer(colnames(data_num)) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = value)) +    # Draw each column as histogram
  geom_histogram() + 
  facet_wrap(~ name, scales = "free")+
  labs(title="Variables numéricas")



## ----echo = F-------------------------------------------------------------------------------------------
## Categorical data
data_cat <- data %>% 
  select_if(~class(.) == 'factor')

# Barplots

p <- list()
cols = colnames(data_cat)
for(i in 1:length(cols)){
  col = cols[i]
  p[[i]] <- data_cat %>%
  ggplot(aes_string(col, fill = col)) + 
  geom_bar() + 
  ggtitle(paste0(col," distribution")) +
  # theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
    theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
}
do.call(grid.arrange,c(p, ncol = 2,top="Variables categóricas"))



## -------------------------------------------------------------------------------------------------------
data <- data %>% 
  mutate(
    thall = factor(if_else(is.na(as.numeric(thall)),2,as.numeric(thall)), 
                   levels = c(1,2,3),
                   labels = c("fixed defect","normal","reversable defect"))
  )



## ---- echo = FALSE--------------------------------------------------------------------------------------

# Boxplots
data_num %>% 
pivot_longer(colnames(data_num)) %>% 
  as.data.frame() %>% 
  ggplot(aes(y = value)) +    # Draw each column as histogram
  geom_boxplot(fill = "#404080", coef = 1.58) + 
  theme_minimal() +
  facet_wrap(~ name, scales = "free") +
  labs(title="Boxplots de variables numéricas")


## -------------------------------------------------------------------------------------------------------

for (col in colnames(data_num)){
  value = data[[col]][data[[col]] %in% boxplot.stats(data[[col]])$out]
  res <- quantile(data[[col]], probs = c(0,0.25,0.5,0.75,1))
  q1 <- res[[2]]
  q3 <- res[[4]]
  iqr <- q3 - q1
  min_thshld <- q1 - 1.58*iqr
  max_thshld <- q3 + 1.58*iqr
  data[[col]][data[[col]] < min_thshld] = min_thshld
  data[[col]][data[[col]] > max_thshld] = max_thshld
}


data %>% 
  select_if(is.numeric) %>% 
pivot_longer(colnames(data_num)) %>% 
  as.data.frame() %>% 
  ggplot(aes(y = value)) +    # Draw each column as histogram
  geom_boxplot(fill = "#404080", coef = 1.58) + 
  theme_minimal() +
  facet_wrap(~ name, scales = "free") +
  labs(title="Boxplots de variables numéricas \n Outliers corregidos")



## ---- echo = FALSE--------------------------------------------------------------------------------------
corr <- round(cor(heart_attack_data), 1)
ggcorrplot(corr,  type = "lower",
   lab = TRUE, p.mat = cor_pmat(heart_attack_data)) +
  labs(title="Correlación entre variables")



## -------------------------------------------------------------------------------------------------------
data_compare <- data %>% 
  select(
    age,
    cp,
    fbs,
    thalachh,
    exng,
    oldpeak,
    slp,
    caa,
    output
  )
head(data_compare)



## ---- echo = FALSE--------------------------------------------------------------------------------------

#númericas

#Shapiro-Wilk -- Levene
print(shapiro.test(data_compare$oldpeak))

print(shapiro.test(data_compare$thalachh))

print(shapiro.test(data_compare$age))

(
data_compare %>%
ggplot(aes(sample = oldpeak)) +
stat_qq() + stat_qq_line() +
labs(title="Normal Q-Q plot \n de oldpeak")
) + (
data_compare %>%
ggplot(aes(sample = thalachh)) +
stat_qq() + stat_qq_line() +
labs(title="Normal Q-Q plot \n de thalachh")
) + (
data_compare %>%
ggplot(aes(sample = age)) +
stat_qq() + stat_qq_line() +
labs(title="Normal Q-Q plot \n de age")
)


## ---- echo = FALSE--------------------------------------------------------------------------------------
(
data_compare %>%
  ggplot(aes(x=output, y=oldpeak, fill=output)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values=c("#404080","#69b3a2")) +
  theme(legend.position = "none",axis.text.x = element_text(angle = 30, hjust = 1))+
  ggtitle("Oldpeak by \n output group")
) + (
data_compare %>%
  ggplot(aes(x=output, y=thalachh, fill=output)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values=c("#404080","#69b3a2")) +
  theme(legend.position = "none",axis.text.x = element_text(angle = 30, hjust = 1))+
  ggtitle("thalachh by \n output group")
) + (
data_compare %>%
  ggplot(aes(x=output, y=age, fill=output)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values=c("#404080","#69b3a2")) +
  theme(legend.position = "none",axis.text.x = element_text(angle = 30, hjust = 1))+
  ggtitle("age by \n output group"))


## -------------------------------------------------------------------------------------------------------
print(leveneTest(oldpeak ~ output, data = data_compare,))
print(leveneTest(thalachh ~ output, data = data_compare,))
print(leveneTest(age ~ output, data = data_compare,))


## -------------------------------------------------------------------------------------------------------
#categoricas Chi-quadrado
# Variable "caa"
table_caa <- table(data_compare$caa, data_compare$output)
print(chisq.test(table_caa))

# Variable "exng"
table_exng <- table(data_compare$exng, data_compare$output)
print(chisq.test(table_exng))

# Variable "cp"
table_cp <- table(data_compare$cp, data_compare$output)
print(chisq.test(table_cp))




## -------------------------------------------------------------------------------------------------------
# wilcox oldpeak
res <- wilcox.test(oldpeak ~ output, data = data_compare)
print(res)
# kruskal thalachh
res <- kruskal.test(thalachh ~ output, data = data_compare)
print(res)

# Regresión logística utilizando 'cp', 'caa' y 'exng' como predictores
model <- glm(output ~ cp + caa + exng, data = data_compare, family = binomial)
summary(model)




## ---- echo = FALSE--------------------------------------------------------------------------------------
write.csv2(data_compare,"data/heart_final.csv")

