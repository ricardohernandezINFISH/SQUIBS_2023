library(dplyr) ##for pipe operaters
library(lubridate) ##using as.date function
library(ggplot2) ## plotting
library(tidyverse) ##used for pivot_wider function
library(RColorBrewer) ### For color palette
library(ggpubr) ## used for stat_cor and stat_ridgeline functions (aka displaying equations)
library(ggridges) ## used for geom_ridgeline plots
library(hrbrthemes) ## themes for vizualizing plots

#and/or/not operators (&, |, !)

#Read in csv file
#test <- read.csv(here::here("data/squib_samples/SQUIB_11_weeks.csv"))
test <- read.csv("~/illex_loligo_project/data/squib_samples/SQUIB_11_weeks.csv")
test <- test[,-1]
test <- test %>% 
  mutate(land_date = as.Date(test$land_date)) %>% ## I have to fix the error 
                                  #in 2023-05-09 land date as it is showing up as a NA
  mutate(organism_no = as.numeric(test$organism_no))

test <- test %>% 
  pivot_wider(names_from = param_type, values_from = param_value_num)
test$sex <- case_when(
  test$TSL > 0 | test$SPL > 0 ~ 'male',
  test$NGL > 0 | test$AGL > 0 ~ 'female',
  TRUE ~ 'indeterminate'
)
test <- test %>% 
  mutate(TSL = case_when(test$sex == 'male' & is.na(TSL) ~ 0 , 
                         test$sex == 'male' & TSL == TSL ~ TSL)) %>% 
  mutate(SPL = case_when(test$sex == 'male' & is.na(test$SPL) ~ 0 , 
                         test$sex == 'male' & test$SPL == test$SPL ~ test$SPL)) %>%
  mutate(NGL = case_when(test$sex == 'female' & is.na(test$NGL) ~ 0 , 
                         test$sex == 'female' & test$NGL == test$NGL ~ test$NGL)) %>% 
  mutate(AGL = case_when(test$sex == 'female' & is.na(test$AGL) ~ 0 , 
                          test$sex == 'female' & test$AGL == test$AGL ~ test$AGL)) %>%
  mutate(EP = case_when(test$sex == 'female' & is.na(test$EP) ~ 1 , 
                        #test$sex == 'female' & test$EP == test$EP ~ test$EP,
                        test$sex == 'female' & test$EP == 0 ~ 1,
                        test$sex == 'female' & test$EP == 1 ~ 2,
                        test$sex == 'female' & test$EP == 2 ~ 3
                        ))
test <- test %>% 
mutate(MW = case_when(test$sex == 'male' & is.na(test$MW) ~ 0 , 
                      test$sex == 'male' & test$MW == test$MW ~ test$MW,
                      test$sex == 'female' & is.na(test$MW) ~ 0 , 
                      test$sex == 'female' & test$MW == test$MW ~ test$MW)) 
  
#The code above works but I am missing MWI index
#I need to make one for indeterminates
  #mutate(EP = case_when(test$sex == 'female' & is.na(test$EP) ~ 0 , 
                        #test$sex == 'female' & test$EP == test$EP ~ test$EP))

## Assigning sex and maturity indicies
test$TSI <- (test$TSL) / (test$ML) 
test$NGI <- (test$NGL) / (test$ML) 
test$SPI <- (test$SPL) / (test$ML) 
test$AGI <- (test$AGL) / (test$ML) 
test$MWI <- (test$MW) / (test$ML) 



test <- test %>% 
  mutate(sex = factor(sex,levels = c("male", "female", "indeterminate")))
  

## Code was used to look at mean values w/ 1STD
#test$maturity <- case_when(     
  #test$TSI > 7.98 & test$TSI < 15.22 & test$MWI > 78.9 & test$MWI < 92.7 & test$SPI == 0 & test$ML > 49.3 & test$ML < 92.7 ~ '1',
  #test$TSI > 15.3 & test$TSI < 22.7 & test$MWI > 64.19 & test$MWI < 79.21 & test$SPI == 0 & test$ML > 77.5 & test$ML< 142.5 ~ '2',
  #test$TSI > 26.97 & test$TSI < 34.43 & test$MWI > 69.26 & test$MWI < 86.74 & test$SPI == 0 & test$ML < 61.3 & test$ML < 154.7 ~ '3',
  #test$TSI > 25.85 & test$TSI < 34.75 & test$MWI > 41.49 & test$MWI < 64.91 & test$SPI > 4.67 & test$SPI < 4.53 & test$ML > 49.3 & test$ML < 92.7 ~ '4'
  #)

test <- test %>% 
  #filter(sex == 'male') %>% 
  mutate(m.1 = (-949.083 * test$SPI) + (112.680 * MWI) + (7.445 * TSI) - 49.822 ,
         m.2 = (-838.030 * test$SPI) + (88.635 * MWI) + (65.620 * TSI) - 39.961,
         m.3 = (-972.007 * test$SPI) + (92.161 * MWI) + (133.984 * TSI) - 60.416,
         m.4 = (-596.250 * test$SPI) + (16.472 * MWI) + (130.547 * TSI) - 35.921,
         f.1 = (9.695 * test$EP) + (-71.476 * AGI) + (98.606 * MWI) + (-26.426 * NGI) - 46.310,
         f.2 = (9.440 * test$EP) + (82.104 * AGI) + (83.025 * MWI) + (-10.377 * NGI) - 37.240,
         f.3 = (13.297 * test$EP) + (233.148 * AGI) + (67.263 * MWI) + (54.122 * NGI) - 49.256,
         f.4 = (28.915 * test$EP) + (282.733 * AGI) + (76.922 * MWI) + (35.443 * NGI) - 105.526
)

test_m <- test %>% 
  filter(sex == 'male')
test_m<- test_m %>% 
  pivot_longer(cols = c("m.1","m.2","m.3","m.4"),
               names_to = "stage",
               values_to = "value")
test_m <- select(test_m, -c("f.1","f.2","f.3","f.4"))  # removing female stage cols
test_m <- test_m %>% 
  mutate(stage = case_when(
    #is.na(t$value) ~ 1,
    test_m$stage == 'm.1' ~ 1,
    test_m$stage == 'm.2' ~ 2,
    test_m$stage == 'm.3' ~ 3,
    test_m$stage == 'm.4' ~ 4
     ))
test_m <- test_m %>% 
  group_by(organism_no, land_date,lon, lat, sex) %>% 
  mutate(mat_stage = which(value == max(value)))


test_f <- test %>% 
  filter(sex == 'female')
test_f<- test_f %>% 
  pivot_longer(cols = c("f.1","f.2","f.3","f.4"),
               names_to = "stage",
               values_to = "value")
test_f <- select(test_f, -c("m.1","m.2","m.3","m.4")) # removing male stage cols
test_f <- test_f %>% 
  mutate(stage = case_when(
    #is.na(t$value) ~ 1,
    test_f$stage == 'f.1' ~ 1,
    test_f$stage == 'f.2' ~ 2,
    test_f$stage == 'f.3' ~ 3,
    test_f$stage == 'f.4' ~ 4
  ))
test_f <- test_f %>% 
  group_by(organism_no, land_date,lon, lat, sex) %>% 
  mutate(mat_stage = which(value == max(value)))



test_i <- test %>% 
  filter(sex == 'indeterminate')
test_i$mat_stage <- 0
test_i <- test_i %>% 
  select(organism_no, land_date,lon,lat, sex,mat_stage)


mat.all <- rbind(test_f,test_m)

mat.all <-  select(mat.all, -c("OW","ML","MW","TSL","SPL", "NGL", "AGL", "TSI",
                               "SPI","MWI","AGI","NGI",
                               "AST", "EP", "AO","year","stage","value","week",
                               "year","month","day"))  


mat.all <- rbind(mat.all,test_i)


mat.all <- mat.all %>% 
  mutate(year = year(land_date),
         month = month(land_date),
         week = week(land_date), 
         day = day(land_date))

t4 <- mat.all[!duplicated(mat.all), ]

write.csv(t4, "C:/Users/Ricardo.Hernandez/Documents/illex_loligo_project/data/squib_samples/maturity_stages.csv")

ggplot(t4,
       aes(x = mat_stage, fill = sex))+
 # geom_bar(position = position_dodge(),color = "black", alpha = 0.4 )+
  geom_bar(color = "black", alpha = 0.4 )+
  labs(fill = 'sex')+
  scale_fill_manual(values = c("female"="#FFF394","indeterminate"="gray","male"= "#05C4B1"))+
  #scale_color_manual(value= 'black')+
  theme_bw()+
  labs(title = "Maturity Stages present \n in males and females")+
  xlab('Maturity Stage') + 
  ylab('Number of \n organisms measured') +
  #theme_ridges( grid = FALSE, center_axis_labels = TRUE)+
  theme(legend.position="right",
        plot.title = element_text(hjust = 0.5),
        legend.key.size = unit(1, 'cm'),
        text= (element_text(size = 28)),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

ggsave(here::here("figures","Maturity Stages present in males and females.png"),
       width= 10, height= 7)

#facet_wrap(~ week)

mat.tally <- t4 %>% 
  group_by(sex, week, lon, lat, mat_stage) %>%
  summarise(n = length(mat_stage))
mat.tally <- as.data.frame(mat.tally)
write.csv(mat.tally,"C:/Users/Ricardo.Hernandez/Documents/illex_loligo_project/data/squib_samples/SQUIB_11_weeks_Mat_stages.csv")


ggplot(data = mat.tally, 
       aes(x = week, y = n, fill = factor(mat_stage))) +
  geom_bar(stat ='identity',
           col = 'black') + 
  scale_fill_manual('Maturity Stage',values=c( '#9D36C7','gold', '#27D1F3', '#5FFF14')) +
  xlab('Weeks') + 
  ylab('Number of organisms \n measured') +
  facet_wrap(~sex) +
  theme_bw()+
  labs(title = "Maturity Stages \n across weeks by sex")+
  #theme_ridges( grid = FALSE, center_axis_labels = TRUE)+
  theme(legend.position="right",
        plot.title = element_text(hjust = 0.5),
        legend.key.size = unit(1, 'cm'),
        text= (element_text(size = 28)),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

ggsave(here::here("figures","Maturity Stages across weeks by sex.png"),
       width= 10, height= 7)
  
  
#na.locs <- which(is.na(mat.all$value))
#mat.all.no.na <- mat.all[-na.locs,]

#hack <- mat.all.no.na %>% 
  #group_by(organism_no, land_date, sex) %>% 
  #mutate(mat_stage = which(value == max(value)))

#t <- test2 %>% 
 #pivot_longer(cols = c("m.1","m.2","m.3","m.4","f.1","f.2","f.3","f.4"),
              # names_to = "stage",
               #values_to = "value")
#t <- t %>% 
 # mutate(stage = case_when(
    #is.na(t$value) ~ 1,
  ##t$stage == 'm.1' ~ 1,
  #t$stage == 'm.2' ~ 2,
  #t$stage == 'm.3' ~ 3,
  #t$stage == 'm.4' ~ 4,
  #t$stage == 'f.1' ~ 1,
  #t$stage == 'f.2' ~ 2,
  #t$stage == 'f.3' ~ 3,
  #t$stage == 'f.4' ~ 4
  
#))

t2 <- mat.all %>%
  select(organism_no,land_date,sex,stage, value) %>%
  mutate(organism_no = as.numeric(mat.all$organism_no)) #%>%
  #filter(!is.na(value))## eliminated 20 obs from mat.all
  
#mutate(value = case_when(
  #is.na(value) ~ 0 ,
  #value == value ~ value
  #))
#test3 <-test2 %>% group_by(land_date) %>% distinct()

#test3 <-test2 %>% group_by(land_date) %>% test2 %>% distinct(org_id)

#t2 <- t2[!duplicated(t2), ]

t2 <- t2 %>% 
  group_by(organism_no, land_date, sex) %>% 
  mutate(mat_stage = which(value == max(value)))

#case_when( value == 0 ~ 1)

t3 <- t2 %>% 
  select(organism_no,sex,mat_stage, land_date) #%>% 

mat.all <- rbind(t3,test_i)

t4 <- mat.all[!duplicated(mat.all), ]
t4 <- t4 %>% 
  mutate(year = year(land_date),
         month = month(land_date),
         week = week(land_date), 
         day = day(land_date))

ggplot(t4,
       aes(x = mat_stage))+
  geom_bar(aes(fill = sex))




mat.tally <- t4 %>% 
  group_by(sex, week, mat_stage) %>%
  summarise(n = length(mat_stage))
mat.tally <- as.data.frame(mat.tally)

mat.tally <- mat.tally %>% 
  filter(sex == c("male","female"))
ggplot(data = mat.tally, 
       aes(x = week, y = n, fill = factor(mat_stage))) +
  geom_bar(stat ='identity',
           col = 'black') + 
  scale_fill_manual('Maturity Stage',values=c('gold1','slateblue3', '#00FF66FF', 'violetred1')) +
  xlab('Statistical Area') + 
  ylab('Number of organisms measured') +
  facet_wrap(~sex) +
  theme_bw()
  #ecodata::theme_ts() 

#filter(!is.na(value))

#%>% 
#mutate(value = case_when(
#t2$sex == 'indeterminate' ~ 1,
#t2$sex == 'male' ~ value,
#t2$sex == 'female' ~ value
#))

t2 <- t2 %>% 
  group_by(organism_no, land_date, sex) %>% 
  mutate(mat_stage = which(value == max(value)))
t3 <- t2 %>% 
  select(organism_no,sex,mat_stage, land_date) #%>% 

t4 <- t3[!duplicated(t3), ]




ggplot(t4,
       aes(x = mat_stage))+
  geom_bar(aes(fill = sex))



## creating a color palette

#cols_2 = brewer.pal(4,'RdYlBu')
cols_2 = rev(brewer.pal(3,'RdYlBu'))
pal_2 <- colorRampPalette(cols_2)(30)



###      Regressions plots     ###

###     Correlation coefficients have a probability (p-value), which shows the  
###     probability that the relationship between the two variables is equal to     
###     zero (null hypotheses; no relationship)

###     Strong correlations have low p-values because the probability that they have
###     no relationship is very low.

g <- ggplot(test, 
            aes(ML, OW))+           
  geom_point(aes(color = sex)) + 
  xlab('Mantle length (mm)') + 
  ylab('Organism Weight (g)')+
  scale_color_manual(values = c("female"="#d8b365","indeterminate"="black","male"= "#05C4B1"))+
  facet_wrap(~sex)

g + stat_smooth(method = "lm", ## method: "lm" generates a line
                formula = y ~ x,
                geom = "smooth") + 
  #stat_cor(label.y = 240, p.accuracy = 0.01, size = 5 )+ #this means is where the r squared and p value will be shown
  #stat_regline_equation(label.y = 260, size= 5)+ #this is where the unit regression line equation will be shown
  labs(title = "Mantle Length \n as a function of Organism Weight")+
  #theme_ridges( grid = FALSE, center_axis_labels = TRUE)+
  #scale_y_log10()+
  #ylim(0,7)+
  theme(legend.position="right",
        plot.title = element_text(hjust = 0.5),
        legend.key.size = unit(1, 'cm'),
        text= (element_text(size = 28)),
        panel.border = element_rect(colour = "black", fill=NA, size=1))+
  guides(color = guide_legend(override.aes = list(size=10)))+
  gghighlight()

ggsave(here::here("figures","Mantle Length as a function of Organism Weight.png"),
       width= 10, height= 7)



ggscatter(test, x = 'ML',y = 'OW', color = "sex",
            palette = c("#5ab4ac","#d8b365","grey"), add = "reg.line")+         
  xlab('Mantle length (mm)') + 
  ylab('Organism Weight (g)')+  
  facet_wrap(~sex)+
  stat_cor(label.y = 221, p.accuracy = 0.01 )+ 
  stat_regline_equation(label.y = 245)+
  labs(title = "Mantle Length as a function of Organism Weight")

###     Female Regression     ###
g <- ggplot(test, 
            aes(ML, NGL))+           
  geom_point() + 
  xlab('Mantle length (mm)') + 
  ylab('Nidamental Gland Length (mm)')

g + stat_smooth(method = "lm",
                formula = y ~ x,
                geom = "smooth") + 
  stat_cor(label.y = 50 , p.accuracy = 0.01, size = 10)+ 
  stat_regline_equation(label.y = 55, size = 10)+
  xlim(50, 220)+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),
        text= (element_text(size = 28))) +#,panel.border = element_rect(colour = "black", fill=NA, size=1))+
  labs(title = "Nidamental Gland Length \n as a function of Mantle Length")
ggsave(here::here("figures",
                  "Nidamental Gland Length as a function of Mantle Length.png"),
       width= 10, height= 7)

g <- ggplot(test, 
            aes(ML, AGL))+           
  geom_point() + 
  xlab('Mantle length (mm)') + 
  ylab('Accessory Gland Length (mm)')

g + stat_smooth(method = "lm",
                formula = y ~ x,
                geom = "smooth") + 
  stat_cor(label.y = 13.5, p.accuracy = 0.01, size = 10 )+ 
  stat_regline_equation(label.y = 16, size= 10)+
  xlim(50, 220)+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),text= (element_text(size = 28))) +#,panel.border = element_rect(colour = "black", fill=NA, size=1))+
  labs(title = "Accessory Gland Length \n as a function of Mantle Length")
ggsave(here::here("figures",
                  "Accessory Gland Length as a function of Mantle Length.png"),
       width= 10, height= 7)

###     Male Regressions    ###
g <- ggplot(test, 
            aes(ML, TSL))+           
  geom_point() + 
  xlab('Mantle length (mm)') + 
  ylab('Testies Length (mm)')

g + stat_smooth(method = "lm",
                formula = y ~ x,
                geom = "smooth") + 
  stat_cor(label.y = 100, p.accuracy = 0.01, size = 10 )+ 
  stat_regline_equation(label.y = 110, size = 10)+
  labs(title = "Testies Length as\n a function of Mantle Length")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),text= (element_text(size = 28))) 
ggsave(here::here("figures",
                  "Testies Length as a  function of Mantle Length.png"),
       width= 10, height= 7)

g <- ggplot(test, 
            aes(ML, SPL))+           
  geom_point() + 
  xlab('Mantle length (mm)') + 
  ylab('Spermatophore Length (mm)')

g + stat_smooth(method = "lm",
                formula = y ~ x,
                geom = "smooth") + 
  stat_cor(label.y = 25, p.accuracy = 0.01, size = 10 )+ 
  stat_regline_equation(label.y = 27, size = 10)+
  labs(title = "Spermatophore Length \n as a function of Mantle Length")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),text= (element_text(size = 28)))
ggsave(here::here("figures",
                  "Spermatophore Length as a function of Mantle Length.png"),
       width= 10, height= 7)

##










###     Ridge Plot for weights and lengths of     ###
###      sex type at different weeks               ###

ggplot(test,
       aes(x = OW, y = factor(week))) + # Distribution of OW per week
  geom_density_ridges() +
  #scale_fill_manual(values = cols_2)+
  labs(fill = 'Week') +
  labs(x = 'Weights (g)', y = 'Week')+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),text= (element_text(size = 28)))+
  labs(title = "Distribution of Organism Weight across Weeks")
ggsave(here::here("figures"," Distribution of Organism Weight per week.png"),
       width= 10, height= 7)
  #ecodata::theme_ts()    I am unable to use the ecodata in my version of R

ggplot(test,
       aes(x = OW, y = factor(week), fill = sex)) + #Distribution of OW of Males and Females per week
  geom_density_ridges( alpha = 0.6)  +
  #scale_fill_manual(values = cols_2)+
  scale_fill_manual(values = c("female"="#FFF394","indeterminate"="gray","male"= "#05C4B1"))+
  labs(fill = 'Sex')+
  labs(x = 'Weights (g)', y = 'Week')+
  #scale_fill_cyclical(values = cols_2, guide = "legend")
  #theme_ridges( grid = FALSE, center_axis_labels = TRUE)+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),text= (element_text(size = 28)))+
  labs(title = "Distribution of Organism Weight across Weeks")
ggsave(here::here("figures","Distribution of Organism Weight of Males and Females per week.png"),
       width= 15, height= 7)
#ecodata::theme_ts()    I am unable to use the ecodata in my version of R


ggplot(test,
       aes(x = ML, y = factor(week) )) + # Distribution of ML per week
  geom_density_ridges() +
  #labs(fill = 'Sex') +
  labs(x = 'Mantle Length (mm)', y = 'Week')+
  #theme_ridges( grid = FALSE, center_axis_labels = TRUE)
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),text= (element_text(size = 28)))+
  labs(title = "Distribution of Mantle Length across week")
ggsave(here::here("figures","Distribution of Mantle Length per week.png"),
       width= 10, height= 7)


ggplot(test,
       aes(x = ML, y = factor(week), fill = sex )) + # Distribution of ML in males and females at different weeks
  geom_density_ridges(alpha = 0.6) +
  scale_fill_manual(values = c("female"="#FFF394","indeterminate"="gray","male"= "#05C4B1"))+
  #scale_fill_manual(values = cols_2)+
  labs(fill = 'Sex') +
  labs(x = 'Mantle Length (mm)', y = 'Week')+
  labs(title = "Distribution of Mantle Length across Weeks")+
  #theme_ridges( grid = FALSE, center_axis_labels = TRUE)
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),text= (element_text(size = 28)))+
  labs(title = "Distribution of Mantle Length across Weeks")
  
ggsave(here::here("figures","Distribution of Mantle Length of Males and Females per week.png"),
       width= 15, height= 7)





###     Looking more closely at week 25      ###

w_25 <- test %>% 
  filter(test$week == 25)

my_binwidth <- 1  
ggplot(w_25, aes(ML))+
  geom_histogram(binwidth = my_binwidth,
                 colour = 1, 
                 fill = "white" )+
  geom_density(aes(y = ..density.. * (nrow(w_25) * my_binwidth)),
               col = 2,  
               fill = 2, alpha = 0.25)+
  xlab('Mantle length (mm)') + 
  ylab('Count of Squid')+
  labs(title = "Frequency for Mantle Length in Week 25")+
  #theme_ridges( center_axis_labels = TRUE)
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),text= (element_text(size = 28)))+
  labs(title = "Distribution of Organism Weight across Week 25")

ggsave(here::here("figures","Frequency for Mantle Length in Week 25.png"),
       width= 15, height= 7)




###     Adding in a Histogram plot of females and males for     ###
###     Week 26                                                 ###


w_26 <- test %>% 
  filter(test$week == 26)
w_26_m <- w_26 %>% 
  filter(sex == 'male')
w_26_f <- w_26 %>% 
  filter(sex == 'female')
w_26_i <- w_26 %>% 
  filter(sex == 'indeterminate')



my_binwidth <- 1  
ggplot(w_26_m, aes(ML))+
  geom_histogram(binwidth = my_binwidth,
                 colour = 1, 
                 fill = "#05C4B1" )+
  geom_density(aes(y = ..density.. * (nrow(w_26_m) * my_binwidth)),
               col = "black",  
               fill = 'black', alpha = 0.25)+
  #facet_wrap( ~sex)
  xlab('Mantle length (mm)') + 
  ylab('Count of Squid')+
  #labs(title = "Frequency for Male Mantle Length in Week 26")+
  #theme_ridges( center_axis_labels = TRUE)
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),text= (element_text(size = 28)))+
  labs(title = "Distribution of Male \n Mantle Length across Week 26")

ggsave(here::here("figures","Frequency for Male Mantle Length in Week 26.png"),
       width= 10, height= 7)


ggplot(w_26_f, aes(ML))+
  geom_histogram(binwidth = my_binwidth,
                 colour = 1, 
                 fill = "#FFF394" )+
  geom_density(aes(y = ..density.. * (nrow(w_26_f) * my_binwidth)),
               col = "black",  
               fill = 'black', alpha = 0.25)+
  #facet_wrap( ~sex)
  xlab('Mantle length (mm)') + 
  ylab('Count of Squid')+
  #labs(title = "Frequency for Male Mantle Length in Week 26")+
  #theme_ridges( center_axis_labels = TRUE)
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),text= (element_text(size = 28)))+
  labs(title = "Distribution of Female \n Mantle Length across Week 26")

ggsave(here::here("figures","Frequency for Female Mantle Length in Week 26.png"),
       width= 10, height= 7)



ggplot(w_26_i, aes(ML))+
  geom_histogram(binwidth = my_binwidth,
                 colour = 1, 
                 fill = "gray" )+
  geom_density(aes(y = ..density.. * (nrow(w_26_i) * my_binwidth)),
               col = "black",  
               fill = 'black', alpha = 0.25)+
  #facet_wrap( ~sex)
  xlab('Mantle length (mm)') + 
  ylab('Count of Squid')+
  #labs(title = "Frequency for Male Mantle Length in Week 26")+
  #theme_ridges( center_axis_labels = TRUE)
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),text= (element_text(size = 28)))+
  labs(title = "Distribution of Indeterminate \n Mantle Length across Week 26")

ggsave(here::here("figures","Frequency for Indeterminate Mantle Length in Week 26.png"),
       width= 10, height= 7)

my_binwidth <- 1 
ggplot(hack, aes(x=mat_stage))+
  geom_bar(aes(fill = sex))




#Male and Female Abundance in progress
s <-ggplot(test, aes(y = sex)) + 
  geom_histogram(color="black", fill="grey45",  binwidth = 0.9) + 
  xlab('Sex Presence') +
  theme_classic2()
s

ggscatter( #observing differences in ML and OW trends between sex's
  test, x = "ML", y = "OW",
  color = "sex", palette = "jco",
  add = "reg.line"
) +
  facet_wrap(~sex) +
  stat_cor(label.y = 200) +
  stat_regline_equation(label.y = 220)


##### try looking at the raster stack and extracting the exact lat and long for the temperature













###     density_ridge plots with 2 y-scales?     ###

ggplot(test, aes(x = ML, y = as.factor(week), fill = sex)) +
  geom_density_ridges(
    aes(point_color = sex, point_fill = sex, point_shape = sex),
    alpha = .2, point_alpha = .5, jittered_points = TRUE
  ) +
  scale_point_color_hue(l = 40) +
  scale_discrete_manual(aesthetics = "point_shape", values = c(20, 21, 22))


ggplot(test, aes(x = ML, y = (count(test$organism_no, test$sex)), fill = sex)) +
  stat_density_ridges(quantile_lines = TRUE)

ggplot(test, aes(x = ML, y = as.factor(week), 
                 fill = factor(stat(quantile)))) +
  stat_density_ridges(aes(fill = as.factor(sex)),
                      geom = "density_ridges_gradient", calc_ecdf = TRUE,
                      quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis_d(name = "Quartiles")+
  facet_grid(as.factor(week) ~ ., switch = "y") +
  theme_classic() +
  scale_y_continuous(position = "right")


ggplot(test, aes(x = ML, y = as.factor(week),
                 fill = factor(stat(quantile)))) +
  stat_density_ridges(aes(fill = sex),
                      geom = "density_ridges_gradient",
                      calc_ecdf = TRUE,
                      quantiles = c(0.025, 0.975)
  ) +
  scale_fill_manual(
    name = "Sex", values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"),
    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")
  )

ggplot(test, aes(x = ML,
                 y = (organism_no))) +
  geom_density_ridges(aes (fill = sex)) +
  #(geom = "step") +
  facet_grid(as.factor(week) ~ ., switch = "y") +
  theme_classic() +
  scale_y_continuous(position = "right") #+
#scale_x_continuous(breaks = 0:4 * 6) +
theme(strip.background    = element_blank(),
      axis.text.x         = element_text(size = 16),
      axis.line.x         = element_blank(),
      axis.ticks.x        = element_line(colour = "gray90"),
      axis.ticks.length.x = unit(10, "points"),
      strip.text.y.left   = element_text(angle = 0, size = 10),
      panel.grid.major.x  = element_line(colour = "gray90"))
count(test$organism_no)
str(test$organism_no)



