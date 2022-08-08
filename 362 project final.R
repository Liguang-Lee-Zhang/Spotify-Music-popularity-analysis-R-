library(tidyr)
library(ggplot2)
library(dplyr)
library(viridis)
Mydata<-read.csv("C:/study/STAT 362/project/tracks.csv", encoding="UTF-8")
Mydata.clean<-dplyr::filter(Mydata,popularity>0,release_date!="1900-01-01",tempo>0,na.rm = TRUE)
#make factors as factor
Mydata.clean2$explicit<-as.factor(Mydata.clean2$explicit)
Mydata.clean2$key<-as.factor(Mydata.clean2$key)
Mydata.clean2$mode<-as.factor(Mydata.clean2$mode)
Mydata.clean2$time_signature<-as.factor(Mydata.clean2$time_signature)
Mydata.clean3<-Mydata.clean2
Mydata.clean3$year <-as.numeric(Mydata.clean3$year)

Mydata.year<-Mydata.clean3
Mydata.noyear<-dplyr::select(Mydata.clean3, -year)

library(caret)
set.seed(1)
index <- createDataPartition(Mydata.year$popularity, p=0.10, list=FALSE)



#convert date to year
Mydata.clean$year<-substr(Mydata.clean$release_date,start = 1, stop = 4)
#group by year
by.year<- dplyr::group_by(Mydata.clean, year)
#average loudness per year
mean.loudness <- dplyr::summarize(by.year, loudness = mean(loudness, na.rm = TRUE))
#average time per year
mean.time<-dplyr::summarize(by.year, time = mean(duration_ms, na.rm = TRUE))


library(hrbrthemes)
library(tidyr)
library(ggplot2)
#line graph
time.label<-c(1922:2021)
ggplot(mean.loudness, aes(x = time.label, y = loudness))+
  geom_line() +labs(title="line graph of average loundness in each year from 1922 to 2021",
                    x="Time",
                    y="loudness(db)")+theme_ipsum()

ggplot(mean.time, aes(x =time.label, y = time)) +
  geom_line() +labs(title="line graph of average duration in each year from 1922 to 2021",
                    x="Time",
                    y="Duration of songs(ms)")+theme_ipsum()


time.label<-c(1975:2021)
mean.loudness<-filter(mean.loudness,year>=1975)
ggplot(mean.loudness, aes(x = time.label, y = loudness))+
  geom_line() +labs(title="line graph of average loundness in each year from 1975 to 2021",
                    x="Time",
                    y="loudness(db)")+theme_ipsum()

time.label<-c(1990:2021)
mean.time<-filter(mean.time,year>=1990)
ggplot(mean.time, aes(x =time.label, y = time)) +
  geom_line() +labs(title="line graph of average duration in each year from 1990 to 2021",
                    x="Time",
                    y="Duration of songs(ms)")+theme_ipsum()
## simple linear regression (1 variable)

#time vs. popularity
#heat map
fit1<-lm(popularity~loudness,Mydata.clean)
summary(fit1)
ggplot(Mydata.clean,aes(x=loudness, y=popularity))+
  geom_bin2d()+
  theme_bw()+  
  geom_bin2d(bins = 25) +
  scale_fill_continuous(type = "viridis") +
  labs(title="Heat map of loundness vs. popularity")

#point plot
ggplot(Mydata.clean,aes(x=loudness, y=popularity))+
  geom_point()+
  theme_bw() +
  scale_fill_continuous(type = "viridis") +
  labs(title="Scatter plot of loundness vs. popularity") 



#loudness vs. popularity
#heat map
fit1<-lm(popularity~loudness,Mydata.clean)
summary(fit1)
ggplot(Mydata.clean,aes(x=loudness, y=popularity))+
  geom_bin2d()+
  theme_bw()+  
  geom_bin2d(bins = 25) +
  scale_fill_continuous(type = "viridis") +geom_smooth(method = glm)+
  labs(title="Heat map of loundness vs. popularity")

#point plot
ggplot(Mydata.clean,aes(x=loudness, y=popularity))+
  geom_point()+
  theme_bw() +
  scale_fill_continuous(type = "viridis") +geom_smooth(method = glm)+
  labs(title="Scatter plot of loundness vs. popularity")  


#energy vs. popularity
ggplot(Mydata.clean,aes(x=loudness, y=energy))+
  geom_bin2d()+
  theme_bw()+  
  geom_bin2d(bins = 25) +
  scale_fill_continuous(type = "viridis") +
  labs(title="Heat map of loundness vs. popularity")
fit2<-lm(popularity~loudness,Mydata.clean)
summary(fit2)





fit1<-lm(popularity~Mydata.year$duration_ms,Mydata.year)
summary(fit1)
ggplot(Mydata.clean,aes(x=duration_ms, y=popularity))+
  geom_bin2d()+
  theme_bw()+  
  geom_bin2d(bins = 30) +
  scale_fill_continuous(type = "viridis") +geom_smooth(method = lm)+
  labs(title="Heat map of duration_ms vs. popularity")
accuracy(list(fit1),plotit = TRUE)

fit1<-lm(popularity~Mydata.year$danceability,Mydata.year)
summary(fit1)
ggplot(Mydata.clean,aes(x=danceability, y=popularity))+
  geom_bin2d()+
  theme_bw()+  
  geom_bin2d(bins = 25) +
  scale_fill_continuous(type = "viridis") +geom_smooth(method = lm)+
  labs(title="Heat map of danceability vs. popularity")
accuracy(list(fit1),plotit = TRUE)

fit1<-lm(popularity~Mydata.year$energy,Mydata.year)
summary(fit1)
ggplot(Mydata.clean,aes(x=energy, y=popularity))+
  geom_bin2d()+
  theme_bw()+  
  geom_bin2d(bins = 25) +
  scale_fill_continuous(type = "viridis") +geom_smooth(method = lm)+
  labs(title="Heat map of energy vs. popularity")
accuracy(list(fit1),plotit = TRUE)

fit1<-lm(popularity~Mydata.year$loudness,Mydata.year)
summary(fit1)
ggplot(Mydata.clean,aes(x=loudness, y=popularity))+
  geom_bin2d()+
  theme_bw()+  
  geom_bin2d(bins = 25) +
  scale_fill_continuous(type = "viridis") +geom_smooth(method = lm)+
  labs(title="Heat map of loudness vs. popularity")
accuracy(list(fit1),plotit = TRUE)

fit1<-lm(popularity~Mydata.year$speechiness,Mydata.year)
summary(fit1)
ggplot(Mydata.clean,aes(x=speechiness, y=popularity))+
  geom_bin2d()+
  theme_bw()+  
  geom_bin2d(bins = 25) +
  scale_fill_continuous(type = "viridis") +geom_smooth(method = lm)+
  labs(title="Heat map of speechiness vs. popularity")
accuracy(list(fit1),plotit = TRUE)

fit1<-lm(popularity~Mydata.year$acousticness,Mydata.year)
summary(fit1)
ggplot(Mydata.clean,aes(x=acousticness, y=popularity))+
  geom_bin2d()+
  theme_bw()+  
  geom_bin2d(bins = 25) +
  scale_fill_continuous(type = "viridis") +geom_smooth(method = lm)+
  labs(title="Heat map of acousticness vs. popularity")
accuracy(list(fit1),plotit = TRUE)

fit1<-lm(popularity~Mydata.year$instrumentalness,Mydata.year)
summary(fit1)
ggplot(Mydata.clean,aes(x=instrumentalness, y=popularity))+
  geom_bin2d()+
  theme_bw()+  
  geom_bin2d(bins = 25) +
  scale_fill_continuous(type = "viridis") +geom_smooth(method = lm)+
  labs(title="Heat map of instrumentalness vs. popularity")
accuracy(list(fit1),plotit = TRUE)

fit1<-lm(popularity~Mydata.year$liveness,Mydata.year)
summary(fit1)
ggplot(Mydata.clean,aes(x=liveness, y=popularity))+
  geom_bin2d()+
  theme_bw()+  
  geom_bin2d(bins = 25) +
  scale_fill_continuous(type = "viridis") +geom_smooth(method = lm)+
  labs(title="Heat map of liveness vs. popularity")
accuracy(list(fit1),plotit = TRUE)

fit1<-lm(popularity~Mydata.year$valence,Mydata.year)
summary(fit1)
ggplot(Mydata.clean,aes(x=valence, y=popularity))+
  geom_bin2d()+
  theme_bw()+  
  geom_bin2d(bins = 25) +
  scale_fill_continuous(type = "viridis") +geom_smooth(method = lm)+
  labs(title="Heat map of valence vs. popularity")
accuracy(list(fit1),plotit = TRUE)

fit1<-lm(popularity~Mydata.year$tempo,Mydata.year)
summary(fit1)
ggplot(Mydata.clean,aes(x=tempo, y=popularity))+
  geom_bin2d()+
  theme_bw()+  
  geom_bin2d(bins = 25) +
  scale_fill_continuous(type = "viridis") +geom_smooth(method = lm)+
  labs(title="Heat map of tempo vs. popularity")
accuracy(list(fit1),plotit = TRUE)

fit1<-lm(popularity~Mydata.year$year,Mydata.year)
summary(fit1)
ggplot(Mydata.clean,aes(x=year, y=popularity))+
  geom_bin2d()+
  theme_bw()+  
  geom_bin2d(bins = 25) +
  scale_fill_continuous(type = "viridis") +geom_smooth(method = lm)+
  labs(title="Heat map of release year vs. popularity")
accuracy(list(fit1),plotit = TRUE)


library(rcompanion)
#mistack!these are categorical.
#fit1<-lm(popularity~time_signature,Mydata.clean)
#summary(fit1)
#ggplot(Mydata.clean,aes(x=keys, y=popularity))+
#  geom_bin2d()+
#  theme_bw()+  
#  geom_bin2d(bins = 25) +
#  scale_fill_continuous(type = "viridis") +geom_smooth(method = glm)+
#  labs(title="Heat map of keys vs. popularity")

#fit1<-lm(popularity~explicit,Mydata.clean)
#summary(fit1)
#ggplot(Mydata.clean,aes(x=explicit, y=popularity))+
#  geom_bin2d()+
#  theme_bw()+  
#  geom_bin2d(bins = 25) +
#  scale_fill_continuous(type = "viridis") +geom_smooth(method = glm,method.args = list(family = "binomial"))+
#  labs(title="Heat map of key vs. popularity")


#fit1<-lm(popularity~mode,Mydata.clean)
#summary(fit1)
#ggplot(Mydata.clean,aes(x=mode, y=popularity))+
#  geom_bin2d()+
#  theme_bw()+  
#  geom_bin2d(bins = 25) +
#  scale_fill_continuous(type = "viridis") +geom_smooth(method = glm,method.args = list(family = "binomial"))+
#  labs(title="Heat map of key vs. popularity")

#quality check
res<-resid(fit2)
plot(fitted(fit2),res)
qqnorm(res)
qqline(res)

#one way anova for the categorical variables
#factor variables: time_signature, explicit,mode,key
aov.key<-aov(data=Mydata.year,popularity~key)
summary(aov.key)
plot(aov.key, 1)

aov.explicit<-aov(data=Mydata.year,popularity~explicit)
summary(aov.explicit)
plot(aov.explicit, 1)

aov.mode<-aov(data=Mydata.year,popularity~mode)
summary(aov.mode)
plot(aov.mode, 1)

aov.time_signature<-aov(data=Mydata.year,popularity~time_signature)
summary(aov.time_signature)
plot(aov.time_signature, 1)

#QC
par(mfrow=c(2,2))
plot(aov.key)
par(mfrow=c(1,1))

library(hrbrthemes)
#visualization
#key
Mydata.year %>%
  ggplot( aes(x=key, y=popularity, fill=key)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  geom_boxplot(width=0.5, color="grey", alpha=0.2) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart of key") 

#time_signature
Mydata.year %>%
  ggplot( aes(x=time_signature, y=popularity, fill=time_signature)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  geom_boxplot(width=0.5, color="grey", alpha=0.2) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart of time signature")

#explicit
Mydata.year %>%
  ggplot( aes(x=explicit, y=popularity, fill=explicit)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  geom_boxplot(width=0.5, color="grey", alpha=0.2) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart of explicit")

#mode
Mydata.year %>%
  ggplot( aes(x=mode, y=popularity, fill=mode)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  geom_boxplot(width=0.5, color="grey", alpha=0.2) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart of mode")

#extra
#loundness vs. energy
ggplot(Mydata.clean,aes(x=loudness, y=energy))+
  geom_bin2d()+
  theme_bw()+  
  geom_bin2d(bins = 25) +
  scale_fill_continuous(type = "viridis") +
  labs(title="Heat map of loundness vs. popularity")
fit2<-lm(popularity~loudness,Mydata.clean)
summary(fit2)

#jump to line 269 to run the result
#stepwise
library(MASS)
library(dplyr)
library(leaps)
library(bestglm)
#select numeric and categories
Mydata.clean2<-dplyr::select(Mydata.clean, -id_artists, -artists, -release_date,
                             -name, -id)
#stepwise linear regression
#full_model <- lm(popularity~. , data = Mydata.clean2)
#step <- stepAIC(full_model, direction="both")
#Start:  AIC=2858742
#popularity ~ duration_ms + explicit + danceability + energy + 
#  key + loudness + mode + speechiness + acousticness + instrumentalness + 
#  liveness + valence + tempo + time_signature + year


#                    Df Sum of Sq       RSS     AIC
#<none>                           105811548 2858742
#- speechiness        1       410 105811958 2858742
#- time_signature     1       961 105812509 2858745
#- key                1      2473 105814022 2858752
#- tempo              1      5056 105816604 2858766
#- duration_ms        1     58402 105869950 2859039
#- mode               1     64382 105875930 2859069
#- valence            1    100861 105912409 2859256
#- energy             1    110855 105922404 2859307
#- danceability       1    130519 105942067 2859408
#- liveness           1    149135 105960684 2859503
#- loudness           1    160741 105972289 2859563
#- instrumentalness   1    410943 106222491 2860841
#- acousticness       1    426873 106238421 2860922
#- explicit           1   1000732 106812281 2863842
#- year             100  25517154 131328702 2975634
#show result
#step$anova 
#Final Model:
#popularity ~ duration_ms + explicit + danceability + energy + 
#  key + loudness + mode + speechiness + acousticness + instrumentalness + 
#  liveness + valence + tempo + time_signature + year
#final model is equal to the full model.

#I found year is categorical variable. Remove year
#no year
#select numeric categories
Mydata.clean4<-dplyr::select(Mydata.clean, -id_artists, -artists, -release_date, -name, -id,-year)
#stepwise linear regression
full_model <- lm(popularity~. , data = Mydata.clean4)
step(full_model, direction="backward")
#Final Model:
#Call:
#  lm(formula = popularity ~ duration_ms + explicit + danceability + 
#       energy + key + loudness + mode + speechiness + acousticness + 
#       instrumentalness + liveness + valence + tempo, data = Mydata.clean4)
#AIC=2975626

#consider time as numeric
Mydata.clean3<-Mydata.clean2
Mydata.clean3$year <-as.numeric(Mydata.clean2$year) 
full_model2 <- lm(popularity~. , data = Mydata.clean3)
step(full_model, direction="backward")
#Call:
#Call:
#  lm(formula = popularity ~ duration_ms + explicit + danceability + 
#       energy + key + loudness + mode + speechiness + acousticness + 
#       instrumentalness + liveness + valence + tempo, data = Mydata.clean4)
#2975626

#Logistic Regression
full_model3 <- glm(popularity~. , data = Mydata.clean3)
step <- step(full_model3, direction="backward",na.rm = TRUE)
#Start:  AIC=4408995
#popularity ~ duration_ms + explicit + danceability + energy + 
#  key + loudness + mode + speechiness + acousticness + instrumentalness + 
#  liveness + valence + tempo + time_signature + year
#same as the full model
full_model3 <- glm(popularity~. , data = Mydata.clean3)
null_model <- lm(popularity ~ 1, data = Mydata.clean3)
step <- step(null_model, direction="forward",na.rm = TRUE,scope = formula(full_model3))
#No year

full_model4 <- glm(popularity~. , data = Mydata.clean4)
step <- step(full_model4, direction="backward",na.rm = TRUE)
#best model
#Start:  AIC=4408995
#popularity ~ duration_ms + explicit + danceability + energy + 
#  key + loudness + mode + speechiness + acousticness + instrumentalness + 
#  liveness + valence + tempo + time_signature 


#USE remove binary to preform stepwise regression!!!

#setting

Mydata<-read.csv("C:/study/STAT 362/project/tracks.csv", encoding="UTF-8")
Mydata.clean<-dplyr::filter(Mydata,popularity>0,release_date!="1900-01-01",tempo>0,na.rm = TRUE)

#convert date to year
Mydata.clean$year<-substr(Mydata.clean$release_date,start = 1, stop = 4)

Mydata.clean2<-dplyr::select(Mydata.clean, -id_artists, -artists, -release_date,
                             -name, -id)
#make factors as factor
Mydata.clean2$explicit<-as.factor(Mydata.clean2$explicit)
Mydata.clean2$key<-as.factor(Mydata.clean2$key)
Mydata.clean2$mode<-as.factor(Mydata.clean2$mode)
Mydata.clean2$time_signature<-as.factor(Mydata.clean2$time_signature)
Mydata.clean3<-Mydata.clean2
Mydata.clean3$year <-as.numeric(Mydata.clean3$year)

Mydata.year<-Mydata.clean3
Mydata.noyear<-dplyr::select(Mydata.clean3, -year)

#with binary and categorical factors
#gaussian
full_model_year.g <- glm(popularity~. , data = Mydata.year)
step_model_year.g <- step(full_model_year.g, direction="both")
#Start:  AIC=4406246
#popularity ~ duration_ms + explicit + danceability + energy + 
#  key + loudness + mode + speechiness + acousticness + instrumentalness + 
#  liveness + valence + tempo + time_signature + year

#poisson
full_model_year.p <- glm(popularity~. , data = Mydata.year,family = poisson)
step_model_year.p <- step(full_model_year.p, direction="both")
#Start:  AIC=6803334
#popularity ~ duration_ms + explicit + danceability + energy + 
#  key + loudness + mode + speechiness + acousticness + instrumentalness + 
#  liveness + valence + tempo + time_signature + year


#no year
#gaussian
full_model_noyear.g <- glm(popularity~. , data = Mydata.noyear)
step_model_noyear.g <- step(full_model_noyear.g, direction="both")
#Start:  AIC=4512243
#popularity ~ duration_ms + explicit + danceability + energy + 
#  key + loudness + mode + speechiness + acousticness + instrumentalness + 
#  liveness + valence + tempo + time_signature

#poisson
full_model_noyear.p <- glm(popularity~. , data = Mydata.noyear,family = poisson)
step_model_noyear.p <- step(full_model_noyear.p, direction="both")
#Start:  AIC=4570669
#popularity ~ duration_ms + explicit + danceability + energy + 
#  key + loudness + mode + speechiness + acousticness + instrumentalness + 
#  liveness + valence + tempo + time_signature

#accuarcy test
library(rcompanion)
accuracy(list(full_model_year.p,full_model_year.g,full_model_noyear.p,full_model_noyear.g))
#$Fit.criteria
#Min.max.accuracy  MAE MedAE MAPE MSE RMSE NRMSE.mean NRMSE.median Efron.r.squared CV.prcnt
#1            0.672 11.1  9.26 1.24 200 14.1      0.474        0.487           0.328     47.4
#2            0.672 11.1  9.14 1.17 200 14.1      0.473        0.487           0.329     47.3
#3            0.645 12.5 11.00 1.45 240 15.5      0.519        0.534           0.195     51.9
#4            0.643 12.6 11.00 1.45 242 15.5      0.521        0.536           0.188     52.1
accuracy(list(full_model_year.p),plotit = TRUE)
accuracy(list(full_model_year.g),plotit = TRUE)
accuracy(list(full_model_noyear.p),plotit = TRUE)
accuracy(list(full_model_noyear.g),plotit = TRUE)


#without binary and categorical factors
Mydata.nobi1<-dplyr::select(Mydata.year,-time_signature, -explicit,-mode,-key)
Mydata.nobi1$year <-as.numeric(Mydata.nobi1$year)
#remove year
Mydata.nobi2<-dplyr::select(Mydata.nobi1, -year)

full_model_nobi <- glm(popularity~. , data = Mydata.nobi1)
step <- step(full_model_nobi, direction="both")
#Start:  AIC=4408409
#popularity ~ duration_ms + danceability + energy + loudness + 
#  speechiness + acousticness + instrumentalness + liveness + 
#  valence + tempo + year

full_model_nobi.p <- glm(popularity~. , data = Mydata.nobi1,family = poisson)
step <- step(full_model_nobi.p, direction="both")
#Start:  AIC=6825224
#popularity ~ duration_ms + danceability + energy + loudness + 
#  speechiness + acousticness + instrumentalness + liveness + 
#  valence + tempo + year

full_model_nobi.G <- glm(popularity~. , data = Mydata.nobi1,family = Gamma)
step <- step(full_model_nobi.G, direction="both")
#Step:  AIC=4498101
#popularity ~ duration_ms + danceability + energy + loudness + 
#  acousticness + instrumentalness + liveness + valence + tempo + 
#  year
lite.model.nobi.G<-glm(popularity ~ duration_ms + danceability + energy + loudness + 
                         acousticness + instrumentalness + liveness + valence + tempo + 
                         year,
                        data = Mydata.nobi1)
#accuracy
accuracy(list(full_model_nobi,full_model_nobi.p,lite.model.nobi.G))
#  `Min.max.accuracy  MAE MedAE MAPE MSE RMSE NRMSE.mean NRMSE.median Efron.r.squared CV.prcnt
#1            0.670 11.1  9.17 1.18 202 14.2      0.476        0.490           0.321     47.6
#2            0.671 11.1  9.28 1.24 201 14.2      0.476        0.489           0.323     47.6
#3            0.670 11.1  9.17 1.18 202 14.2      0.476        0.490           0.321     47.6
accuracy(list(full_model_nobi),plotit = TRUE)
accuracy(list(full_model_nobi.p),plotit = TRUE)
accuracy(list(lite.model.nobi.G),plotit = TRUE)

#no year
Mydata.nobi2<-dplyr::select(Mydata.year, -time_signature, -explicit,-mode,-key,-year)
full_model_nobi2 <- glm(popularity~. , data = Mydata.nobi2)
step <- step(full_model_nobi2, direction="both")
#Step:  AIC=4525159
#popularity ~ duration_ms + danceability + energy + loudness + 
#  acousticness + instrumentalness + liveness + valence + tempo
lite.model.nobi2<-glm(popularity ~ duration_ms + danceability + energy + loudness + 
                            acousticness + instrumentalness + liveness + valence + tempo,
                            data = Mydata.nobi2)

full_model_nobi2.p <- glm(popularity~. , data = Mydata.nobi2,family = poisson)
step <- step(full_model_nobi2.p, direction="both")
#Start:  AIC=7727785
#popularity ~ duration_ms + danceability + energy + loudness + 
#  speechiness + acousticness + instrumentalness + liveness + 
#  valence + tempo


full_model_nobi2.G <- glm(popularity~. , data = Mydata.nobi2,family = Gamma)
step <- step(full_model_nobi2.G, direction="both")
#Start:  AIC=4574047
#popularity ~ duration_ms + danceability + energy + loudness + 
#  speechiness + acousticness + instrumentalness + liveness + 
#  valence + tempo
#accuracy
accuracy(list(lite.model.nobi2,full_model_nobi2.p,full_model_nobi2.G))
#  Min.max.accuracy  MAE MedAE MAPE MSE RMSE NRMSE.mean NRMSE.median Efron.r.squared CV.prcnt
#1            0.642 12.7  11.1 1.46 247 15.7      0.527        0.542           0.168     52.7
#2            0.643 12.7  11.1 1.46 244 15.6      0.523        0.539           0.180     52.3
#3            0.643 12.7  11.1 1.47 244 15.6      0.524        0.539           0.179     52.4

accuracy(list(lite.model.nobi2),plotit = TRUE)
accuracy(list(full_model_nobi2.p),plotit = TRUE)
accuracy(list(full_model_nobi2.G),plotit = TRUE)

#QC for one predict variable vs. response variable
library(ggfortify)
autoplot(lm(popularity ~ duration_ms,data = Mydata.clean3), which=1:4, data=Mydata.clean3)

autoplot(lm(popularity ~ loudness,data = Mydata.clean3), which=1:4, data=Mydata.clean3)

autoplot(lmfit2, which=1:4, data=Mydata.clean4)

#accuarcy test
library(rcompanion)
accuracy(list(glmfit1,glmfit2,lmfit1,lmfit2,glmfit3,glmfit4,glmfit5,glmfit6))
accuracy(list(glmfit1),plotit = TRUE)
accuracy(list(glmfit2),plotit = TRUE)
accuracy(list(glmfit3),plotit = TRUE)
accuracy(list(glmfit4),plotit = TRUE)
accuracy(list(lmfit1),plotit = TRUE)
accuracy(list(lmfit2),plotit = TRUE)

#another way to test accuracy
#setting
Mydata.year.train<-Mydata.year[index,]
Mydata.year.test<-Mydata.year[-index,]
#define popularity as >=29.84
Mydata.year.test<-dplyr::mutate(Mydata.year.test,pop.bi=ifelse(popularity >= 29.84, "1", "0"))
Mydata.year.train<-dplyr::mutate(Mydata.year.train,pop.bi=ifelse(popularity >= 29.84, "1", "0"))

Mydata.year<-dplyr::mutate(Mydata.year,pop.bi=ifelse(popularity >= 29.84, "1", "0"))
Mydata.year$pop.bi<-as.factor(Mydata.year$pop.bi)

Mydata.year.test$pop.bi<-as.factor(Mydata.year.test$pop.bi)
Mydata.year.train$pop.bi<-as.factor(Mydata.year.train$pop.bi) 

Mydata.noyear.train<-dplyr::select(Mydata.year.train,-year)
Mydata.noyear.test<-dplyr::select(Mydata.year.test,-year)

Mydata.nobi.year.train<-dplyr::select(Mydata.year.train,-time_signature, -explicit,-mode,-key)
Mydata.nobi.year.test<-dplyr::select(Mydata.year.test,-time_signature, -explicit,-mode,-key)

Mydata.nobi.noyear.train<-dplyr::select(Mydata.noyear.train,-time_signature, -explicit,-mode,-key)
Mydata.nobi.noyear.test<-dplyr::select(Mydata.noyear.test,-time_signature, -explicit,-mode,-key)


#coding
fit1.train<-glm(pop.bi~mode,Mydata.year.train,family = binomial)
prob <- predict(fit1.train, Mydata.year.test, type = "response")
predicted_class <- ifelse(prob > 0.5, "1", "0")
table(predicted_class, Mydata.year.test$pop.bi)
mean(predicted_class == Mydata.year.test$pop.bi)

fit1.train<-lm(popularity~mode,Mydata.year.train)
cor(predict(fit1.train, Mydata.year.test), Mydata.year.test$popularity, use = "complete.obs")

fit1.train<-glm(popularity~.,Mydata.nobi.noyear.train,family = gaussian)
cor(predict(fit1.train, Mydata.nobi.noyear.test), Mydata.nobi.noyear.test$popularity, use = "complete.obs")

fit1<-glm(popularity~Mydata.year$time_signature,Mydata.year,family = gaussian)
library(rcompanion)liexplicitbrary(rcompanion)
accuracy(list(fit1))
summary(fit1)

fit1<-glm(pop.bi~key,Mydata.year,family = binomial)
accuracy(list(fit1))
summary(fit1)

