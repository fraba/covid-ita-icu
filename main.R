library(gridExtra)
library(tidyverse)
library(lubridate)
library(scales)


dat <- 
  read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")

dat_sumstats <-
  dat %>%
  dplyr::group_by(day = as.Date(data), 
                  codice_regione) %>%
  dplyr::summarise(icu_patients = sum(terapia_intensiva))

icu_sole24ore_14oct <- 
  read.csv("icu-sole24ore-14oct.csv")

dat$codice_regione[dat$codice_regione == 21] <- 4.1
dat$codice_regione[dat$codice_regione == 22] <- 4.2

dat_sumstats$codice_regione[dat_sumstats$codice_regione == 21] <- 4.1
dat_sumstats$codice_regione[dat_sumstats$codice_regione == 22] <- 4.2

regioni$codice_regione[regioni$codice_regione == 21] <- 4.1
regioni$codice_regione[regioni$codice_regione == 22] <- 4.2

dat_sumstats$icu_beds_2020 <-
  icu_sole24ore_14oct$ICU.today[match(dat_sumstats$codice_regione,
                                      icu_sole24ore_14oct$Code)]

dat_sumstats$icu_covid_perc <- 
  dat_sumstats$icu_patients / dat_sumstats$icu_beds_2020

dat_sumstats$Regione <- 
  dat$denominazione_regione[match(dat_sumstats$codice_regione,
                                  dat$codice_regione)]

regioni <- 
  dat %>%
  dplyr::distinct(codice_regione, denominazione_regione)

ggplot(dat_sumstats, aes(x=day,y=factor(Regione, 
                                           levels = 
                                          rev(regioni$denominazione_regione[order(regioni$codice_regione)])))) +
  geom_tile(aes(fill = icu_covid_perc)) +
  scale_fill_distiller(palette = "Spectral", direction = -1,
                       label = percent) +
  labs(x = NULL, y = NULL, title = "COVID-19 patients on 2020 capacity") +
  theme_bw()

dat_nationwide <- 
  dat %>%
  dplyr::group_by(day = as.Date(data)) %>%
  dplyr::summarise(icu_patients = sum(terapia_intensiva))

updated_on <-
  max(dat_nationwide$day)

train <-
  dat_nationwide %>% 
  dplyr::filter(day >= as.Date("2020-07-01")) %>%
  dplyr::mutate(day = as.numeric(day) - 
                  as.numeric(as.Date("2020-06-30")))

c.0 <- min(train$icu_patients) * 0.5
model.0 <- lm(log(icu_patients - c.0) ~ day, data=train)
start <- list(a=exp(coef(model.0)[1]), b=coef(model.0)[2], c=c.0)

fit <- 
  nls(icu_patients ~  a * exp(b * day) + c, 
      data=train,
      start = start)

plot(train)
p <- coef(fit)
curve(p["a"] * exp(p["b"] * x) + p["c"], lwd=2, col="Red", add=TRUE)

diffdays <- 
  as.numeric(updated_on+24 - as.Date("2020-07-01"))

prediction <- 
  predict(fit,
          newdata = data.frame(day = 1:diffdays))

dat_nationwide %>%
  ggplot(aes(x=day, y = icu_patients)) +
  geom_line() + 
  geom_point(size = .5) +
  theme_bw() +
  labs(title = sprintf("COVID-19 ICU patients (Italy, updated %s)", updated_on), 
       caption = "1st wave peak on 3 Apr 2020,\n24 days after nation-wide lockdown",
       x = NULL, y = NULL) +
  
  annotate("rect", xmin=as.Date("2020-03-09"), 
           xmax=as.Date("2020-04-03"),
           ymax = Inf, 
           ymin=0,
           fill="red", alpha=0.3) +
  
  annotate("rect", xmin=updated_on, 
           xmax=updated_on+24,
           ymax = Inf, 
           ymin=0,
           fill="blue", alpha=0.3) +
  
  geom_vline(xintercept = as.Date("2020-04-03"),
             linetype = 2) +
  geom_hline(yintercept = sum(icu_sole24ore_14oct$ICU.today)) +
  annotate("text", x = as.Date("2020-07-01"), 
           y = sum(icu_sole24ore_14oct$ICU.today) + 300,
           label = sprintf("Current ICU capacity = %s", 
                           sum(icu_sole24ore_14oct$ICU.today))) +
  geom_hline(yintercept = sum(icu_sole24ore_14oct$ICU.today) +
               sum(icu_sole24ore_14oct$ICU.extra.planned)) +
  annotate("text", x = as.Date("2020-07-01"), 
           y = sum(icu_sole24ore_14oct$ICU.today) +
             sum(icu_sole24ore_14oct$ICU.extra.planned) + 300,
           label = sprintf("Planned ICU capacity = %s", 
                           sum(icu_sole24ore_14oct$ICU.today) + 
                             sum(icu_sole24ore_14oct$ICU.extra.planned))) +
  geom_line(data = data.frame(icu_patients = prediction,
                              day = seq(as.Date("2020-07-01"),
                                        length.out = diffdays,
                                        by = 1)),
            colour = 'red')
  

# Regional models

regional_predictions <- 
  data.frame()

regional_overcapacity_on.df <- 
  data.frame()

reg_plots.list <- list()

for (i in 1:nrow(regioni)) {
  
  # if (regioni$codice_regione[i] %in% c(4.2, 14)) next
  
  print(i)
  
  this_dat <- 
    dat_sumstats %>% 
    dplyr::filter(codice_regione == 
                    regioni$codice_regione[i]) %>%
    dplyr::select(day, icu_patients)
  
  this_train <-
    this_dat %>% 
    dplyr::filter(day >= as.Date("2020-07-01"))
  
  this_model_start_on <- 
    this_train$day[min(which(this_train$icu_patients > 0))]
  
  this_train <-
    this_dat %>% 
    dplyr::filter(day >= this_model_start_on) %>%
    dplyr::mutate(day = as.numeric(day) - 
                    as.numeric(this_model_start_on - 1))
  
  this_train$icu_patients[this_train$icu_patients == 0] <- 1
  
  c.0 <- min(this_train$icu_patients) * 0.5
  model.0 <- lm(log(icu_patients - c.0) ~ day, data=this_train)
  start <- list(a=exp(coef(model.0)[1]), b=coef(model.0)[2], c=c.0)
  
  this_fit <- 
    nls(icu_patients ~  a * exp(b * day) + c, 
        data=this_train,
        start = start)
  
  # plot(this_train)
  # p <- coef(this_fit)
  # curve(p["a"] * exp(p["b"] * x) + p["c"], lwd=2, col="Red", add=TRUE)
  
  this_diffdays <- 
    as.numeric(updated_on+30 - this_model_start_on)
  
  this_prediction <- 
    predict(this_fit,
            newdata = data.frame(day = 1:this_diffdays))
  
  this_prediction.df <- 
    data.frame(codice_regione = regioni$codice_regione[i],
               icu_patients = this_prediction,
               day = seq(this_model_start_on,
                         (this_model_start_on-1)+this_diffdays,
                         1))
  
  regional_predictions <- 
    rbind(regional_predictions,
          this_prediction.df)
  
  this_train$day <- 
    seq(this_model_start_on,
        updated_on,
        1)
  
  this_prediction_overcapacity.df <- 
    this_prediction.df %>%
    dplyr::filter(icu_patients > 
                    icu_sole24ore_14oct$ICU.today[icu_sole24ore_14oct$Code == 
                                                    unique(this_prediction.df$codice_regione)])
  
  regional_overcapacity_on.df <- 
    rbind(regional_overcapacity_on.df, 
    data.frame(name = regioni$denominazione_regione[i],
               code = regioni$codice_regione[i],
               date = min(this_prediction_overcapacity.df$day)))
    
  reg_plots.list[[i]] <- 
    ggplot(this_train,
           aes(x = day, y = icu_patients)) +
    geom_point(shape = 1) +
    geom_line(data = this_prediction.df %>%
                filter(day <= updated_on), aes(x= day, y = icu_patients),
              colour = 'red') +
    labs(x = NULL, y = NULL, title = regioni$denominazione_regione[i],
           caption = sprintf("Model starts on: %s\nCapacity predicted to be reached on %s",
                             this_model_start_on, 
                             min(this_prediction_overcapacity.df$day)))
}

ggsave(filename="regional_models-1.png", width = 9, height = 6,
       do.call("grid.arrange", c(reg_plots.list[1:7], ncol=3)))

ggsave(filename="regional_models-2.png", width = 9, height = 6,
       do.call("grid.arrange", c(reg_plots.list[8:14], ncol=3)))

ggsave(filename="regional_models-3.png", width = 9, height = 6,
       do.call("grid.arrange", c(reg_plots.list[15:21], ncol=3)))


regional_predictions <- 
  regional_predictions %>%
  dplyr::filter(day > updated_on)

regional_predictions$icu_beds_2020 <- 
  icu_sole24ore_14oct$ICU.today[match(regional_predictions$codice_regione,
                                      icu_sole24ore_14oct$Code)]

regional_predictions$icu_covid_perc <- 
  regional_predictions$icu_patients / regional_predictions$icu_beds_2020

dat_sumstats_prediction <- 
  rbind(dat_sumstats %>% 
          dplyr::select(codice_regione, icu_patients, day, 
          icu_beds_2020, icu_covid_perc), 
        regional_predictions)

dat_sumstats_prediction$Regione <- 
  regioni$denominazione_regione[match(dat_sumstats_prediction$codice_regione,
                                      regioni$codice_regione)]

ggplot(dat_sumstats_prediction %>%
         filter(day < as.Date("2020-11-25")), 
       aes(x=day,y=factor(Regione, 
                          levels = 
                            rev(regioni$denominazione_regione[order(regioni$codice_regione)])))) +
  geom_tile(aes(fill = icu_covid_perc)) +
  scale_fill_distiller(palette = "Spectral", direction = -1,
                       label = percent) +
  labs(x = NULL, y = NULL, title = "COVID-19 patients on total ICU capacity",
       fill = "regional capacity",
      source: "Protezione civile, Sole 24ore") +
  theme_bw() +
  geom_vline(xintercept = updated_on, alpha = .5) +
  geom_point(data  = regional_overcapacity_on.df %>%
               dplyr::filter(date < as.Date("2020-11-25")),
             aes(x = date, y = name, shape = "1")) +
  scale_shape_manual(name = NULL, 
                      values = 1, labels = "100% capacity reached")
  