library(gridExtra)
library(tidyverse)
library(lubridate)
library(scales)
library(zoo)

dat <- 
  read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")

dat_sumstats <-
  dat %>%
  dplyr::group_by(day = as.Date(data), 
                  codice_regione) %>%
  dplyr::summarise(icu_patients = sum(terapia_intensiva))

icu_sole24ore_28oct <- 
  read.csv("icu-sole24ore-28oct.csv")

dat$codice_regione[dat$codice_regione == 21] <- 4.1
dat$codice_regione[dat$codice_regione == 22] <- 4.2

dat_sumstats$codice_regione[dat_sumstats$codice_regione == 21] <- 4.1
dat_sumstats$codice_regione[dat_sumstats$codice_regione == 22] <- 4.2

regioni$codice_regione[regioni$codice_regione == 21] <- 4.1
regioni$codice_regione[regioni$codice_regione == 22] <- 4.2

dat_sumstats$icu_beds_2020 <-
  icu_sole24ore_28oct$ICU.today[match(dat_sumstats$codice_regione,
                                      icu_sole24ore_28oct$Code)]

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

ggsave(filename = "line.png", width = 10, height = 5,
       dat_nationwide %>%
         ggplot(aes(x=day, y = icu_patients)) +
         geom_line() + 
         geom_point(size = .5) +
         theme_bw() +
         labs(title = sprintf("COVID-19 ICU patients (Italy, updated on %s)", updated_on), 
              caption = "1st wave peak on 3 Apr 2020,\n24 days after nation-wide lockdown\nSources: Protezione civile, Il Sole 24 Ore; Code: git.io/JTAiO",
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
         geom_hline(yintercept = sum(icu_sole24ore_28oct$ICU.today * .3)) +
         annotate("text", x = as.Date("2020-07-01"), 
                  y = sum(icu_sole24ore_28oct$ICU.today * .3) + 300,
                  label = "30% of current ICU capacity") +
         geom_hline(yintercept = sum(icu_sole24ore_28oct$ICU.today)) +
         annotate("text", x = as.Date("2020-07-01"), 
                  y = sum(icu_sole24ore_28oct$ICU.today) + 300,
                  label = sprintf("Current ICU capacity = %s", 
                                  sum(icu_sole24ore_28oct$ICU.today))) +
         geom_line(data = data.frame(icu_patients = prediction,
                                     day = seq(as.Date("2020-07-01"),
                                               length.out = diffdays,
                                               by = 1)),
                   colour = 'red'))
  

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
                    icu_sole24ore_28oct$ICU.today[icu_sole24ore_28oct$Code == 
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


regional_overcapacity_on_diff7.df <- 
  data.frame()

for (i in 1:nrow(regioni)) {
  
  if (regioni$codice_regione[i] %in% c(4.2)) next
  
  print(i)
  
  this_dat <- 
    dat_sumstats %>% 
    dplyr::filter(codice_regione == 
                    regioni$codice_regione[i] &
                    day <= (updated_on - 7)) %>%
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
    as.numeric(updated_on+30-7 - this_model_start_on)
  
  this_prediction <- 
    predict(this_fit,
            newdata = data.frame(day = 1:this_diffdays))
  
  this_prediction.df <- 
    data.frame(codice_regione = regioni$codice_regione[i],
               icu_patients = this_prediction,
               day = seq(this_model_start_on,
                         (this_model_start_on-1)+this_diffdays,
                         1))
  
  this_train$day <- 
    seq(this_model_start_on,
        updated_on-7,
        1)
  
  this_prediction_overcapacity.df <- 
    this_prediction.df %>%
    dplyr::filter(icu_patients > 
                    icu_sole24ore_28oct$ICU.today[icu_sole24ore_28oct$Code == 
                                                    unique(this_prediction.df$codice_regione)])
  
  regional_overcapacity_on_diff7.df <- 
    rbind(regional_overcapacity_on_diff7.df, 
          data.frame(name = regioni$denominazione_regione[i],
                     code = regioni$codice_regione[i],
                     date = min(this_prediction_overcapacity.df$day)))
}

regional_predictions <- 
  regional_predictions %>%
  dplyr::filter(day > updated_on)

regional_predictions$icu_beds_2020 <- 
  icu_sole24ore_28oct$ICU.today[match(regional_predictions$codice_regione,
                                      icu_sole24ore_28oct$Code)]

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

ggsave(filename = "heatmap.png", width = 10, height = 5,
       ggplot(dat_sumstats_prediction %>%
                filter(day < as.Date("2020-11-30")), 
              aes(x=day,y=factor(Regione, 
                                 levels = 
                                   rev(regioni$denominazione_regione[order(regioni$codice_regione)])))) +
         geom_tile(aes(fill = icu_covid_perc)) +
         scale_fill_distiller(palette = "Spectral", direction = -1,
                              label = percent) +
         labs(x = NULL, y = NULL, title = sprintf("COVID-19 patients on total current ICU capacity, updated on %s", updated_on),
              fill = "regional capacity",
              caption = "Sources: Protezione civile, Il Sole 24 Ore; Code: git.io/JTAiO") +
         theme_bw() +
         geom_vline(xintercept = updated_on, alpha = .5) +
         geom_point(data  = regional_overcapacity_on.df %>%
                      dplyr::filter(date < as.Date("2020-11-30")),
                    aes(x = date, y = name, shape = "1")) +
         geom_point(data  = regional_overcapacity_on_diff7.df %>%
                      dplyr::filter(date < as.Date("2020-11-25")),
                    aes(x = date, y = name, shape = "2")) +
         scale_shape_manual(name = NULL, 
                            values = c(1,2), 
                            labels = c("current model",
                                       "last week model")) +
         guides(shape=guide_legend(title="100% capacity reached")))


ggsave(filename = "heatmap-zoom.png", width = 10, height = 5,
       ggplot(dat_sumstats_prediction %>%
                filter(day < as.Date("2020-11-30")), 
              aes(x=day,y=factor(Regione, 
                                 levels = 
                                   rev(regioni$denominazione_regione[order(regioni$codice_regione)])))) +
         geom_tile(aes(fill = icu_covid_perc)) +
         scale_fill_distiller(palette = "Spectral", direction = -1,
                              label = percent) +
         labs(x = NULL, y = NULL, title = sprintf("COVID-19 patients on total current ICU capacity, updated on %s", updated_on),
              fill = "regional capacity",
              caption = "Sources: Protezione civile, Il Sole 24 Ore; Code: git.io/JTAiO") +
         theme_bw() +
         geom_vline(xintercept = updated_on, alpha = .5) +
         geom_point(data  = regional_overcapacity_on.df %>%
                      dplyr::filter(date < as.Date("2020-11-30")),
                    aes(x = date, y = name, shape = "1")) +
         geom_point(data  = regional_overcapacity_on_diff7.df %>%
                      dplyr::filter(date < as.Date("2020-11-25")),
                    aes(x = date, y = name, shape = "2")) +
         scale_shape_manual(name = NULL, 
                            values = c(1,2), 
                            labels = c("current model",
                                       "last week model")) +
         guides(shape=guide_legend(title="100% capacity reached")) +
         coord_cartesian(xlim = c(ymd("2020-09-01"), ymd("2020-12-01"))))

# Deaths ( control)

dat <- 
  read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")

nationwide_deaths.df <-
  dat %>%
  dplyr::mutate(day = as.Date(data)) %>%
  dplyr::group_by(day) %>%
  dplyr::summarise(deceduti = sum(deceduti)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(day) %>%
  dplyr::mutate(variazione_deceduti = deceduti - lag(deceduti),
                variazione_deceduti_ma7 = 
                  rollmean(variazione_deceduti, 7, align = "right", fill = NA))

train <-
  nationwide_deaths.df %>% 
  dplyr::filter(day >= as.Date("2020-07-01")) %>%
  dplyr::mutate(day = as.numeric(day) - 
                  as.numeric(as.Date("2020-06-30"))) %>%
  dplyr::select(day, variazione_deceduti_ma7)

c.0 <- min(train$variazione_deceduti_ma7) * 0.5
model.0 <- lm(log(variazione_deceduti_ma7 - c.0) ~ day, data=train)
start <- list(a=exp(coef(model.0)[1]), b=coef(model.0)[2], c=c.0)

fit <- 
  nls(variazione_deceduti_ma7 ~  a * exp(b * day) + c, 
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

prediction_today.df <-
  data.frame(variazione_deceduti_ma7 = prediction,
             day = seq(as.Date("2020-07-01"),
                       length.out = diffdays,
                       by = 1))

train <-
  nationwide_deaths.df %>% 
  dplyr::filter(day >= as.Date("2020-07-01") &
                  day < (updated_on - 7)) %>%
  dplyr::mutate(day = as.numeric(day) - 
                  as.numeric(as.Date("2020-06-30"))) %>%
  dplyr::select(day, variazione_deceduti_ma7)

c.0 <- min(train$variazione_deceduti_ma7) * 0.5
model.0 <- lm(log(variazione_deceduti_ma7 - c.0) ~ day, data=train)
start <- list(a=exp(coef(model.0)[1]), b=coef(model.0)[2], c=c.0)

fit <- 
  nls(variazione_deceduti_ma7 ~  a * exp(b * day) + c, 
      data=train,
      start = start)

# plot(train)
# p <- coef(fit)
# curve(p["a"] * exp(p["b"] * x) + p["c"], lwd=2, col="Red", add=TRUE)

diffdays <- 
  as.numeric(updated_on+24 - as.Date("2020-07-01"))

prediction <- 
  predict(fit,
          newdata = data.frame(day = 1:diffdays))

prediction_diff7.df <-
  data.frame(variazione_deceduti_ma7 = prediction,
             day = seq(as.Date("2020-07-01"),
                       length.out = diffdays,
                       by = 1))

peak_deaths_on_diff7 <- 
  prediction_diff7.df %>%
  dplyr::filter(variazione_deceduti_ma7 > 
                  821.428571)
peak_deaths_on_diff7 <- min(peak_deaths_on_diff7$day)

peak_deaths_on_today <- 
  prediction_today.df %>%
  dplyr::filter(variazione_deceduti_ma7 > 
                  821.428571)
peak_deaths_on_today <- min(peak_deaths_on_today$day)


ggsave(filename = "line-death.png", width = 7, height = 4,
       ggplot(nationwide_deaths.df, 
              aes(x = day, y = variazione_deceduti_ma7)) +
         geom_point() +
         geom_line(data = prediction_today.df, colour = "red", aes(linetype = "1")) +
         geom_line(data = prediction_diff7.df, colour = "red", aes(linetype = "2")) +
         
         annotate("text", label = "1st wave peak: 821 deaths",
                  x = as.Date("2020-10-22"),
                  y = 850) +
         geom_hline(yintercept = 821.428571, linetype = 2) +
         
         coord_cartesian(xlim = c(ymd("2020-10-01"), ymd(updated_on + 7)),
                         ylim = c(0, 1000)) +
         scale_linetype_manual(values = c(1,2), 
                               labels = c(sprintf("%s model, 1st wave peak reached on %s", 
                                                  format(updated_on, "%d %b"),
                                                  format(peak_deaths_on_today, "%d %b")),
                                          sprintf("%s model, 1st wave peak reached on %s", 
                                                  format(updated_on - 8, "%d %b"),
                                                  format(peak_deaths_on_diff7, "%d %b")))) +
         labs(y = NULL, x = "new deaths (7-day moving average)", 
              linetype = 'forecasts',
              caption = "Sources: Protezione civile; Code: git.io/JTAiO") +
         theme_bw() +
         theme(legend.position = c(.3,.4),
               legend.direction = 'vertical'))
  


## Other 


region_cases_icu.df <-
  dat %>%
  dplyr::mutate(day = as.Date(data)) %>%
  dplyr::group_by(codice_regione) %>%
  dplyr::arrange(day) %>%
  dplyr::mutate(variazione_terapia_intensiva = 
                  terapia_intensiva - lag(terapia_intensiva), 
                variazione_totale_positivi_ma7 = 
                  rollmean(variazione_totale_positivi, 7, fill = NA, align = 'right'),
                variazione_terapia_intensiva_ma7 = 
                  rollmean(variazione_terapia_intensiva, 7, fill = NA, align = 'right')) %>%
  dplyr::select(codice_regione, denominazione_regione, 
                day, variazione_totale_positivi_ma7, variazione_terapia_intensiva_ma7)


region_cases_icu.df %>%
  dplyr::filter(day >= as.Date("2020-10-07")) %>%
  ggplot(aes(x=day, y = 
               (variazione_terapia_intensiva_ma7 / variazione_totale_positivi_ma7) * 100)) +
  geom_line() +
  theme_bw() +
  facet_wrap(denominazione_regione~., scales = "free_y")

  


# Additional control


dat <- 
  dat %>%
  dplyr::group_by(codice_regione) %>%
  dplyr::arrange(as.Date(data)) %>%
  dplyr::mutate(variazione_deceduti = deceduti - lag(deceduti)) %>%
  dplyr::mutate(variazione_deceduti_ma7 = 
                  rollmean(variazione_deceduti, 7, fill = NA, align = 'right'))

max_deaths_regione <- 
  dat %>%
  dplyr::filter(as.Date(data) < as.Date("2020-07-01")) %>%
  dplyr::group_by(codice_regione) %>%
  dplyr::summarise(deaths = max(variazione_deceduti, na.rm = T))

dat_deaths_sumstats <- 
  dat %>%
  dplyr::group_by(day = as.Date(data), 
                  codice_regione) %>%
  dplyr::summarise(deaths = sum(variazione_deceduti_ma7))
  
regional_deaths_predictions <- 
  data.frame()

regional_deaths_on.df <- 
  data.frame()

reg_death_plots.list <- list()

for (i in 1:nrow(regioni)) {
  
  if (regioni$codice_regione[i] %in% c(8, 14, 18)) next
  
  print(i)
  
  this_dat <- 
    dat_deaths_sumstats %>% 
    dplyr::filter(codice_regione == 
                    regioni$codice_regione[i]) %>%
    dplyr::select(day, deaths)
  
  this_train <-
    this_dat %>% 
    dplyr::filter(day >= as.Date("2020-08-01"))
  
  this_model_start_on <- 
    this_train$day[min(which(this_train$deaths > 0))]
  
  this_train <-
    this_dat %>% 
    dplyr::filter(day >= this_model_start_on) %>%
    dplyr::mutate(day = as.numeric(day) - 
                    as.numeric(this_model_start_on - 1))
  
  c.0 <- min(this_train$deaths) * 0.5
  
  this_train$deaths <- this_train$deaths + 1
  
  model.0 <- lm(log(deaths - c.0) ~ day, data=this_train)
  start <- list(a=exp(coef(model.0)[1]), b=coef(model.0)[2], c=c.0)
  
  this_fit <- 
    nls(deaths ~  a * exp(b * day) + c, 
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
               deaths = this_prediction,
               day = seq(this_model_start_on,
                         (this_model_start_on-1)+this_diffdays,
                         1))
  
  regional_deaths_predictions <- 
    rbind(regional_deaths_predictions,
          this_prediction.df)
  
  this_train$day <- 
    seq(this_model_start_on,
        updated_on,
        1)
  
  this_prediction_deaths.df <- 
    this_prediction.df %>%
    dplyr::filter(deaths > 
                    max_deaths_regione$deaths[max_deaths_regione$codice_regione == 
                                                    unique(this_prediction.df$codice_regione)])
  
  regional_deaths_on.df <- 
    rbind(regional_deaths_on.df, 
          data.frame(name = regioni$denominazione_regione[i],
                     code = regioni$codice_regione[i],
                     date = min(this_prediction_deaths.df$day)))
  
  reg_death_plots.list[[i]] <- 
    ggplot(this_train,
           aes(x = day, y = deaths)) +
    geom_point(shape = 1) +
    geom_line(data = this_prediction.df %>%
                filter(day <= updated_on), aes(x= day, y = deaths),
              colour = 'red') +
    labs(x = NULL, y = NULL, title = regioni$denominazione_regione[i],
         caption = sprintf("Model starts on: %s\nMax 1st wave daily mortality predicted on %s",
                           this_model_start_on, 
                           min(this_prediction_deaths.df$day)))
}
  

ggsave(filename="regional_deaths_models-1.png", width = 9, height = 6,
       do.call("grid.arrange", c(reg_death_plots.list[c(1, 4, 6:10)], ncol=3)))

ggsave(filename="regional_deaths_models-2.png", width = 9, height = 6,
       do.call("grid.arrange", c(reg_death_plots.list[12:17], ncol=3)))

ggsave(filename="regional_deaths_models-3.png", width = 9, height = 6,
       do.call("grid.arrange", c(reg_death_plots.list[c(18:21)], ncol=3)))

regional_deaths_on_7diff.df <- 
  data.frame()

for (i in 1:nrow(regioni)) {
  
  if (regioni$codice_regione[i] %in% c(8, 14, 18)) next
  
  print(i)
  
  this_dat <- 
    dat_deaths_sumstats %>% 
    dplyr::filter(codice_regione == 
                    regioni$codice_regione[i] &
                    day <= (updated_on - 7)) %>%
    dplyr::select(day, deaths)
  
  this_train <-
    this_dat %>% 
    dplyr::filter(day >= as.Date("2020-08-01"))
  
  this_model_start_on <- 
    this_train$day[min(which(this_train$deaths > 0))]
  
  this_train <-
    this_dat %>% 
    dplyr::filter(day >= this_model_start_on) %>%
    dplyr::mutate(day = as.numeric(day) - 
                    as.numeric(this_model_start_on - 1))
  
  c.0 <- min(this_train$deaths) * 0.5
  
  this_train$deaths <- this_train$deaths + 1
  
  model.0 <- lm(log(deaths - c.0) ~ day, data=this_train)
  start <- list(a=exp(coef(model.0)[1]), b=coef(model.0)[2], c=c.0)
  
  this_fit <- 
    nls(deaths ~  a * exp(b * day) + c, 
        data=this_train,
        start = start)
  
  # plot(this_train)
  # p <- coef(this_fit)
  # curve(p["a"] * exp(p["b"] * x) + p["c"], lwd=2, col="Red", add=TRUE)
  
  this_diffdays <- 
    as.numeric(updated_on+30-7 - this_model_start_on)
  
  this_prediction <- 
    predict(this_fit,
            newdata = data.frame(day = 1:this_diffdays))
  
  this_prediction.df <- 
    data.frame(codice_regione = regioni$codice_regione[i],
               deaths = this_prediction,
               day = seq(this_model_start_on,
                         (this_model_start_on-1)+this_diffdays,
                         1))

  this_train$day <- 
    seq(this_model_start_on,
        updated_on-7,
        1)
  
  this_prediction_deaths.df <- 
    this_prediction.df %>%
    dplyr::filter(deaths > 
                    max_deaths_regione$deaths[max_deaths_regione$codice_regione == 
                                                unique(this_prediction.df$codice_regione)])
  
  regional_deaths_on_7diff.df <- 
    rbind(regional_deaths_on_7diff.df, 
          data.frame(name = regioni$denominazione_regione[i],
                     code = regioni$codice_regione[i],
                     date = min(this_prediction_deaths.df$day)))
}

regioni_pop <- 
  read.csv("regioni-pop.csv", skip = 1) %>%
  dplyr::filter(Età == "Totale",
                Regione != "Trentino-Alto Adige/Südtirol") %>%
  dplyr::mutate(tot = Totale.Femmine + Totale.Maschi) %>%
  dplyr::select(Regione, pop = tot)

province_pop <- 
  read.csv("province-pop.csv", skip = 1) %>%
  dplyr::filter(Età == "Totale",
                 Provincia %in% c("Bolzano/Bozen", "Trento")) %>%
  dplyr::mutate(tot = Totale.Femmine + Totale.Maschi) %>%
  dplyr::select(Regione = Provincia, pop = tot)

regioni_pop <- 
  rbind(regioni_pop, province_pop)

regioni_pop$codice_regione <- 
  regioni$codice_regione[match(regioni_pop$Regione, 
                               regioni$denominazione_regione)]

regioni_pop$codice_regione[
  regioni_pop$Regione == "Valle d'Aosta/Vallée d'Aoste"] <- 2.0
regioni_pop$codice_regione[
  regioni_pop$Regione == "Friuli-Venezia Giulia"] <- 6.0
regioni_pop$codice_regione[
  regioni_pop$Regione == "Bolzano/Bozen"] <- 4.1
regioni_pop$codice_regione[
  regioni_pop$Regione == "Trento"] <- 4.2

dat_deaths_sumstats$pop <- 
  regioni_pop$pop[match(dat_deaths_sumstats$codice_regione,
                        regioni_pop$codice_regione)]

dat_deaths_sumstats$deaths_perc <- 
  dat_deaths_sumstats$deaths / (dat_deaths_sumstats$pop / 10000)

regional_deaths_predictions <- 
  regional_deaths_predictions %>%
  dplyr::filter(day > updated_on)

regional_deaths_predictions$pop <- 
  regioni_pop$pop[match(regional_deaths_predictions$codice_regione,
                        regioni_pop$codice_regione)]

regional_deaths_predictions$deaths_perc <- 
  regional_deaths_predictions$deaths / 
  (regional_deaths_predictions$pop / 10000)

dat_deaths_sumstats_prediction <- 
  rbind(dat_deaths_sumstats,
        regional_deaths_predictions)

dat_deaths_sumstats_prediction$Regione <- 
  regioni$denominazione_regione[match(dat_deaths_sumstats_prediction$codice_regione,
                                      regioni$codice_regione)]


ggsave(filename = "heatmap_deaths.png", width = 10, height = 5,
       ggplot(dat_deaths_sumstats_prediction %>%
                filter(day < as.Date("2020-11-30")), 
              aes(x=day,y=factor(Regione, 
                                 levels = 
                                   rev(regioni$denominazione_regione[order(regioni$codice_regione)])))) +
         geom_tile(aes(fill = deaths_perc)) +
         scale_fill_distiller(palette = "Spectral", direction = -1) +
         labs(x = NULL, y = NULL, title = sprintf("COVID-19 deaths, updated on %s", updated_on),
              fill = "per 10,000 people",
              caption = "Sources: Protezione civile, Il Sole 24 Ore; Code: git.io/JTAiO") +
         theme_bw() +
         geom_vline(xintercept = updated_on, alpha = .5) +
         geom_point(data  = regional_deaths_on.df %>%
                      dplyr::filter(date < as.Date("2020-11-30")),
                    aes(x = date, y = name, shape = "1")) +
         geom_point(data  = regional_deaths_on_7diff.df %>%
                      dplyr::filter(date < as.Date("2020-11-30")),
                    aes(x = date, y = name, shape = "2")) +
         scale_shape_manual(name = NULL, 
                            values = c(1,2), 
                            labels = c("current model",
                                       "last week model")) +
         guides(shape=guide_legend(title="100% capacity reached")))


# Casi / Provincia
dat <- 
  read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv") %>%
  dplyr::mutate(date = as.Date(data)) %>%
  dplyr::group_by(denominazione_provincia) %>%
  dplyr::arrange() %>%
  dplyr::mutate(nuovi_casi = totale_casi - lag(totale_casi))

dat$sigla_provincia[is.na(dat$sigla_provincia)] <- "NA"

province_pop <- 
  read.csv("province-pop.csv", skip = 1) %>%
  dplyr::filter(Età == "Totale") %>%
  dplyr::mutate(tot = Totale.Femmine + Totale.Maschi) %>%
  dplyr::select(Provincia = Provincia, pop = tot)

province_pop$Provincia[province_pop$Provincia == "Bolzano/Bozen"] <- "Bolzano"
province_pop$Provincia[province_pop$Provincia == "Valle d'Aosta/Vallée d'Aoste"] <- "Aosta"
province_pop$Provincia[province_pop$Provincia == "Massa-Carrara"] <- "Massa Carrara"

dat$pop <- 
  province_pop$pop[match(dat$denominazione_provincia,
                         province_pop$Provincia)]

require(zoo)

dat <- 
  dat %>%
  dplyr::group_by(sigla_provincia) %>%
  dplyr::mutate(nuovi_casi_per10k_mar7 = rollmean(nuovi_casi / (pop / 10000), 
                                                  7, 
                                                  align = "right",
                                                  fill = NA)) %>%
  dplyr::filter(!is.na(pop) & date == max(date))

dat$sigla_provincia <-
  factor(dat$sigla_provincia, 
         levels = dat$sigla_provincia[order(dat$nuovi_casi_per10k_mar7, decreasing = T)])

dat$colore_regione <- "giallo"
dat$colore_regione[dat$denominazione_regione %in% 
                     c("Valle d'Aosta", "Piemonte", "Lombardia",
                       "Calabria")] <- "rosso"
dat$colore_regione[dat$denominazione_regione %in% 
                     c("Puglia", "Sicilia", "Liguria", "Toscana", 
                     "Umbria", "Abruzzo", "Basilicata")] <- "arancione"
dat$colore_regione[dat$denominazione_provincia %in% 
                     c("Bolzano")] <- "rosso"
dat$colore_regione <- 
  factor(dat$colore_regione, levels = c("giallo","arancione","rosso"))

ggsave(filename = "bar.png", width = 11, height = 8,
       dat %>% 
         ggplot(aes(x = sigla_provincia, y =  nuovi_casi_per10k_mar7)) +
         geom_bar(stat = 'identity', aes(fill = colore_regione)) +
         scale_fill_manual(labels = c("scenario 2", "scenario 3", "scenario 4"),
                           values = c("#ffff99", "#ff7f00", "#e31a1c")) +
         theme_bw() +
         labs(x = sprintf('nuovi casi per 10,000 abitanti (%s, media mobile 7 giorni)', 
                          format(max(dat$date), format = "%d %b")), y = NULL,
              fill = "colore regione (11 Nov)") +
         theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
               legend.position = "bottom"))

unique(dat$denominazione_provincia[is.na(dat$pop)])

prop.table(table(dat$colore_regione[dat$nuovi_casi_per10k_mar7 >
                                      quantile(dat$nuovi_casi_per10k_mar7, p = .8)]))


require(sf)
prov.sf <- 
  read_sf("../covid-ita-mobility/ProvCM01012020_g/ProvCM01012020_g_WGS84.shp")

prov.sf <- 
  merge(prov.sf,
        dat,
        by.x = "SIGLA", 
        by.y = "sigla_provincia")

color.sf <- 
  prov.sf %>%
  dplyr::group_by(colore_regione) %>%
  dplyr::summarize(mean = mean(nuovi_casi_per10k_mar7))

ggsave(filename = "map-prov.png", width = 10, height = 9,
       ggplot(prov.sf) + 
         geom_sf(aes(fill = nuovi_casi_per10k_mar7)) +
         scale_fill_distiller(palette = "YlGnBu", direction = 1) +
         geom_sf(data = color.sf, fill = NA, 
                 aes(colour = colore_regione),
                 size = 1.5, alpha = .85) +
         scale_color_manual(labels = c("scenario 2", "scenario 3", "scenario 4"),
                           values = c("#ffff99", "#ff7f00", "#e31a1c")) +
         labs(fill = sprintf("nuovi casi per 10mila abitanti\n(%s, media mobile 7 giorni)",
                             format(max(dat$date), format = "%d %b")),
              colour = NULL) +
         theme_bw()) 

  