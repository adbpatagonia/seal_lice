# ADB
# IAA
# 2024-03-12

# Leopard Seals
# Mensaje de Flor Soto describiendo el problema

# La idea era estudiar el efecto del sexo, la clase de edad (juvenil o
# adulto), la variación anual (años 2014, 2015, 2019 y 2020) y el índice
# de condicion corporal en la prevalencia y/o abundancia media.

# Los datos tienen el desafio que que hay muchos ceros, así que no sé si puede modelar..

# libraries ----
library(data.table)
library(tidyverse)
library(plotly)
library(lme4)
library(car)
library(performance)
library(modelbased)
library(quantreg)


# functions -----
source("R/compile_rmd.R")


# read data -----
leo <- read.csv("data/leopard.csv", sep = ";", dec = ",") %>%
  as.data.table()

# look at data structure ----
str(leo)
# 50 unique seals
unique(leo$Sp)

# transform weight to numeric
leo$weight_kg <- as.numeric(leo$weight_kg)

# transform sex to factor
leo$sex <- as.factor(leo$sex)

# transform age_class to factor
leo$age_class <- as.factor(leo$age_class)

# code presence
leo[, presence := ifelse(Lice == 0, 0, 1)]


# explore data -----
## largo - peso ----
# no estoy seguro que se puedan usar estos datos para estimar una relacion largo-peso
# para asi poder estimar la condicion corporal
# Opciones:
# 1. usar de todas maneras, pero tener en cuenta en las dicusiones
# 2. encontrar relaciones largo-peso publicadas

# Aqui, tomare la opcion 1, pero es facil cambiarla

leo <- leo %>% arrange(sex, SL_cm)
m.lw.m <- lm(log(weight_kg) ~ log(SL_cm), data = leo[sex == "M"])
m.lw.f <- lm(log(weight_kg) ~ log(SL_cm), data = leo[sex == "F"])

pred.w <- bind_rows(
  data.frame(predict(m.lw.f, se.fit = TRUE, newdata = leo[sex == "F"])) %>%
    mutate(pred_weight = exp(fit),
           pred_weight_lci = exp(fit - 1.96 * se.fit),
           pred_weight_uci = exp(fit + 1.96 * se.fit),
           sex = "F") %>%
    select(pred_weight:pred_weight_uci),
  data.frame(predict(m.lw.m, se.fit = TRUE, newdata = leo[sex == "M"])) %>%
    mutate(pred_weight = exp(fit),
           pred_weight_lci = exp(fit - 1.96 * se.fit),
           pred_weight_uci = exp(fit + 1.96 * se.fit),
           sex = "M") %>%
    select(pred_weight:pred_weight_uci)) %>%
  data.table()

leo <- bind_cols(leo, pred.w)

p.lw <- ggplot(data = leo, aes(x = SL_cm, y = weight_kg)) +
  geom_line(aes(y = pred_weight)) +
  geom_ribbon(aes(ymin = pred_weight_lci, ymax = pred_weight_uci), alpha = 0.2) +
  geom_point() +
  theme_bw() +
  facet_wrap(.~sex)

## calcular condicion -----
### relative condition ----
leo[, rel_cond := weight_kg/pred_weight]

### mass per standard lenght ----
# Boveng et al 2020
leo[, mass_length := weight_kg/SL_cm]
### plots ----

p.cond <- ggplot(data = leo, aes(x = rel_cond, fill = sex, color = sex)) +
  theme_bw() +
  geom_density(alpha = 0.3) +
  xlab("Relative condition") +
  theme(legend.position = c(.05, .85),
        legend.background = element_rect(colour = NA, fill = NA),
        legend.title = element_blank()) +
  labs(title = expression(R[k]*" = W/"*hat(W)*"      "*hat(W)*" = a"~L^{"b"}))
       # caption = expression(paste("Si la condicion de la foca es promedio, "*R[k]*" = 1
       #                             Si la condicion de la foca es menor al promedio, "*R[k]*" < 1
       #                            Si la condicion de la foca es mayor al promedio, "*R[k]*" > 1"
       #                            )
       # ))

p.mass_length <- ggplot(data = leo, aes(x = mass_length, fill = sex, color = sex)) +
  theme_bw() +
  geom_density(alpha = 0.3) +
  xlab("Mass per standard lenght") +
  theme(legend.position = c(.05, .85),
        legend.background = element_rect(colour = NA, fill = NA),
        legend.title = element_blank()) +
  labs(title = "Mass per standard length as per Boveng et al 2020")

## Lice abundance ----
p.lice.sex <- ggplot(leo, aes(x = sex, y = Lice)) +
  theme_bw() +
  geom_point(position = position_dodge2(width = 0.4), alpha = 0.5)

p.lice.cond <- ggplot(leo, aes(x = rel_cond, y = Lice, color = sex)) +
  theme_bw() +
  geom_point(alpha = 0.8) +
  # geom_smooth() +
  geom_vline(xintercept = 1, lty = 2)
ggplotly(p.lice.cond)

p.lice.age <- ggplot(leo, aes(x = age_class, y = Lice)) +
  theme_bw() +
  geom_point(position = position_dodge2(width = 0.4), alpha = 0.5)

p.lice.year <- ggplot(leo, aes(x = year, y = Lice)) +
  theme_bw() +
  geom_point(position = position_dodge2(width = 0.4), alpha = 0.5)
## Lice presence ----
p.lice.sex.presence <- ggplot(leo, aes(x = sex, y = presence)) +
  theme_bw() +
  geom_point(position = position_dodge2(width = 0.4), alpha = 0.5)

p.lice.cond.presence <- ggplot(leo, aes(x = rel_cond, y = presence, color = sex)) +
  theme_bw() +
  geom_point(alpha = 0.8) +
  # geom_smooth() +
  geom_vline(xintercept = 1, lty = 2)
ggplotly(p.lice.cond.presence)

p.lice.age.presence <- ggplot(leo, aes(x = age_class, y = presence)) +
  theme_bw() +
  geom_point(position = position_dodge2(width = 0.4), alpha = 0.5)

p.lice.year.presence <- ggplot(leo, aes(x = year, y = presence)) +
  theme_bw() +
  geom_point(position = position_dodge2(width = 0.4), alpha = 0.5)


## data summaries -----
### sex ----
t.lice.presence.sex <- leo %>%
  group_by(sex) %>%
  reframe(n = n(),
          pres = sum(presence)) %>%
  mutate(prop = pres/n)

t.lice.abundance.sex <- leo %>%
  group_by(sex) %>%
  reframe(median_lice = median(Lice),
          mean_lice = mean(Lice),
          sd_lice = sd(Lice),
          CV_lice = 100 * sd_lice/mean_lice)
### age_class ----
t.lice.presence.age_class <- leo %>%
  group_by(age_class) %>%
  reframe(n = n(),
          pres = sum(presence)) %>%
  mutate(prop = pres/n)

t.lice.abundance.age_class <- leo %>%
  group_by(age_class) %>%
  reframe(median_lice = median(Lice),
          mean_lice = mean(Lice),
          sd_lice = sd(Lice),
          CV_lice = 100 * sd_lice/mean_lice)




# model -----
## presence ~ condition + age + year -----
# can't use sex as only one F with lice
m.lice.cond.age.year <- glmmTMB::glmmTMB(presence ~ rel_cond + age_class  + (1|year),
      data = leo,
      family = "binomial")

performance::check_model(m.lice.cond.age.year)

# remove random year effect
## presence ~ condition + age  -----
m.lice.cond.age <- glmmTMB::glmmTMB(presence ~ rel_cond + age_class ,
                           data = leo,
                           family = "binomial")

performance::check_model(m.lice.cond.age)
performance::check_convergence(m.lice.cond.age)
performance::check_singularity(m.lice.cond.age)
performance::model_performance(m.lice.cond.age)
car::Anova(m.lice.cond.age)
summary(m.lice.cond.age)


## presence ~ condition  -----
m.lice.cond <- glmmTMB::glmmTMB(presence ~ rel_cond  ,
                                    data = leo,
                                    family = "binomial")

performance::check_model(m.lice.cond)
performance::check_convergence(m.lice.cond)
performance::check_singularity(m.lice.cond)
performance::model_performance(m.lice.cond)
car::Anova(m.lice.cond)
summary(m.lice.cond)


leo[!is.na(weight_kg), E.pres.cond := (modelbased::estimate_prediction(m.lice.cond)$Predicted)]
leo[!is.na(weight_kg), E.pres.cond_lci := (modelbased::estimate_prediction(m.lice.cond)$CI_low)]
leo[!is.na(weight_kg), E.pres.cond_uci := (modelbased::estimate_prediction(m.lice.cond)$CI_high)]


p.m.pres <- ggplot(data = leo, aes(x = rel_cond, y = presence)) +
  geom_line(aes(y = E.pres.cond)) +
  geom_ribbon(aes(ymin = E.pres.cond_lci, ymax = E.pres.cond_uci), alpha = 0.2) +
  geom_point() +
  theme_bw()

## presence ~ age  -----
m.lice.age <- glmmTMB::glmmTMB(presence ~ age_class  ,
                               data = leo,
                               family = "binomial")

performance::check_model(m.lice.age)
performance::check_convergence(m.lice.age)
performance::check_singularity(m.lice.age)
performance::model_performance(m.lice.age)
car::Anova(m.lice.age)
summary(m.lice.age)

## presence ~ condition +  year -----
# can't use sex as only one F with lice
# can't use age_class as only one juvenile with lice

m.lice.cond.year <- glmmTMB::glmmTMB(presence ~ rel_cond   + (1|year),
                                         data = leo,
                                         family = "binomial")

performance::check_model(m.lice.cond.year)
summary(m.lice.cond.year)
# effect of year is negligible

## abundance ~ condition +  year -----
# can't use sex as only one F with lice
# can't use age_class as only one juvenile with lice

m.lice.abun.cond.year <- glmmTMB::glmmTMB(Lice ~ rel_cond   + (1|year),
                                     data = leo)

performance::check_model(m.lice.abun.cond.year)
# no es horrible, pero el efecto de ano es despreciable


## abundance ~ condition -----
# can't use sex as only one F with lice
# can't use age_class as only one juvenile with lice

m.lice.abun.cond <- glmmTMB::glmmTMB(Lice ~ rel_cond   ,
                                          data = leo)

performance::check_model(m.lice.abun.cond)
# fulero, pero veamos igual
performance::check_convergence(m.lice.abun.cond)
performance::check_singularity(m.lice.abun.cond)
# r2 es despreciable 0.002 - no explicamos nada de la variabilidad
performance::model_performance(m.lice.abun.cond)
car::Anova(m.lice.abun.cond)
summary(m.lice.abun.cond)

leo[!is.na(weight_kg), E.abun.cond := (modelbased::estimate_prediction(m.lice.abun.cond)$Predicted)]
leo[!is.na(weight_kg), E.abun.cond_lci := (modelbased::estimate_prediction(m.lice.abun.cond)$CI_low)]
leo[!is.na(weight_kg), E.abun.cond_uci := (modelbased::estimate_prediction(m.lice.abun.cond)$CI_high)]


p.m.abun <- ggplot(data = leo, aes(x = rel_cond, y = Lice)) +
  geom_line(aes(y = E.abun.cond)) +
  geom_ribbon(aes(ymin = E.abun.cond_lci, ymax = E.abun.cond_uci), alpha = 0.2) +
  geom_point() +
  theme_bw()

## abundance ~ condition, only for animals with lice-----
# can't use sex as only one F with lice
# can't use age_class as only one juvenile with lice
leo.present <- leo[ presence == 1]

m.lice.abun.cond.nozero <- glmmTMB::glmmTMB(Lice  ~ rel_cond   ,
                                     data = leo.present)

performance::check_model(m.lice.abun.cond.nozero)
# fulero, pero veamos igual
performance::check_convergence(m.lice.abun.cond.nozero)
performance::check_singularity(m.lice.abun.cond.nozero)
# r2: 0.87
performance::model_performance(m.lice.abun.cond.nozero)
car::Anova(m.lice.abun.cond.nozero)
parameters::model_parameters(m.lice.abun.cond.nozero)
summary(m.lice.abun.cond.nozero)

leo.present$ELice <- predict(m.lice.abun.cond.nozero, newdata = leo.present)


ggplot(leo.present, aes(x = rel_cond, y = Lice)) +
  theme_bw() +
  geom_line(aes(y = ELice)) +
  geom_point()
# this does not really make sense

# try quantile regression

#### quantile regression ------
m.lice.abun.cond.nozero.qr <- quantreg::rq(Lice  ~ rel_cond   ,
                                           tau = seq(0, 1, by = 0.1),
                                            data = leo.present)
m.lice.abun.cond.nozero.qr
m.lice.abun.cond.nozero.qr <- quantreg::rq(Lice  ~ rel_cond   ,
                                           tau = 0.5,
                                           data = leo.present)
performance::check_singularity(m.lice.abun.cond.nozero.qr)
# r2 es despreciable 0.002 - no explicamos nada de la variabilidad
performance::model_performance(m.lice.abun.cond.nozero.qr)
# quantreg::anova.rq(m.lice.abun.cond.nozero.qr)
parameters::model_parameters(m.lice.abun.cond.nozero.qr)
summary(m.lice.abun.cond.nozero.qr)

leo.present$ELice.qr <- predict(m.lice.abun.cond.nozero.qr, newdata = leo.present)


p.m.qr.abun <- ggplot(leo.present, aes(x = rel_cond, y = Lice)) +
  theme_bw() +
  geom_line(aes(y = ELice.qr)) +
  geom_point()




# plot for poster -----

licelabel <- expression(bold(paste("Abundance of "*italic("A. ogmorhini"))))
relcondlabel <- expression(bold(paste('Seal Relative Condition ('*italic('K'["r"])*')')))
cols <- c("grey70", "#2f606a")

p.poster <- ggplot(leo, aes(x = rel_cond, y = Lice,
                            color = as.factor(presence))) +
  theme_bw() +
  geom_point(alpha = 0.8, size = 3) +
  scale_color_manual(values = cols) +
  # geom_smooth() +
  # geom_vline(xintercept = 1, lty = 2) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(family = "Calibri", size = 13),
        plot.title = element_text(size = 12, face = "bold"),
        axis.title = element_text(family = "Calibri", face = "bold", size = 16),
        axis.text = element_text(family = "Calibri", size = 13, colour = "black")
  ) +
  xlab(relcondlabel) +
  ylab(licelabel) +
  NULL
p.poster.sex <- ggplot(leo, aes(x = rel_cond, y = Lice,
                                shape = sex,
                            color = as.factor(presence))) +
  theme_bw() +
  geom_point(alpha = 0.8, size = 3) +
  scale_color_manual(values = cols) +
  guides(color = "none") +
  # geom_smooth() +
  # geom_vline(xintercept = 1, lty = 2) +
  theme(legend.position = c(0.87,0.9),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(family = "Calibri", size = 13),
        plot.title = element_text(size = 12, face = "bold"),
        axis.title = element_text(family = "Calibri", face = "bold", size = 16),
        axis.text = element_text(family = "Calibri", size = 13, colour = "black")
        ) +
  xlab(relcondlabel) +
  ylab(licelabel) +
  NULL

p.poster.sex.qr <- ggplot(leo, aes(x = rel_cond, y = Lice,
                                   shape = sex,
                                   color = as.factor(presence))) +
  theme_bw() +
  geom_line(alpha = 0.6, data = leo.present, aes(x = rel_cond,y = ELice.qr), color = cols[2], size = 1.1) +
  geom_point(alpha = 0.8, size = 3) +
  scale_color_manual(values = cols) +
  guides(color = "none") +
  # geom_smooth() +
  # geom_vline(xintercept = 1, lty = 2) +
  theme(legend.position = c(0.87,0.9),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(family = "Calibri", size = 13),
        plot.title = element_text(size = 12, face = "bold"),
        axis.title = element_text(family = "Calibri", face = "bold", size = 16),
        axis.text = element_text(family = "Calibri", size = 13, colour = "black")
  ) +
  xlab(relcondlabel) +
  ylab(licelabel) +
  NULL
# compile report ------
compile_rmd("PiojosFocaLeopardo")
# output -----
ggsave(p.poster, filename = "output/LiceAbundance_bigfont.png", height = 5, width = 8)
ggsave(p.poster.sex, filename = "output/LiceAbundance_sex.png", height = 5, width = 8)
ggsave(p.poster.sex.qr, filename = "output/LiceAbundance_sex_quantileregression.png", height = 5, width = 8)
