# ADB
# CONICET - IAA
# 2024-08-19

# THis is a follow up to the project started with Florencia Soto to present a poster at the SCAR Confrence

# THe aim is to synthesize information on Leopard, Weddell, and crabeater seals

# Is the prevalence/strength of lice infestation o function of body condition?


# libraries ----
library(data.table)
library(tidyverse)
library(plotly)
library(lme4)
library(car)
library(performance)
library(modelbased)
library(quantreg)
library(broom)
# read data -----
leo <- read.csv((paste0(here::here(), "/data/leopard.csv")), sep = ";", dec = ",") %>%
  mutate(species = "Leopard Seal") %>%
  as.data.table()

wed <- openxlsx::read.xlsx((paste0(here::here(), "/data/Weddell.xlsx"))) %>%
  mutate(species = "Weddell Seal") %>%
  as.data.table()

crab <- openxlsx::read.xlsx((paste0(here::here(), "/data/Crabeater seals.xlsx"))) %>%
  mutate(species = "Crabeater Seal") %>%
  as.data.table()

# wrangle data ------
seal <- rbindlist(l = list(leo, wed, crab))

## look at data structure ----
str(seal)
# 50 unique seals
unique(seal$Sp)

# transform weight to numeric
seal$weight_kg <- as.numeric(seal$weight_kg)

# transform sex to factor
seal$sex <- as.factor(seal$sex)

# transform age_class to factor
seal$age_class <- as.factor(seal$age_class)

# code presence
seal[, presence := ifelse(Lice == 0, 0, 1)]

# replace zero in length

seal[SL_cm == 0, SL_cm := NA]

# explore data -----
## largo - peso ----
# forestmangr::lm_table(
#   seal[!is.na(sex)],
#   model = log(weight_kg) ~ log(SL_cm),
#   .groups = c("species", "sex"))

## fit length-weight relationships ----
fitted_models <-
  # remove observations with NA in sex
  seal[!is.na(sex)] %>%
  group_by(species, sex) %>%
  do(model = lm(log(weight_kg) ~ log(SL_cm), data = .)) %>%
  data.table()

# # models diagnostics -----
# they all look OK
performance::check_model(fitted_models$model[[1]])
performance::check_model(fitted_models$model[[2]])
performance::check_model(fitted_models$model[[3]])
performance::check_model(fitted_models$model[[4]])
performance::check_model(fitted_models$model[[5]])
performance::check_model(fitted_models$model[[6]])

## obtain expected values ----
preds <- data.table()
# can't get pred weight if we have no info on length
seal_lw <- seal[!is.na(SL_cm)]

for (i in 1:nrow(fitted_models)){
  this.pred <-
    # get predicted using the argument new data as the subset for the specific combination of species & sex
    data.frame(predict(fitted_models$model[[i]], se.fit = TRUE,
                       newdata = seal_lw[sex == pull(fitted_models[i, .(sex)]) &
                                           species == pull(fitted_models[i, .(species)] )  ])) %>%
    # exponentiate the prediction, and get 95% CI
    mutate(pred_weight = exp(fit),
           pred_weight_lci = exp(fit - 1.96 * se.fit),
           pred_weight_uci = exp(fit + 1.96 * se.fit)) %>%
    # bind the data to get complete dataset
    bind_cols(., seal_lw[sex == pull(fitted_models[i, .(sex)]) &
                           species == pull(fitted_models[i, .(species)] )  ] ) %>%
    data.table()

  preds <- rbindlist(l = list(preds, this.pred))
}


## plot l-w -----
p.lw <- ggplot(data = preds, aes(x = SL_cm, y = weight_kg)) +
  geom_line(aes(y = pred_weight)) +
  geom_ribbon(aes(ymin = pred_weight_lci, ymax = pred_weight_uci), alpha = 0.2) +
  geom_point() +
  theme_bw() +
  facet_grid(species~sex)

# relative condition ----
preds[, rel_cond := weight_kg/pred_weight]

licelabel <- expression(bold(paste("Abundance of "*italic("A. ogmorhini"))))
relcondlabel <- expression(bold(paste('Seal Relative Condition ('*italic('K'["r"])*')')))
cols <- c("grey70", "#2f606a")

# plots ----
## abundance ~f(condition) ----
p.cond.lice <- ggplot(preds, aes(x = rel_cond, y = Lice,
                                 shape = sex,
                                 color = as.factor(presence))) +
  theme_bw() +
  geom_point(alpha = 0.8, size = 3) +
  scale_color_manual(values = cols) +
  guides(color = "none") +
  # geom_smooth() +
  # geom_vline(xintercept = 1, lty = 2) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(family = "Calibri", size = 13),
        plot.title = element_text(size = 12, face = "bold"),
        axis.title = element_text(family = "Calibri", face = "bold", size = 16),
        axis.text = element_text(family = "Calibri", size = 13, colour = "black")
  ) +
  # xlab(relcondlabel) +
  ylab("Abundance of A. ogmorhini") +
  xlab('Seal Relative Condition') +
  facet_grid(species~., scales = "free_y") +
  NULL

## abundance ~f(condition) --------
p.cond.lice.presence <- ggplot(preds, aes(x = rel_cond, y = presence,
                                          shape = sex,
                                          color = as.factor(presence))) +
  theme_bw() +
  geom_point(alpha = 0.8, size = 3) +
  scale_color_manual(values = cols) +
  guides(color = "none") +
  # geom_smooth() +
  # geom_vline(xintercept = 1, lty = 2) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(family = "Calibri", size = 13),
        plot.title = element_text(size = 12, face = "bold"),
        axis.title = element_text(family = "Calibri", face = "bold", size = 16),
        axis.text = element_text(family = "Calibri", size = 13, colour = "black")
  ) +
  ylab(relcondlabel) +
  xlab(licelabel) +
  facet_grid(species~., scales = "free_y") +
  NULL

ggplotly(p.cond.lice)
ggplotly(p.cond.lice.presence)


ggplotly(


  ggplot(preds, aes(x = rel_cond, y = Lice,
                    shape = sex,
                    color = species)) +
    theme_bw() +
    geom_point(alpha = 0.8, size = 3) +
    # scale_color_manual(values = cols) +
    # guides(color = "none") +
    # geom_smooth() +
    # geom_vline(xintercept = 1, lty = 2) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.text = element_text(family = "Calibri", size = 13),
          plot.title = element_text(size = 12, face = "bold"),
          axis.title = element_text(family = "Calibri", face = "bold", size = 16),
          axis.text = element_text(family = "Calibri", size = 13, colour = "black")
    ) +
    # xlab(relcondlabel) +
    ylab("Abundance of A. ogmorhini") +
    xlab('Seal Relative Condition') +
    # facet_grid(species~., scales = "free_y") +
    NULL
)



m <- glmmTMB::glmmTMB(Lice  ~ rel_cond + as.factor(species) + as.factor(sex)  ,
                                            data = preds)

performance::check_model(m)
# la abundancia de weddell esta en una escala diferente, demasiada influencia
# quizxas estandarizar dentro de cada spp?
summary(m
        )


preds <- preds %>%
  group_by(species) %>%
  mutate(Lice_std = standardize(Lice)) %>%
  mutate(rel_cond_std = standardize(rel_cond))
m <- glmmTMB::glmmTMB(Lice_std  ~ rel_cond_std + as.factor(species) + as.factor(sex)  ,
                      data = preds)

performance::check_model(m)
# la abundancia de weddell esta en una escala diferente, demasiada influencia
# quizxas estandarizar dentro de cada spp?
summary(m
)

ggplotly(


  ggplot(preds, aes(x = rel_cond_std, y = Lice_std,
                    shape = as.factor(presence),
                    color = species)) +
    theme_bw() +
    geom_point(alpha = 0.8, size = 3) +
    # scale_color_manual(values = cols) +
    # guides(color = "none") +
    # geom_smooth() +
    # geom_vline(xintercept = 1, lty = 2) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.text = element_text(family = "Calibri", size = 13),
          plot.title = element_text(size = 12, face = "bold"),
          axis.title = element_text(family = "Calibri", face = "bold", size = 16),
          axis.text = element_text(family = "Calibri", size = 13, colour = "black")
    ) +
    # xlab(relcondlabel) +
    ylab("Standardized Abundance of A. ogmorhini") +
    xlab('Standardized Seal Relative Condition') +
    facet_grid(sex~., scales = "free_y") +
    NULL
)
