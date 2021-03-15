library(dplyr)
library(tidyr)
library(purrr)
library(tidybayes)
library(ggplot2)
library(multiverse)

hurricane <- hurricane %>%
  rename(
    year = Year,
    name = Name,
    dam = NDAM,
    death = alldeaths,
    female = Gender_MF,
    masfem = MasFem,
    category = Category,
    pressure = Minpressure_Updated_2014,
    wind = HighestWindSpeed
  ) %>%
  mutate(
    post = if_else(year > 1979, 1, 0),
    zcat = as.numeric(scale(category)),
    zmin = -scale(pressure),
    zwin = as.numeric(scale(wind)),
    z3 = as.numeric((zmin + zcat + zwin) / 3)
  )

mv <- create_multiverse(hurricane)

death_outliers <- filter_branch(
  TRUE,
  name != 'Katrina',
  !(name %in% c('Katrina', 'Audrey'))
)
damage_outliers <- filter_branch(
  TRUE,
  name != 'Sandy',
  !(name %in% c('Sandy', 'Andrew')),
  !(name %in% c('Sandy', 'Andrew', 'Donna'))
)
mv <- mv %>%
  add_filter_branch(death_outliers, damage_outliers)
summary(mv)

femininity <- mutate_branch(
  female,
  masfem
)
damage <- mutate_branch(
  dam,
  log(dam)
)
mv <- mv %>%
  add_mutate_branch(femininity, damage)
summary(mv)


multiverse::inside(
  mv, {
    model <- glm(
      branch(
        model,
        'linear' ~ log(death + 1),
        'poisson' ~ death
      ) ~ branch(
        femininity_damage_interaction,
        'yes' ~ femininity * damage,
        'no' ~ femininity + damage
      ) + branch(
        other_predictors,
        'pressure' ~ femininity * zmin,
        'wind' ~ femininity * zwin,
        'category' ~ femininity * zcat,
        'all' ~ femininity * z3,
        'all_no_interaction' ~ z3,
        'none' ~ NULL
      ) + branch(
        covariates,
        '1' ~ NULL,
        '2' ~ year:damage,
        '3' ~ post:damage
      ), family = branch(
        model,
        'linear' ~ gaussian,
        'poisson' ~ poisson
      ), data = data
    )
    coef <- broom::tidy(model)
  }
)
mv <- mv %>%
  execute_multiverse()

branched <- extract(mv)

mtable <- summary(mv)

spec_curve(mv,var = "femininity",order=TRUE)

mtable <- multiverse::expand(mv) %>%
  mutate(res = map(.results, 'coef')) %>%
  unnest(res) %>%
  select(.universe, !! names(multiverse::parameters(mv)),
         term, estimate, p.value, std.error)
mtable <- display_branch_rules(mtable, mv)
data.spec_curve <- mtable %>%
  filter(term == "femininity") %>%
  arrange(estimate) %>%
  mutate(.universe = 1:nrow(.)) %>%
  select(-term)

data.spec_curve <- data.spec_curve %>%
  mutate(color = ifelse((p.value < 0.05),TRUE,FALSE)) %>%
  arrange(color,estimate) %>%
  mutate(.universe = seq(1,nrow(data.spec_curve)))

data.spec_curve %>%
  ggplot(aes(.universe, estimate, color = (p.value < 0.05))) +
  geom_point(size = 0.25) +
  labs(x = "", y = paste("coefficient of\n:"))

data.info <- data.spec_curve %>%
  gather( "parameter_name", "parameter_option",
          !! names(multiverse::parameters(mv)) ) %>%
  select(.universe, parameter_name, parameter_option, p.value) %>%
  filter(parameter_name %in% names(multiverse::parameters(mv)))

data.info %>%
  mutate(
    parameter_name = factor(str_replace(parameter_name, "_", "\n"))) %>%
  ggplot() +
  geom_point(aes(x = .universe, y = parameter_option,
                 color = (p.value < 0.05)), size = 2,shape = 124) +
  labs( x = "universe #",
        y = "option included in the analysis specification")+
  facet_grid(parameter_name ~ ., space="free_y", scales="free_y")+
  theme(strip.placement = "outside",
        strip.background = element_rect(fill=NA,colour=NA),
        panel.spacing.x=unit(0.15,"cm"),
        strip.text.y = element_text(angle = 0, face="bold", size=8),
        legend.position = "none",
        panel.spacing = unit(0.25, "lines"))


data.spec_curve %>%
  ggplot(aes(x = p.value, color=color)) +
  geom_histogram(fill="white", position="dodge")+
  labs(x = "", y = paste("coefficient of\n:"))+
  theme(legend.position="top")
