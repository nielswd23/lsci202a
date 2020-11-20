rm(list = ls())
library(tidyverse)
library(modelr)

# 1 Linear and Logistic Models in R 
# 1.1 Linear regression
d = read_tsv("http://socsci.uci.edu/~rfutrell/teaching/MWPND_by_picture.txt")


# 1 
ggplot(data = d, mapping = aes(x = name_H , y = RT_M)) +
  geom_point(alpha = 1/6) +
  facet_wrap(~lang) +
  geom_smooth()
ggsave('H_stat.pdf')

# 2
RT_model = lm(RT_M ~ name_H, data = d)

# 3
d %>% add_predictions(RT_model) %>%
  ggplot(aes(name_H, RT_M)) +
  geom_point(alpha = 1/8) +
  geom_line(aes(y = pred)) + 
  facet_wrap(~lang)
ggsave('H_stat_regression.pdf')
# For certain languages, the model is over-estimating the RT, and
# for others, the model is under-estimating the RT. Some salient 
# examples are the BG, GR, and RU language graphs where the model is
# under-estimated the RT for these languages as the points are 
# clustered above the regression line. You can see the model is over-
# estimating the RT for the DE, DU, and FI languages. 
 
# 4
d %>% add_residuals(RT_model) %>%
  ggplot(aes(name_H, resid)) +
  geom_point(alpha = 1/8) +
  facet_wrap(~lang) + 
  geom_smooth()
ggsave('H_stat_residuals.pdf')
# This plot confirms that our model is not adequately capturing the 
# relationship between RT_M and name_H for each language. In a well-
# fitted model, the residuals, the difference between the actual 
# data points and the predicted values, should look like random 
# noise without any visible patterns. But, as seen in this plotting 
# of the residuals, some languages show a pattern between the 
# residuals and name_H. Compared with the previous graph of the 
# predictions, we can see that the languages with severe over- or 
# under-estimations have a stronger relationship between residuals 
# and name_H (i.e. DE and GR language plots). 

# 5 
model_lang  = lm(RT_M ~ name_H + lang, data = d)
d %>% add_predictions(model_lang) %>%
  ggplot(aes(name_H, RT_M)) +
  geom_point(alpha = 1/8) +
  geom_line(aes(y = pred)) + 
  facet_wrap(~lang)
ggsave('H_stat_regression_by_lang.pdf')

# the folloeing plot is just to confirm that the slopes 
# are all the same (removing facet wrap)
d %>% add_predictions(model_lang) %>%
  ggplot(aes(name_H, RT_M)) +
  geom_point(alpha = 1/8) +
  geom_line(aes(y = pred, color = lang)) 

d %>% add_residuals(model_lang) %>%
  ggplot(aes(name_H, resid)) +
  geom_point(alpha = 1/8) +
  facet_wrap(~lang) + 
  geom_smooth()
ggsave('H_stat_residuals_by_lang.pdf')

# 6 
model_lang_full = lm(RT_M ~ name_H * lang, data = d)

d %>% add_predictions(model_lang_full) %>%
  ggplot(aes(name_H, RT_M)) +
  geom_point(alpha = 1/8) +
  geom_line(aes(y = pred)) + 
  facet_wrap(~lang)
ggsave('H_stat_regression_by_lang_slope.pdf')

d %>% add_residuals(model_lang_full) %>%
  ggplot(aes(name_H, resid)) +
  geom_point(alpha = 1/8) +
  facet_wrap(~lang) + 
  geom_smooth()
ggsave('H_stat_residuals_by_lang_slope.pdf')

# 7 
full_predictions = d %>%
  add_predictions(model_lang_full) %>%
  add_residuals(model_lang_full)

write_csv(full_predictions, path = 'full_predictions.csv')


# 1.2 Logistic Regression 
url = "https://tinyurl.com/y5fgh9mk"
d = read_csv(url)

# 1
d2 = d %>%
  filter(!is.na(Theme.definiteness)) %>%
  mutate(Theme.animacy = if_else(Theme.animacy == 'A', 'animate', 'inanimate'), 
         Recipient.animacy = if_else(Recipient.animacy == 'A', 'animate', 'inanimate'),
         Theme.definiteness = if_else(Theme.definiteness == 'Definite' | 
                                        Theme.definiteness == 'Definite-pn', 'D', 'I'), 
         Recipient.definiteness = if_else(Recipient.definiteness == 'Definite' | 
                                            Recipient.definiteness == 'Definite-pn', 'D', 'I'))

# 2 
# I will first use dummy coding for the Response.variable column to make 
# calculating the proportion easier
d3 = d2 %>%
  mutate(is_DOD = ifelse(Response.variable == 'D', 1, 0))

d3 %>%
  gather(Theme.animacy, Recipient.animacy, key = 'type', value = 'animacy') %>%
  group_by(type, animacy) %>% 
  summarize(proportion = sum(is_DOD)/n()) %>%
  ungroup() %>%
  ggplot(aes(x = animacy, y = proportion)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~type) +
  ylab('proportion of D')
ggsave('animacy_bars.pdf')

# 3
d3 %>%
  gather(Theme.definiteness, Recipient.definiteness, key = 'type', value = 'definiteness') %>%
  group_by(type, definiteness) %>% 
  summarize(proportion = sum(is_DOD)/n()) %>%
  ungroup() %>%
  ggplot(aes(x = definiteness, y = proportion)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~type) +
  ylab('proportion of D')
ggsave('definiteness_bars.pdf')

# 4
# When the recipient is animate, the sentence is more likely to be the double object 
# construction than the inanimate reipient sentences, but when the theme is inanimate, 
# the sentence is more likely to be the double object construction than the animate 
# theme sentences. So, there seems to be an interaction between the recipient/theme 
# dimension and animacy. The definiteness shows the same pattern, so there is a similar
# interaction between the recipient/theme dimension and definiteness. 

# 5 
log_model = glm(is_DOD ~ Recipient.animacy + Recipient.definiteness + 
                Theme.animacy + Theme.definiteness, data = d3, family = 'binomial')


# 6
logisitic = function(x) {
  1/(1 + exp(-x))
}

d3 %>% 
  add_predictions(log_model) %>%
  mutate(pred = logisitic(pred)) %>%
  gather(Theme.animacy, Recipient.animacy, key = 'type', value = 'animacy') %>%
  group_by(type, animacy) %>% 
  summarize(proportion = sum(pred)/n()) %>%
  ungroup() %>%
  ggplot(aes(x = animacy, y = proportion)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~type) + 
  ylab('predicted prop of D')

d3 %>% 
  add_predictions(log_model) %>%
  mutate(pred = logisitic(pred)) %>%
  gather(Theme.definiteness, Recipient.definiteness, key = 'type', value = 'definiteness') %>%
  group_by(type, definiteness) %>% 
  summarize(proportion = sum(pred)/n()) %>%
  ungroup() %>%
  ggplot(aes(x = definiteness, y = proportion)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~type) + 
  ylab('predicted prop of D')


# 7 
# The predicted proportions are exactly the same as the empirical proportions.
# It does not seem that the regression is missing anything. 


