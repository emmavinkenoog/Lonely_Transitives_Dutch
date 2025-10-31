library(tidyverse)
library(effectsize)

setwd('./Lonely Transitives Dutch')

# load data
survey <- read_delim('./Data/Survey data.txt', col_names = TRUE)
# convert to long format
surveylong <- survey %>% pivot_longer(cols = '1':'35', names_to = 'Sentence',
                                      values_to = 'Acceptability')
head(surveylong)


# add columns: item type (test vs. control),
# for test items CT, adv./ptcl. or not and verb
surveylong <- surveylong %>%
  mutate(
    Item = case_when(
      Sentence %in% c(1,12,18,32,34) ~ 'pos_control',
      Sentence %in% c(7,13,21,26,35) ~ 'neg_control',
      Sentence %in% c(3,9,22,27,33) ~ 'sem_control',
      TRUE ~ 'test'
    ),
    CT = case_when(
      Sentence %in% c(6,10,14,16,17,24,30) ~ 'int',
      Sentence %in% c(4,8,19,23,29) ~ 'decl_emb',
      Sentence %in% c(2,5,11,15,20,25,28,31) ~ 'decl_nonemb',
      TRUE ~ NA_character_
    ),
    Adv_Ptcl = case_when(
      Sentence %in% c(5,10,14,17,19,20,24,28,29,31) ~ 'Y',
      Sentence %in% c(2,4,6,8,11,15,16,23,25,30) ~ 'N',
      TRUE ~ NA_character_
    ),
    Verb = case_when(
      Sentence %in% c(4,5,6,10,15,16,19,24,25,31) ~ 'willen',
      Sentence %in% c(2,8,11,14,17,20,23,28,29,30) ~ 'hebben',
      TRUE ~ NA_character_
    )
  )

# reorder columns and convert to factors
surveylong <- surveylong %>%
  select(Participant,Age,Gender,Item,CT,Adv_Ptcl,Verb,Sentence,Acceptability)
surveylong <- surveylong %>%
  mutate(across(c(Participant,Gender,Item,CT,Adv_Ptcl,Verb,Sentence),
                as.factor))


# check the controls
# positive controls: mean rating should not be < 3
surveylong %>%
  group_by(Item) %>%
  filter(Item == 'pos_control' & mean(Acceptability) < 3)
# no data to eliminate here
# negative controls: mean rating should not be > 3
surveylong %>%
  group_by(Item) %>%
  filter(Item == 'neg_control' & mean(Acceptability) > 3)
# no data to eliminate here
# semantic controls: mean rating should not be < 3
surveylong %>%
  group_by(Item) %>%
  filter(Item == 'sem_control' & mean(Acceptability) < 3)
# no data to eliminate here; nothing is eliminated


# select test items only
lont_data <- surveylong %>%
  filter(Item == 'test')


# overview
mean(lont_data$Acceptability)
sd(lont_data$Acceptability)
ggplot(lont_data, aes(x = "", y = Acceptability)) +
  geom_boxplot() + 
  stat_summary(fun = mean, geom = "crossbar", width = 0.4,
               color = "red", linetype = "dotted") +
  labs(
    x = "",
    y = "Acceptability Rating",
    title = "Acceptability Ratings of LonTs"
  ) +
  theme_minimal()

# create bar chart
ggplot(lont_data, aes(x = Acceptability)) +
  geom_bar() +
  scale_x_continuous(breaks = 1:5) +
  labs(
    x = 'Acceptability Rating',
    y = 'Frequency',
    title = 'Bar Chart of Acceptability Ratings'
  ) +
  theme_minimal()


# check correlation between gender and acceptability
# check distribution
table(survey$Gender)
# create boxplot
ggplot(lont_data, aes(x = Gender, y = Acceptability)) + geom_boxplot() +
  stat_summary(fun = mean, geom = 'crossbar', width = 0.6,
               color = 'red', linetype = 'dotted') + 
  labs(
    x = 'Gender',
    y = 'Acceptability Rating',
    title = 'Acceptability Ratings per Gender'
  ) + theme_minimal()

# compute cohen's d and t (between-subjects)
cohens_d(Acceptability ~ Gender, data = lont_data)
t.test(Acceptability ~ Gender, data = lont_data)
# no significant difference


# check correlation between age and acceptability
# check mean age and distribution
mean(survey$Age)
sort(survey$Age)
ggplot(survey, aes(x = Age)) + geom_histogram(binwidth = 2, boundary = 0) +
  labs(
    x = 'Age',
    y = 'Frequency',
    title = 'Histogram of Participant Ages'
  ) + theme_minimal()

# create scatterplot (with mean rating per participant to avoid overplotting)
lont_data_part_age <- lont_data %>%
  group_by(Participant, Age) %>%
  summarise(mean_rating = mean(Acceptability)) %>%
  ungroup()
ggplot(lont_data_part_age, aes(x = Age, y = mean_rating)) +
  geom_point() + geom_point() + geom_smooth(method='lm') +
  labs(
    x = 'Age',
    y = 'Mean Acceptability Rating',
    title = 'Mean Acceptabitity Ratings and Age per Participant'
  ) + theme_minimal()

# compute correlation (between-subjects)
cor.test(lont_data$Age, lont_data$Acceptability)
# negative correlation, but not significant


# effect of verb
# compute means
lont_data_wil <- lont_data %>%
  filter(Verb == 'willen')
mean(lont_data_wil$Acceptability)
lont_data_heb <- lont_data %>%
  filter(Verb == 'hebben')
mean(lont_data_heb$Acceptability)
# convert to wide format
verb_wide <- lont_data %>%
  group_by(Participant, Verb) %>%
  summarise(Acceptability = mean(Acceptability), .groups = "drop") %>%
  pivot_wider(names_from = Verb, values_from = Acceptability)
# compute dz and t (within-subjects)
cohens_d(verb_wide$willen, verb_wide$hebben, paired = TRUE)
t.test(verb_wide$willen, verb_wide$hebben, paired = TRUE)

# create boxplot
ggplot(lont_data, aes(x = Verb, y = Acceptability)) + geom_boxplot() +
  stat_summary(fun = mean, geom = 'crossbar', width = 0.6,
               color = 'red', linetype = 'dotted') +
  scale_x_discrete(limits = c('willen', 'hebben'),
                   labels = c(willen = 'Willen',
                              hebben = 'Hebben')) +
  labs(
    x = 'Verb',
    y = 'Acceptability Rating',
    title = 'Acceptability Ratings per Verb'
  ) + theme_minimal()


# effect of CT
# int vs. decl
# add column to distinguish int from decl
lont_data <- lont_data %>%
  mutate(
    CT1 = case_when(
      CT %in% c('int') ~ 'int',
      CT %in% c('decl_emb','decl_nonemb') ~ 'decl',
    )
  )
lont_data <- lont_data %>%
  mutate(across(c(CT1), as.factor))
# compute means
lont_data_int <- lont_data %>%
  filter(CT1 == 'int')
mean(lont_data_int$Acceptability)
lont_data_decl <- lont_data %>%
  filter(CT1 == 'decl')
mean(lont_data_decl$Acceptability)
# convert to wide format
CT1_wide <- lont_data %>%
  group_by(Participant, CT1) %>%
  summarise(Acceptability = mean(Acceptability), .groups = "drop") %>%
  pivot_wider(names_from = CT1, values_from = Acceptability)
# compute dz and t (within-subjects)
cohens_d(CT1_wide$int, CT1_wide$decl, paired = TRUE)
t.test(CT1_wide$int, CT1_wide$decl, paired = TRUE)

# decl_emb vs. decl_nonemb
# compute means
lont_data_decl_emb <- lont_data_decl %>%
  filter(CT == 'decl_emb')
lont_data_decl_nonemb <- lont_data_decl %>%
  filter(CT == 'decl_nonemb')
mean(lont_data_decl_emb$Acceptability)
mean(lont_data_decl_nonemb$Acceptability)
# convert to wide format
CT_wide <- lont_data_decl %>%
  group_by(Participant, CT) %>%
  summarise(Acceptability = mean(Acceptability), .groups = "drop") %>%
  pivot_wider(names_from = CT, values_from = Acceptability)
# compute dz and t (within-subjects)
cohens_d(CT_wide$decl_emb, CT_wide$decl_nonemb, paired = TRUE)
t.test(CT_wide$decl_emb, CT_wide$decl_nonemb, paired = TRUE)

# create boxplots
# int. vs. decl
ggplot(lont_data, aes(x = CT1, y = Acceptability)) + geom_boxplot() +
  stat_summary(fun = mean, geom = 'crossbar', width = 0.6,
               color = 'red', linetype = 'dotted') +
  scale_x_discrete(limits = c('int', 'decl'),
                   labels = c(int = 'Interrogative',
                              decl = 'Declarative')) +
  labs(
    x = 'Clause Type',
    y = 'Acceptability Rating',
    title = 'Acceptability Ratings per Clause Type'
  ) + theme_minimal()

# decl_emb vs. decl_nonemb
ggplot(lont_data_decl, aes(x = CT, y = Acceptability)) + geom_boxplot() +
  stat_summary(fun = mean, geom = 'crossbar', width = 0.6,
               color = 'red', linetype = 'dotted') +
  scale_x_discrete(limits = c('decl_nonemb', 'decl_emb'),
                   labels = c(decl_nonemb = 'Non-Embedded Declarative',
                              decl_emb = 'Embedded Declarative')) +
  labs(
    x = 'Clause Type',
    y = 'Acceptability Rating',
    title = 'Acceptability Ratings per Clause Type'
  ) + theme_minimal()


# effect of particles
# compute means
lont_data_APY <- lont_data %>%
  filter(Adv_Ptcl == 'Y')
mean(lont_data_APY$Acceptability)
lont_data_APN <- lont_data %>%
  filter(Adv_Ptcl == 'N')
mean(lont_data_APN$Acceptability)
# convert to wide format
Adv_Ptcl_wide <- lont_data %>%
  group_by(Participant, Adv_Ptcl) %>%
  summarise(Acceptability = mean(Acceptability), .groups = 'drop') %>%
  pivot_wider(names_from = Adv_Ptcl, values_from = Acceptability) 
# compute dz and t (within-subjects)
cohens_d(Adv_Ptcl_wide$Y, Adv_Ptcl_wide$N, paired = TRUE)
t.test(Adv_Ptcl_wide$Y, Adv_Ptcl_wide$N, paired = TRUE)

# create boxplot
ggplot(lont_data, aes(x = Adv_Ptcl, y = Acceptability)) + geom_boxplot() +
  stat_summary(fun = mean, geom = 'crossbar', width = 0.6,
               color = 'red', linetype = 'dotted') +
  scale_x_discrete(limits = c('Y', 'N')) +
  labs(
    x = 'Particle Present Yes or No',
    y = 'Acceptability Rating',
    title = 'Acceptability Ratings with and without Particles'
  ) + theme_minimal()