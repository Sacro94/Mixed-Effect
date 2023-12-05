install.packages("tidyverse")
install.packages("broom")
install.packages("lme4")
library(lme4)
library(tidyverse)
library(broom)

data(sleepstudy)

str(sleepstudy)
ggplot(sleepstudy, aes(x = Days, y = Reaction)) +
  geom_point(color = "blue") +
  labs(title = "Study Data", x = "Days", y = "Reaction Time")

sleepstudy %>%
  summary() %>%
  print()
ggplot(sleepstudy, aes(x = Reaction)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Reaction Time", x = "Reaction Time")

ggplot(sleepstudy, aes(x = factor(Days), y = Reaction, fill = factor(Days))) +
  geom_boxplot() +
  labs(title = "Reaction Time Over Different Days", x = "Days", y = "Reaction Time") +
  scale_fill_manual(values = "lightgreen")

model <- lm(Reaction ~ Days + Subject, data = sleepstudy) %>%
  tidy()
print(model)



residuals <- lm(Reaction ~ Days + Subject, data = sleepstudy) %>%
  augment() %>%
  select(.resid)
ggplot(sleepstudy, aes(x = Days, y = .resid)) +
  geom_point(color = "red") +
  labs(title = "Residuals Analysis", x = "Days", y = "Residuals") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue")

