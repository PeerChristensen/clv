
# check distributions


library(tidyverse)

df <- read_csv2("clv_virksomheder_2.csv")


df %>%
  select(starts_with("CLTV")) %>%
  gather() %>%
  ggplot(aes(value)) +
  geom_density() +
  facet_wrap(~key)

# mean and median
df %>%
  select(starts_with("CLTV")) %>%
  gather() %>%
  group_by(key) %>%
  summarise(m = mean(value),
            median = median(value))

df %>%
  select(starts_with("CLTV")) %>%
  gather() %>%
  ggplot(aes(value)) +
  geom_density() +
  xlim(c(-100,5000)) +
  geom_vline(xintercept=130, linetype="dashed") +
  facet_wrap(~key,ncol=1)
