# download metrics from shinyapps.io

library(tidyverse)
library(lubridate)
library(scales)
library(ggthemes)

# appName <- "pasture19_comp"
appName <- "pasture_embed"

# http://docs.rstudio.com/shinyapps.io/metrics.html#ApplicationMetrics
df <- rsconnect::showMetrics("container_status",
                             c("connect_count", 
                               "connect_procs"),
                             appName=appName,
                             server="shinyapps.io",
                             from="36w",
                             interval="1m"
                             ) 

df1 <- df %>% 
  mutate(date=as_datetime(timestamp)) %>% 
  select(-timestamp) %>% 
  arrange(date) %>% 
  mutate(
    n_count=cumsum(connect_count),
    n_procs=cumsum(connect_procs),
    new_connect=case_when(
      is.na(lag(connect_count,1)) ~ 0,
      connect_count>lag(connect_count,1) ~ connect_count-lag(connect_count,1),
      TRUE ~ 0),
    n_connect=cumsum(new_connect) # approximate
  ) %>% 
  filter(n_count>0)

df2 <- df1 %>%  
  select(n_connect, date) %>% 
  gather(key="key", value="value", -date)

p2 <- ggplot(df2) +
  labs(title="Cumulative Connections", x="", y="") +
  geom_line(aes(x=date, y=value, colour=key)) +
  facet_wrap(~key) +
  theme_solarized() +
  scale_x_datetime(labels=date_format("%b-%Y")) +
  scale_colour_discrete(guide=FALSE)

p2 <- ggplot(df1) +
  labs(title=paste("Cumulative Connections (", appName, ")", sep=""), x="", y="") +
  geom_line(aes(x=date, y=n_connect, colour="red")) +
  theme_solarized() +
  # scale_x_datetime(labels=date_format("%b-%Y")) +
  scale_colour_discrete(guide=FALSE)

print(p2)

ggsave(paste0("usage-", as.Date(Sys.time()), ".png"))
