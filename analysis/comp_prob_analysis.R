library(dplyr)
library(ggplot2)
library(tidyr)
library(arrow)


comp_preds<-read_parquet('full_completion_preds_all_receivers.parquet')

comp_preds%>%
  filter(event=='pass_forward')%>%
  select(is_complete,complete_prob)%>%
  group_by(is_complete)%>%
  mutate(complete_prob =median(complete_prob))%>%distinct()