compareScoreDF = data.frame(score = c(noTune_EMD, tune_EMD, noTune_guess, tune_guess),
                            tuneFlag = rep( c("No Tuning", "Tuning", "No Tuning", "Tuning"), each = length(noTune_EMD)),
                            type = rep( c("EMD", "Diff."), each = length(noTune_EMD)*2)) 

EMDScoreDistPlot = compareScoreDF %>% 
  filter(type == "EMD") %>%
  ggplot(aes(x = score, color = tuneFlag, fill = tuneFlag)) +
  geom_histogram(position = "identity", alpha = 0.2, binwidth = 3) + 
  scale_color_manual(values = c("#4e79a7", "#f28e2b")) +
  scale_fill_manual(values = c("#4e79a7", "#f28e2b")) +
  scale_y_continuous(breaks = pretty_breaks(2))  +
  labs(title = "EMD Score", 
       subtitle = "Num. Participant: 51",
       x = "Total EMD", y = "Num. Obs", color = NULL, fill = NULL) + 
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.margin=margin(t = -3, r = 0, b = 0, l = 0),
        legend.text = element_text(size = 8),
        legend.key.size = unit(1,"line"))
EMDScoreDistPlot


probabilityErrorDistPlot = compareScoreDF %>% 
  filter(type == "Diff.") %>%
  ggplot(aes(x = score, color = tuneFlag, fill = tuneFlag)) +
  geom_histogram(position = "identity", alpha = 0.2, binwidth = 16) + 
  scale_color_manual(values = c("#4e79a7", "#f28e2b")) +
  scale_fill_manual(values = c("#4e79a7", "#f28e2b")) +
  scale_y_continuous(breaks = pretty_breaks(2))  +
  labs(title = "Probability Estimation Error", 
       subtitle = "Num. Participant: 51",
       x = "Total Abs. Error", y = "Num. Obs", color = NULL, fill = NULL) + 
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.margin=margin(t = -3, r = 0, b = 0, l = 0),
        legend.text = element_text(size = 8),
        legend.key.size = unit(1,"line"))
probabilityErrorDistPlot