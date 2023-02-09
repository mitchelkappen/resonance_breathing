# function for plot
plotfunction <-
  function(emmean_dataframe, title){
    ggplot(emmean_dataframe, aes(x=Phase, y=emmean, colour = Breathing_Condition)) +
      geom_point(aes(group = Breathing_Condition), size = 4, position = position_dodge(width = 0.3)) +
      geom_line(aes(group = Breathing_Condition),size = 1, position = position_dodge(width = 0.3))+
      geom_errorbar(width=.25, size = 1, aes(ymin=emmean-SE, ymax=emmean+SE), position = position_dodge(width = 0.3))+
      labs(y = title, x = "Phase")+
      scale_colour_manual(values=cbPalette)+
      theme_apa()
  }

# One general theme to clean up code
plot_theme_apa <-
  function(...){
    theme(
      # legend.key.size=unit(1.3, 'cm'),
      # legend.text=element_text(size=13),
      legend.position = "none",
      plot.title = element_text(size=rel(2)),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.grid.major.y = element_line( size=.1, color="#dedede" ),
      axis.text.x=element_text(size=rel(2)),
      axis.title.y=element_text(size=rel(1.5)),
      axis.title.x = element_text(size=rel(1.5)))
  }


# plotfunction <-
#   function(emmean_dataframe, title){
#     ggplot(emmean_dataframe, aes(x=taskType, y=emmean, colour = Breathing_Cond)) +
#       geom_point(aes(group = Breathing_Cond), size = 4, position = position_dodge(width = 0.3)) + 
#       geom_line(aes(group = Breathing_Cond), size = 1, position = position_dodge(width = 0.3)) +
#       geom_errorbar(width=.25, size = 1, aes(ymin=emmean-SE, ymax=emmean+SE), position = position_dodge(width = 0.3)) +
#       labs(y = title, x = "Phase")+
#       scale_colour_manual(values=cbPalette)+
#       plot_theme_apa()
#   }
