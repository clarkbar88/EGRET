#' Creates ggplot multiplot
#' @param eList named list with at least the Daily dataframe

library(cowplot)
library(ggplot2)
library(scales)
conc_multi_plot<-function(eList){
  plot_grid(
  ggdraw()+
    draw_label(paste0(eList$INFO$shortName,'\n', eList$INFO$paramShortName)),
  plot_grid(
    eList$Sample %>%
      ggplot(aes(x=Q*35.31467,y=ConcAve))+
      geom_point()+
      scale_x_log10('Discharge (cfs) (log-scale)',breaks=scales::breaks_log(),minor_breaks=.5*10^(-10:10))+
      scale_y_log10(paste0('Conc. (',eList$INFO$param.units,')'))+
      theme_bw()
    ,
    eList$Sample %>%
      ggplot(aes(x=Date,y=ConcAve))+
      geom_point()+
      # scale_x_log10('Discharge (cfs) (log-scale)',breaks=scales::breaks_log(),minor_breaks=.5*10^(-10:10))+
      geom_hline(yintercept = 0)+
      scale_y_log10(paste0('Conc. (',eList$INFO$param.units,')'))+
      theme_bw()
    ,
    eList$Sample %>%
      ggplot(aes(x=factor(month.abb[Month],levels=month.abb),group=month.abb[Month],y=ConcAve))+
      geom_boxplot()+
      xlab('Month')+
      geom_hline(yintercept = 0)+
      scale_y_log10(paste0('Conc. (',eList$INFO$param.units,')'))+
      theme_bw()
    ,
    NULL,
    ncol=2),
  ncol=1,rel_heights =c(.1,1))
}
