#flux bias plots
library(cowplot)
library(ggplot2)
library(scales)
augSample<-makeAugmentedSample(eList)$Sample
flux_bias<-plot_grid(
  ggdraw()+
    draw_label(paste0(eList$INFO$shortName,', ', eList$INFO$paramShortName,'\n',
                      'Model is WRTDS Flux Bias Statistic: ',round(fluxBiasStat(eList$Sample)[1],3))),
  plot_grid(
augSample %>%
  ggplot(aes(x=ConcHat,y=rResid))+
  geom_point()+
  scale_x_log10('Estimated Concentration (log-scale)',breaks=scales::breaks_log(),minor_breaks=.5*10^(-10:10))+
  geom_hline(yintercept = 0)+
  scale_y_continuous('Residual',limits = c(-1,1)*max(abs(augSample$rResid)))+
  theme_bw(),
augSample %>%
  ggplot(aes(x=Q*35.31467,y=rResid))+
  geom_point()+
  scale_x_log10('Discharge (cfs) (log-scale)',breaks=scales::breaks_log(),minor_breaks=.5*10^(-10:10))+
  geom_hline(yintercept = 0)+
  scale_y_continuous('Residual',limits = c(-1,1)*max(abs(augSample$rResid)))+
  theme_bw(),
augSample %>%
  ggplot(aes(x=Date,y=rResid))+
  geom_point()+
  # scale_x_log10('Discharge (cfs) (log-scale)',breaks=scales::breaks_log(),minor_breaks=.5*10^(-10:10))+
  geom_hline(yintercept = 0)+
  scale_y_continuous('Residual',limits = c(-1,1)*max(abs(augSample$rResid)))+
  theme_bw(),
augSample %>%
  ggplot(aes(x=factor(month.abb[Month],levels=month.abb),group=month.abb[Month],y=rResid))+
  geom_boxplot()+
  xlab('Month')+
  geom_hline(yintercept = 0)+
  scale_y_continuous('Residual',limits = c(-1,1)*max(abs(augSample$rResid)))+
  theme_bw(),
augSample %>%
  select(Q,SampledValues=ConcAve,SampledEstimates=ConcHat) %>%
  tidyr::pivot_longer(SampledValues:SampledEstimates) %>%
  bind_rows(eList$Daily %>% select(Q,AllEstimates=ConcDay) %>% tidyr::pivot_longer(AllEstimates)) %>%
  ggplot(aes(x=factor(name,levels=c('SampledValues','SampledEstimates','AllEstimates')),group=name,y=value))+
  geom_boxplot()+
  xlab('')+
  scale_y_log10(
                breaks=scales::breaks_log(),minor_breaks=.5*10^(-10:10))+
  theme_bw(),
augSample %>%
  ggplot(aes(x=ConcHat,y=ConcAve))+
  geom_point()+
  geom_abline(slope = 1)+
  scale_y_log10('Observed Concentration',
                breaks=scales::breaks_log(),minor_breaks=.5*10^(-10:10))+
  scale_x_log10('Estimated Concentration',
                breaks=scales::breaks_log(),minor_breaks=.5*10^(-10:10))+
  theme_bw(),
augSample %>%
  select(Q,SampledValues=ConcAve) %>%
  tidyr::pivot_longer(SampledValues) %>%
  bind_rows(eList$Daily %>% select(Q,AllEstimates=ConcDay) %>% tidyr::pivot_longer(AllEstimates)) %>%
  ggplot(aes(x=factor(name,levels=c('SampledValues','AllEstimates')),group=name,y=Q*35.31467))+
  geom_boxplot()+
  xlab('')+
  scale_y_log10('Discharge (cfs)',
                breaks=scales::breaks_log(),minor_breaks=.5*10^(-10:10))+
  theme_bw(),
augSample %>%
  ggplot(aes(x=ConcHat*Q*86.4*ifelse(parmID=='fc',10/1000,1),
             y=ConcAve*Q*86.4*ifelse(parmID=='fc',10/1000,1)))+
  geom_point()+
  geom_abline(slope = 1)+
  scale_y_log10(ifelse(parmID=='fc','Observed Flux (1000 CFU/d)','Observed Flux (kg/d)'),
                breaks=scales::breaks_log(),minor_breaks=.5*10^(-10:10))+
  scale_x_log10(ifelse(parmID=='fc','Estimated Flux (1000 CFU/d)','Observed Flux (kg/d)'),
                breaks=scales::breaks_log(),minor_breaks=.5*10^(-10:10))+
  theme_bw(),
ncol = 2
),
ncol=1,rel_heights =c(.1,1))
