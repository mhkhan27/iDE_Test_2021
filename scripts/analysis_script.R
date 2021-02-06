rm (list=ls())

library(dplyr)
library(butteR)
library(srvyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggrepel)

# input -------------------------------------------------------------------

df <- read.csv("inputs/datset.csv",na.strings = c(""," ","n/a","NA"),stringsAsFactors = F) %>% mutate(
  treat_status = if_else(treat == 1,"yes","no",NULL)
)





# descriptive_analysis  ---------------------------------------------------


#### general check ######


number_of_survey_by_district <- df$district %>% AMR::freq()
number_of_survey_by_region <- df$region %>% AMR::freq()
number_of_survey_by_treatment <- df$treat_status %>% AMR::freq()

hist(df$hh_size)

hh_median_overall <- df$hh_size %>% median(na.rm = T)

hh_median_treatment <- (df %>% filter(treat_status == "yes"))$hh_size %>% median(na.rm = T)
hh_median_control <- (df %>% filter(treat_status == "no"))$hh_size %>% median(na.rm = T)


col_for_descrpt <- c("treat_status","q2_8_hoh_gender","hh_size","ppi_raw","q2_9_hh_status","q3_1a_land_rice","q3_1b_land_veg","q3_1c_land_fruit")



df_for_descriptive <- df %>% select(col_for_descrpt)

dfsvy_for_des <- as_survey(df)

descriptive_analysis_overall <-mean_prop_working(dfsvy_for_des,list_of_variables = col_for_descrpt,aggregation_level = NULL)

descriptive_analysis_by_treatment <-mean_prop_working(dfsvy_for_des,list_of_variables = col_for_descrpt,aggregation_level = "treat_status")



# poverty_analysis  -------------------------------------------------------

overall_poverty <- descriptive_analysis_overall %>% select(contains("q2_9_hh_status")) 

overall_poverty_pi_long <- overall_poverty %>% 
  tidyr::pivot_longer(cols = names(overall_poverty),values_to = "percentage",names_to = "proverty_status") %>% mutate(
    treat_status = "Total Population"
  )


treatment_poverty <- descriptive_analysis_by_treatment %>% select(treat_status,contains("q2_9_hh_status")) 

treatment_poverty_pi_long <- treatment_poverty %>% pivot_longer(cols = !treat_status,names_to = "proverty_status",values_to = "percentage")
treatment_poverty_pi_long$treat_status <- if_else(treatment_poverty_pi_long$treat_status  == "yes","Treatment Group","Control Group")

poverty_full <- bind_rows(overall_poverty_pi_long,treatment_poverty_pi_long)
poverty_full$percentage <- poverty_full$percentage*100

poverty_full$proverty_status <- poverty_full$proverty_status %>% str_replace_all("q2_9_hh_status.","")

poverty_full$proverty_status <- case_when(poverty_full$proverty_status == "near_poor" ~ "Near poor",
                                          poverty_full$proverty_status == "non_poor" ~ "Non poor",
                                            poverty_full$proverty_status == "poor"~ "Poor")


poverty_full$treat_status <- factor(poverty_full$treat_status, levels = c("Total Population", "Control Group", "Treatment Group"))


df2 <-poverty_full


df2$proverty_status <- factor(df2$proverty_status,levels = c(  "Poor","Near poor", "Non poor"))

poverty_graph <- ggplot(df2, aes(x = "", y = percentage, fill = proverty_status)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0,direction = -1)+
  #scale_fill_brewer(palette  = "Accent", direction = 3)+
  # geom_text(aes(label = paste0(round(percentage,1),"%"),
  #                hjust= .5,
  #                vjust=.5
  #                ), color = "black", size = 3.5,
  #            position = position_stack(vjust = .5))+
  
  #scale_fill_manual(values = c("#ee5859","#58585a","blue")) +
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=.8),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.spacing = unit(0,"cm"),
        legend.title=element_blank(),
        legend.text = element_text(size = 8,color="#58585A"),
        legend.position = "bottom",
        legend.justification = .45,
        legend.key.width =  unit(1,"cm"),
        legend.spacing.x = unit(.5, "cm"),
        legend.spacing.y = unit(.9, "cm"),
        legend.key.size = unit(1, 'lines'),
        legend.key = element_rect(fill = NA),
        plot.margin = unit(c(.1, .1, 0, 0), "cm"),
        legend.text.align = 0,
        strip.text = element_text(size=9,angle = 0, hjust = 0,face="bold"))+
facet_wrap(treat_status~.,ncol = 3,labeller = label_wrap_gen(width=38))

poverty_graph

ggsave(path = "outputs/",filename =paste0("poverty_graph",".jpg") ,width=14,height=8,units="cm",scale = 1,dpi = 400)


# land holding  -----------------------------------------------------------

overall_land_holding <- descriptive_analysis_overall %>% select(starts_with("q3_")) %>% mutate(
  total_area_of_holding_land = q3_1a_land_rice + q3_1b_land_veg +q3_1c_land_fruit,
  Rice = q3_1a_land_rice/total_area_of_holding_land*100,
  Vegetable = q3_1b_land_veg/total_area_of_holding_land*100,
  Fruits = q3_1c_land_fruit/total_area_of_holding_land*100,
) %>% select(Rice,Vegetable,Fruits)

overall_land_holding_pi_long <- overall_land_holding %>% 
  tidyr::pivot_longer(cols = names(overall_land_holding),values_to = "percentage",names_to = "product") %>% mutate(
    treat_status = "Total Population"
  )


treatment_land_holding <- descriptive_analysis_by_treatment %>% select(treat_status,contains("q3_"))  %>% mutate(
  total_area_of_holding_land = q3_1a_land_rice + q3_1b_land_veg +q3_1c_land_fruit,
  Rice = q3_1a_land_rice/total_area_of_holding_land*100,
  Vegetable = q3_1b_land_veg/total_area_of_holding_land*100,
  Fruits = q3_1c_land_fruit/total_area_of_holding_land*100,
) %>% select(treat_status,Rice,Vegetable,Fruits)

treatment_land_holding_pi_long <- treatment_land_holding %>% pivot_longer(cols = !treat_status,names_to = "product",values_to = "percentage")
treatment_land_holding_pi_long$treat_status <- if_else(treatment_land_holding_pi_long$treat_status  == "yes","Treatment Group","Control Group")

land_holding_full <- bind_rows(overall_land_holding_pi_long,treatment_land_holding_pi_long)

land_holding_full$treat_status <- factor(land_holding_full$treat_status, levels = c("Total Population", "Control Group", "Treatment Group"))


land_holding_graph <- ggplot(land_holding_full, aes(x = "", y = percentage, fill = product)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0,direction = -1)+
  geom_text(aes( label = paste0(round(percentage,1),"%"),x = 1.1), color = "black",size =2.8,fontface = "bold",
            position = position_stack(vjust = 0.5))+
  #scale_fill_manual(values = c("#ee5859","#58585a","blue")) +
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=.8),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.spacing = unit(0,"cm"),
        legend.title=element_blank(),
        legend.text = element_text(size = 8,color="#58585A"),
        legend.position = "bottom",
        legend.justification = .45,
        legend.key.width =  unit(1,"cm"),
        legend.spacing.x = unit(.5, "cm"),
        legend.spacing.y = unit(.9, "cm"),
        legend.key.size = unit(1, 'lines'),
        legend.key = element_rect(fill = NA),
        plot.margin = unit(c(.1, .1, 0, 0), "cm"),
        legend.text.align = 0,
        strip.text = element_text(size=9,angle = 0, hjust = 0,face="bold" ))+
  facet_wrap(~treat_status,ncol = 3,labeller = label_wrap_gen(width=38))
land_holding_graph
ggsave(path = "outputs/",filename =paste0("land_holding_graph",".jpg") ,width=14,height=8,units="cm",scale = 1,dpi = 400)




# t test ------------------------------------------------------------------


df_t_test <- df %>% select(treat_status,profit_total_ppp,total_profit_ppp,total_profit_m2_ppp)
df_t_test$treat_status <- if_else(df_t_test$treat_status   == "yes","Treatment","Control")



dat <- df_t_test


dat$treat_status <- factor(dat$treat_status)

t_test_re<- list()

for (i in 2:4) { # variables to compare are variables 2 to 4
  print(dat[i] %>% names())
  name_indicator <-dat[i] %>% names()
  boxplot(dat[, i] ~ dat$treat_status, # draw boxplots by group
          ylab = names(dat[i]), # rename y-axis with variable's name
          xlab = "population group"
  )
  t_test_re[[name_indicator]] <- t.test(dat[, i] ~ dat$treat_status) 
  print(t.test(dat[, i] ~ dat$treat_status)) # print results of t-test
}

result_r <- do.call("rbind",t_test_re) %>% as.data.frame()


openxlsx::write.xlsx(result_r,"outputs/t_test_result.xlsx")


# boxchart ----------------------------------------------------------------

pivot_t_long <- df_t_test %>% pivot_longer(cols = !treat_status,names_to = "indicator",values_to = "value")

fun_median <- function(x){
  return(data.frame(y=median(x,na.rm = T),label= round(median(x,na.rm=T),0)))}
fun_max <- function(x){
  return(data.frame(y=max(x,na.rm = T),label=round(max(x,na.rm=T))))}
fun_min <- function(x){
  return(data.frame(y=min(x,na.rm = T),label=round(min(x,na.rm=T))))}
fun_mean <- function(x){
  return(data.frame(y=mean(x,na.rm = T),label=round(min(x,na.rm=T))))}

data_for_graph2<-pivot_t_long



data_for_graph2 <- data_for_graph2  %>% mutate(
  order = if_else(indicator == "profit_total_ppp",1,
                  if_else(indicator == "total_profit_ppp",2,3))
) %>% arrange(order)

data_for_graph2$treat_status %>% unique()



ymax <- max(data_for_graph2$value,na.rm = T)+25

ggplot(data_for_graph2, aes(x = fct_reorder(treat_status,-value),y = value))+
  geom_boxplot(width=0.3)+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        
        axis.text = element_text(size = 14),
        axis.text.y = element_text(angle = 90,size = 14,vjust = .5,hjust = .5),
        panel.background = element_rect(fill = "#FFFFFF", colour = "#505050",
                                        size = 2, linetype = "solid"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                          colour = "#c1c1c1"),
        strip.text = element_text(size=14))+ylab("")+
  stat_summary(fun.data = fun_median, geom="text", size=4,vjust=-.5 ,hjust=-.8)+
  #stat_summary(fun.data = fun_max, geom="text", size=4,vjust=-.6)+
  #stat_summary(fun.data = fun_min, geom="text", size=4, vjust=1.4 )+
  facet_wrap(.~reorder(indicator,order),scales = "free") #+
  #ylim(0,ymax)


ggsave(path = "outputs/",filename =paste0("Box_chart",".jpg") ,width=14,height=8,units="cm",scale = 1.8,dpi = 400)


# household_histogram -----------------------------------------------------


df_for_hist<- df
df_for_hist$treat_status <- if_else(df_for_hist$treat_status == "yes","Treatment Group","Control Group")

ggplot(df_for_hist,aes(x=hh_size)) +
  geom_histogram(aes(x = hh_size,fill ="white"), 
                 position = "identity", alpha = 0.5,colour = "black", binwidth = 1) +
  facet_wrap(.~treat_status,ncol = 2)+
  theme(axis.title = element_text(),
        axis.line = element_blank(),
        #axis.text = element_text(),
        panel.border = element_rect(colour = "black", fill=NA, size=.8),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.spacing = unit(0,"cm"),
        legend.title=element_blank(),
        legend.text = element_text(size = 8,color="#58585A"),
        legend.position = "",
        legend.justification = .45,
        legend.key.width =  unit(1,"cm"),
        legend.spacing.x = unit(.5, "cm"),
        legend.spacing.y = unit(.9, "cm"),
        legend.key.size = unit(1, 'lines'),
        legend.key = element_rect(fill = NA),
        plot.margin = unit(c(.1, .1, 0, 0), "cm"),
        legend.text.align = 0,
        strip.text = element_text(size=9,angle = 0, hjust = 0,face = "bold"))+
  scale_x_continuous(breaks = seq(0,12,3))+ ylab("Count") + xlab("Household Size")+
  geom_density(aes(y=1*..count..), colour="black", adjust=4) 

ggsave(path = "outputs/",filename =paste0("histogram",".jpg") ,width=14,height=8,units="cm",scale = 1,dpi = 400)
