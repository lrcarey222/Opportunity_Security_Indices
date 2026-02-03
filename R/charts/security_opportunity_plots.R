# security_opportunity_plots 
country1="India"
group1 <- c("India","Viet Nam","South Korea","Japan")


#Energy Access------

#Energy consumption per capita
for var in unique(energy_access_tbl$variable){

plot1<-energy_access_tbl %>%
  filter(Country == country1,
         variable==var,
         data_type=="raw") %>%
  arrange(desc(value))

}

