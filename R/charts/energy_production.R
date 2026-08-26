#Energy Production

#Electricity by Fuel

elec_fuel <- ei %>%
  filter(grepl("electbyfuel",Var)) %>%
  mutate(fuel=str_replace(Var,"electbyfuel_","")) %>%
  filter(fuel!="total")

japan_elc<-elec_fuel %>%
  filter(Country=="Japan",
         fuel != "other",
         Year>1990)

ggplot(data=japan_elc,aes(x=Year,y=Value,color=fuel))+geom_line()+theme_minimal()

write.csv(japan_elc %>%
            select(Year,Value,fuel) %>%
            pivot_wider(names_from=fuel,values_from=Value),"data/processed/charts/japan_electricity.csv")
