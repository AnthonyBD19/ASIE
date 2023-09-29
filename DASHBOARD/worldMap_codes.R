#-----

rm(list = ls())

worldMap='https://github.com/SocialAnalytics-StrategicIntelligence/codes/raw/main/maps/worlMapData.gpkg'

#install.packages("sf")

library(sf)
st_layers(worldMap)

#-----

countries=sf::st_read(worldMap,layer='countries')

#-----

library(ggplot2)
base=countries #estoy creando una copia
base$Total_jc5_cat=NULL #truco para que aparezca el mundo

baseMap=ggplot() + geom_sf(data=base, color='grey60',fill='white',lwd = 0.1) + theme_minimal()
choro1=baseMap + geom_sf(data=countries,aes(fill=Total_jc5_cat),lwd = 0.1,show.legend = F) 
choro1=choro1 + facet_wrap(~Total_jc5_cat,ncol = 2)
choro1=choro1 + scale_fill_manual(values = c("green", "blue", "grey","red","black"))
choro1

saveRDS(choro1, file = "choro1.rds")

#-----

baseHist=ggplot(data=countries,aes(x=Total))  
hist1=baseHist + geom_histogram()
hist1
saveRDS(hist1, file = "hist1.rds")
#-----

library(dplyr)
library(ggrepel)

countriesWorst=countries %>% group_by(continent) %>% top_n(1, Total)
countriesBest=countries %>% group_by(continent) %>% top_n(-1, Total)
worstAndBest=c(countriesWorst$name,countriesBest$name)



countries$label=ifelse(countries$name%in%worstAndBest,countries$name,"")

baseScatter=ggplot(data=countries,aes(x=C1_SecurityApparatus,
                                      y=C2_FactionalizedElites)) + theme_linedraw() 
scatter1=baseScatter+geom_point(color='grey60',shape = 21)


scatter1=scatter1 + facet_wrap(~continent) 
scatter1=scatter1 + geom_text_repel(aes(label=label,size=C3_GroupGrievance),min.segment.length = 0.05)

saveRDS(scatter1, file = "scatter1.rds")


#------

library(leaflet)
factpal <- colorFactor(c("green", "blue", "grey","red","black"), 
                       countries$Total_jc5_cat)

leaflet1=leaflet(countries) %>%addTiles()%>%
  addPolygons(label=countries$name,stroke = FALSE, 
              smoothFactor = 0.2, fillOpacity = 1,
              color = ~factpal(Total_jc5_cat)) %>%
  addLegend("bottomright", pal = factpal,
            values = ~Total_jc5_cat,
            title = "Fragility",
            opacity = 1)
leaflet1
saveRDS(leaflet1, file = "leaflet1.rds")

#------

# 1 -install: flexdashboard
# 2- cerrar y abrir RStudio