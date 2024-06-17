
colorRampAlpha <- function(..., n, alpha) {
  colors <- colorRampPalette(...)(n)
  paste(colors, sprintf("%x", ceiling(255*alpha)), sep="")
}

library(sf)
library(RColorBrewer)
library(psych)
library(ggplot2)
library(reshape2)
library(dplyr)
library(usmap)
library(tidyr)
library(scales)
library(tibble)
library(tmap)
library(colorspace)
library(MASS)
library(jtools)
library(gridExtra)


#============================================================================================================
#============================================================================================================
# Data
#============================================================================================================
#============================================================================================================


#----- EDH data
df <- read.csv("MEII_data.csv")
df$GEOID = as.character(as.numeric(df$GEOID))
head(df)

#----- TRACT
tracts  <- st_read('cb_2019_us_tract_500k/cb_2019_us_tract_500k.shp')
tracts$GEOID = as.character(as.numeric(tracts$GEOID))
head(tracts)


left_join(tracts, df, by = 'GEOID') -> MapData 
projected = st_transform(MapData, 3085)
table(projected$ST)
projected = projected[!(projected$ST %in% c("02", "15", "60","66","69","72","78")),]
table(projected$ST)
head(projected)
names(projected)


#============================================================================================================
#============================================================================================================
# Maps
#============================================================================================================
#============================================================================================================


#----- Heavy precipitations

nlegend =round(max(projected$heavyprec,na.rm=T)-min(projected$heavyprec,na.rm=T))+1

tm_shape(projected) + 
  tm_fill("heavyprec",
          style = "cont",
          palette =  hcl.colors(nlegend, "Temps"),
          title = "ANNUAL \nDAYS")  +
  tm_layout(legend.text.size = 1,
            legend.title.size = 1,
            main.title = "Heavy precipitation",
            main.title.size = 2,
            legend.position = c("right", "bottom"),
            frame = FALSE)+
  tm_borders(alpha=.9,lwd=0)  + 
  tm_scale_bar(text.size = 0.8,position=c("left", "bottom"))




#----- HEAT WAVES 

nlegend =round(max(projected$heatw)-min(projected$heatw))+1

tm_shape(projected) + 
  tm_fill("heatw",
          style = "cont",
          palette =  hcl.colors(nlegend, "Temps"),
          title = "ANNUAL \nDAYS")  +
  tm_layout(legend.text.size = 1,
            legend.title.size = 1,
            main.title = "Heat Waves (May-September)",
            main.title.size = 2,
            legend.position = c("right", "bottom"),
            frame = FALSE)+
  tm_borders(alpha=.9,lwd=0)  + 
  tm_scale_bar(text.size = 0.8,position=c("left", "bottom"))




#----- COLD WAVES

nlegend =round(max(projected$coldw)-min(projected$coldw))+1

tm_shape(projected) + 
  tm_fill("coldw",
          style = "cont",
          palette =  hcl.colors(nlegend, "Temps"),
          title = "ANNUAL \nDAYS")  +
  tm_layout(legend.text.size = 1,
            legend.title.size = 1,
            main.title = "Cold Waves (November-March)",
            main.title.size = 2,
            legend.position = c("right", "bottom"),
            frame = FALSE)+
  tm_borders(alpha=.9,lwd=0)  + 
  tm_scale_bar(text.size = 0.8,position=c("left", "bottom"))



#----- TRI

nlegend =round(max(projected$tri,na.rm=T)-min(projected$tri,na.rm=T))+1

tm_shape(projected) + 
  tm_fill("tri",
          style = "cont",
          palette =  hcl.colors(nlegend, "Temps"),
          title = "POPULATION \nPROPORTION")  +
  tm_layout(legend.text.size = 1,
            legend.title.size = 1,
            main.title = "Exposure to releases of toxic chemicals",
            main.title.size = 2,
            legend.position = c("right", "bottom"),
            frame = FALSE)+
  tm_borders(alpha=.9,lwd=0)  + 
  tm_scale_bar(text.size = 0.8,position=c("left", "bottom"))



#----- Radon
#Zone 3 Low Potential: less than 2 pCi/L
#Zone 2 Moderate Potential: between 2 and 4 pCi/L 
#Zone 1 Highest Potential: greater than 4 pCi/L 

#df$radcont <- NA
#df$radcont[df$rad == "[0,2)"] <- round(runif(length(df$radcont[df$rad == "[0,2)"]),0,1.9),2)
#df$radcont[df$rad == "[2;4)"] <- round(runif(length(df$radcont[df$rad == "[2;4)"]),2,3.9),2)
#df$radcont[df$rad == "[4+)"] <- round(runif(length(df$radcont[df$rad == "[4+)"]),4,10),2)

tm_shape(projected) + 
  tm_fill("rad",
          style = "pretty",
          palette =  hcl.colors(3, "Zissou"),
          title = "AVERAGE \npCi/L")  +
  tm_layout(legend.text.size = 1,
            legend.title.size = 1,
            main.title = "Indoor radon concentration",
            main.title.size = 2,
            legend.position = c("right", "bottom"),
            frame = FALSE)+
  tm_borders(alpha=.9,lwd=0)  + 
  tm_scale_bar(text.size = 0.8,position=c("left", "bottom"))




#----- GREEN areas + OpenSpace

tm_shape(projected) + 
  tm_fill("greenaopens",
          style = "cont",
          palette =  rev(hcl.colors(10, "Temps")),
          title = "%")  +
  tm_layout(legend.text.size = 1,
            legend.title.size = 1,
            main.title = "Green areas including open space",
            main.title.size = 2,
            legend.position = c("right", "bottom"),
            frame = FALSE)+
  tm_borders(alpha=.9,lwd=0)  + 
  tm_scale_bar(text.size = 0.8,position=c("left", "bottom"))




#----- PM2.5

nlegend =round(max(projected$pm25,na.rm=T)-min(projected$pm25,na.rm=T))+1

tm_shape(projected) + 
  tm_fill("pm25",
          style = "cont",
          palette =  (hcl.colors(nlegend, "Temps")), 
          title = "AVERAGE \nµg/m3")  +
  tm_layout(legend.text.size = 1,
            legend.title.size = 1,
            main.title = "Particulate Matter (2.5 μm and smaller diameter)",
            main.title.size = 2,
            legend.position = c("right", "bottom"),
            frame = FALSE)+
  tm_borders(alpha=.9,lwd=0)  + 
  tm_scale_bar(text.size = 0.8,position=c("left", "bottom"))


#----- Ozone

nlegend =round(max(projected$ozone,na.rm=T)-min(projected$ozone,na.rm=T))+1


tm_shape(projected) + 
  tm_fill("ozone",
          style = "cont",
          palette =  hcl.colors(nlegend, "Temps"),
          title = "AVERAGE \nppm")  +
  tm_layout(legend.text.size = 1,
            legend.title.size = 1,
            main.title = "Daily 8-hours maximum ozone concentration",
            main.title.size = 2,
            legend.position = c("right", "bottom"),
            frame = FALSE)+
  tm_borders(alpha=.9,lwd=0)  + 
  tm_scale_bar(text.size = 0.8,position=c("left", "bottom"))



#--- UV

nlegend =round(max(projected$uv,na.rm=T)-min(projected$uv,na.rm=T))+1


tm_shape(projected) + 
  tm_fill("uv",
          style = "cont",
          palette =  rev(hcl.colors(nlegend, "Temps")), 
          title = "AVERAGE \nMJ/m2")  +
  tm_layout(legend.text.size = 1,
            legend.title.size = 1,
            main.title = "Ultraviolet (UV) radiation",
            main.title.size = 2,
            legend.position = c("right", "bottom"),
            frame = FALSE)+
  tm_borders(alpha=.9,lwd=0)  + 
  tm_scale_bar(text.size = 0.8,position=c("left", "bottom"))



#----- NOISE

nlegend =round(max(projected$noise,na.rm=T)-min(projected$noise,na.rm=T))+1


tm_shape(projected) + 
  tm_fill("noise",
          style = "cont",
          palette =  hcl.colors(nlegend, "Temps"), 
          title = "dBA")  +
  tm_layout(legend.text.size = 1,
            legend.title.size = 1,
            main.title = "Transportation noise (rail, road, aviation)",
            main.title.size = 2,
            legend.position = c("right", "bottom"),
            frame = FALSE)+
  tm_borders(alpha=.9,lwd=0)  + 
  tm_scale_bar(text.size = 0.8,position=c("left", "bottom"))







#============================================================================================================
#============================================================================================================
# EDH Index
#============================================================================================================
#============================================================================================================


X <- projected[,c("heavyprec","heatw","coldw","tri","greenaopens",
                  "pm25","ozone","uv","noisemax","radcont")]

topQ <- apply(X,2,function(x) quantile(as.numeric(unlist(x)),0.65,na.rm=T))

# detrimental (+1): 
heavyprec <- ifelse(projected$heavyprec > topQ["heavyprec"],1,0)
coldw <- ifelse(projected$coldw > topQ["coldw"],1,0)      
heatw <- ifelse(projected$heatw > topQ["heatw"],1,0)   
ozone <- ifelse(projected$ozone > topQ["ozone"],1,0)
pm25 <- ifelse(projected$pm25 > topQ["pm25"],1,0)
radcont <- ifelse(projected$radcont > topQ["radcont"],1,0)
tri <- ifelse(projected$tri > topQ["tri"],1,0)
noisemax <- ifelse(projected$noisemax > topQ["noisemax"],1,0)
# beneficial (-1): 
greenaopens <- ifelse(projected$greenaopens > topQ["greenaopens"],-1,0)
uv <- ifelse(projected$uv > topQ["uv"],-1,0)


trvars <- cbind(
  # detrimental (+1): 
  heavyprec,heatw,coldw,tri,
  pm25,ozone,noisemax,radcont,
  # beneficial (-1): 
  greenaopens,uv)

projected$MEII <- factor(rowSums(trvars, na.rm = T), 
                  levels = range(as.numeric(rowSums(trvars, na.rm = T)),na.rm=T)[1]:range(as.numeric(rowSums(trvars, na.rm = T)),na.rm=T)[2])

table(projected$MEII)
round(table(projected$MEII)/72333*100,2)

#            (least)                                                      (most)
# MEII class     -2     -1      0      1      2      3      4      5     6     7 
# N Tracts       64  1,829  8,529 15,874 18,568 14,932  8,365  3,298   770   104 
# %            0.09   2.53  11.79  21.95  25.67  20.64  11.56   4.56  1.06  0.14 


#---------------------------------

tm_shape(projected) + 
  tm_fill("MEII",
          style = "quantile",
          palette =  hcl.colors(5, "Geyser"),  
          title = "")  +
  tm_layout(legend.text.size = 1,
            legend.title.size = 0.8,
            main.title = "US Census Tract Multi-Exposure Environmental Index (MEEI)",
            main.title.size = 1.5,
            legend.position = c("right", "bottom"),
            frame = FALSE)+
  tm_borders(alpha=.9,lwd=0)  + 
  tm_scale_bar(text.size = 0.6,position=c("left", "bottom"))



#---------------------------------

means = apply(X[-c(ncol(X))],2,mean)
col_names <- colnames(X)
col_names <- col_names[-c(ncol(X))]

colsc <- c("noisemax"="#F1E2CC", "heavyprec"="#FDCDAC", 
           "coldw"="#FDCDAC","heatw"="#FDCDAC", "pm25"="#F4CAE4", 
           "ozone"="#F4CAE4", "radcont" = "#CBD5E8",
           "uv"="#FFF2AE",
           "greenaopens"="#B3E2CD","greena"="#B3E2CD","opens"="#B3E2CD","tri"="#CCCCCC")
namesc <- c("noisemax"="Transportation noise", 
            "heavyprec"= "Heavy precipitation", "coldw"="Cold waves", 
            "heatw"="Hot waves","pm25"="PM 2.5","ozone"="Ozone", "radcont" = "Indoor radon",
            "uv"="UV radiation","greenaopens"="Green areas including open space","greena"="Green areas","tri"="Toxic releases facilities")


plot_list <- list()
for (i in col_names){
  plot <- ggplot(projected, aes_string(x="MEII", y=i)) +
    geom_boxplot() + 
    geom_violin(trim=F,fill=colsc[i]) +
    theme_light() +
    geom_hline(yintercept = means[i]) +
    stat_smooth(method = "gam", se=TRUE, aes(group=1),formula = y ~ splines::ns(x, 6)) + 
    labs(x = "MEII Class", y = namesc[i]) +
    theme(text = element_text(size = 18))
  plot_list[[i]] <- plot
}
plot_grob <- arrangeGrob(grobs=plot_list)
grid.arrange(plot_grob)





