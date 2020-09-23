
setwd('C:/Users/mcros/Desktop/Postdoc UGA/Whitefly project 2020/manuscripts/review Insects special issue')

library(rgdal)
library(rgeos)
library(tidyverse)
library(sf)        # for manipulation of simple features objects
library(lwgeom) 

winkel_tripel = "+proj=wintri +datum=WGS84 +no_defs +over"
world = spTransform(readOGR(dsn='./shapefiles/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp',layer='99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk',verbose=F,stringsAsFactors=F),CRS(winkel_tripel))
grat_sf = st_graticule(lat = c(-89.9, seq(-80, 80, 20), 89.9)) %>%  st_transform_proj(crs = winkel_tripel) #grid for mapping
st_write(grat_sf,'./shapefiles/graticule_winkel-tripel.shp')

dat = read.table('whitefly.review.summary.txt',sep='\t',as.is=T,check.names=F,header=T)

dat2 = data.frame('Country'=NA,'Biotype'=NA,'Year1'=-999,'Year2'=-999,'Marker'=NA,'N.markers'=-999)
dx = 1
for (i in 1:nrow(dat)){
	cs = strsplit(dat$Geography[i],';')[[1]]
	biotypes = strsplit(dat$Biotypes[i],';')[[1]]
	for (j in 1:length(cs)){
		for (k in 1:length(biotypes)){
			dat2[dx,1] = cs[j]
			dat2[dx,2] = biotypes[k]
			dat2[dx,3] = dat$Year1[i]
			dat2[dx,4] = dat$Year2[i]
			dat2[dx,5] = dat$Marker[i]
			dat2[dx,6] = dat$N.markers[i]
			dx = dx + 1
		}
	}
}

uc = unique(dat2$Country)
out = data.frame('Country'=uc,'Biotypes'=NA,'Markers'=NA,'SSR'=NA,'MEAM1'=NA,'MED'=NA,'SNP'=NA)
for (i in 1:length(uc)){
	biotypes = sort(unique(dat2$Biotype[which(dat2$Country==uc[i])]))
	if (length(which(biotypes=='Unknown'))==length(biotypes)){
		biotypes2 = biotypes
	} else {
		biotypes2 = paste0(biotypes[which(biotypes!='Unknown')],collapse=';')
	}
	markers = paste0(unique(dat2$Marker[which(dat2$Country==uc[i])]),collapse=';')
	out[i,2] = biotypes2
	out[i,3] = markers
	out[i,4] = ceiling(length(grep('SSR',markers))/length(markers))
	out[i,5] = ceiling(length(grep('MEAM 1',biotypes2))/length(biotypes2))
	out[i,6] = ceiling(length(grep('MED',biotypes2))/length(biotypes2))
	out[i,7] = ceiling(length(grep('SNP',markers))/length(markers))
}
out$MEAM1.MED = paste(out$MEAM1,out$MED,sep='_')

world_sf = as(world,'sf') #for mapping
world_sf2 = merge(world_sf,out,by.x='CNTRY_NAME',by.y='Country',all.x=T)
st_write(world_sf2,'./shapefiles/world_winkel-tripel_whitefly.shp')

states = spTransform(readOGR(dsn='D:/Landscape_Environmental_data/National_atlas/statesp010g.shp',layer='statesp010g',verbose=F,stringsAsFactors=F),CRS(winkel_tripel))
ak_hi = as(states[which(states@data$STATE_ABBR=='AK' | states@data$STATE_ABBR=='HI'),],'sf')
st_write(ak_hi,'./shapefiles/AK_HI.shp')

#library(cowplot)   # for theme_map()
#ggplot() + 
#  geom_sf(data = grat_sf, color = "gray80", size = 0.25/.pt) + 
#  geom_sf(data = world_sf, color = "white", size = 0.5/.pt) +
#  coord_sf(datum = NULL) +
#  theme_map()
  
  