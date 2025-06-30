#need file river which is the matrix of species
#need file riversites 
#need file riverrich

river<-read.table("C:/Users/aecsk/Documents/GitHub/Wildrice_code/river_taxa.txt",header=TRUE)
riversites<-read.table("C:/Users/aecsk/Documents/GitHub/Wildrice_code/river_sites.txt",header=TRUE)
riverrich<-read.table("C:/Users/aecsk/Documents/GitHub/Wildrice_code/river_rich.txt",header=TRUE)

#NOTE MARCH 4 2024: SITE AND RICE ARE COLLINEAR! which, obviously, bc site is either rice or no rice, always
#So let's not consider site. It's not important. What we want is rice and no rice. 

#load vegan
library(vegan)
library(wesanderson)

library(cowplot)
library(ggplot2)
library(tidyr)

#compute NMDS
rivernmds<-metaMDS(river)

#plot NMDS in base R
ordiplot(rivernmds,type="n")
ordiellipse(rivernmds,groups=riversites$Rice,draw="polygon",col="grey90",label=F)
orditorp(rivernmds,display="species",col="black",air=0.01)
orditorp(rivernmds,display="sites",col="red",air=0.01)

#analysis of similarity for sites and regions
anosim(river,riversites$Month)
#anosim(river,riversites$Enviro)
anosim(river,riversites$Rice)

#compute PERMANOVA with a space and time interaction
adonis2(formula=river~riversites$Rice*riversites$Month+riversites$Enviro)
#Changed that code March 5 2024 so that the model order matches the richness/diversity ones


#moving plot to ggplot

data.scores <- as.data.frame(scores(rivernmds)$sites)
datascores<-cbind(data.scores,riversites)
head(datascores)
species.scores <- as.data.frame(scores(rivernmds, "species"))
species.scores$species <- rownames(species.scores)
head(species.scores)



#make hulls, one for each sea

grp.a <- data.scores[datascores$Month == "June", ][chull(datascores[datascores$Month == 
                                                                       "June", c("NMDS1", "NMDS2")]), ]
grp.b <- data.scores[datascores$Month == "August", ][chull(datascores[datascores$Month == 
                                                                      "August", c("NMDS1", "NMDS2")]), ]
grp.c <- data.scores[datascores$Month == "October", ][chull(datascores[datascores$Month == 
                                                                         "October", c("NMDS1", "NMDS2")]), ]
hull.data <- rbind(grp.a, grp.b,grp.c) #turn the hulls into a single dataframe
hull.month<-c("June","June","June","June","June","June","June","August","August","August","August","August","August","October","October","October","October","October","October","October","October") #add column for groups (these are based on this data only)
hull.data<-cbind(hull.data,hull.month) #attach group names to hull dataframe

#plot in ggplot

palwes<-c("#F21A00","#EBCC2A","#3B9AB2")

morphotime<-ggplot() +
  geom_point(data=datascores,aes(x=NMDS1,y=NMDS2,colour=Month),size=3) + # add the point markers
  scale_colour_manual(values = palwes) +
  coord_equal() +
  theme_bw()+
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())+ 
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,group=hull.month),alpha=0.15) #add polygon based on the hulls calculated


#By rice
grp.a <- data.scores[datascores$Rice == "Y", ][chull(datascores[datascores$Rice == 
                                                                      "Y", c("NMDS1", "NMDS2")]), ]
grp.b <- data.scores[datascores$Rice == "N", ][chull(datascores[datascores$Rice == 
                                                                        "N", c("NMDS1", "NMDS2")]), ]
hull.data <- rbind(grp.a, grp.b) #turn the hulls into a single dataframe
hull.rice<-c("Y","Y","Y","Y","Y","Y","Y","N","N","N","N","N","N","N") #add column for groups (these are based on this data only)
hull.data<-cbind(hull.data,hull.rice) #attach group names to hull dataframe

morphorice<-ggplot() +
  geom_point(data=datascores,aes(x=NMDS1,y=NMDS2,colour=Rice),size=3) + # add the point markers
  scale_colour_manual(values=rev(wes_palette("Zissou1", n = 2, type="continuous"))) +
  coord_equal() +
  theme_bw()+
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())+ 
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,group=hull.rice),alpha=0.10) #add polygon based on the hulls calculated

plot_grid(morphotime,morphorice,labels=c("A","B"),ncol=1)



#diversity statistics

riverdiv<-cbind(diversity(river,index="simpson"),riversites) #calculate simpsons index, bind to site information

colnames(riverdiv)<-c("Simpsons","Enviro","Month","Site","Replicate","Rice") #rename columns

summary(aov(riverdiv$Simpsons~riverdiv$Rice*riverdiv$Month+riverdiv$Enviro)) #two-way ANOVA



diversityfig<-ggplot(riverdiv,aes(x=Month,y=Simpsons,fill=Rice))+
  geom_boxplot()+ 
  geom_point(alpha=0.6, position=position_jitterdodge(0.2))+
  scale_fill_manual(values=rev(wes_palette("Zissou1", n = 2, type="continuous"))) +
  scale_color_manual(values=rev(wes_palette("Zissou1", n = 2, type="continuous"))) +
  ylim(0,1)+
  theme_bw()+
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1),
    axis.title.x = element_blank(), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())


#richness stats

summary(aov(riverrich$Richness~riverrich$Rice*riverrich$Month+riverrich$Enviro)) #two-way ANOVA

#ricerich.lm <- lm(formula = Richness ~ Rice*Month,
                 #data = riverrich)

#car::Anova(ricerich.lm, type = 3)

richnessfig<-ggplot(riverrich,aes(x=Month,y=Richness,fill=Rice))+
  geom_boxplot()+ 
  geom_point(alpha=0.6, position=position_jitterdodge(0.2))+
  scale_fill_manual(values=rev(wes_palette("Zissou1", n = 2, type="continuous"))) +
  scale_color_manual(values=rev(wes_palette("Zissou1", n = 2, type="continuous"))) +
  ylim(0,3)+
  theme_bw()+
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1),
        axis.title.x = element_blank(), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

plot_grid(richnessfig,diversityfig,labels=c("A","B"),ncol=1)


#Composition plot tries, March 6 start, from ASUS code
#make collapsed list of taxa across months
ricecomp<-cbind(river,riversites)

sites<-c(1:ncol(river))
b = NULL

for (i in sites) {
  collapsed<-tapply(ricecomp[,i],ricecomp$Enviro,sum)
  b<-cbind(b,collapsed)
  
}

b
colnames(b)<-colnames(ricecomp[1:38])

#Find taxa that represent at least 0.5% in the dataset

tots<-colSums(b)
grandtot<-sum(tots)
percs<-100*(tots/grandtot)

#Remove all taxa that do not represent 0.5% in the dataset

filtered = NULL
taxanames<-colnames(b)
morphonamesvec = NULL

taxa<-c(1:38)

for (i in taxa) {
  if (percs[i]>= 0.5) {
    filtered<-cbind(filtered,b[,i])
    morphonamesvec<-c(morphonamesvec,taxanames[i])
  }
}

colnames(filtered)<-morphonamesvec
sitenames<-rownames(filtered)
filtered<-as.data.frame(cbind(filtered,sitenames))

filtered_scale<-read.table("scalebysite.txt",header=T) #read in file created June 29 to scale to smallest counts

morphofiltered<-pivot_longer(filtered_scale, cols=1:(ncol(filtered)-1), 
             values_to = "value", values_drop_na = FALSE)


compositionplotSite <-ggplot(morphofiltered, aes(x=sitenames, y=as.numeric(value), fill=name)) +
  geom_bar(stat="identity") + 
  theme_bw()+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  xlab("Site")+
  theme(axis.title.x = element_text(size=12))+
  theme(axis.title.y=element_text(size=12))+
  scale_fill_brewer( type = "div" , palette = "Set3" ) +
  theme_bw() + ylab("Abundance\n") + 
  theme(legend.position="none")+
  theme(axis.text.x= element_text(angle = 90))

#OK, let's try it again for the months, that is, collapsing across sites
ricecomp<-cbind(river,riversites)

taxanames<-c(1:ncol(river))
b = NULL

for (i in taxanames) {
  collapsed<-tapply(ricecomp[,i],ricecomp$Month,sum)
  b<-cbind(b,collapsed)
  
}

b
colnames(b)<-colnames(ricecomp[1:ncol(river)])

#Find taxa that represent at least 0.5% in the dataset

tots<-colSums(b)
grandtot<-sum(tots)
percs<-100*(tots/grandtot)

#Remove all taxa that do not represent 0.5% in the dataset

filtered = NULL
taxanames<-colnames(b)
morphonamesvec = NULL

taxa<-c(1:ncol(river))

for (i in taxa) {
  if (percs[i]>= 0.5) {
    filtered<-cbind(filtered,b[,i])
    morphonamesvec<-c(morphonamesvec,taxanames[i])
  }
}

colnames(filtered)<-morphonamesvec
monthnames<-rownames(filtered)
filtered<-as.data.frame(cbind(filtered,monthnames))

filtered_month<-read.table("scalebymonth.txt",header=TRUE) #scaling to correct for sample size

morphofiltered<-pivot_longer(filtered_month, cols=1:(ncol(filtered)-1), 
                             values_to = "value", values_drop_na = FALSE)


compositionplotMonth <-ggplot(morphofiltered, aes(x=monthnames, y=as.numeric(value), fill=name)) +
  geom_bar(stat="identity") + 
  theme_bw()+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  xlab("Month")+
  theme(axis.title.x = element_text(size=12))+
  theme(axis.title.y=element_text(size=12))+
  scale_fill_brewer( type = "div" , palette = "Set3" ) +
  theme_bw() + ylab("Abundance\n") + 
  theme(legend.position="none")+
  theme(axis.text.x= element_text(angle = 90))


#Now let's try with and without rice
ricecomp<-cbind(river,riversites)

taxanames<-c(1:ncol(river))
b = NULL

for (i in taxanames) {
  collapsed<-tapply(ricecomp[,i],ricecomp$Rice,sum)
  b<-cbind(b,collapsed)
  
}

b
colnames(b)<-colnames(ricecomp[1:ncol(river)])

#Find taxa that represent at least 0.5% in the dataset

tots<-colSums(b)
grandtot<-sum(tots)
percs<-100*(tots/grandtot)

#Remove all taxa that do not represent 0.5% in the dataset

filtered = NULL
taxanames<-colnames(b)
morphonamesvec = NULL

taxa<-c(1:ncol(river))

for (i in taxa) {
  if (percs[i]>= 0.5) {
    filtered<-cbind(filtered,b[,i])
    morphonamesvec<-c(morphonamesvec,taxanames[i])
  }
}

colnames(filtered)<-morphonamesvec
ricenames<-rownames(filtered)
filtered<-as.data.frame(cbind(filtered,ricenames))

filtered_rice<-read.table("scalebyrice.txt",header=TRUE)

morphofiltered<-pivot_longer(filtered, cols=1:(ncol(filtered)-1), 
                             values_to = "value", values_drop_na = FALSE)


compositionplotRice <-ggplot(morphofiltered, aes(x=ricenames, y=as.numeric(value), fill=name)) +
  geom_bar(stat="identity") + 
  theme_bw()+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  xlab("Rice")+
  theme(axis.title.x = element_text(size=12))+
  theme(axis.title.y=element_text(size=12))+
  scale_fill_brewer( type = "div" , palette = "Set3" ) +
  theme_bw() + ylab("Abundance\n") + 
  theme(axis.text.x= element_text(angle = 90))



plot_grid(compositionplotSite,compositionplotMonth,compositionplotRice,labels=c("A","B","C"),ncol=3)


# September 6 2024
# Let's do the analyses without amphipods

#First, remove amphipods from dataset
river_no_amphi<-cbind(river[,1:16], river[,18:38])

#diversity statistics

riverdiv_no_amphi<-cbind(diversity(river_no_amphi,index="simpson"),riversites) #calculate simpsons index, bind to site information

colnames(riverdiv_no_amphi)<-c("Simpsons","Enviro","Month","Site","Replicate","Rice") #rename columns

summary(aov(riverdiv_no_amphi$Simpsons~riverdiv_no_amphi$Rice*riverdiv_no_amphi$Month)) #two-way ANOVA


#Permanova
adonis2(formula=river_no_amphi~riversites$Rice*riversites$Month+riversites$Enviro)

# Richness
rich_noamphi<-read.table("rich_no_amphi.txt",header=TRUE)
rich_noamphi<-cbind(riverrich,rich_noamphi)

summary(aov(rich_noamphi$R~rich_noamphi$Rice*rich_noamphi$Month+rich_noamphi$Enviro)) #two-way ANOVA
