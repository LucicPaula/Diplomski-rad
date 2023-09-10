#install.packages("jsonlite", repos="https://cran.rstudio.com/")
library("jsonlite")

#install.packages("tidyverse")
library("scales")
library(tidyverse)
library(scatterplot3d)

json_file <- 'https://datahub.io/core/s-and-p-500-companies-financials/datapackage.json'
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

# get list of all resources:
print(json_data$resources$name)

# print all tabular data(if exists any)
for(i in 1:length(json_data$resources$datahub$type)){
  if(json_data$resources$datahub$type[i]=='derived/csv'){
    path_to_file = json_data$resources$path[i]
    data <- read.csv(url(path_to_file))
    print(data)
  }
}
path_to_file=json_data$resources$path[3]
data <- read.csv(url(path_to_file))
View(data)

data=na.omit(data)
pca=prcomp(data[, 4:13], scale = T, center=T)#kad je scale true onda je corr matrica
summary(pca)

pov <- pca$sdev^2/sum(pca$sdev^2)
pov=label_percent()(pov)

eigenvectors=pca$rotation
eigenvalues=pca$sdev^2
sum(eigenvalues)
#scores
PC1=pca$x[, 1] 
PC2=pca$x[, 2]

data=cbind(data, pca$x)
sector=data$Sector

#svi sektori
ggplot(data=data, aes(x=PC1, y=PC2, color=sector))+geom_point()+
coord_fixed(xlim=c(-10, 5), ylim=c(-5, 5))+
labs(x=paste0("PC1: ", pov[1]),
     y=paste0("PC2: ", pov[2]))

#istaknuti sektori
data %>%
ggplot(aes(x=PC1, y=PC2))+
geom_point(color="grey")+
geom_point(data=data %>% filter(Sector=="Utilities"), aes(PC1, PC2, color=Sector))+
geom_point(data=data %>% filter(Sector=="Health Care"), aes(PC1, PC2,  color=Sector))+
coord_fixed(xlim=c(-5, 2.7), ylim=c(-5, 5))+
labs(x=paste0("PC1: ", pov[1]),
     y=paste0("PC2: ", pov[2]))




