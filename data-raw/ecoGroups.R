# Generate Ecotype Groupings 

# Create a shell to fill with data
ecoGroup <- list(); length(ecoGroup) <- 12; 
dim(ecoGroup) <- c(4,3)                             # Three rows (different classifiers), and three columns (type of data we could extract)
rownames(ecoGroup)=c('identity','domSpecies','maxGranularity','domGroup') # Rows are different grouping scenarios
colnames(ecoGroup)=c('transform','labels','colours')                      # Cols are different things we could look up

# Fill with data
ecoGroup['identity',] <- list( c(1:27), 
                               paste0('BS',1:27), 
                               1:27 )
ecoGroup['domSpecies',] <- list( ,
                                 c('1'="Barren",'2'="Pine",'3'="Black Sp",'4'="White Sp",'5'="Birch",'6'="Aspen",'7'="Swamp",'8'="Bog",'9'="Fen",'10'="Shore"),
                                 c('1'="gray",'2'="tan1",'3'="green4",'4'="seagreen",'5'="green3",'6'="green2",'7'="steelblue",'8'="steelblue2",'9'="steelblue4",'10'="azure") )
ecoGroup['maxGranularity',] <- list( c(1,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,15,16,17,17,18,18,19,20,20,21,21),
                                     paste0('BS',c(1,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,15,16,17,17,18,18,19,20,20,21,21)),
                                     1:21)
ecoGroup['domGroup',] <- list( c(1,1,2,2,2,2,2,2,2,2,2,2,3,3,3,4,4,4,4,4,4,4,4,4,4, 4, 4),
                               c("Barren","Conifer","Decid","Wetland"),
                               c('1'="gray",'2'="green4",'3'="green2",'4'="steelblue") )
