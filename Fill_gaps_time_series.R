setwd("C:/Users/isma-/OneDrive/Escritorio/full databse/DATA")

df <- read_excel("Global_dataset.xlsx")

unique(df$country)
df1<-  df %>% filter(country=="Luxembourg")

df1<- df1[,c(1:4)]


########## Loop ############


time_series <- unique(df1$site_id)
species <- unique(df1$taxon)

for(i in 1:length(time_series)){
# Subset the data frame to include only the current time series
subset_df <- df1 %>% filter(site_id == time_series[i]) 

# Extract the unique species from the subseted dataframe
species <- unique(subset_df$taxon)
 
for(j in 1:length(species)){
  # Subset the data frame to include only the current species
  subset_species_df <- subset_df %>% filter(taxon == species[j])
  
  if(sum(!is.na(subset_species_df$abundance[subset_species_df$taxon == species[j]])) < 2) {
    # if true, skip the loop for this species
    next
  }
  
  
  subset_species_df <- subset_species_df %>%  group_by(site_id)  %>% ## Add missing years e.g. 2010
    complete(year = seq(min(year), max(year), period = 1))   
  
sampling_years <- unique(subset_df$year)
 
subset_species_df2 <- subset_species_df %>% filter(year %in% sampling_years)

subset_species_df2$taxon <- species[j]
subset_species_df2[is.na(subset_species_df2)] = 0

subset_species_df3 <- subset_species_df2 %>%  group_by(site_id)  %>% ## Add missing years e.g. 2010
  complete(year = seq(min(year), max(year), period = 1)) 

subset_species_df3$taxon <- species[j]

subset_species_df3$abundance<- as.numeric(subset_species_df3$abundance)
Paride <- complete(mice(subset_species_df3, m = 5, maxit = 50, method = "pmm", seed = 123))
write_xlsx(Paride,paste0(time_series[i], "_",species[j],".xlsx"))
cat("\n The loop for-->",time_series[i])
}

}







##################################################
##################################################
##################################################
##################################################
##################################################
##################################################
##################################################

time_series <- unique(df1$site_id)

for(i in 1:length(time_series)){

  subset_df <- df1 %>% filter(site_id == time_series[i]) 
  
  # Extract the unique species from the subseted dataframe
  species <- unique(subset_df$taxon)
  
  for(j in 1:length(species)){
    
    
  # Subset the data frame to include only the current species
  subset_species_df <- subset_df %>% filter(taxon == species[j])

  
  # check if the species has less than 2  values
  if(sum(!is.na(subset_species_df$abundance[subset_species_df$taxon == species[j]])) < 2) {
    # if true, skip the loop for this species
    next
  }
  
  
  subset_species_df <- subset_species_df %>%  group_by(site_id)  %>% ## Add missing years e.g. 2010
    complete(year = seq(min(year), max(year), period = 1))   
  
  sampling_years <- unique(subset_df$year)
  
  subset_species_df2 <- subset_species_df %>% filter(year %in% sampling_years)
  
  subset_species_df2$taxon <- species[j]
  subset_species_df2[is.na(subset_species_df2)] = 0
  
  subset_species_df3 <- subset_species_df2 %>%  group_by(site_id)  %>% ## Add missing years e.g. 2010
    complete(year = seq(min(year), max(year), period = 1)) 
  
  subset_species_df3$taxon <- species[j]
  
  subset_species_df3$abundance<- as.numeric(subset_species_df3$abundance)
  Paride <- complete(mice(subset_species_df3, m = 5, maxit = 50, method = "pmm", seed = 123))
  write_xlsx(Paride,paste0(time_series[i], species[j],".xlsx"))
  cat("\n The loop for-->",time_series[i])
  }

}





########################################################################
########################           Mathieu      ########################
########################################################################


setwd("C:/Users/isma-/Downloads")

HG <- read.csv2("Hungary.csv")

HG <- HG %>% group_by(site_id) %>% mutate(End = max(year),
                                          star=min(year))


plot(HG$star,c(1:length(HG$star)), xlim=c(1968, 2020), type="n", 
     las=1, axes=F, xlab="Year - Hungary", ylab= "Time series")
segments(x0=HG$star, x1=HG$End, y0=c(1:length(HG$star)),
         y1=c(1:length(HG$star)), col="#FB9A99",lwd=1.7)

abline(v=c(1970,1980,1990,2000,2010,2020), col=c("black"), lty=c(2), lwd=c(1))
abline(v=c(2007), col=c("green"), lwd=3)
abline(v=c(2017), col=c("green"), lwd=3)
axis(1)

### Clean Species

HG<-HG[!grepl("/", HG$taxon),]
HG<-HG[!grepl("sp.", HG$taxon),]
HG<-HG[!grepl("spp.", HG$taxon),]
HG<-HG[!grepl("gen. sp.", HG$taxon),]
HG<-HG[!grepl("gen. sp. lv.", HG$taxon),]
HG<-HG[!grepl("gen. sp. ad.", HG$taxon),]



# clean time series between 2007 & 2017

HG_clean=HG%>% filter(!star>"2007")%>%filter(!End<"2017")

df1<- RH
df1<- df1[,c(1:4)]

time_series <- unique(df1$site_id)

for(i in 1:length(time_series)){
  
  subset_df <- df1 %>% filter(site_id == time_series[i]) 
  
  # Extract the unique species from the subseted dataframe
  species <- unique(subset_df$taxon)
  
  for(j in 1:length(species)){
    
    
    # Subset the data frame to include only the current species
    subset_species_df <- subset_df %>% filter(taxon == species[j])
    
    
    # check if the species has less than 2  values
    if(sum(!is.na(subset_species_df$abundance[subset_species_df$taxon == species[j]])) < 2) {
      # if true, skip the loop for this species
      next
    }
    
    
    subset_species_df <- subset_species_df %>%  group_by(site_id)  %>% ## Add missing years e.g. 2010
      complete(year = seq(min(year), max(year), period = 1))   
    
    sampling_years <- unique(subset_df$year)
    
    subset_species_df2 <- subset_species_df %>% filter(year %in% sampling_years)
    
    subset_species_df2$taxon <- species[j]
    subset_species_df2$abundance<- as.numeric(subset_species_df2$abundance)
    subset_species_df2[is.na(subset_species_df2)] = 0
    
    subset_species_df3 <- subset_species_df2 %>%  group_by(site_id)  %>% ## Add missing years e.g. 2010
      complete(year = seq(min(year), max(year), period = 1)) 
    
    subset_species_df3$taxon <- species[j]
    
    subset_species_df3$abundance<- as.numeric(subset_species_df3$abundance)
    Paride <- complete(mice(subset_species_df3, m = 5, maxit = 50, method = "pmm", seed = 123))
    write_xlsx(Paride,paste0(time_series[i],"_", species[j],".xlsx"))
    cat("The loop for-->",time_series[i])
  }

}




# specify the path to the folder containing the xlsx files
folder_path <- "C:/Users/isma-/OneDrive/Escritorio/Nueva carpeta"

# create a list of all the xlsx files in the folder
xlsx_files <- list.files(path = folder_path, pattern = "*.xlsx", full.names = TRUE)

# initialize an empty data frame to store the combined data from all xlsx files
combined_data <- data.frame()

# loop through the list of xlsx files
for (file in xlsx_files) {
  
  # read the current xlsx file
  current_data <- read_excel(file)
  
  # append the current data to the combined data
  combined_data <- rbind(combined_data, current_data)
  
}

# view the combined data
hungary<- print(combined_data)

unique(hungary$site_id)
unique(clean_RH$site_id)

write_xlsx(hungary, "RN_full.xlsx")








UK <- read.csv2("UK.csv")
UK <- UK %>% group_by(site_id) %>% mutate(End = max(year),star=min(year))

# view all data

plot(UK$star,c(1:length(UK$star)), xlim=c(1968, 2020), type="n", 
     las=1, axes=F, xlab="Year - UK", ylab= "Time series")
segments(x0=UK$star, x1=UK$End, y0=c(1:length(UK$star)),
         y1=c(1:length(UK$star)), col="#FB9A99",lwd=1.7)
abline(v=c(1970,1980,1990,2000,2010,2020), col=c("black"), lty=c(2), lwd=c(1))
abline(v=c(2007), col=c("green"), lwd=3)
abline(v=c(2018), col=c("green"), lwd=3)
axis(1)


UK<-UK[!grepl("/", UK$taxon),]
UK<-UK[!grepl("sp.", UK$taxon),]
UK<-UK[!grepl("spp.", UK$taxon),]
UK<-UK[!grepl("gen. sp.", UK$taxon),]
UK<-UK[!grepl("gen. sp. lv.", UK$taxon),]
UK<-UK[!grepl("gen. sp. ad.", UK$taxon),]
# clean time series between 2007 & 2019 and fill the gaps at the extremities

UK_clean=UK%>% filter(!End<"2018")%>% filter(!star> "2007")



DN <- read.csv2("Denmark.csv")
DN <- DN %>% group_by(site_id) %>% mutate(End = max(year),
                                          star=min(year))


plot(DN$star,c(1:length(DN$star)), xlim=c(1968, 2020), type="n", 
     las=1, axes=F, xlab="Year - UK", ylab= "Time series")
segments(x0=DN$star, x1=DN$End, y0=c(1:length(DN$star)),
         y1=c(1:length(UK$star)), col="#FB9A99",lwd=1.7)
abline(v=c(1970,1980,1990,2000,2010,2020), col=c("black"), lty=c(2), lwd=c(1))
abline(v=c(1999), col=c("green"), lwd=3)
abline(v=c(2018), col=c("green"), lwd=3)
axis(1)

DN<-DN[!grepl("/", DN$taxon),]
DN<-DN[!grepl("sp.", DN$taxon),]
DN<-DN[!grepl("spp.", DN$taxon),]
DN<-DN[!grepl("gen. sp.", DN$taxon),]
DN<-DN[!grepl("gen. sp. lv.", DN$taxon),]
DN<-DN[!grepl("gen. sp. ad.", DN$taxon),]

# clean time series between 1999 & 2018

DN_clean=DN%>% filter(!star>"1998")%>% filter(!End<"2017")





RH <- read.csv2("Rhine.csv")

RH <- RH %>% group_by(site_id) %>% mutate(End = max(year),
                                          star=min(year))

plot(RH$star,c(1:length(RH$star)), xlim=c(1968, 2020), type="n", 
     las=1, axes=F, xlab="Year - Hungary", ylab= "Time series")
segments(x0=RH$star, x1=RH$End, y0=c(1:length(RH$star)),
         y1=c(1:length(RH$star)), col="#FB9A99",lwd=1.7)
abline(v=c(1970,1980,1990,2000,2010,2020), col=c("black"), lty=c(2), lwd=c(1))
abline(v=c(1982), col=c("green"), lwd=3)
abline(v=c(2003), col=c("green"), lwd=3)
axis(1)


RH<-RH[!grepl("/", RH$taxon),]
RH<-RH[!grepl("sp.", RH$taxon),]
RH<-RH[!grepl("spp.", RH$taxon),]
RH<-RH[!grepl("gen. sp.", RH$taxon),]
RH<-RH[!grepl("gen. sp. lv.", RH$taxon),]
RH<-RH[!grepl("gen. sp. ad.", RH$taxon),]


## Clean data
clean_RH = RH %>% filter(!star>"1982")%>% filter(!End<"2003")
