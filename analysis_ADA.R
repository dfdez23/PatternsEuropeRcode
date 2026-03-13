######################

rm(list=ls())
graphics.off()

pacman::p_load(readr,readxl,dplyr,tidyverse,archetypes,tidyr, hopkins, 
               compareGroups,vcd,pheatmap,ggplot2, kableExtra, ggtern, pdftools)

setwd("/Users/dfmartinez/Library/CloudStorage/Dropbox/MyPapers/00.Submitted/ADA_election_PaolaMarica_Paper1")
load(file="./Rdata/preliminary.RData")

#dir <- "/Users/dfmartinez/Library/CloudStorage/Dropbox/00.Submitted/ADA_election_PaolaMarica_Paper1"
#setwd(dir)

#We need this for running ADA. Irene (developer of admethodts package) told me 
#that I don't like how the functions in adamethods are structured, 
#because they're too overloaded.
source('LstepArchetypesMod.R')
source('LFunctions_to_calculate_real_archetypes_with_swap2.R')
source('LstepArchetypoids.R')

Dataset <- read_csv("./data/CHES_2024_final_v2.csv", show_col_types = FALSE)
Dataset <- as.data.frame(Dataset)
class(Dataset)
dim(Dataset)
summary(Dataset)
str(Dataset)
#transform character in factor
Dataset <- Dataset %>%
  mutate(across(where(is.character), as.factor))
str(Dataset)

#This will be the key primary (party_id) and the descriptive of each row
legend <- Dataset[,c("country","party_id","party","family","electionyear")]
head(legend)

#Are there duplicatesin party_id
any(duplicated(Dataset$party_id)) # Returns TRUE if duplicates exist
sum(duplicated(Dataset$party_id)) # must be 0

#let's change the name of the rows to have the primaryk ey of the dataset
rownames(Dataset) <- Dataset[,"party_id"]
head(Dataset)

# select policy dimensions. This would be the same as Paola & Marica ches2
ches2bis <- Dataset %>% select("immigrate_policy","multiculturalism","redistribution","climate_change","environment","spendvtax",
                             "deregulation","civlib_laworder","womens_rights","lgbtq_rights","samesex_marriage","religious_principles",
                             "ethnic_minorities","nationalism","urban_rural","protectionism","regions","executive_power",
                             "judicial_independence") %>% na.omit()
class(ches2bis)
dim(ches2bis)
summary(ches2bis)
str(ches2bis)


#Let's start de ADA analysis
X <- as.data.frame(scale(ches2bis))
class(X)
str(X)
summary(X)

#cluster tendency 
hopkins(X) #not useful for ADA, so better do some PCA to check the spread


# PCA Spread Check before ADA

# Perform PCA
pca_res <- prcomp(X, scale. = FALSE)  # X is already scaled

# Summary of variance explained
summary(pca_res)

# Extract the scores for the first two principal components
pca_scores <- as.data.frame(pca_res$x[,1:2])
pca_scores$party_id <- rownames(X)

# Plot parties in PC1 vs PC2
pdf(file="./Results/PCA_initial.pdf",width=14,height=10,paper="special")
ggplot(pca_scores, aes(x = PC1, y = PC2, label = party_id)) +
  geom_point(color = "blue", size = 3) +  # bigger points
  geom_text(aes(label = party_id), 
            hjust = 0.5, vjust = -0.7,    # slightly above points
            size = 5,                     # bigger text labels
            color = "black") +
  labs(title = "PCA of CHES 2024 parties",
       x = "PC1 (62.7%)",
       y = "PC2 (14.2%)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 30),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  )
dev.off()

# Quantitative measure: convex hull area (spread)
library(geometry)
hull_idx <- convhulln(pca_scores[,1:2])
hull_points <- pca_scores[hull_idx, 1:2]
hull_area <- polyarea(hull_points$PC1, hull_points$PC2)
cat("Convex hull area of parties in PC1-PC2 space:", hull_area, "\n")


# ADA
norep=20
set.seed(2025)
max.k=8
lass10 <- stepLArchetypoids3(X,k=1:max.k,norep=norep) 
class(lass10)



save.image(file="./Rdata/preliminary.RData")
#load(file="./Rdata/preliminary.RData")

#Screeplot

dfscreeplot <- data.frame(
  k = 1:max.k,
  RSS = sapply(1:max.k, function(i) lass10[[i]][[2]]) 
)

pdf(file="./Results/elbow.pdf",width=12,height=8,paper="special")

# screeplot(lass10) 
# abline(v = 3, lty = 2, col = "red", lwd = 2)
ggplot(dfscreeplot, aes(x = k, y = RSS)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "blue", size = 2) +
  geom_vline(xintercept = 3, linetype = "dashed", color = "red", linewidth = 1) +
  labs(title = "Scree plot of ADA for CHES 2024 data",
       x = "Number of archetypoids (k)",
       y = "Residual Sum of Squares (RSS)") +
  scale_x_continuous(breaks = 1:8) +   # <-- set ticks at 1,2,...,8
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 22),   # Title size
    axis.title.x = element_text(size = 20), # X-axis title
    axis.title.y = element_text(size = 20), # Y-axis title
    axis.text.x = element_text(size = 16),                # X-axis numbers
    axis.text.y = element_text(size = 16)                 # Y-axis numbers
  )

dev.off()


#For paper 2
pdf(file="./Results/elbow_paper2.pdf",width=12,height=8,paper="special")

# screeplot(lass10) 
# abline(v = 3, lty = 2, col = "red", lwd = 2)
ggplot(dfscreeplot[1:6,], aes(x = k, y = RSS)) +
  geom_line(color = "green", linewidth = 3) +
  geom_point(color = "green", size = 2) +
  geom_vline(xintercept = 3, linetype = "dashed", color = "orange", linewidth = 2) +
  labs(title = "CHES 2024 data: ADA scree plot",
       x = "Archetypoids number (k)",
       y = "Residual Sum of Squares (RSS)") +
  scale_x_continuous(breaks = 1:6) +   # <-- set ticks at 1,2,...,8
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 26, , hjust = 0.5),   # Title size
    axis.title.x = element_text(size = 20), # X-axis title
    axis.title.y = element_text(size = 20), # Y-axis title
    axis.text.x = element_text(size = 16),                # X-axis numbers
    axis.text.y = element_text(size = 16)                 # Y-axis numbers
  )

dev.off()



#We decided to take 3 clusters according to their similarities to the archetipoids. 
#Next table describes each of the clusters according to their characteristics.

numarq <- 3
##-- Number archetypoids -------------------------------------------------------
#n_arquetipoyds contains the individual numbers which are archetypoids
n_arquetipoyds <- lass10[[numarq]][[1]]
n_arquetipoyds
(parties_ADA <- as.integer(rownames(ches2bis[n_arquetipoyds,]))) # arquetypoid parties

# Find indices where party_id matches any value in parties_ADA
matching_indicesADAs <- which(Dataset[,"party_id"] %in% parties_ADA)
#Who are the parties
(whoADAare <- legend[matching_indicesADAs,])
kable(whoADAare, format = "html", table.attr = "class='table table-striped'") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  save_kable("./Results/WhoADAare.html")

#The policy dimensions information of these 3 parties (ADAs)
ches2bis[n_arquetipoyds,] # arquetypoid parties - values of variables
#All the information in the orginal data set
Dataset[matching_indicesADAs,]


### Tables to the Appendix EDA (with the 19 variablws.)

# Subset of policy-related variables
varsel_subset <- c(
  "party", 
  "immigrate_policy", "multiculturalism", "redistribution",       
  "climate_change", "environment", "spendvtax",            
  "deregulation", "civlib_laworder", "womens_rights",        
  "lgbtq_rights", "samesex_marriage", "religious_principles", 
  "ethnic_minorities", "nationalism", "urban_rural",          
  "protectionism", "regions", "executive_power", "judicial_independence"
)

# Create HTML table for archetypoid parties
kable(Dataset[matching_indicesADAs, varsel_subset], 
      format = "html", 
      table.attr = "class='table table-striped'") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  save_kable("./Results/WhoADAare_varsubset.html")



#### Old Tables

# #General Questions on European Integration
# varsel_general <- c("party", "eu_position", "eu_salience", "eu_dissent", "eu_blur") 
# kable(Dataset[matching_indicesADAs,varsel_general], format = "html", table.attr = "class='table table-striped'") %>%
#   kable_styling(bootstrap_options = c("striped", "hover")) %>%
#   save_kable("./Results/WhoADAare_vargeneral.html")
# 
# 
# #Ideological Questions 
# varsel_ideological <- c("party", 
#                         "lrecon", "lrecon_blur", "lrecon_dissent", "lrecon_salience",
#                         "galtan", "galtan_blur", "galtan_dissent", "galtan_salience",
#                         "lrgen")
# 
# # Create HTML table for archetypoid parties
# kable(Dataset[matching_indicesADAs, varsel_ideological], 
#       format = "html", 
#       table.attr = "class='table table-striped'") %>%
#   kable_styling(bootstrap_options = c("striped", "hover")) %>%
#   save_kable("./Results/WhoADAare_varideological.html")
# 
# # Policy Dimensions
# varsel_policy <- c("party", 
#                    "immigrate_policy", "immigrate_salience", "immigrate_dissent",
#                    "multiculturalism", "multicult_salience",
#                    "redistribution", "redist_salience",
#                    "climate_change","climate_change_salience",
#                    "environment", "environment_salience", "spendvtax", 
#                    "deregulation",
#                    "civlib_laworder","womens_rights", "lgbtq_rights",
#                    "samesex_marriage",
#                    "religious_principles", "ethnic_minorities", "nationalism",
#                    "urban_rural", "protectionism", "regions",
#                    "executive_power", "judicial_independence"
#                    )
# 
# # Create HTML table for archetypoid parties
# kable(Dataset[matching_indicesADAs, varsel_policy], 
#       format = "html", 
#       table.attr = "class='table table-striped'") %>%
#   kable_styling(bootstrap_options = c("striped", "hover")) %>%
#   save_kable("./Results/WhoADAare_varpolicy.html")
# 
# 
# # Party Characteristics Dimensions
# varsel_characteristics <- c(
#   "party", 
#   "corrupt_salience",        # Reducing political corruption salience
#   "anti_islam",              # Anti-Islam rhetoric salience
#   "people_v_elite",          # Populism dimension: people vs elected
#   "anti_elite_salience"      # Anti-elite rhetoric salience
# )
# 
# # Create HTML table for archetypoid parties
# kable(Dataset[matching_indicesADAs, varsel_characteristics], 
#       format = "html", 
#       table.attr = "class='table table-striped'") %>%
#   kable_styling(bootstrap_options = c("striped", "hover")) %>%
#   save_kable("./Results/WhoADAare_varcharacteristics.html")
# 
# 
# # EU Policy Questions
# varsel_eu <- c("party", 
#                "eu_foreign", 
#                "eu_intmark", 
#                "eu_russia"
# )
# 
# # Create HTML table for archetypoid parties
# kable(Dataset[matching_indicesADAs, varsel_eu], 
#       format = "html", 
#       table.attr = "class='table table-striped'") %>%
#   kable_styling(bootstrap_options = c("striped", "hover")) %>%
#   save_kable("./Results/WhoADA_are_varEU.html")
# 

##-- Obtain the alpha matrix (n x numarq)------------------------------------------------------

#huge = 200 means that the algorithm strongly enforces the constraint 
#that each data point is approximated as a convex combination of archetypoids.
#Use a smaller value (e.g., huge <- 50) only if you're experimenting with convergence issues or numerical instability.
huge<- 200 #Penalty value that enforces the convex combination constraints in Archetypoid Analysis.

n <- ncol(t(X))
x_gvv <- rbind(t(X), rep(huge, n))
zs<-x_gvv [,n_arquetipoyds]  #Please, replace with the concrete archetypoids

ae <- matrix(0, nrow = numarq, ncol = n)

for (j in 1 : n){
  ae[, j] = coef(nnls(zs, x_gvv[,j]))
}
ae <- t(ae)
dim(ae)
#In ae you have alpha matrix for X data set
ae

##-- Assign to clusters --------------------------------------------------------

#Returns the index of the maximum value of each row of the matrix alpha
ches2bis$cluster <- apply(ae,1,which.max)
#Number of parties by archetypoyd
table(ches2bis$cluster)
# a bit of stats for each archetypoyd

#Obtain the stats broken down by group
res  <- compareGroups(cluster ~ ., data=ches2bis)
export2html(createTable(res), file = "./Results/ADA3_summary.html")

###-- Interpretation/Visualization----------------------------

#We have here the parties in each cluster
sel_1 <- which(ches2bis$cluster==1)
sel_2 <- which(ches2bis$cluster==2)
sel_3 <- which(ches2bis$cluster==3)

#Check that the 3 archetypoids (n_arquetipoyds) we found before are in each group
for (i in 1:length(n_arquetipoyds)) {
  found <- n_arquetipoyds[i] %in% which(ches2bis$cluster == i)  # Must be TRUE
  if (found) {
    print(paste0("The arquetipoyd ", i," (",n_arquetipoyds[i],") is in cluster ", i))
  } else {
    print(paste0("The arquetipoyd ", i," (",n_arquetipoyds[i],") is NOT in cluster ", i))
  }
}

### Composition of each archetypoid

####Cluster 1: Representative archetypoid: Razem

#Add the information of the party for each element of the cluster
alpha_sel1 <- ae[sel_1,]
rows_sel1 <- as.integer(rownames(ches2bis[sel_1,])) # arquetypoid parties in this cluster
nrow(alpha_sel1)==length(rows_sel1) # Checking. Must be TRUE
# Find indices where party_id matches any value in arquetypoid parties in this cluster
matching_indices_sel1 <- which(Dataset[,"party_id"] %in% rows_sel1)
labels_sel1 <- legend[matching_indices_sel1,"party_id"]
length(labels_sel1)==length(rows_sel1) # Checking. Must be TRU
cex_lab <- 0.5
pdf(file="./Results/stars_1.pdf",width=10,height=10,paper="special")
stars(alpha_sel1,draw.segments=T,
      labels=labels_sel1, cex=cex_lab, scale=FALSE,
      main = "Star plot of Cluster 1 (Left-wing archetypoid: Partia Razem)) in black")
dev.off()


#Let's check one party in particular for th review: the 4503 and 108

round(alpha_sel1[which(labels_sel1==4503),],3)
legend[which(Dataset[,"party_id"]==4503),]

round(alpha_sel1[which(labels_sel1==108),],3)
legend[which(Dataset[,"party_id"]==108),]


#Cluster 2:  Representative archetypoid: Republika

#Add the information of the party for each element of the cluster
alpha_sel2 <- ae[sel_2,]
rows_sel2 <- as.integer(rownames(ches2bis[sel_2,])) # arquetypoid parties in this cluster
nrow(alpha_sel2)==length(rows_sel2) # Checking. Must be TRUE
# Find indices where party_id matches any value in arquetypoid parties in this cluster
matching_indices_sel2 <- which(Dataset[,"party_id"] %in% rows_sel2)
labels_sel2 <- legend[matching_indices_sel2,"party_id"]
length(labels_sel2)==length(rows_sel2) # Checking. Must be TRU
cex_lab <- 0.5
pdf(file="./Results/stars_2.pdf",width=10,height=10,paper="special")
stars(alpha_sel2,draw.segments=T,
      labels=labels_sel2, cex=cex_lab, scale=FALSE)
dev.off()

#Cluster 3: Representative archetypoid: FDP/PLR

#Add the information of the party for each element of the cluster
alpha_sel3 <- ae[sel_3,]
rows_sel3 <- as.integer(rownames(ches2bis[sel_3,])) # arquetypoid parties in this cluster
nrow(alpha_sel3)==length(rows_sel3) # Checking. Must be TRUE
# Find indices where party_id matches any value in arquetypoid parties in this cluster
matching_indices_sel3 <- which(Dataset[,"party_id"] %in% rows_sel3)
labels_sel3 <- legend[matching_indices_sel3,"party_id"]
length(labels_sel3)==length(rows_sel3) # Checking. Must be TRU
cex_lab <- 0.5
pdf(file="./Results/stars_3.pdf",width=10,height=10,paper="special")
stars(alpha_sel3,draw.segments=T,
      labels=labels_sel3, cex=cex_lab, scale=FALSE)
dev.off()

# Create one PDF with 3 star plots (one per cluster)
pdf(file = "./Results/stars_all.pdf", width = 12, height = 5, paper = "special")

# Arrange plots in 1 row, 3 columns
par(mfrow = c(1, 3), mar = c(3, 0.5, 3, 0.5), xpd = NA) # xpd=NA allows text in margins

# tweakable parameters
cex_lab <- 0.9    # text size for labels
star_len <- 0.8   # scale factor for star radius (smaller -> labels closer)


# Cluster 1: Razem
label_offset <- 0.7  # distance of labels from centres (fraction of char width
loc1 <- stars(alpha_sel1, draw.segments = TRUE,
              labels = NULL,        # <- do NOT draw labels here
              cex = cex_lab, scale = FALSE,
              len = star_len,       # shrink stars a bit
              main = "Cluster 1: Left-wing archetypoid",
              col.segments = c("red","blue","green"))

# add labels manually (pos = 1 -> below; offset small -> closer)
text(loc1, labels = labels_sel1, cex = cex_lab, pos = 1, offset = label_offset, xpd = TRUE)


# Cluster 2: Republika
label_offset <- 1.1  # distance of labels from centres (fraction of char width
loc2 <- stars(alpha_sel2, draw.segments = TRUE,
              labels = NULL,
              cex = cex_lab, scale = FALSE,
              len = star_len,
              main = "Cluster 2: Right-wing archetypoid",
              col.segments = c("red","blue","green"))

text(loc2, labels = labels_sel2, cex = cex_lab, pos = 1, offset = label_offset, xpd = TRUE)


# Cluster 3: FDP/PLR
label_offset <- 1.3  # distance of labels from centres (fraction of char width
loc3 <- stars(alpha_sel3, draw.segments = TRUE,
              labels = NULL,
              cex = cex_lab, scale = FALSE,
              len = star_len,
              main = "Cluster 3: Center archetypoid",
              col.segments = c("red","blue","green"))

text(loc3, labels = labels_sel3, cex = cex_lab, pos = 1, offset = label_offset, xpd = TRUE)

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)  # default margins

dev.off()



### Ternary plot (this is possible if number of archetypoids is 3)


###-------------- Ternary pot by country

# Which countries we have?


Dataset2 <- Dataset %>% select("immigrate_policy","multiculturalism","redistribution","climate_change","environment","spendvtax",
                               "deregulation","civlib_laworder","womens_rights","lgbtq_rights","samesex_marriage","religious_principles",
                               "ethnic_minorities","nationalism","urban_rural","protectionism","regions","executive_power",
                               "judicial_independence","party_id","seatperc","country") %>% na.omit()

countries <- as.vector(unique(Dataset2$country))

# Lookup vector for full names
country_names <- c(
  be = "Belgium", dk = "Denmark", ge = "Germany", esp = "Spain", fr = "France",
  irl = "Ireland", it = "Italy", nl = "Netherlands", uk = "United Kingdom",
  por = "Portugal", aus = "Austria", fin = "Finland", sv = "Sweden", bul = "Bulgaria",
  cz = "Czech Republic", est = "Estonia", hun = "Hungary", lat = "Latvia",
  lith = "Lithuania", pol = "Poland", rom = "Romania", slo = "Slovakia",
  sle = "Slovenia", cro = "Croatia", tur = "Turkey", nor = "Norway",
  swi = "Switzerland", cyp = "Cyprus", ice = "Iceland"
)
# Map codes to full names
full_names <- country_names[countries]

pdf(file="./Results/Ternary_countries.pdf",width=10,height=10,paper="special")



cex_lab <- 1

for (i in 1:length(countries)) {
  cat("Plotting:", countries[i], "\n")
  
  subset_country <- Dataset2 %>% 
    filter(country %in% countries[i]) %>% 
    select(country, party_id)
  
  sel_country <- which(Dataset2$party_id %in% subset_country$party_id)
  alpha_matrix_sel_country <- as.matrix(ae[sel_country, ])
  
  ternaryplot(alpha_matrix_sel_country,
              grid = TRUE,
              dimnames = whoADAare$family,
              col = i,
              cex = 2,                      # smaller text to fit in grid
              main = full_names[i])
}


# Close the PDF device
dev.off()

#We repeat the ternary plots representing parties with points of different size, 
#proportional to their percentage seats in Parliaments.
#The variable is seatperc

library(ggtern)


#Do first the individuals Spain (red), Hungary (blue) and Sweden (green)

pdf(file = "./Results/Ternary_Spain_seatperc.pdf", width = 8, height = 8)

i <- 4 # Spain
subset_country <- Dataset2 %>% filter(country %in% countries[i]) %>% 
    select(country, party_id, seatperc)  
  sel_country   <- which(Dataset2$party_id %in% subset_country$party_id)
  alpha_matrix_sel_country <- as.data.frame(ae[sel_country, ])
  colnames(alpha_matrix_sel_country) <- c("ADA1", "ADA2", "ADA3")
  
  # Combine with metadata
  df_country <- bind_cols(subset_country, alpha_matrix_sel_country)
  df_country <- df_country %>%
    left_join(legend %>% select(party_id, party), by = "party_id")
  
  #We add a tiny offset so points are never exactly on the edge (Aesthetic criterion):
  epsilon <- 0.04  # small shift
  df_country <- df_country %>%
    mutate(
      ADA1_nudge = pmax(ADA1, epsilon),
      ADA2_nudge = pmax(ADA2, epsilon),
      ADA3_nudge = pmax(ADA3, epsilon)
    )
  
  p <-     ggtern(df_country, aes(x = ADA1_nudge, y = ADA3_nudge, z = ADA2_nudge)) +
    theme_bw() +
    theme(legend.key = element_rect(fill = "white"),
          tern.axis.line = element_line(linewidth = 0.05),
          plot.title = element_text(hjust = 0.5)) +
    geom_point(aes(size = seatperc), alpha = 0.8, color = "black", fill = "red", shape = 21) +
    #geom_text(aes(label = party), size = 3, color = "black", hjust = 1.2) +
    geom_text(aes(label = party), size = 3, color = "black", vjust = -1) +
    scale_size_area(breaks = c(5, 10, 20, 40, 60, 80,100), limits = c(0, 100), max_size = 12, name = "% Seats") +
    #labs(x = as.vector(whoADAare$family[1]), 
    #     y = as.vector(whoADAare$family[3]), 
    #      z = as.vector(whoADAare$family[2])) +
    labs(x = "Left", 
         y = "Center", 
         z = "Right") +
    ggtitle(paste(full_names[i])) 
  
  
  print(p)
  
dev.off() 



pdf(file = "./Results/Ternary_Hungary_seatperc.pdf", width = 8, height = 8)

i <- 17 # Hungary
subset_country <- Dataset2 %>% filter(country %in% countries[i]) %>% 
  select(country, party_id, seatperc)  
sel_country   <- which(Dataset2$party_id %in% subset_country$party_id)
alpha_matrix_sel_country <- as.data.frame(ae[sel_country, ])
colnames(alpha_matrix_sel_country) <- c("ADA1", "ADA2", "ADA3")

# Combine with metadata
df_country <- bind_cols(subset_country, alpha_matrix_sel_country)
df_country <- df_country %>%
  left_join(legend %>% select(party_id, party), by = "party_id")

#We add a tiny offset so points are never exactly on the edge (Aesthetic criterion):
epsilon <- 0.04  # small shift
df_country <- df_country %>%
  mutate(
    ADA1_nudge = pmax(ADA1, epsilon),
    ADA2_nudge = pmax(ADA2, epsilon),
    ADA3_nudge = pmax(ADA3, epsilon)
  )

p <-     ggtern(df_country, aes(x = ADA1_nudge, y = ADA3_nudge, z = ADA2_nudge)) +
  theme_bw() +
  theme(legend.key = element_rect(fill = "white"),
        tern.axis.line = element_line(linewidth = 0.05),
        plot.title = element_text(hjust = 0.5)) +
  geom_point(aes(size = seatperc), alpha = 0.8, color = "black", fill = "blue", shape = 21) +
  #geom_text(aes(label = party), size = 3, color = "black", hjust = 1.2) +
  geom_text(aes(label = party), size = 3, color = "black", vjust = -1) +
  scale_size_area(breaks = c(5, 10, 20, 40, 60, 80,100), limits = c(0, 100), max_size = 12, name = "% Seats") +
  #labs(x = as.vector(whoADAare$family[1]), 
  #     y = as.vector(whoADAare$family[3]), 
  #      z = as.vector(whoADAare$family[2])) +
  labs(x = "Left", 
       y = "Center", 
       z = "Right") +
  ggtitle(paste(full_names[i])) 


print(p)

dev.off()




pdf(file = "./Results/Ternary_Sweden_seatperc.pdf", width = 8, height = 8)

i <- 13 # Sweden
subset_country <- Dataset2 %>% filter(country %in% countries[i]) %>% 
  select(country, party_id, seatperc)  
sel_country   <- which(Dataset2$party_id %in% subset_country$party_id)
alpha_matrix_sel_country <- as.data.frame(ae[sel_country, ])
colnames(alpha_matrix_sel_country) <- c("ADA1", "ADA2", "ADA3")

# Combine with metadata
df_country <- bind_cols(subset_country, alpha_matrix_sel_country)
df_country <- df_country %>%
  left_join(legend %>% select(party_id, party), by = "party_id")

#We add a tiny offset so points are never exactly on the edge (Aesthetic criterion):
epsilon <- 0.04  # small shift
df_country <- df_country %>%
  mutate(
    ADA1_nudge = pmax(ADA1, epsilon),
    ADA2_nudge = pmax(ADA2, epsilon),
    ADA3_nudge = pmax(ADA3, epsilon)
  )

p <-     ggtern(df_country, aes(x = ADA1_nudge, y = ADA3_nudge, z = ADA2_nudge)) +
  theme_bw() +
  theme(legend.key = element_rect(fill = "white"),
        tern.axis.line = element_line(linewidth = 0.05),
        plot.title = element_text(hjust = 0.5)) +
  geom_point(aes(size = seatperc), alpha = 0.8, color = "black", fill = "green", shape = 21) +
  #geom_text(aes(label = party), size = 3, color = "black", hjust = 1.2) +
  geom_text(aes(label = party), size = 3, color = "black", vjust = -1) +
  scale_size_area(breaks = c(5, 10, 20, 40, 60, 80,100), limits = c(0, 100), max_size = 12, name = "% Seats") +
  #labs(x = as.vector(whoADAare$family[1]), 
  #     y = as.vector(whoADAare$family[3]), 
  #      z = as.vector(whoADAare$family[2])) +
  labs(x = "Left", 
       y = "Center", 
       z = "Right") +
  ggtitle(paste(full_names[i])) 


print(p)

dev.off()



paper1 <- FALSE #If paper1 is TRUE means do for paper 1, if FALSE is for paper 2
#paper1=TRUE:  We do all countries now except those three: Spain, Hungary and Sweden
#paper1=FALSE: We do all countries

pdf(file = "./Results/Ternary_countries_seatperc.pdf", width = 8, height = 8)

for (i in 1:(length(countries))) {
  if (paper1){
      if (i %in% c(4, 13, 17)) {
       next  # skip this iteration
      }
  }  
  cat("Plotting:", countries[i], "\n")
  
  # Subset by country
  subset_country <- Dataset2 %>% filter(country %in% countries[i]) %>% 
                      select(country, party_id, seatperc)  
  sel_country   <- which(Dataset2$party_id %in% subset_country$party_id)
  alpha_matrix_sel_country <- as.data.frame(ae[sel_country, ])
  colnames(alpha_matrix_sel_country) <- c("ADA1", "ADA2", "ADA3")
  
  # Combine with metadata
  df_country <- bind_cols(subset_country, alpha_matrix_sel_country)
  df_country <- df_country %>%
    left_join(legend %>% select(party_id, party), by = "party_id")

  #We add a tiny offset so points are never exactly on the edge (Aesthetic criterion):
  epsilon <- 0.030  # small shift
  df_country <- df_country %>%
    mutate(
      ADA1_nudge = pmax(ADA1, epsilon),
      ADA2_nudge = pmax(ADA2, epsilon),
      ADA3_nudge = pmax(ADA3, epsilon)
    )
  
  p <-     ggtern(df_country, aes(x = ADA1_nudge, y = ADA3_nudge, z = ADA2_nudge)) +
    theme_bw() +
    theme(legend.key = element_rect(fill = "white"),
          tern.axis.line = element_line(linewidth = 0.05),
          plot.title = element_text(hjust = 0.5)) +
    geom_point(aes(size = seatperc), alpha = 0.8, color = "black", fill = "darkgoldenrod1", shape = 21) +
    #geom_text(aes(label = party), size = 3, color = "black", hjust = 1.2) +
    geom_text(aes(label = party), size = 3, color = "black", vjust = -1) +
    scale_size_area(breaks = c(5, 10, 20, 40, 60, 80,100), limits = c(0, 100), max_size = 12, name = "% Seats") +
    #labs(x = as.vector(whoADAare$family[1]), 
    #     y = as.vector(whoADAare$family[3]), 
  #      z = as.vector(whoADAare$family[2])) +
    labs(x = "Left", 
         y = "Center", 
         z = "Right") +
    ggtitle(paste(full_names[i])) 

  
  print(p)
}

dev.off()

#split a one PDF with 26 (29-3) sheets into individual PDFs (one per page) in R
library(pdftools)

# Path to your original PDF
pdf_file <- "./Results/Ternary_countries_seatperc.pdf"

# Get the number of pages
n_pages <- pdf_info(pdf_file)$pages

# Split each page into a separate PDF
for (i in 1:n_pages) {
  # Output filename (you can use country names if you like)
  out_file <- paste0("./Results/Ternary_countries_page_", i, ".pdf")
  
  # Extract page i and write as new PDF
  pdf_subset(pdf_file, pages = i, output = out_file)
}


save.image(file="./Rdata/preliminary.RData")
#load(file="./Rdata/preliminary.RData")


# #We can show the distribution of ADA alpha matrix values 
# #for number of archetypoids 3 with a ternary plot
# 
# #General ternary plot
# alpha_matrix <- as.matrix(ae)
# 
# 
# pdf(file="./Results/Ternary_all.pdf",width=10,height=10,paper="special")
# ternaryplot(alpha_matrix,grid=T,dimnames=c("ADA1","ADA2","ADA3"),
#             col=1,cex=.6,main='Ternary plot')
# dev.off()
# 
# #For example, let's do the ternary plot for Radical Right (RADRT)
# # & Radical Left (RADLEFT)
# 
# #We select the Radical Right (RADRT)
# 
# radical_right <- Dataset %>% filter(family %in% c("radrt", "con"))   %>% na.omit()
# radical_left  <- Dataset %>% filter(family %in% c("radleft", "soc")) %>% na.omit()
# 
# 
# sel_radrt   <- which(Dataset$party_id %in% radical_right$party_id)
# sel_radleft <- which(Dataset$party_id %in% radical_left$party_id)
# 
# 
# alpha_matrix_sel_radrt    <- as.matrix(ae[sel_radrt,])
# alpha_matrix_sel_radleft  <- as.matrix(ae[sel_radleft,])
# 
# pdf(file="./Results/Ternary_family.pdf",width=10,height=10,paper="special")
# 
# # Set layout to 1 row, 2 columns
# # Plot 1: Radical Right
# ternaryplot(alpha_matrix_sel_radrt,grid=T,dimnames=c("ADA1","ADA2","ADA3"),
#             col = "blue",cex=1.5,main='Ternary plot - Radical Right + Conservatives')
# 
# # Plot 2: Radical Left
# ternaryplot(alpha_matrix_sel_radleft,grid=T,dimnames=c("ADA1","ADA2","ADA3"),
#             col="red",cex=1.5,main='Ternary plot - Radical Left + Socialist')
# 
# # Close the PDF device
# dev.off()
# 
# save.image(file="./Rdata/preliminary.RData")
# #load(file="./Rdata/preliminary.RData")



###-------------- Line plot---



(legend[matching_indicesADAs,])
profiles <- Dataset[matching_indicesADAs,] %>% 
              select("immigrate_policy","multiculturalism","redistribution","climate_change","environment","spendvtax",
                      "deregulation","civlib_laworder","womens_rights","lgbtq_rights","samesex_marriage","religious_principles",
                      "ethnic_minorities","nationalism","urban_rural","protectionism","regions","executive_power",
                      "judicial_independence") %>% na.omit()  %>% t(.)
dim(profiles)
colnames(profiles) <- as.vector(legend[matching_indicesADAs,"family"])

#Select the 3 archetypoids 

prof <- data.frame(
  var = rownames(profiles),
  val = as.vector(profiles),
  # arch = rep(paste0("ADA_", 1:ncol(profiles)), each = nrow(profiles))
  arch = as.vector(c(rep(colnames(profiles)[1],  nrow(profiles)),
           rep(colnames(profiles)[2],  nrow(profiles)),
           rep(colnames(profiles)[3],  nrow(profiles))))
)

ches2_fam <- Dataset %>% 	select("family","immigrate_policy","multiculturalism","redistribution","climate_change","environment","spendvtax",
                               "deregulation","civlib_laworder","womens_rights","lgbtq_rights","samesex_marriage","religious_principles",
                               "ethnic_minorities","nationalism","urban_rural","protectionism","regions","executive_power",
                               "judicial_independence") %>% na.omit %>% 
  group_by(family) %>% 
  summarise_all(.funs = mean) %>%
  as.data.frame()

ches2_long <- ches2_fam %>%
  pivot_longer(-family, names_to = "var", values_to = "val")

prof$var <- factor(prof$var, levels = unique(prof$var))
ches2_long$var <- factor(ches2_long$var, levels = levels(prof$var))

pdf(file="./Results/lines3.pdf",width=15,height=10,paper="special")
ggplot(prof, aes(x = var, y = val, color = arch, group = arch)) +
  geom_line(size = 1) +
  labs(x = "Policy Variable", y = "Value", color = "Archetypal") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
dev.off()

first_var <- levels(prof$var)[1]
label_data <- ches2_long %>%
  filter(var == first_var)

#We remove the 3 ADAs ones: lib, radleft, radrt

label_data <- label_data %>% 
                  filter(!family %in% c("lib", "radrt", "radleft"))

ches2_long_withoutADA <- ches2_long %>% 
  filter(!family %in% c("lib", "radrt", "radleft"))


pdf(file="./Results/lines3_fam.pdf",width=15,height=10,paper="special")
ggplot(prof, aes(x = var, y = val, color = arch, group = arch)) +
  geom_line(size = 1) +
  geom_line(data = ches2_long_withoutADA,
            aes(x = var, y = val, group = family),
            color = "gray40", linetype = "dashed", size = 0.5,
            inherit.aes = FALSE) +
  
  geom_text(data = label_data,
            aes(x = var, y = val, label = family),
            hjust = -0.1, vjust = 0.5, size = 3, color = "gray20",
            inherit.aes = FALSE) +
  
  labs(x = "Variable", y = "Value", color = "Archetypoid") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  coord_cartesian(clip = "off")
dev.off()


save.image(file="./Rdata/preliminary.RData")
#load(file="./Rdata/preliminary.RData")

#### MDS without and with clustering
# 1. Compute the Euclidean distance matrix
dist_matrix_withoutclust <- dist(ches2bis %>% select(-cluster), method = "euclidean")
dist_matrix_withclust <- dist(ches2bis, method = "euclidean")


# 2. Run classical MDS (cmdscale) to reduce to 2 dimensions
mds_coords_withoutclust <- cmdscale(dist_matrix_withoutclust, k = 2)
mds_coords_withclust    <- cmdscale(dist_matrix_withclust, k = 2)


# 3. Plot the MDS result

pdf(file="./Results/MDS.pdf",width=15,height=10,paper="special")

par(mfrow = c(1, 2))  # 1 row, 2 columns

plot(mds_coords_withoutclust, type = "p",
     main = "MDS Plot (Euclidean Distance). Original data",
     xlab = "Dimension 1", ylab = "Dimension 2",
     pch = 19, col = "blue")

plot(mds_coords_withclust, type = "p",
     main = "MDS Plot (Euclidean Distance). Three clusters",
     xlab = "Dimension 1", ylab = "Dimension 2",
     pch = 19, col = ches2bis$cluster)

dev.off()

par(mfrow = c(1, 1))

save.image(file="./Rdata/preliminary.RData")
#load(file="./Rdata/preliminary.RData")






###-------------- More visualizations (Just testing)---
# 
# 
# #Distaces between alpha matrices:
# #• Computes Euclidean distances and cosine similarities
# #  • A dendrogram (dendrogram_plot.png)
# #  • A cosine similarity heatmap (cosine_heatmap.png)
# #  • A 2D MDS map (mds_plot.png)
# 
# # Dendrogram (Euclidean)
# dist_matrix <- dist(alpha_matrix, method="euclidian")
# hc <- hclust(dist_matrix)
#   
# pdf(file="./Results/dendogram.pdf",width=10,height=10,paper="special")
#   plot(hc, main = "Dendrogram of Subjects (Euclidean Distance)", xlab = "", sub = "")
# dev.off()
#   
# # Cosine similarity
#   cosine_sim <- function(x) {
#     norm_x <- sqrt(rowSums(x^2))
#     sim <- x %*% t(x) / (norm_x %*% t(norm_x))
#     diag(sim) <- 1
#     return(sim)
#   }
#   
# cos_sim_matrix <- cosine_sim(alpha_matrix)
#   
# 
# pdf(file="./Results/cosine_heatmap.pdf",width=10,height=10,paper="special")
# pheatmap(
#   mat = cos_sim_matrix,
#   color = colorRampPalette(c("white", "darkgreen"))(100),
#   cluster_rows = TRUE,
#   cluster_cols = TRUE,
#   main = "Cosine Similarity Heatmap"
# )
#   dev.off()
#   
#  # MDS Map
# mds_coords <- cmdscale(dist_matrix, k = 2)
#   
# pdf(file="./Results/mds_plot.pdf",width=10,height=10,paper="special")
# 
# plot(as.matrix(mds_coords), type = "p", main = "MDS Map of Parties",
#        xlab = "Dimension 1", ylab = "Dimension 2")
# text(mds_coords, labels = rownames(ches2bis), col = "blue", cex = 1.2)
# dev.off()
#  
# save.image(file="./Rdata/preliminary.RData")
# #load(file="./Rdata/preliminary.RData")