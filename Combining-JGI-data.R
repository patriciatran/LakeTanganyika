# Patricia Tran
# November 2020

# combining the JGI data. We want to make a giant table with all the annotations from the JGI pipeline, as an extra layer of confidence in our "custom" annotations.
# I used GLOBUS to download the IMG data for the 24 metagenomes of Lake Tanganyika.
# Specifically, I downloaded the .assembled.COG, EC, KO, name_map, Pfam.blout, product_names files.

# Load packages:
library(data.table)
library(tidyverse)



# Load the list of binned fasta scaffolds:
binned.scaffs <- read.delim("~/Documents/JGI-LakeTanganyika-Globus-Data/list.of.headers.Tang.523MAGS.txt", header=FALSE)
binned.scaffs <- binned.scaffs %>% separate(V1, into=c("MAG","header"), sep=":>")
head(binned.scaffs)
# Now remove all the characters before .*Ga in the "header" column. 
binned.scaffs$header <- str_replace(binned.scaffs$header, ".*Ga", "Ga")
# Now let's just make sure we have captured all the MAGS:
length(unique(binned.scaffs$MAG))
# Should be 523, yes it is :) 

# So to double check let's see how many characters there are in the column header.
unique(nchar(binned.scaffs$header))
# They all have 18 characters which means that in the combined table later on, we'll want to split the names after 18 characters.




setwd("~/Documents/JGI-LakeTanganyika-Globus-Data/")

list.of.folders <- list.files()
list.of.folders <- setdiff(list.of.folders,"list.of.headers.Tang.523MAGS.txt")

# Start a timer:

for (i in 1:length(list.of.folders)){
  ptm <- proc.time()
  setwd(paste0("~/Documents/JGI-LakeTanganyika-Globus-Data/",list.of.folders[i],"/"))
  print(paste0("Now in the folder: ",list.of.folders[i]))
  
  # Make subfolder:
  dir.create("Figures")
  dir.create("Tables")
  print("created folders for outputs")
  
  files.cog <- list.files(pattern = "\\.COG$")
  files.ec <- list.files(pattern = "\\.EC$")
  files.ko <- list.files(pattern = "\\.KO$")
  files.names_map <- list.files(pattern = "\\.names_map$")
  files.pfam.blout <- list.files(pattern = "\\.Pfam.blout$")
  files.product.names <- list.files(pattern = "\\.product_names$")
  files.cov <- list.files(pattern = "\\.cov$")
  
  #class(files.cog)
  filename <- str_split(files.cog, pattern="[.]")[[1]][1]
  
  
  cog <- fread(files.cog, sep="\t")
  ec <- fread(files.ec, sep="\t",)
  KO <- fread(files.ko, sep="\t",)
  name_map <- fread(files.names_map, sep="\t", header=FALSE)
  Pfam <- fread(files.pfam.blout, sep="\t",)
  product_names <- fread(files.product.names, sep="\t", header=FALSE)
  cov <- fread(files.cov, sep="\t",)
  
  #head(cog)
  #head(ec)
  #head(KO)
  #head(name_map) # The names.match allows us to combine with the coverage file.
  #head(cov) # this table has headers
  #head(Pfam)
  #head(product_names)
  
  # Product names had the most number of rows
  # Let's check the distribution of % ID
  pdf(file = paste0("Figures/",filename,".pdf"),   # The directory you want to save the file in
      width = 8, # The width of the plot in inches
      height = 8) # The height of the plot in inches
  
  par(mfrow=c(2,2))
  hist(cog$V3, xlab="Percent ID")
  hist(ec$V4, xlab="Percent ID")
  hist(KO$V4,xlab="Percent ID")
  hist(Pfam$V3, xlab="Percent ID")
  
  dev.off()
  
  combined <- left_join(product_names, Pfam, by = "V1")
  nrow(combined)
  combined <- left_join(combined, KO, by="V1")
  nrow(combined)
  combined <- left_join(combined, cog, by="V1")
  nrow(combined)
  combined <- left_join(combined, ec, by="V1")
  nrow(combined)
  # Now we have our super table. 
  
  # Now an useful thing would be to have a column with information about whether it was binned or not.
  # Before we start splitting this table let's ensure that the scaffold ID have at least 18 characters.
  unique(nchar(combined$V1))
  # They either have 19, 20 or 21 characters...good :)
  
  combined <- combined %>% mutate(Scaffold_ID_Loci = V1) %>% 
    separate(V1, into=c("Scaffold_ID","Loci"),sep=18)
  
  # Bring that last column to the beginning:
  combined <- combined %>%
    select(Scaffold_ID_Loci, everything())
  
  # Now let's match the Scaffold_ID from combined table with the binned.scaffolds column called "header"
  combined <- left_join(combined, binned.scaffs, by=c("Scaffold_ID"="header"))
  
  # To ensure we are good calculate the number of scaffolds in MAGS vs not:
  combined.count <- combined %>% select(Scaffold_ID, MAG) %>% group_by(MAG) %>% tally()
  
  combined.count %>% ggplot(aes(x=MAG, y=n)) +
    geom_bar(stat="identity")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90))+
    ggtitle(filename)
  
  ggsave(paste0("Figures/",filename,".binned.bar.plot.pdf"))
  
  # Now let's attend to combine name_map and cov:
  coverage.mapped <- left_join(name_map, cov, by=c("V1"="#ID"))
  
  # Write files to the folder:
  write_delim(combined, paste0("Tables/",filename,".combined.tsv"), delim="\t")
  write_delim(coverage.mapped, paste0("Tables/",filename,"coverage.mapped.tsv"), delim="\t")
  write_delim(combined.count, paste0("Tables/",filename,"MAGs.barplot.tsv", delim="\t"))
  
  print("Finished saving tables and figures, moving to the next folder")
  time.it.took <- proc.time() - ptm
  print(time.it.took)
}
