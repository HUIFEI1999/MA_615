library(tidyverse)
library(magrittr)
library(readxl)

strawb <- read_xlsx("strawberries-2022oct30-a.xlsx", col_names = T)
cnames <- colnames(strawb)
x <- 1:dim(strawb)[2]
T <- NULL

for(i in x){T <- c(T, dim(unique(strawb[i]))[1])}
drop_cols <- cnames[which(T == 1)]

strawb %<>% dplyr::select(!all_of(drop_cols))
strawb %<>% arrange(Year, State)
temp1 <- strawb %>% dplyr::select(`Data Item`) %>% distinct()

strawb %<>% separate(col=`Data Item`,
                     into = c("Strawberries", "type", "items", "units"),
                     sep = ",",
                     fill = "right")
og_2016_ca <- subset(strawb,
                     State == "CALIFORNIA" & Year == 2016,
                     select = c("Year","State","type","Value"))

og_2016_ca$Value <- as.integer(og_2016_ca$Value)
og_2016_ca<-na.omit(og_2016_ca)
ans <- data.frame(aggregate(og_2016_ca$Value, by=list(og_2016_ca$type), FUN=sum))

chemical <- filter(strawb, Domain != 'ORGANIC STATUS' & Domain != 'TOTAL')
count <- grep("TOTAL", chemical$`Domain Category`,ignore.case = T)
nrow(unique(chemical[11])) - length(count)

ca <- chemical[chemical$State == "CALIFORNIA",]
fl <- chemical[chemical$State == "FLORIDA",]
length(grep("TOTAL",fl$`Domain Category`,ignore.case = T))
nrow(unique(fl[11]))
length(grep("TOTAL",ca$`Domain Category`,ignore.case = T))
nrow(unique(ca[11]))