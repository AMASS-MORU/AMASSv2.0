#***-------------------------------------------------------------------------------------------------***#
#*** AutoMated tool for Antimicrobial resistance Surveillance System version 2.0 (AMASS version 2.0) ***#
#***-------------------------------------------------------------------------------------------------***#
# Aim: to enable hospitals with microbiology data available in electronic formats
# to analyze their own data and generate AMR surveillance reports systematically.

#### Dated changes ####
# Created on 20th April 2022
#----Sub-divided from AMASSplus released on 25th March 2021
# Added rename_str_indf() on 04th September 2022 


# Core function: to rename the variables ####
fun_setvarname <- function(x, oldname, newname, allow.absent.cols=F) {
  if (!allow.absent.cols) {
    setnames(x, oldname, newname) #setnames is a function of the data.table package
  } else {
    ix <- match(names(x), oldname, 0L)
    setnames(x, oldname[ix], newname[ix])
  }
}

# Core function: to move variables ####
# moving columns
movecolumn <- function (columnlist, varlocation) {
  varlocation <- lapply(strsplit(strsplit(varlocation, ";")[[1]],
                                 ",|\\s+"), function(x) x[x != ""])
  movelist <- lapply(varlocation, function(x) {
    Where <- x[which(x %in% c("before", "after", "first", "last")):length(x)]
    ToMove <- setdiff(x, Where)
    list(ToMove, Where)
  })
  myVec <- columnlist
  for (i in seq_along(movelist)) {
    temp <- setdiff(myVec, movelist[[i]][[1]])
    A <- movelist[[i]][[2]][1]
    if (A %in% c("before", "after")) {
      ba <- movelist[[i]][[2]][2]
      if (A == "before") {
        after <- match(ba, temp) - 1
      }
      else if (A == "after") {
        after <- match(ba, temp)
      }
    }
    else if (A == "first") {
      after <- 0
    }
    else if (A == "last") {
      after <- length(myVec)
    }
    myVec <- append(temp, values = movelist[[i]][[1]], after = after)
  }
  myVec
}
# Core function: to clean "Date" variables ####
# Function to change the "Date" variable to the same format
fun_datevariable <- function(data, datevar){
  # replace the blank with 1 (1900-01-01) to identify the blank values
  datevar <- sub("^$", "1", datevar)
  # The following section is to solve the issue of date data comes with date & time
  # Split date and time for samples that combined date and time in the same cell
  date2 <- data.frame(sapply(strsplit(datevar, ' '), '[', 1))
  colnames(date2) <- "x1"
  # A new data frame to incorporate the splitted variables back to the hospital clinical data
  data2 <- data.frame(data, date2)
  return(data2)
}
# Function to deal with multiple date format within the same variable
multidate <- function(data){
  Datefmts <- c(# assuming the order is date-month-year
    "%d/%h/%y", "%d/%h/%Y",
    "%d/%m/%y", "%d/%m/%Y",
    "%d/%b/%y", "%d/%b/%Y",
    "%d-%h-%y", "%d-%h-%Y",
    "%d-%m-%y",  "%d-%m-%Y",
    "%d-%b-%y",  "%d-%b-%Y",
    "%d%h%y", "%d%h%Y",
    "%d%m%y", "%d%m%Y",
    "%d%b%y", "%d%b%Y",
    "%Y-%m-%d")
  a<-list()
  for(i in 1:length(Datefmts)){
    # change "Date" variable from character to date format
    a[[i]]<- as.Date(data,format=Datefmts[i], origin = "1900-01-01")
    a[[i]][a[[i]]>Sys.Date() | a[[i]]<as.Date("1000-01-01")] <- NA
    a[[1]][!is.na(a[[i]])]<-a[[i]][!is.na(a[[i]])]
  }
  a[[1]]
}
# Functions to generate data for the reports ####
# Function to de-duplicate
# to create de-duplicated dataset stratified by organism
fun_data_dedup_org <- function(sampletype, org_cat){
  a <- # per sample type
    sampletype %>%
    # per pathogen
    dplyr::filter(organismCat==org_cat) %>%
    # order data by hospital number (patient identifier), and sample collection date
    dplyr::arrange(hospital_number, DateSpc, desc(AMR)) %>%
    # group by patient (hospital_number)
    dplyr::group_by(hn) %>%
    dplyr::mutate(inanalysis=row_number()) %>%
    # keep only the first isolate
    dplyr::filter(inanalysis==1)
  return(a)
}
# Function to de-duplicate
# to create de-duplicated dataset grouped by hn
fun_data_dedup <- function(sampletype){
  a <- # per sample type
    sampletype %>%
    # order data by hospital number (patient identifier), and sample collection date
    dplyr::arrange(hn, DateSpc) %>%
    # group by patient (hn)
    dplyr::group_by(hn) %>%
    dplyr::mutate(inanalysis=row_number()) %>%
    # keep only the first isolate
    dplyr::filter(inanalysis==1)
  return(a)
}
# Function to count number of patients
fun_count_patient_data <- function(data){
  a <- # per sample type
    data %>%
    # order data by hospital number (patient identifier) and Sample date
    dplyr::arrange(hn, specimen_collection_date) %>%
    # identify the first patient
    dplyr::group_by(hn) %>%
    dplyr::mutate(inanalysis=row_number()) %>%
    # keep only the first unique patient identifier
    dplyr::filter(inanalysis==1)
  b <- nrow(a)
  return(b)
}
# Function to count number of admission
fun_count_admission_data <- function(data){
  a <- # per sample type
    data %>%
    # order data by hospital number (patient identifier) and Sample date
    dplyr::arrange(hn, DateAdm) %>%
    # identify the first patient
    dplyr::group_by(hn, DateAdm) %>%
    dplyr::mutate(inanalysis=row_number()) %>%
    # keep only the first unique patient identifier
    dplyr::filter(inanalysis==1)
  b <- nrow(a)
  return(b)
}
# Function to create summary data
# to create RIS table for each antibiotic stratified by organism
fun_data_asttable <- function(amr_group2){
  a <- as.matrix(table(amr_group2))
  a <- as.data.frame(t(a))
  # to create R I S aND variables and replace if obs of any of those is 0
  a$R <- ifelse(any(names(a) == 'R'), a$R, 0)
  a$I <- ifelse(any(names(a) == 'I'), a$I, 0)
  a$S <- ifelse(any(names(a) == 'S'), a$S, 0)
  a$aND <- ifelse(any(names(a) == 'aND'), a$aND, 0)
  return(a)
}
# Function to create summary data
# to create RIS table for each antibiotic category stratified by organism
fun_data_asttable1 <- function(amr_group2){
  a <- as.matrix(table(amr_group2, exclude = NULL))
  a <- as.data.frame(t(a))
  # to create R I S aND variables and replace if obs of any of those is 0
  colnames(a) <- ifelse(is.na(names(a))==TRUE, "aND", colnames(a))
  a$S <- ifelse(any(names(a) == '0'), a$"0", 0)
  a$R <- ifelse(any(names(a) == '1'), a$"1", 0)
  b <- a[,(ncol(a)-2):ncol(a)]
  return(b)
}
# Function to create summary data
# to format the summary data table
fun_data_asttable2 <- function(data){
  a <- data
  a$NonS <- paste(a[,5],"%"," (",a[,3],"/",a[,4],")")
  a$NonS <- gsub(" ", "", a$NonS)
  a$NonS <- gsub("%", "% ", a$NonS)
  a$NonS <- replace(a$NonS, a[,4]==0, values="NA")
  a$NonS <- gsub("NA", "NA ", a$NonS)
  a$CI <- paste(a[,6], "%"," - ", a[,7], "%")
  a$CI <- gsub(" ", "", a$CI)
  a$CI <- replace(a$CI, (a[,4]==0), values="-")
  a[,2:7] <- NULL
  names(a) <- c("Antibiotic agent", "% NS (n)", "95% CI")
  return(a)
}
# Function to create summary data
# to generate data on frequency per a defined denominator
fun_data_incidence <- function(data1, data2, data3, data4,
                               data5, data6, data7, data8,
                               denom, perpop){
  # Incidence of infection for all organism
  a <- data.frame(c("S. aureus", "Enterococcus spp.", "S. pneumoniae",
                    "Salmonella spp.", "E. coli", "K. pneumoniae",
                    "P. aeruginosa", "Acinetobacter spp."),
                  c(nrow(data1), nrow(data2), nrow(data3),
                    nrow(data4), nrow(data5), nrow(data6),
                    nrow(data7), nrow(data8)))
  # Name the columns
  names(a) <- c("Organism", "NumberofPatients")
  # Calculate incidence of infection per tested population and round up to whole number
  a$incid_tested <- (a$NumberofPatients/denom)*perpop
  # 95% CI for the incidence
  a$incid_tested_lci <- (perpop/100)*fun_wilson_lowerCI(a$NumberofPatients, denom, 0.95, 10)
  a$incid_tested_uci <- (perpop/100)*fun_wilson_upperCI(a$NumberofPatients, denom, 0.95, 10)
  return(a)
}
fun_data_incidence_2 <- function(data1, data2, data3, data4,
                                 data5_1, data5_2, data6_1, data6_2, data7, data8,
                                 denom, perpop){
  # Incidence of infection for all organism
  a <- data.frame(c("S. aureus", "Enterococcus spp.",
                    "S. pneumoniae", "Salmonella spp.",
                    "E. coli", "E. coli",
                    "K. pneumoniae", "K. pneumoniae",
                    "P. aeruginosa", "Acinetobacter spp."),
                  c("MRSA ",
                    "Vancomycin-NS\nEnterococcus spp.",
                    "Penicillin-NS\nS. pneumoniae",
                    "Fluoroquinolone-NS\nSalmonella spp.",
                    "3GC-NS E. coli",
                    "Carbapenem-NS\nE. coli",
                    "3GC-NS K. pneumoniae",
                    "Carbapenem-NS\nK. pneumoniae",
                    "Carbapenem-NS\nP. aeruginosa",
                    "Carbapenem-NS\nAcinetobacter spp."),
                  c(length(which(data1==1)), length(which(data2==1)),
                    length(which(data3==1)), length(which(data4==1)),
                    length(which(data5_1==1)), length(which(data5_2==1)),
                    length(which(data6_1==1)), length(which(data6_2==1)),
                    length(which(data7==1)), length(which(data8==1))))
  names(a) <- c("Organism", "Priority", "NumberofPatients")
  # Calculate incidence of infection per tested population and round up to whole number
  a$incid_tested <- (a$NumberofPatients/denom)*perpop
  # 95% CI for the incidence
  a$incid_tested_lci <- (perpop/100)*fun_wilson_lowerCI(a$NumberofPatients, denom, 0.95, 10)
  a$incid_tested_uci <- (perpop/100)*fun_wilson_upperCI(a$NumberofPatients, denom, 0.95, 10)
  return(a)
}
# Function to create summary data
# to combine all the individual AST summary tables into one
fun_data_astcombine <- function(ib_blood_a){
  ib_blood_sa_b <-do.call(function(...) {
    tmp <- plyr::rbind.fill(...)
    rownames(tmp) <- sapply(ib_blood_a, function(i) {
      rownames(i)
    })
    return(tmp)
  }, ib_blood_a)
  ib_blood_sa_b <- replace(ib_blood_sa_b, is.na(ib_blood_sa_b), 0)
  AST <- rownames(ib_blood_sa_b)
  rownames(ib_blood_sa_b) <- NULL
  ib_blood_sa_c <- cbind(AST,ib_blood_sa_b)
  ib_blood_sa_c$Antibiotic <- ib_blood_sa_c$AST
  ib_blood_sa_c$Susceptible <- ib_blood_sa_c$S
  ib_blood_sa_c$Intermediate <- ib_blood_sa_c$I
  ib_blood_sa_c$Resistance <- ib_blood_sa_c$R
  ib_blood_sa_c$NotDone <- ib_blood_sa_c$aND
  ib_blood_sa_c$AST <- NULL
  ib_blood_sa_c$aND <- NULL
  ib_blood_sa_c$I <- NULL
  ib_blood_sa_c$R <- NULL
  ib_blood_sa_c$S <- NULL
  ib_blood_sa_d <- data.frame(ib_blood_sa_c$Antibiotic, ib_blood_sa_c$Susceptible,
                              ib_blood_sa_c$Intermediate, ib_blood_sa_c$Resistance,
                              ib_blood_sa_c$NotDone)
  names(ib_blood_sa_d) <- c("Antibiotic", "Susceptible", "Intermediate", "Resistance", "NotDone")
  return(ib_blood_sa_d)
}
# Function to combine for mortality data
fun_data_astcombine2 <- function(ib_blood_a){
  ib_blood_sa_b <-do.call(function(...) {
    tmp <- plyr::rbind.fill(...)
    rownames(tmp) <- sapply(ib_blood_a, function(i) {
      rownames(i)
    })
    return(tmp)
  }, ib_blood_a)
  ib_blood_sa_b <- replace(ib_blood_sa_b, is.na(ib_blood_sa_b), 0)
  AST <- rownames(ib_blood_sa_b)
  rownames(ib_blood_sa_b) <- NULL
  ib_blood_sa_c <- cbind(AST,ib_blood_sa_b)
  ib_blood_sa_d <- data.frame(ib_blood_sa_c$AST, ib_blood_sa_c$D,
                              ib_blood_sa_c$Mortality, ib_blood_sa_c$lowerCI,
                              ib_blood_sa_c$upperCI, ib_blood_sa_c$CI)
  names(ib_blood_sa_d) <- c("Antibiotic", "D", "Mortality", "lowerCI", "upperCI", "CI")
  return(ib_blood_sa_d)
}
# Function to calculate wilson
# to calculate lower 95% CI
fun_wilson_lowerCI <- function(x,n,conflevel,decimalplace){
  zalpha <- abs(qnorm((1-conflevel)/2))
  phat <- x/n
  bound <- (zalpha*((phat*(1-phat)+(zalpha^2)/(4*n))/n)^(1/2))/(1+(zalpha^2)/n)
  midpnt <- (phat+(zalpha**2)/(2*n))/(1+(zalpha**2)/n)
  lowlim <- round((midpnt - bound)*100,decimalplace)
  return(lowlim)
}
# to calculate upper 95% CI
fun_wilson_upperCI <- function(x,n,conflevel, decimalplace){
  zalpha <- abs(qnorm((1-conflevel)/2))
  phat <- x/n
  bound <- (zalpha*((phat*(1-phat)+(zalpha^2)/(4*n))/n)^(1/2))/(1+(zalpha^2)/n)
  midpnt <- (phat+(zalpha**2)/(2*n))/(1+(zalpha**2)/n)
  uplim <- round((midpnt + bound)*100,decimalplace)
  return(uplim)
}


# Function to create summary data
# to create the summary table that contains
# proportion, 95% CI, incidence, exact 95% CI that will include in the reports
fun_table_report <- function(data, conflevel, Nsamples){
  a <- data
  # NonS = non-susceptible = the total number of patients with
  # with organism that is intermediate or resistance to antibiotic cultured
  # from the first sample
  a$NonS <- a$Intermediate + a$Resistance
  # total = the total number of patients with sample cultured and AST done
  a$total <- a$Susceptible + a$Intermediate + a$Resistance
  a$NonSPercent <- (round((a$NonS/(a$total)), 2))*100
  # remove the columns that is not needed further
  a$Intermediate <- NULL
  a$Resistance <- NULL
  a$NotDone <- NULL
  # wilson 95% CI of the proportion
  a$lowerCI <- fun_wilson_lowerCI(x=a$NonS, n=a$total, conflevel, decimalplace=1)
  a$upperCI <- fun_wilson_upperCI(x=a$NonS, n=a$total, conflevel, decimalplace=1)
  # calculate the incidence
  # Nsamples = the total number of patients with at least 1 sample collected for culture
  # Round up to whole number
  a$incidence <- ceiling((a$NonS/(Nsamples))*100000)
  # Exact 95% CI of the incidence
  a$lowerinciCI <- ceiling(((fun_wilson_lowerCI(x = a$NonS, n=Nsamples, conflevel, decimalplace=5))/100)*100000)
  a$upperinciCI <- ceiling(((fun_wilson_upperCI(x = a$NonS, n=Nsamples, conflevel, decimalplace=5))/100)*100000)

  # Format the 95% CI of the proportion and incidence
  a$lowerinciCI <- replace(a$lowerinciCI, a$total==0, values=0)
  a$upperinciCI <- replace(a$upperinciCI, a$total==0, values=0)
  a$lowerCI <- replace(a$lowerCI, a$total==0, values="NA")
  a$upperCI <- replace(a$upperCI, a$total==0, values="NA")
  # Name the columns
  names(a) <- c("Antibiotic", "Susceptible(N)", "Non-susceptible(N)", "Total(N)",
                "Non-susceptible(%)", "lower95CI(%)*", "upper95CI(%)*",
                "Incidence", "Exact95up", "Exact95do")
  return(a)
}
# Function to select the infection origin variable
fun_data_infori <- function(data){
  df = data
  df$InfOri = NA
  df$InfOri = replace(df$InfOri, avai_Infect_Ori=="yes", values = df$InfOri_hosp)
  df$InfOri = replace(df$InfOri, avai_Infect_Ori=="no", values = df$InfOri_cal)
  return(df)
}
# Function to create summary data on discharge outcome
# to create the summary table that contains
# mortality and 95% CI that will include in the extended reports
fun_data_deathtable <- function(data, AST, susceptible, atbname){
  data_met <- data[which(AST==susceptible),]
  a <- as.matrix(table(data_met$disoutcome2_cat))
  a <- as.data.frame(t(a))
  # to create the summary table
  row.names(a) <- atbname
  a$total <- a$alive + a$died
  a$lowerCI <- fun_wilson_lowerCI(x=a$died, n=a$total, conflevel=0.95, decimalplace=1)
  a$upperCI <- fun_wilson_upperCI(x=a$died, n=a$total, conflevel=0.95, decimalplace=1)
  a$D <- round((a$died/a$total)*100, digits=0)
  a$Mortality <- paste(a$D, "% ", "(", a$died, "/", a$total, ")", sep = "")
  a$Mortality <- replace(a$Mortality, a$total==0, values = "NA")
  a$CI <- paste(a$lowerCI, "%","-", a$upperCI, "%", sep = "")
  a$CI <- replace(a$CI, (a$total==0), values = "-")
  b <- a[,(ncol(a)-4):ncol(a)]
  return(b)
}
# Function for barplot (isolate-based report)
# to create barplot for isolate-based report
fun_barplot_isolate = function(data, col_main) {
  names(data) <- c("antibiotic", "s", "ns", "t", "nsp", "lowerci", "upperci", "inci", "inci95low", "inci95up")
  data$lowerci <- replace(data$lowerci, data$lowerci=="NA", values=0)
  data$upperci <- replace(data$upperci, data$upperci=="NA", values=0)
  data$lowerci <- as.numeric(data$lowerci)
  data$upperci <- as.numeric(data$upperci)
  data$num <- c(1:nrow(data))
  data <- data[order(data$num, decreasing = TRUE),]
  cols <- c(col_main, "lightgrey")[((data$s+data$ns)/max(data$t) < 0.7) + 1]
  cols2 <- c("black", "white")[(data$upperci == 0) + 1]
  #barplot
  bar_blood <- barplot(data$nsp,
                       xlab="*Proportion of non-susceptible isolates (%)",
                       ylab="",
                       xlim=c(0,100),
                       cex.lab=1.5,
                       cex.axis = 1.5,
                       horiz = TRUE,
                       names.arg = data$antibiotic,
                       cex.names=1.5,
                       border = cols,
                       axes = TRUE,
                       col = cols,
                       space = 0.3,
                       las=1)
  segments(data$lowerci, bar_blood,
           data$upperci, bar_blood, lwd = 3, col = cols2)
  arrows(data$lowerci, bar_blood,
         data$upperci, bar_blood,
         lwd = 3, angle = 90,
         code = 3, length = 0.05, col = cols2)
}
# Function for barplot (Sample-based report)
fun_barplot_sample = function(data, inci, lci_inci, uci_inci, xtitle) {
  a <- data
  a$num <- c(1:nrow(a))
  a <- a[order(a$num, decreasing = TRUE),]
  for (i in 1:nrow(a)){
    a$col[i] <- if(a$Organism[i]=="S. aureus"){
      "plum4"
    }else if(a$Organism[i]=="Enterococcus spp."){
      "orange1"
    }else if(a$Organism[i]=="S. pneumoniae"){
      "indianred"
    }else if(a$Organism[i]=="Salmonella spp."){
      "steelblue2"
    }else if(a$Organism[i]=="E. coli"){
      "tan4"
    }else if(a$Organism[i]=="K. pneumoniae"){
      "olivedrab3"
    }else if(a$Organism[i]=="P. aeruginosa"){
      "hotpink3"
    }else if(a$Organism[i]=="Acinetobacter spp."){
      "lightgoldenrod3"
    }else{"black"}
  }
  #barplot
  bar_blood_sample <- barplot(a[[inci]],
                              ylab="",
                              xlim=c(0,max(a[[uci_inci]])*1.05),
                              cex.lab=1.5,
                              cex.axis = 1.5,
                              horiz = TRUE,
                              names.arg = a$Organism,
                              cex.names=1.5,
                              border = a$col,
                              col = a$col,
                              axes = TRUE,
                              space = 0.3,
                              las=1)
  title(xlab=xtitle,
        cex.lab=1.5, line = 5)
  segments(a[[lci_inci]], bar_blood_sample,
           a[[uci_inci]], bar_blood_sample, lwd = 3)
  arrows(a[[lci_inci]], bar_blood_sample,
         a[[uci_inci]], bar_blood_sample,
         lwd = 3, angle = 90,
         code = 3, length = 0.05)
}
fun_barplot_sample2 = function(data, inci, lci_inci, uci_inci, xtitle, max) {
  a <- data
  a$num <- c(1:nrow(a))
  a <- a[order(a$num, decreasing = TRUE),]
  for (i in 1:nrow(a)){
    a$col[i] <- if(a$Organism[i]=="S. aureus"){
      "plum4"
    }else if(a$Organism[i]=="Enterococcus spp."){
      "orange1"
    }else if(a$Organism[i]=="S. pneumoniae"){
      "indianred"
    }else if(a$Organism[i]=="Salmonella spp."){
      "steelblue2"
    }else if(a$Organism[i]=="E. coli"){
      "tan4"
    }else if(a$Organism[i]=="K. pneumoniae"){
      "olivedrab3"
    }else if(a$Organism[i]=="P. aeruginosa"){
      "hotpink3"
    }else if(a$Organism[i]=="Acinetobacter spp."){
      "lightgoldenrod3"
    }else{"black"}
  }
  #barplot
  bar_blood_sample <- barplot(a[[inci]],
                              ylab="",
                              xlim=c(0,max),
                              cex.lab=1.5,
                              cex.axis = 1.5,
                              horiz = TRUE,
                              names.arg = a$Priority,
                              cex.names=1.5,
                              border = a$col,
                              col = a$col,
                              axes = TRUE,
                              space = 0.3,
                              las=1)
  title(xlab=xtitle,
        cex.lab=1.5, line = 5)
  segments(a[[lci_inci]], bar_blood_sample,
           a[[uci_inci]], bar_blood_sample, lwd = 3)
  arrows(a[[lci_inci]], bar_blood_sample,
         a[[uci_inci]], bar_blood_sample,
         lwd = 3, angle = 90,
         code = 3, length = 0.05)
}
# Function for barplot (Extended report)
# to create barplot for Extended report
fun_barplot_extend = function(data, maintext1, maintext2, cols4) {
  names(data) <- c("Antibiotic", "d", "mortality", "lowerci", "upperci", "ci")
  bar_blood <- barplot(data$d,
                       xlab="*Mortality (%)",
                       ylab="",
                       xlim=c(0,100),
                       cex.lab=1.5,
                       cex.axis = 1.5,
                       horiz = TRUE,
                       names.arg = data$Antibiotic,
                       cex.names=1.5,
                       border = "black", col = cols4,
                       axes = TRUE,
                       las=1)
  segments(data$lowerci, bar_blood,
           data$upperci, bar_blood, lwd = 3)
  arrows(data$lowerci, bar_blood,
         data$upperci, bar_blood,
         lwd = 3, angle = 90,
         code = 3, length = 0.05)
  title(main= substitute(paste(italic(maintext1), maintext2)), adj=0)
}


##De-duplicating data by hn
fun_deduplicate <- function(posDF){
  posDF_dedup <- # per sample type 
    posDF %>%
    # order data by hospital number (patient identifier), and sample collection date
    dplyr::arrange(hn, DateSpc) %>%
    # group by patient (hn)
    dplyr::group_by(hn) %>%
    dplyr::mutate(inanalysis=row_number()) %>%
    # keep only the first isolate
    dplyr::filter(inanalysis==1)
  return (posDF_dedup)
}

##Creating first capital of each value in the vector
fun_firstCapital <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

##Renaming full name of interested microorganisms to shorter name
#change from "xxxBurkholderia xxx pseudomallei" >>> B. pseudomallei
fun_clean_org <- function(rawDF, orgName){
  for (org in orgName) {
    if (str_sub(org,start=2,end=3) == ". "){
      rawDF$organismplus <- gsub(paste(".*",str_sub(org,start=1,end=1),".*",str_sub(org,start=4), sep = "", collapse = NULL), org, rawDF$organismplus)
    }
  }
  return (rawDF)
}


##Renaming string in dataframe
#when check_str is True  : Acinetobacter spp. >>> Acinetobacter baumannii
#when check_str is False : pass
rename_str_indf <- function(df, df_col, check_str, new_str, ori_str){
  if (check_str) {
    df[df_col] <- data.frame(lapply(df[df_col],function(x){gsub(ori_str, new_str,x)}))
  } else {}
  return (df)
}
