#### this is an ongoing document in which I record data cleaning procedures for the affirmation work
library(reshape2)
library(plyr)
library(dplyr)
library(stringr)
library(foreign)
library(xlsx)

###################functions (i.e. greatest hits)
cleanannotation<-function(essaytext){
  annotation.rex <- "(^\\[\\s*)|(\\s*\\]*$)|(\\\\{1,})"
  gsub(annotation.rex, '', essaytext)
}

correctessay <- function(essaytext){
  #additions
  dol.punct <- "(\\s?)\\$([.:,?;])\\$"
  dol <- "(\\s?)\\$(.*?)\\$"
  essaytext <- gsub(dol.punct, '\\2', essaytext)
  essaytext <- gsub(dol, '\\1\\2', essaytext)
  
  #deletions
  multi.car <- "\\{.*?}\\s?\\^(.*?)\\^"
  single.car <- "(.*)\\s?\\^\\1\\^"
  essaytext <- gsub(multi.car, '', essaytext)
  essaytext <- gsub(single.car, '', essaytext)
  
  #substitutions
  punct <- "[.;,?:]\\s?@([.;,?:])@"
  multi.sub <- "\\{.*?}\\s?@(.*?)@"
  single.sub <- "\\w*'?\\w*['.,]?\\s?@(.*?)@"
  essaytext <- gsub(punct, '\\1', essaytext)
  essaytext <- gsub(multi.sub, '\\1', essaytext)
  essaytext <- gsub(single.sub, '\\1', essaytext)
}


################## BMI STUDY ###################################################
bmi <- read.csv('Datasets from Geoff/Colorado middle school materials/bmi study colorado.csv')
names(bmi)[1] <- 'ID'

bmi.long <- melt(bmi, id.vars = c('ID', 'date'),
                 measure.vars = c('test1numCorrect', 'test2numCorrect', 
                                  'test3numCorrect'))

bmi.long$variable <- revalue(bmi.long$variable, 
                             c('test1numCorrect' = 'Lab Test 1',
                               'test2numCorrect' = 'Lab Test 2',
                               'test3numCorrect' = 'Lab Test 3'))

bmi.grades <- data.frame(ID=bmi.long$ID, 
                         Study=rep('CO BMI Study', length(bmi.long$ID)),
                         Grade=bmi.long$value,
                         Grade_type=bmi.long$variable, 
                         est_Grade_date=bmi.long$date,
                         intervention_year=rep(1, length(bmi.long$ID)))

df.grades <- rbind(df.grades, bmi.grades)

write.csv(df.grades, 'grades9.11.15.csv', sep=',')

################## date fixing
df.essays <- read.csv('essays9.27.15.csv', sep='|')
df.demo <- read.csv('demog9.11.15.csv')
df.grades <- read.csv('grades9.11.15.csv')

temp <- filter(df.grades, Study == 'Belonging')
temp$est_Grade_date <- '01/01/2011'

df.essays$Intervention_Date <- as.character(df.essays$Intervention_Date)
df.essays[7929:8097,5] <- '01/01/2011' #date originally read just 2011

df.essays[which(df.essays$Intervention_Date=='01.30.08'),]
temp<-df.essays[7582:7786,c(1,4,5)]
temp$Intervention_Date <- as.Date(temp$Intervention_Date, format = '%m.%d.%y')
temp$Intervention_Date <- format(temp$Intervention_Date, '%m/%d/%Y')
df.essays[7582:7786, 5] <- temp$Intervention_Date

df <- left_join(df.essays, df.demo)

df.temp <- data.frame(ID=df$ID,
                      Study=df$Study,
                      Intervention_number=df$Intervention_number,
                      Essay=df$Essay,
                      Condition=df$Condition,
                      Intervention_Date=df$Intervention_Date) 

write.table(df.temp, 'essays9.28.15.csv', sep='|', row.names=F, quote=F)

####### adding the colorado essays

df.col <- read.xlsx('JPSP_affirmation_text.xlsx', sheetName = 'COLORADO')

table(df.temp$Study)
table(df.demo$Study)
##colorado
col.melt <- melt(df.col, id.vars = c('ID', 'Grade.in.school'),
                 measure.vars = c('int1.actual.control.essay.student.wrote',
                                  'int1.actual.treatment.essay.student.wrote',
                                  'int2.actual.control.essay.student.wrote',
                                  'int2.actual.treatment.essay.student.wrote',
                                  'int3.actual.control.essay.student.wrote',
                                  'int3.actual.treatment.essay.student.wrote',
                                  'int1.actual.control.essay.student.wrote.1',
                                  'int1.actual.treatment.essay.student.wrote.1'))

temp <- col.melt[which(col.melt$value!='<NA>'),]
brokenstring <- strsplit(as.character(temp$variable), split='\\.')
intnum <- substr(unlist(lapply(brokenstring, '[[', 1)), 4,4)
temp$intervention <- intnum
temp$condition <- unlist(lapply(brokenstring, '[[', 3))
temp <- temp[,-3]
int4 <- data.frame(ID=df.col$ID, 
                   Grade.in.school=df.col$Grade.in.school,
                   value=df.col$int4.actual.essay.student.wrote,
                   intervention='4',
                   condition=factor(df.col$condition..0...control..1...affirmed., 
                                    labels=c('control', 'treatment')))
temp.col<-rbind(temp, int4)
temp.no8 <- temp.col[which(temp.col$Grade.in.school!='8'),]
temp.no8$Date <- ''
temp.no8$Date[which(temp.no8$intervention=='1')] <- '09/10/08'
temp.no8$Date[which(temp.no8$intervention=='2')] <- '10/11/08'
temp.no8$Date[which(temp.no8$intervention=='3')] <- '01/07/09'
temp.no8$Date[which(temp.no8$intervention=='4')] <- '02/24/09'


col.8 <- read.xlsx('co_8th_grade_only.xls', sheetIndex = 1)
col8.melt <- melt(col.8, id.vars=c('subnum', 'Grade'),
                  measure.vars=c('int1.', 'int2.', 'Int.3.', 'Int.4', 'Int.5'))

intnum <- str_extract(col8.melt$variable, '[0-9]')
col8.melt$intervention <- intnum
names(col8.melt)[1] <- 'ID'
temp <- temp.col[which(temp.col$Grade.in.school=='8'),]
col8.melt <- left_join(col8.melt, temp[,c('ID', 'condition')])
col8.melt$Date[which(col8.melt$intervention=='1')] <- '12/14/07'
col8.melt$Date[which(col8.melt$intervention=='2')] <- '02/28/08'
col8.melt$Date[which(col8.melt$intervention=='3')] <- '04/09/08'
col8.melt$Date[which(col8.melt$intervention=='4')] <- '01/07/09'
col8.melt$Date[which(col8.melt$intervention=='5')] <- '03/02/09'
col8.melt<-distinct(col8.melt)


write.table(col8.melt, 'col8.temp.csv', sep='|', row.names=F)
#I edited a few rows by hand =()
df<-read.csv('col8.temp2.csv', sep='|')
col8.melt$condition <- df$condition
col8.melt <- col8.melt[,-3]

names(temp.no8)[2] <- 'Grade'
df.col <- rbind(col8.melt, temp.no8)

##california
df.cal <- read.xlsx('JPSP_affirmation_text.xlsx', sheetName = 'CALIFORNIA')
sel <- grepl("essay",names(df.cal))
df.cal[sel] <- lapply(df.cal[sel], function(x) as.character(x))
df.cal$essay1.2[is.na(df.cal$essay1.2)] <- ''
df.cal$essay2.2[is.na(df.cal$essay2.2)] <- ''
df.cal$essay3.2[is.na(df.cal$essay3.2)] <- ''
df.cal$essay1[is.na(df.cal$essay1)] <- ''
df.cal$essay2[is.na(df.cal$essay2)] <- ''
df.cal$essay3[is.na(df.cal$essay3)] <- ''

df.cal$essay.time1 <- paste(df.cal$essay1, df.cal$essay2, df.cal$essay3, sep=' ')
df.cal$essay.time2 <- paste(df.cal$essay1.2, df.cal$essay2.2, df.cal$essay3.2, sep=' ')
cal.melt <- melt(df.cal, 
                 id.vars=c('ID','Condition...1...Control..1...Affirmation'),
                 measure.vars=c('essay.time1', 'essay.time2'))


cal.melt$intervention <- substr(cal.melt$variable, 11, 11)
cal.melt$condition <- factor(cal.melt$Condition...1...Control..1...Affirmation, 
                         labels=c('control', 'treatment'))
cal.melt$Date[which(cal.melt$intervention=='1')] <- '09/10/08'
cal.melt$Date[which(cal.melt$intervention=='2')] <- '04/28/09'
cal.melt$Grade <- ''

cal.melt <- cal.melt[,c('ID', 'Grade', 'value', 'intervention', 'condition', 'Date')]

df.coca.long <- rbind(df.col, cal.melt)
write.table(df.coca.long, 'CO_CA_long.csv', sep='|', row.names=F, quote = F)

df <- data.frame(ID=c(as.character(df.temp$ID), as.character(df.coca.long$ID)),
                 Study=c(as.character(df.temp$Study), 
                         rep('CO/CA Latino', length(df.coca.long$ID))),
                 Intervention_number=c(df.temp$Intervention_number, df.coca.long$intervention),
                 Essay=c(as.character(df.temp$Essay), 
                         as.character(df.coca.long$value)),
                 Condition=c(as.character(df.temp$Condition), 
                            as.character(df.coca.long$condition)),
                 Intervention_Date=c(as.character(df.temp$Intervention_Date), 
                                     as.character(df.coca.long$Date)))

write.table(df, 'essays9.29.15.csv', sep='|', row.names=F, quote=F)

######## prompts file creation
test<-read.csv('essays9.29.15.csv', sep='|', quote="")

prompts.schools <- left_join(test, df.demo) %>%
  select(Intervention_number, Condition, Intervention_Date, Study, Cohort) %>%
  filter(Study == 'Connecticut' | Study == 'CO/CA Latino') %>%
  distinct()

prompts.other <- left_join(test, df.demo) %>%
  select(Intervention_number, Condition, Study, Cohort) %>%
  filter(Study != 'Connecticut') %>%
  distinct()

prompts.other <- prompts.other[which(prompts.other$Study!='CO/CA Latino'),]

prompts <- data.frame(Intervention_number=c(as.character(prompts.schools$Intervention_number), 
                                            as.character(prompts.other$Intervention_number)),
                      Condition=c(as.character(prompts.schools$Condition), 
                                  as.character(prompts.other$Condition)),
                      Intervention_date=c(as.character(prompts.schools$Intervention_Date),
                                          rep('', length(prompts.other$Study))),
                      Study=c(prompts.schools$Study, prompts.other$Study),
                      Cohort=c(prompts.schools$Cohort, prompts.other$Cohort))

write.csv(prompts, 'prompts9.29.11.csv')

######## fixing id numbers for CO/CA latino
df.e <- read.csv('../Data/3 CSV Files/essays9.29.15.csv', sep='|', quote="")
df.d <- read.csv('../Data/3 CSV Files/demog9.11.15.csv')

lat.ids <- df.e$ID[which(df.e$Study=='CO/CA Latino')]
lat.ids <- paste(as.character(lat.ids), '.3', sep='')
df.e$ID <- as.character(df.e$ID)
df.e$ID[which(df.e$Study=='CO/CA Latino')] <- lat.ids

## writing cohort number into essay file

df <- left_join(df.e, df.d[,c(1,3)])
df <- df[,1:7]
df$Essay <- as.character(df$Essay)
df<-unique(df)

write.table(df, 'essays9.30.15.csv', sep='|', row.names=F, quote=F)

######## Creating files for RAs to append selected values
library(dplyr)
df.essays <- read.csv('../Data/3 CSV Files/essays9.30.15.csv', sep = '|', quote="")

df.RA1 <- filter(df.essays, Cohort == 'Cohort 3' | 
                   Cohort == 'Cohort 4' | Cohort == 'Cohort 8' ) ##JB
df.RA2 <- filter(df.essays, Cohort == 'Cohort 1' | 
                   Cohort == 'Cohort 2' | Cohort == 'Cohort 5' ) ##TT
df.RA3 <- filter(df.essays, Cohort == 'Cohort 6 ' | 
                   Cohort == 'Cohort 7' | Cohort == 'Cohort 8' ) ##MB
df.RA3 <- filter(df.essays, Study == 'CO/CA Latino') %>%
  bind_rows(df.RA3)

write.table(df.RA1 , 'RA1.csv', sep='|', row.names=F, quote=F)
write.table(df.RA2 , 'RA2.csv', sep='|', row.names=F, quote=F)
write.table(df.RA3 , 'RA3.csv', sep='|', row.names=F, quote=F)

a <- read.csv('../Data/3 CSV Files/RA1a.csv', sep='|', quote="")

######## Getting lab test scores from group affirmation data
df <- read.spss("../Data/Group Affirmation Chapter WORKING FILE-Mar29.sav", to.data.frame = T)

df.melt <- melt(df, id.vars = c('subjectnum', 'interventcond', 'stereotyped'), 
                measure.vars=c('test1numCorrect', 
                               'test2numCorrect', 
                               'test3numCorrect'))
df.melt$subjectnum <- paste(df.melt$subjectnum, '.2', sep = '')
names(df.melt)[1] <- 'ID'
df.outcome.group <- df.essays[which(df.essays$Study == 'Group Affirmation'),
                              c(1,6)]
df.outcome.group <- left_join(df.outcome.group, df.melt)
df.outcome.group$variable <- factor(df.outcome.group$variable, 
                                 labels = c('Lab Test 1', 'Lab Test 2', 
                                            'Lab Test 3'))

df.outcome.group <- data.frame(ID=df.outcome.group$ID,
                               Study=rep('Group Affirmation', 
                                         length(df.outcome.group$ID)),
                               Grade=df.outcome.group$value,
                               Grade_type=df.outcome.group$variable,
                               est_Grade_date=df.outcome.group$Intervention_Date,
                               intervention_year='')

df.outcomes <- rbind(df.outcomes, df.outcome.group)

write.csv(df.outcomes, '../Data/3 CSV Files/grades10.5.15.csv')

######## Prompts
df.prompts <- read.csv('../Data/3 CSV Files/prompts10.5.15.csv', sep='|', quote="")

editdist <- function(charlevels){
  library(stringdist)
  mx <- matrix(nrow = length(charlevels), ncol=length(charlevels))
  for(i in 1:length(charlevels)){
    for(j in 1:length(charlevels)){
      mx[i,j] <- stringdist(charlevels[i], charlevels[j], method='lv')
    }
  }
  mx
}

edit.mat <- editdist(levels(df.prompts$Essay.Prompt))
edit.mat[lower.tri(edit.mat)] <- NA
indicators <- which(edit.mat<10, arr.ind=T)
indicators <- indicators[which(indicators[,1]!=indicators[,2], arr.ind=T), ]
indicators <- indicators[which(!(indicators[,1] %in% indicators[,2])),]

levels(df.prompts$Essay.Prompt)[indicators[,2]] <- levels(df.prompts$Essay.Prompt)[indicators[,1]]

write.table(df.prompts , 'prompts10.6.15.csv', sep='|', row.names=F, quote=F)

df <- read.csv('../Data/3 CSV Files/prompts10.8.15.csv', sep='|', quote="")

df.temp <- filter(df, Study == 'Connecticut')
df.temp <- df.temp[which(df.temp$Essay.Prompt!=''),]
df.temp$Essay.Prompt <- droplevels(df.temp$Essay.Prompt)
length(unique(df.temp$Essay.Prompt))
df.temp$Condition <- droplevels(df.temp$Condition)
levels(df.temp$Condition) <- c(rep('Treat', 2), rep('Control', 8), 
                            rep('Treat', 7))
C <- temp[which(temp$Condition == 'Control'),]
Tr <- temp[which(temp$Condition == 'Treat'),]
######## Appending annotated essays

#function returns paths to all text files in the directory
getannotations <- function(parentdir, filetype){
  dirlist <- list.files(parentdir, pattern='lirsm*')
  essays <- ''
  paths <- ''
  for(i in dirlist){
    esslist <- list.files(paste(parentdir, i, sep='/'), pattern='.*.txt')
    path <- paste(parentdir, i, esslist, sep='/')
    essays <- c(essays, esslist)
    paths <- c(paths, path)
  }
  return(data.frame(essays, paths))
}

path <- '../Data/annotation_tool/My Files/'

ess.file <- getannotations(path) #get the file names
ess.first <- ess.file[which(str_detect(ess.file$essays, '^[0-9]')),] #only keeping the ones that start with anumber (i.e. first round)
file.comp <- str_split(ess.first$essays, pattern = '_') #split filename into id, condition, date, and intervention number
file.comp <- file.comp[-1]#get rid of example essay
ess.first <- ess.first[-1,]#get rid of example essay

#store these details in a dataframe
filedetails <- data.frame(filename=ess.first$essays,
                        id=unlist(lapply(file.comp, '[[', 1)),
                        cond=unlist(lapply(file.comp, '[[', 2)),
                        date=unlist(lapply(file.comp, '[[', 3)),
                        int=unlist(lapply(file.comp, '[[', 4)),
                        path=ess.first$paths)

filedetails$int <- substr(filedetails$int, 1,1) #remove the '.txt.'

#get the text in each file
filedetails$essay <- ''                         
for(i in 1:length(filedetails$path)){
  try(filedetails$essay[i]<-readLines(as.character(filedetails$path[i])))
}           

cleanannotation<-function(essaytext){
  annotation.rex <- "(^\\[\\s*)|(\\s*\\]*$)|(\\\\{1,})"
  gsub(annotation.rex, '', essaytext)
}

correctessay <- function(essaytext){
  #additions
  dol.punct <- "(\\s?)\\$([.:,?;])\\$"
  dol <- "(\\s?)\\$(.*?)\\$"
  essaytext <- gsub(dol.punct, '\\2', essaytext)
  essaytext <- gsub(dol, '\\1\\2', essaytext)
  
  #deletions
  multi.car <- "\\{.*?}\\s?\\^(.*?)\\^"
  single.car <- "(.*)\\s?\\^\\1\\^"
  essaytext <- gsub(multi.car, '', essaytext)
  essaytext <- gsub(single.car, '', essaytext)
  
  #substitutions
  punct <- "[.;,?:]\\s?@([.;,?:])@"
  multi.sub <- "\\{.*?}\\s?@(.*?)@"
  single.sub <- "\\w*'?\\w*['.,]?\\s?@(.*?)@"
  essaytext <- gsub(punct, '\\1', essaytext)
  essaytext <- gsub(multi.sub, '\\1', essaytext)
  essaytext <- gsub(single.sub, '\\1', essaytext)
}

filedetails$annot <- cleanannotation(filedetails$essay)
filedetails$corrected <- correctessay(filedetails$annot)
filedetails$joinme <- paste(filedetails$id, filedetails$int)

path <- '../Data/annotation_tool/New Files/'

og.file <- getannotations(path) #get the file names
og.first <- og.file[which(str_detect(og.file$essays, '^[0-9]')),] #only keeping the ones that start with anumber (i.e. first round)
og.comp <- str_split(og.first$essays, pattern = '_') #split filename into id, condition, date, and intervention number
og.comp <- og.comp[-1]#get rid of example essay
og.first <- og.first[-1,]#get rid of example essay

#store these details in a dataframe
og.details <- data.frame(filename=og.first$essays,
                         id=unlist(lapply(og.comp, '[[', 1)),
                         cond=unlist(lapply(og.comp, '[[', 2)),
                         date=unlist(lapply(og.comp, '[[', 3)),
                         int=unlist(lapply(og.comp, '[[', 4)),
                         path=og.first$paths)

og.details$int <- substr(og.details$int, 1,1) #remove the '.txt.'

#get the text in each file
og.details$essay <- ''                         
for(i in 1:length(og.details$path)){
  try(og.details$essay[i]<-readLines(as.character(og.details$path[i])))
}           

#get original essay text
og.details$essay <- cleanannotation(og.details$essay)

#join based on filename
df.annotations <- left_join(filedetails, og.details, by='filename')
#remove duplicated entries based on filename
df.annotations <- df.annotations[which(!duplicated(df.annotations$filename)),]

df.essays <- read.csv('../Data/essays9.30.15.csv', sep='|')
#join to essays file on essay text
names(df.annotations)[16] <- 'Essay'
df <- left_join(df.essays, df.annotations[,c('annot', 'corrected', 'Essay')], 
                by='Essay')
#remove nan and duplicate essays
df <- df[which(!duplicated(paste(df$Essay, df$ID))),]

#remove the "N/A" character
a <- df$corrected[45]
df$corrected[which(df$corrected==a)] <- NA
table(is.na(df$corrected))

write.table(df, 'annotations10.12.15.csv', sep='|', row.names=F, quote=F)
#fixed some formatting by hand here
df.essays <- read.csv('annotations10.12.15.csv', sep='|',quote="")

###############
#incorporating Emily's work'
#Emily dug through the data to figure out why some participants were mismatched 
#with cohorts. Mostly, it's because they stayed back a grade, but there are a 
#few participants who ended up completing more than one intervention on the same 
#day

df.essays <- read.csv('annotations10.12.15.csv', sep='|',quote="")
questionable.entries <- read.xlsx('../Data/PPIDs with Mismatched Data-Multiple Cohorts.xlsx', 
                                  sheetIndex = 1)
questionable.entries <- questionable.entries[1:631, 1:7]
kosher.entries <- read.xlsx('../Data/PPIDs with Mismatched Data-Multiple Cohorts.xlsx', 
                            sheetIndex = 2) # these should be kept
kosher.entries <- kosher.entries[1:172, 1:7]

emily.filter <- anti_join(questionable.entries, kosher.entries) #these should be removed

names(emily.filter)[c(1,2,5)] <- c('ID', 'Intervention_number', 
                                   'Intervention_Date')
emily.filter <- emily.filter[,1:6]
emily.filter$Intervention_Date <- as.character(emily.filter$Intervention_Date)

df.essays <- anti_join(df.essays, emily.filter, 
                       by= c('ID', 'Intervention_number', 'Essay'))

#This does not seem perfect, but I guess it will do for now.


############################## time to remove blank entries ####################
df.essays <- read.csv('essays10.13.15.csv', sep='|', quote="")
df.essays <- df.essays[which(df.essays$Essay!=''),]

write.table(df.essays, 'essays10.15.15.csv', sep='|', row.names=F, quote=F)

############################# write files for annotating #######################
df.annotate <- read.csv('../Data/3 CSV Files/essays11.12.15.csv', sep='|', quote="")

df.annotate$Intervention_Date <- as.Date(df.annotate$Intervention_Date, 
                                         format='%m/%d/%Y')
df.annotate$Intervention_Date <- format(df.annotate$Intervention_Date, 
                                        "%m-%d-%Y")
levels(df.annotate$Condition)[c(16)] <- c('c.t')

df.annotate <- df.annotate[which(df.annotate$Study=='Connecticut'),]
df.annotate <- df.annotate[which(is.na(df.annotate$corrected)),]

filename <- paste(df.annotate$ID, 
                  df.annotate$Condition,
                  df.annotate$Intervention_Date,
                  df.annotate$Intervention_number,
                  sep='_')
for(i in 1:length(df.annotate$Essay)) {
  text <- paste('[', df.annotate$Essay[i], ']', sep='')
  write(text, file=paste('../Data/ann/',filename[i], '.txt', sep=''))
}

write.table(df.annotate, '../Data/ann/to_annotate.csv', sep='|', row.names=F, 
            quote=F)

############################# get skipped files ################################
path <- '../Data/ann/completed/'

ess.file <- list.files(path) #get the file names
file.comp <- str_split(ess.file, pattern = '_') #split filename into id, condition, date, and intervention number

#store these details in a dataframe
filedetails <- data.frame(filename=ess.file,
                          id=unlist(lapply(file.comp, '[[', 1)),
                          cond=unlist(lapply(file.comp, '[[', 2)),
                          date=unlist(lapply(file.comp, '[[', 3)),
                          int=unlist(lapply(file.comp, '[[', 4)))

filedetails$int <- substr(filedetails$int, 1,1) #remove the '.txt.'

#get the text in each file
filedetails$essay <- ''                         
for(i in 1:length(filedetails$filename)){
  try(
    filedetails$essay[i]<-readLines(
      paste(path, as.character(filedetails$filename[i]), sep='')
    )
  )
} 

filedetails$annot <- cleanannotation(filedetails$essay)
filedetails$corrected <- correctessay(filedetails$annot)

filedetails <- filedetails[which(filedetails$corrected!=''),]
filedetails <- filedetails[which(filedetails$corrected!='N/A'),]

write.table(filedetails, 'toappend.csv', sep='|', row.names=F, quote=F)

df.essays <- read.csv('essays10.15.15.csv', sep='|', quote="")
head(df.essays)

#the only dupes here are from co/ca latino
df.essays[which(duplicated(paste(df.essays$ID, df.essays$Intervention_number, df.essays$Intervention_Date))),]

#no dupes
length(unique(paste(filedetails$id, filedetails$int, filedetails$date)))

#the above are valid joins. Make sure dates are equal formatting
df.essays$Intervention_Date <- as.Date(as.character(df.essays$Intervention_Date), 
                                       format='%m/%d/%Y')
df.essays$Intervention_Date <- format(df.essays$Intervention_Date,
                                      "%m-%d-%Y")
df.essays$join.col <- paste(df.essays$ID, df.essays$Intervention_number, 
                            df.essays$Intervention_Date)
filedetails$join.col <- paste(filedetails$id, filedetails$int, filedetails$date)

test<-left_join(df.essays, filedetails[,c('annot', 'corrected', 'join.col')])

#####check this for consistency!!!!!! Look to see if anything has been mucked up!

length(unique(paste(df.essays$ID, df.essays$Intervention_number, df.essays$Intervention_Date)))



