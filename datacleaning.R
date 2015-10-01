#### this is an ongoing document in which I record data cleaning procedures for the affirmation work
library(reshape2)
library(plyr)
library(dplyr)
library(stringr)

################## BMI STUDY
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
library(xlsx)

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
#I edited a few rows by hand
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

######## Appending annotated essays

getannotations <- function(parentdir){
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

d <- '../Data/annotation_tool/My Files/'

ess.file <- getannotations(d)
ess.first <- ess.file[which(str_detect(ess.file$essays, '^[0-9]')),]
file.comp <- str_split(ess.first$essays, pattern = '_')
file.comp <- file.comp[-1]
ess.first <- ess.first[-1,]
filedetails <- data.frame(filename=ess.first$essays,
                        id=unlist(lapply(file.comp, '[[', 1)),
                        cond=unlist(lapply(file.comp, '[[', 2)),
                        date=unlist(lapply(file.comp, '[[', 3)),
                        int=unlist(lapply(file.comp, '[[', 4)),
                        path=ess.first$paths)

filedetails$int <- substr(filedetails$int, 1,1)

filedetails$essay <- ''                         
for(i in 1:length(filedetails$path)){
  try(filedetails$essay[i]<-readLines(as.character(filedetails$path[i])))
}           
