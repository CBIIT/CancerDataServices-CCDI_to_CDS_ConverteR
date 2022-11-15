#!/usr/bin/env Rscript

#Cancer Data Services - CCDI_to_CDS_converteR v1.3.1


##################
#
# USAGE
#
##################

#This takes a CCDI submission template data file as input that and converts it to a CDS template.

#Run the following command in a terminal where R is installed for help.

#Rscript --vanilla CDDI_to_CDS_converteR.R --help

##################
#
# Env. Setup
#
##################

#List of needed packages
list_of_packages=c("dplyr","readr","stringi","janitor","readxl","optparse","tools",'openxlsx')

#Based on the packages that are present, install ones that are required.
new.packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
suppressMessages(if(length(new.packages)) install.packages(new.packages))

#Load libraries.
suppressMessages(library(dplyr,verbose = F))
suppressMessages(library(readr,verbose = F))
suppressMessages(library(stringi,verbose = F))
suppressMessages(library(janitor,verbose = F))
suppressMessages(library(optparse,verbose = F))
suppressMessages(library(tools,verbose = F))
suppressMessages(library(readxl,verbose = F))
suppressMessages(library(openxlsx,verbose = F))



#remove objects that are no longer used.
rm(list_of_packages)
rm(new.packages)


##################
#
# Arg parse
#
##################

#Option list for arg parse
option_list = list(
  make_option(c("-f", "--file"), type="character", default=NULL, 
              help="CCDI submission template dataset file (.xlsx)", metavar="character"),
  make_option(c("-t", "--template"), type="character", default=NULL, 
              help="CDS submission template file (.xlsx)", metavar="character")
)

#create list of options and values for file input
opt_parser = OptionParser(option_list=option_list, description = "\nCCDI_to_CDS_converteR v2.0.0")
opt = parse_args(opt_parser)

#If no options are presented, return --help, stop and print the following message.
if (is.null(opt$file)&is.null(opt$template)){
  print_help(opt_parser)
  cat("Please supply both the input file (-f) and template file (-t), CDS_submission_metadata_template.xlsx.\n\n")
  suppressMessages(stop(call.=FALSE))
}

#Data file pathway
file_path=file_path_as_absolute(opt$file)

#Template file pathway
template_path=file_path_as_absolute(opt$template)

#A start message for the user that the validation is underway.
cat("The CCDI data template is being converted at this time.\n\n")


###########
#
# File name rework
#
###########

#Rework the file path to obtain a file name, this will be used for the output file.
file_name=stri_reverse(stri_split_fixed(stri_reverse(basename(file_path)),pattern = ".", n=2)[[1]][2])

ext=tolower(stri_reverse(stri_split_fixed(stri_reverse(basename(file_path)),pattern = ".", n=2)[[1]][1]))

path=paste(dirname(file_path),"/",sep = "")



#Output file name based on input file name and date/time stamped.
output_file=paste(file_name,
                  "_CDSTemplate",
                  stri_replace_all_fixed(
                    str = Sys.Date(),
                    pattern = "-",
                    replacement = ""),
                  sep="")

#Read in CDS metadata template
df_metadata=suppressMessages(read_xlsx(path =template_path,sheet = "Metadata"))


# Read in CCDI workbook, and then pull each node page out.
sheet_names=excel_sheets(path = file_path)
sheet_names=sheet_names[!sheet_names %in% c("README and INSTRUCTIONS","Dictionary","Terms and Value Sets")]

workbook_list=list()

#create a list of all node pages with data
for (sheet in sheet_names){
  df=readWorkbook(xlsxFile = file_path,sheet = sheet)
  df_empty_test=df%>%
    select(-type)%>%
    mutate(across(everything(), as.character))
  df_empty_test=remove_empty(df_empty_test,c("rows","cols"))
  
  if (dim(df_empty_test)[1]>0){
    workbook_list=append(x = workbook_list,values = list(df_empty_test))
    names(workbook_list)[length(workbook_list)]<-sheet
  }
}


###########
#
# WORKING PATH DISCOVERY
#
###########

#Flatten list to get all property names
everything=names(unlist(workbook_list, recursive = F))

#Only grab property names if they have multiple "." which notes they are connecting key properties.
everything_links=everything[grep(pattern = ".*\\..*\\..*", x = everything)]

node_path=c()

node_paths=list()

next_node=""

#Go through each available link chain 
for (link in everything_links){
  restart_link=link
  link_counter = 1
  while_counter= 0
  while(while_counter<link_counter){
    node=""
    node_path=c()
    link=restart_link
    while(node!="study"){
      while_counter=while_counter+1
      node=stri_split_fixed(str = link, pattern = '.', n = 2)[[1]][1]
      node_path=c(node_path, node)
      node=stri_split_fixed(str = stri_split_fixed(str = link, pattern = '.', n = 2)[[1]][2], pattern = '.', n = 2)[[1]][1]
      link=everything_links[startsWith(x = everything_links, prefix = paste(node,'.', sep = ""))]
      if (length(link)>1){
        node_path=c(node_path, node)
        link=link[link_counter]
        linking_prop=stri_split_fixed(str = link, pattern = '.', n = 2)[[1]][2]
        node=stri_split_fixed(str = linking_prop, pattern = '.', n = 2)[[1]][1]
        link_counter=link_counter+1
        link=everything_links[startsWith(x = everything_links, prefix = paste(node,'.', sep = ""))]
      }
    }
    node_paths=append(x = node_paths, values = list(node_path))
    names(node_paths)[length(node_paths)]<-stri_split_fixed(str = restart_link, pattern = '.', n = 2)[[1]][1]
  }
}

#Determine each path and end with the study node. THIS ASSUMES STUDY NODE IS THE TOP MOST TERMINAL NODE.
for (vec in 1:length(names(node_paths))){
  vec_node= node_paths[vec][[1]][1]
  node_paths[vec][[1]]=c(node_paths[vec][[1]],"study")
  names(node_paths)[vec]<-vec_node
}

#Determine the longest path
max_num=0
link_vec=c()

for (paths in 1:length(node_paths)){
  num=length(node_paths[[paths]])  
  if (num > max_num){
    max_num=num
  }
}

#Create a data frame with the longest path as columns
links=data.frame(matrix(data = NA,nrow = 0,ncol = max_num))
links_add=data.frame(matrix(data = NA,nrow = 1,ncol = max_num))

#Add each path as a row for columns in the data frame, with the longest path being in the first column and all paths ending in the last column with STUDY.
for (paths in 1:length(node_paths)){
  link_vec=node_paths[[paths]]
  num=length(node_paths[[paths]]) 
  if (num < max_num){
    num_add=max_num-num
    link_vec=c(rep(NA,num_add),link_vec)
  }
  for(x in 1:max_num){
    links_add[x]=link_vec[x]
  }
  links=rbind(links, links_add)
}


###############
#
# Data path connection via walking
#
###############

list_of_all=workbook_list
done_nodes=c()

#For each column, starting at the longest path, work through the connections at that level, for each connection in that node. Apply and expand that information to the parent node and then remove that node from the pool of possibilities, if it shows up later in the sequence of paths.
for (col_num in 1:(dim(links)[2]-1)){
  working_rows=grep(pattern = TRUE, x = !is.na(links[,col_num]))
  working_rows_names=unique(links[,col_num][working_rows])
  working_rows_names=working_rows_names[!working_rows_names %in% done_nodes]
  for (node in working_rows_names){
    done_nodes=c(done_nodes, node)
    df_mod_level=list_of_all[node][[1]]
    df_mod_level_names=colnames(df_mod_level)
    remove_node_splits=grep(pattern = "\\.", df_mod_level_names)
    for (node_split in remove_node_splits){
      df_mod_level=list_of_all[node][[1]]
      next_node=unlist(stri_split_fixed(str = df_mod_level_names[node_split],pattern = "."))[1]
      new_name=unlist(stri_split_fixed(str = df_mod_level_names[node_split],pattern = "."))[2]
      colnames(df_mod_level)[node_split]=new_name
      if (length(remove_node_splits)>1){
        df_mod_level=df_mod_level[,-remove_node_splits[!remove_node_splits %in% node_split]]
      }
      df_mod_level=df_mod_level[!is.na(df_mod_level[new_name]),]
      df_mod_level=remove_empty(dat = df_mod_level, which = "cols")
      df_mod_level=unique(df_mod_level)
      colname_test=grep(pattern = TRUE, (colnames(df_mod_level)%in%colnames(list_of_all[next_node][[1]])))
      #This path is if there are 4 or more columns names that are in common with the parent data frame. This is to avoid duplicate columns for data file nodes (file_name_1, file_name_2, instead of just file_name). This does then assume that most nodes that are not data file nodes contain their own unique columns.
      if (length(colname_test)>3){
        df_edit=list_of_all[next_node][[1]]
        df_edit_level_only= df_edit[,colnames(df_edit) %in% colnames(workbook_list[next_node][[1]])]
        df_edit_level_only= remove_empty(dat = df_edit_level_only, c("rows"))
        df_edit_level_only=suppressMessages(left_join(df_edit_level_only,df_mod_level))
        list_of_all[next_node][[1]]=bind_rows(list_of_all[next_node][[1]],df_edit_level_only)
      }else{
        df_edit=list_of_all[next_node][[1]]
        df_edit=suppressMessages(left_join(df_edit,df_mod_level))
        list_of_all[next_node][[1]]=df_edit
      }
    }
  }
}

#Pull out the new data frame from the amalgamation of all data frames being applied to the study data frame.
df_all=list_of_all["study"][[1]]
df_all=remove_empty(dat = df_all, which = "cols")
df_all=unique(df_all)

#Break out the PI and Co-PI columns. 
PI_pos=grep(pattern = "PI", df_all$personnel_type)
CO_PI_pos=grep(pattern = "Co-PI",x = df_all$personnel_type)
PI_pos=PI_pos[!PI_pos %in% CO_PI_pos]

if (length(PI_pos)>0){
  PIs=unique(df_all$personnel_name[PI_pos])
  PI_email=unique(df_all$email_address[PI_pos])
  df_all$primary_investigator_name=PIs
  df_all$primary_investigator_email=PI_email
}

if (length(CO_PI_pos)>0){
  COPIs=unique(df_all$personnel_name[CO_PI_pos])
  COPI_email=unique(df_all$email_address[CO_PI_pos])
  df_all$co_primary_investigator_name=COPIs
  df_all$co_primary_investigator_email=COPI_email
}

#Remove the different personnel columns that are from CCDI.
personnel_cols=c('personnel_type','personnel_name','email_address','study_personnel_id')

for (personnel in personnel_cols){
  if (personnel %in% colnames(df_all)){
    df_all=select(df_all,-all_of(personnel))
  }
}

#Ensure the new data frame is unique.
df_all=unique(df_all)

#Progress bar setup
pb=txtProgressBar(min=0,max=length(unique(df_all$file_url_in_cds)),style = 3)
position=0

# If there are some properties for a file, that is not found in the other row with the same file url, make them equal. The url was chosen as this ensures that there are no name clashes as they have to exist in a unique location, while file name would only do a portion of this, and duplicates of a form can give the same md5sum. This will then unique out in a later step.
for (file_url in unique(df_all$file_url_in_cds)){
  position=position+1
  setTxtProgressBar(pb,position)
  if (!is.na(file_url)){
    file_pos=grep(pattern = file_url, x = df_all$file_url_in_cds)
    if (length(file_pos)>1){
      for(row_num in file_pos){
        for(col_num in 1:dim(df_all)[2]){
          row_val=df_all[row_num,col_num]
          if(is.na(row_val)){
            new_row_num=file_pos[!file_pos %in% row_num]
            new_row_val=unique(df_all[new_row_num,col_num])
            new_row_val=new_row_val[!is.na(new_row_val)]
            if (length(new_row_val)==1){
              df_all[row_num,col_num]<-new_row_val
            }
          }
        }
      }
    }  
  }
}

#Remove lines that don't have files in a bucket, as the CDS template is specific for indexing and need a location.
df_all=df_all[!is.na(df_all$file_url_in_cds),]

df_all=unique(df_all)

#Create a data frame based on the template that was supplied.
df_metadata_add=data.frame(matrix(NA,nrow=dim(df_all)[1],ncol = dim(df_metadata)[2]))
colnames(df_metadata_add)<-colnames(df_metadata)

#Move data from the CCDI flatten template over to the CDS template where it is 1:1 colnames.
for (colname in colnames(df_metadata)){
  if (any(colnames(df_all) %in% colname)){
    df_metadata_add[colname]=df_all[colname]
  }
}

#For column names that differ from CCDI to CDS
#This will have to be a hard coded list of columns:
df_metadata_add$bases=df_all$number_of_bp
                 


#Check the number of files that were imported and in the output. The CDS template is file oriented, so the two checks should return the same number.
input_file_length=length(grep(pattern = "file_url_in_cds", x = names(unlist(workbook_list, recursive = T))))
output_file_length=length(df_metadata_add$file_url_in_cds)

if(input_file_length!=output_file_length){
  cat(paste("\nThe number of files in the input file, ",input_file_length,", does not equal the number of files in the output file, ",output_file_length,".\n\nThis is likely due to incorrect or missing links between nodes. Please check to make sure the expected link ids are present and the strings are identical between nodes.\n\n",sep = ""))
}


############
#
# Write out to CDS template
#
############

#Open the template, create a workbook, apply the data, write out a new output file based on the template. 
wb=openxlsx::loadWorkbook(file = template_path)

deleteData(wb, sheet = "Metadata",rows = 1:dim(df_metadata)[1],cols=1:dim(df_metadata)[2],gridExpand = TRUE)

writeData(wb=wb, sheet="Metadata", df_metadata_add)

openxlsx::saveWorkbook(wb = wb,file = paste(path,output_file,".xlsx",sep = ""), overwrite = T)

cat(paste("\n\nProcess Complete.\n\nThe output file can be found here: ",path,"\n\n",sep = "")) 
