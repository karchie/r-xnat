library(RCurl)
library(XML)
  
setClass('XNAT',
         representation(baseURL = "character", jsid = "character"),
         validity = function (object) {
           passCondition <- function (w) w
           tryCatch(is.character(request(object, '/data/version')),
                    warning = passCondition,
                    error = passCondition)
         })

setMethod('initialize',
          signature('XNAT'),
          function (.Object, baseURL, jsid) {
            .Object@baseURL = baseURL
            .Object@jsid = jsid
            .Object
          });


XNAT <- function(baseURL, username = NULL, password = NULL) {
  reader <- basicTextGatherer()
  header <- basicTextGatherer()
  if (is.null(username)) {  # try no-login jsessionid
    curlPerform(url = paste(baseURL, '/', sep = ''), 
                writefunction = reader$update, 
                headerfunction = header$update, 
                ssl.verifypeer = FALSE)
    jscookie <- grep('^Set-Cookie: JSESSIONID=',
                     strsplit(header$value(), '\n')[[1]],
                     value = TRUE)
    if (is.null(jscookie)) {
      stop('unable to retrieve JSESSIONID from host')
    }
    jsessionid <- strsplit(jscookie, '[=;]')[[1]][[2]]
  } else {
    if (is.null(password)) {
      have.tcltk <- TRUE
      tryCatch(library(tcltk),
               error = function(e) have.tcltk <<- FALSE)
      if (!have.tcltk) {
        stop('need Tcl/Tk to prompt for password')
      }
      tt <- tktoplevel()
      tktitle(tt) <- 'XNAT Password'
      lab <- tklabel(tt, text = 'Password:')
      pwVar <- tclVar()
      pw <- tkentry(tt, textvariable = pwVar, show = '*')
      but <- tkbutton(tt, text = 'OK', command = function() {
        tkgrab.release(tt)
        tkdestroy(tt)
      })
      tkgrid(lab, pw, but)
      tkfocus(tt)
      tkwait.window(tt)
      password <- tclvalue(pwVar)
    }
    curlPerform(url = paste(baseURL, '/data/JSESSION', sep = ''), 
                writefunction = reader$update, 
                headerfunction = header$update, 
                ssl.verifypeer = FALSE, 
                userpwd = paste(username, password, sep = ':'))
    status <- parseHTTPHeader(header$value())['status']
    if(status == 401) {
      stop('bad username/password')
    } else if(status != 200) {
      stop('error authenticating')
    }
    jsessionid <- reader$value()
  }
  if(is.null(jsessionid)) {
    stop('unable to establish XNAT server session')
  }
  new('XNAT', baseURL=baseURL, jsid=jsessionid)
}
  

setGeneric('request', function (.Object, ...) standardGeneric('request'))
setMethod('request',
          signature('XNAT'),
          function (.Object, request, method = 'GET', entity = '') {
            if (is.null(.Object@jsid))
              stop('not connected')
            reader <- basicTextGatherer()
            header <- basicTextGatherer()
            curlPerform(url = paste(.Object@baseURL, request, sep = ''), 
                        writefunction = reader$update, 
                        headerfunction = header$update, 
                        customrequest = method,
                        postfields = entity, 
                        ssl.verifypeer = FALSE, 
                        cookie = paste('JSESSIONID=', .Object@jsid, sep = ''))
            if (parseHTTPHeader(header$value())['status'] != 200) {
              stop(paste('request failed:', header$value()))
            }
            return(reader$value())
          })

setGeneric('getTypes', function (.Object) standardGeneric('getTypes'))
setMethod('getTypes', signature('XNAT'),
          function (.Object) {
            text <- request(.Object, '/data/search/elements?format=csv')
            table <- .csv.from.string(text)
            table <- table[,c('ELEMENT_NAME','SINGULAR')]
            table <- .frame.columns.rename(table, 'ELEMENT_NAME'='type', 'SINGULAR'='name')
          })

setGeneric('getFields', function (.Object, ...) standardGeneric('getFields'))
setMethod('getFields', signature('XNAT'),
          function (.Object, xsitype) {
            response <- request(.Object, sprintf('/data/search/elements/%s?format=csv', xsitype))
            table <- .csv.from.string(response)
            table <- table[,c('FIELD_ID', 'DESC')]
            table <- table[grep('PROJECT_IDENTIFIER=', table$FIELD_ID, invert=TRUE),]
            table <- table[grep('_FIELD_MAP=', table$FIELD_ID, invert=TRUE),]
            ns <- toupper(sub(':\\w+', '', xsitype))
            table <- table[grep(sprintf('^%s_COL_', ns), table$FIELD_ID, invert=TRUE),]
            .frame.columns.rename(table, 'FIELD_ID'='name', 'DESC'='description')
          })

setGeneric('getProjects', function (.Object) standardGeneric('getProjects'))
setMethod('getProjects', signature('XNAT'),
          function (.Object) {
            text <- request(.Object, '/data/projects?format=csv')
            projects <- .csv.from.string(text)
            projects <- projects[with(projects, order(ID)),]
          })

setGeneric('getSubjects', function (.Object, ...) standardGeneric('getSubjects'))
setMethod('getSubjects', signature('XNAT'),
          function (.Object, project = NULL) {
            projects <- getProjects(.Object)
            if (!is.null(project) && !project %in% projects$ID) {
              stop(sprintf('unknown project "%s"', project))
            } 
            fields <- c('PROJECT','ID','LABEL','GENDER_TEXT','HANDEDNESS_TEXT',
                        'DOB','EDUC','SES','SUB_GROUP','RACE','ETHNICITY')
            if (!is.null(project)) {
              project <- c('PROJECT','=',project, 'sharing/share/project','=',project)
            }
            text <- request(.Object, '/data/search?format=csv',
                            method = 'POST',
                            entity=.xnat.search.xml.build('xnat:subjectData', fields,
                              constraints=project))
            table <- .csv.from.string(text)

            ## we don't want some of the columns; drop these
            table <- subset(table, select=-c(quarantine_status, subject_id))

            ## some of the columns returned from XNAT are weirdly named; rename these
            table <- .frame.columns.rename(table,
                                           'subjectid'='ID',
                                           'xnat_col_subjectdatalabel'='label',
                                           'gender_text'='gender',
                                           'handedness_text'='handedness',
                                           'educ'='education',
                                           'sub_group'='group')
            
            table <- table[with(table, order(project, label)),]
          })
          

kJoinedSubjectRenames <- list('xnat_subjectdata_subject_label'='subject_label',
                              'xnat_subjectdata_project'='subject_project')
kJoinedSubjectDiscards <- c('xnat_subjectdata_subjectid',
                            'xnat_subjectdata_insert_user',
                            'xnat_subjectdata_insert_date',
                            'xnat_subjectdata_projects')

setGeneric('getData', function (.Object, ...) standardGeneric('getData'))
setMethod('getData',
          signature('XNAT'),
          function (.Object, type, project=NULL, add.fields=NULL) {
            if (!is.null(project)) {
              project <- c('PROJECT','=',project, 'sharing/share/project','=',project)
            }
            text <- request(.Object, '/data/search?format=csv',
                            method = 'POST',
                            entity = .xnat.search.xml.build(type,
                              getFields(.Object, type)$name,
                              project,
                              add.fields = add.fields))
            table <- .csv.from.string(text)
            ns <- tolower(sub(':\\w+', '', type))
            nstype <- tolower(sub('\\w+:', '', type))
            subjlabel <- sprintf('xnat_subjectdata_subject_label', ns, nstype)
            
            table <- do.call(.frame.columns.rename, c(list(table), kJoinedSubjectRenames))
            
            if (type != 'xnat:subjectData') {
              subject.fields <- matrix(as.list(add.fields), nrow=2, byrow=TRUE)
              subject.fields <- subject.fields[subject.fields[1] == 'xnat:subjectData'][2]
              discards <- subset(kJoinedSubjectDiscards, 
                                 subset=!(subject.fields %in% kJoinedSubjectDiscards))
              table <- table[,!(colnames(table) %in% discards)]
            }
          })

setGeneric('doStoredSearch', function(.Object, ...) standardGeneric('doStoredSearch'))
setMethod('doStoredSearch', signature('XNAT'),
          function (.Object, searchID) {
            text <- request(.Object, paste('/data/search/saved/',
                                           searchID,
                                           '/results?format=csv',
                                           sep = ''))
            return(.csv.from.string(text))
          })

setMethod('close', signature('XNAT'),
          function (con) {
            request(con, '/data/JSESSION', method='DELETE')
            con@jsid <- as.character(NULL)
          })


kOmitSubjectTypes <- c('xnat:subjectData')

### fields is a vector of field names, all in type xsitype
### add.fields is NULL or a vector or pairs: xsitype, field, xsitype, field, ...
### constraints is NULL or a vector of triplets: field, constraint-op, value, ...
.xnat.search.xml.build <- function (xsitype, fields, add.fields = NULL, constraints = NULL) {
  header <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
  <xdat:search allow-diff-columns="0" secure="false"
  brief-description="generated by xnat.R"
  xmlns:xdat="http://nrg.wustl.edu/security"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <xdat:root_element_name>%s</xdat:root_element_name>', xsitype)
  footer <- '\n</xdat:search>'

  fields.m <- cbind(xsitype, fields, deparse.level = 0)
  if (! xsitype %in% kOmitSubjectTypes) {
    add.fields <- c(add.fields, 'xnat:subjectData', 'SUBJECT_LABEL')
  }
  if (! is.null(add.fields)) {
    fields.m <- rbind(fields.m, matrix(add.fields, ncol=2, byrow=TRUE))
  }
  
  fields.text <- mapply(function(type, field, index) {
    sprintf('
<xdat:search_field>
  <xdat:element_name>%s</xdat:element_name>
  <xdat:field_ID>%s</xdat:field_ID>
  <xdat:sequence>%d</xdat:sequence>
</xdat:search_field>', type, field, index)
  }, fields.m[,1], fields.m[,2], seq(0, nrow(fields.m)-1),
                        SIMPLIFY=TRUE, USE.NAMES=FALSE)
  
  if (! xsitype %in% kOmitSubjectTypes) {
    fields.text <- c(fields.text, sprintf('
<xdat:search_field>
  <xdat:element_name>xnat:subjectData</xdat:element_name>
  <xdat:field_ID>SUBJECT_LABEL</xdat:field_ID>
  <xdat:sequence>%s</xdat:sequence>
</xdat:search_field>', length(fields)))
  }
  
  constraint.text <- if (is.null(constraints) || 0 == length(constraints))
    ''
  else {
    constraint.matrix <- matrix(constraints, ncol=3, byrow=TRUE)
    c('<xdat:search_where method="OR">',
      mapply(function(field, comparison, value) {
        sprintf('
  <xdat:criteria override_value_formatting="0">
    <xdat:schema_field>%s/%s</xdat:schema_field>
    <xdat:comparison_type>%s</xdat:comparison_type>
    <xdat:value>%s</xdat:value>
  </xdat:criteria>', xsitype, field, comparison, value)
      }, constraint.matrix[,1], constraint.matrix[,2], constraint.matrix[,3]),
      '</xdat:search_where>')
  }
  
  do.call(paste0, as.list(c(header, fields.text, constraint.text, footer)))
}


.csv.from.string <- function (string) {
  c <- textConnection(string)
  csv <- read.csv(c, header = TRUE, as.is = TRUE)
  close(c)
  
  return(csv)
}

.frame.columns.rename <- function (frame, ...) {
  renames <- list(...)
  to.rename <- colnames(frame) %in% names(renames)
  colnames(frame)[to.rename] <- lapply(colnames(frame)[to.rename],
                                       function(name) renames[[name]])
  frame
}

# eof
