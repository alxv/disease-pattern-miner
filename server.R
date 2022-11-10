options(shiny.maxRequestSize = 400*1024^2)
demo=fread('./Data/Demo_Serendip.csv')

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))


server <- function(input, output, session) {
#===============================================================================
  #                                         SERENDIP
#===============================================================================
    
#====================================Input check================================
  DATA_format=reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    transactions <- read.csv(inFile$datapath)
    required_columns <- c('id', 'date','item')
    column_names <- colnames(transactions)
    max_columns <- 3
    
    shiny::validate(
      need(ncol(transactions) <= max_columns, "Your data has too many columns"),
      need(all(required_columns %in% column_names), "check column names please!")
    )
    'Data format check passed...'
  })
  ##
  output$checkminSEQ=renderText({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    dfs=DATA_format()
    if(dfs=='Data format check passed...'){
      'Data format check passed...'
    } else {'Mining stoped!'}
  })
#=================================Serendip Frequency plot=======================
  output$Srn_frquency = renderPlot({
    
    SRNi_inFile <- input$file1
    if (is.null(SRNi_inFile))
      return(NULL)
    SRNfrq_dfs=DATA_format()
    if(SRNfrq_dfs=='Data format check passed...'){
      retail_srn_frq = read.csv(SRNi_inFile$datapath)
      retail_srn_frq[retail_srn_frq==""]<-NA
      retail_srn_frq=drop_na(retail_srn_frq)
      retail_srn_frq=retail_srn_frq %>% dplyr::select(id, date, item)
      retail_srn_frq = retail_srn_frq %>% distinct()
      transactionData = retail_srn_frq %>% group_by(id, date) %>% summarise_all(funs(paste(na.omit(.), collapse = ",")))
      
      transactionData$id = NULL
      transactionData$date = NULL
      colnames(transactionData) <- c("items")
      items <- strsplit(as.character(transactionData$items), ",")
      trans <- as(items, "transactions")
      
      # frequency plot
      arules::itemFrequencyPlot(trans, topN = 10,
                                col = brewer.pal(8, 'Pastel2'),
                                main = 'Absolute Item Frequency Plot',
                                type = "absolute",
                                ylab = "Frequency (absolute)")
      
      
    } else {'No plot generated'}
    
  })
  #====================================Master===================================
  
  Master_data=reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    transactions <- read.csv(inFile$datapath)
    required_columns <- c('id', 'date','item')
    column_names <- colnames(transactions)
    max_columns <- 3
    
    shiny::validate(
      need(ncol(transactions) <= max_columns, "Your data has too many columns"),
      need(all(required_columns %in% column_names), "check column names please!")
    )
    ############File reading completed here
    colnames(transactions)=c('SID', 'EID','ITEMS')
    transactions=transactions%>% group_by(SID)%>% arrange(EID)%>% distinct(ITEMS, .keep_all = T)
    
    trans_sequence <- transactions %>%
      group_by(SID, EID) %>%
      summarize(
        SIZE = dplyr::n(),
        ITEMS = paste(as.character(ITEMS), collapse = ',')
      )
    
    ## til date present after this data disappears
    trans_sequence$EID=as.Date(trans_sequence$EID,format='%d/%m/%Y')
    #===========================================================================
    # Two or more disease
    InitailData=trans_sequence
    colnames(InitailData)=c('SID','EID','Count','ITEMS')
    
    InitailData=InitailData%>% group_by(SID)%>% mutate('order'=seq.int(ITEMS))
    oneDisease=InitailData%>% filter(Count==1)
    twoandMore=InitailData%>% filter(Count>=2)
    #for more than two disease conditions
    twoandMore$ITEMS=sapply(twoandMore$ITEMS, gsub, pattern = ",", replacement= "and")
    
    # combine one and two more
    ProperData=rbind(oneDisease,twoandMore)
    ProperData=ProperData%>% arrange(SID, order)
    ProperDataPat_2more=ProperData%>% filter(order>=2)%>% distinct(SID)
    Fdata=inner_join(ProperDataPat_2more,ProperData)
    
    #===========================================================================
    #5. Mining Patterns
    sampleData=Fdata
    sampleData$order=NULL
    colnames(sampleData)=c('SID','EID','Count','ITEMS')
    sampleData=sampleData%>% group_by(SID)%>%arrange(SID,EID)
    sampleData$EID=NULL
    sampleData$Count=NULL
    sampleData=sampleData%>% group_by(SID)%>% mutate('EID'=seq.int(ITEMS))
    
    #Sequence format
    # subsequent pasting function
    Adding_seq = function(x, .sep = ",") 
      Reduce(function(x1, x2) paste(x1, x2, sep = .sep), x, accumulate = TRUE)
    #===========================================================================
    Seq=sampleData
    LL=ave(as.character(Seq$ITEMS), Seq$SID, FUN = Adding_seq)
    LL=as.data.frame(LL)
    Seq$ITEMS=NULL
    Seq$ITEMS=LL$LL
    Seq=setDT(Seq)[, .SD[which.max(EID)], SID]
    Seq$EID=NULL
    Seq$Diseases=str_count(Seq$ITEMS, ',')
    Seq$Diseases=Seq$Diseases+1
    #Seq
    # Max event
    maxevent=max(sampleData$EID)
    maxevent
    # Occurrence Matrix
    EID_items=sampleData%>% ungroup(SID)%>% select(EID, ITEMS)
    OccurenceMatrix=as.matrix(table(EID_items))
    #===========================================================================
    #Conversion of matrix into data frame for further use
    OMdataFrame=data.frame(OccurenceMatrix)
    # Threshold sigma
    sigma=1
    #===========================================================================
    # One item set
    #OccurenceMatrix[1, ]  # row first, quoted column name second, and order does matter
    I1=OMdataFrame%>% filter(EID==1, Freq>=1)
    I1_selected=as.vector(I1$ITEMS)
    #===========================================================================
    #Two item set
    k=2
    K2=OMdataFrame%>% filter(EID==k, Freq>=sigma)
    I2_selected=as.vector(K2$ITEMS)
    #===========================================================================
    #Function to extract first two words
    string_fun_2 <- function(x) {
      ul = unlist(strsplit(x, split = ","))[1:2]
      paste(ul,collapse=",")
    }
    #===========================================================================
    # Find ordered match 
    Seq=Seq%>%filter(Diseases>=2)
    totalPats=nrow(Seq)
    totalPats_2=totalPats
    Seq$ITEMS=as.character(Seq$ITEMS)
    Seq=Seq%>% group_by(SID)%>% mutate(two=string_fun_2(ITEMS))
    S2=Seq%>% ungroup()%>%select(two)%>%distinct(two)%>% arrange(two)
    sup_2=as.data.frame(table(Seq$two))
    colnames(sup_2)=c('Rule', 'support')
    sup_2_for3=sup_2
    sup_2_for3=as.data.frame(sup_2_for3)
    #===========================================================================
    #Function to extract first one word
    string_fun_1 <- function(x) {
      ul = unlist(strsplit(x, split = ","))[1:1]
      paste(ul,collapse=",")
    }
    #===========================================================================
    #ADD X
    X=S2%>% select(two)%>%rowwise() %>%mutate(ITEMS=string_fun_1(two))
    X=X%>% select(ITEMS)
    X=left_join(X,I1)
    sup_2=cbind(sup_2, X$Freq)
    colnames(sup_2)[3]='X_Freq'
    
    sup_2$Rule=gsub(",", "=>", sup_2$Rule)
    #===========================================================================
    #ADD Y
    #Function to extract y2 alone one word
    string_fun_y2 <- function(x) {
      ul = unlist(strsplit(x, split = ","))[2:2]
      paste(ul,collapse=",")
    }
    #===========================================================================
    y2=S2%>% select(two)%>%rowwise()%>%mutate(ITEMS=string_fun_y2(two))
    y2=y2%>%select(ITEMS)
    
    y2=left_join(y2,K2)
    y2=y2%>% rename(Y_Freq=Freq)
    sup_2=cbind(sup_2,y2$Y_Freq)
    colnames(sup_2)[4]='Y_Freq'
    #support for x,y. Confidence and Lift
    sup_2=as.data.frame(sup_2)
    sup_2$X_Freq=as.numeric(sup_2$X_Freq)
    sup_2=sup_2%>%mutate(x_sup=X_Freq/totalPats)
    
    sup_2$Y_Freq=as.numeric(sup_2$Y_Freq)
    sup_2=sup_2%>%mutate(y_sup=Y_Freq/totalPats)
    
    sup_2$support=as.numeric(sup_2$support)
    sup_2=sup_2%>%mutate(XY_sup=support/totalPats)
    
    sup_2=sup_2%>%mutate(Confidence=XY_sup/x_sup)
    sup_2=sup_2%>%mutate(Lift=Confidence/y_sup)
    sup_2=sup_2%>% filter(support>=1)
    sup_2=sup_2%>%mutate(Patterns_of='Two')
    #===========================================================================
    # Three item set
    I2=OMdataFrame%>% filter(EID==2, Freq>=1)
    I2_selected=as.vector(I2$ITEMS)
    k=3
    K3=OMdataFrame%>% filter(EID==k, Freq>=sigma)
    #===========================================================================
    #Function to extract first three words
    string_fun_3 <- function(x) {
      ul = unlist(strsplit(x, split = ","))[1:3]
      paste(ul,collapse=",")
    }
    #===========================================================================
    # Find ordered match
    Seq=Seq%>% filter(Diseases>=3)
    totalPats=nrow(Seq)
    totalPats_3=totalPats
    Seq=Seq%>% group_by(SID)%>% mutate(three=string_fun_3(ITEMS))
    S3=Seq%>% ungroup()%>%select(three)%>%distinct(three) %>%arrange(three)
    
    sup_3=as.data.frame(table(Seq$three))
    colnames(sup_3)=c('Rule', 'support')
    sup_3_for4=sup_3
    sup_3_for4=as.data.frame(sup_3_for4)
    sup_3$Rule=sub(",([^,]*)$"," =>\\1", sup_3$Rule)#last comma
    sup_3=as.data.frame(sup_3)
    #===========================================================================
    #ADD X
    X3=as.data.frame(table(Seq$two))
    sup_3_for4$Rule=as.character(sup_3_for4$Rule)
    sup_3_for4=sup_3_for4%>% rowwise()%>% mutate(onlytwo=string_fun_2(Rule))
    
    sup_3_for4=X3%>% rename(onlytwo=Var1)%>% left_join(sup_3_for4)
    
    sup_3=cbind(sup_3, sup_3_for4$Freq)
    colnames(sup_3)[3]='X_Freq'
    sup_3$X_Freq=as.numeric(sup_3$X_Freq)
    #===========================================================================
    #ADD Y
    #Function to extract y3 alone one word
    string_fun_y3 <- function(x) {
      ul = unlist(strsplit(x, split = ","))[3:3]
      paste(ul,collapse=",")
    }
    #===========================================================================
    y3=S3%>% select(three)%>%rowwise()%>%mutate(ITEMS=string_fun_y3(three))
    y3=y3%>% ungroup()%>%select(ITEMS)
    y3=left_join(y3,K3)
    y3=y3%>% rename(Y_Freq=Freq)
    sup_3=cbind(sup_3,y3$Y_Freq)
    colnames(sup_3)[4]='Y_Freq'
    #===========================================================================
    #support for x,y. Confidence and Lift
    sup_3=as.data.frame(sup_3)
    sup_3$X_Freq=as.numeric(sup_3$X_Freq)
    sup_3=sup_3%>%mutate(x_sup=X_Freq/totalPats)
    
    sup_3$Y_Freq=as.numeric(sup_3$Y_Freq)
    sup_3=sup_3%>%mutate(y_sup=Y_Freq/totalPats)
    
    sup_3$support=as.numeric(sup_3$support)
    sup_3=sup_3%>%mutate(XY_sup=support/totalPats)
    
    sup_3=sup_3%>%mutate(Confidence=XY_sup/x_sup)
    sup_3=sup_3%>%mutate(Lift=Confidence/y_sup)
    sup_3=sup_3%>% filter(support>=1)
    sup_3=sup_3%>%mutate(Patterns_of='Three')
    #===========================================================================
    # Four item set
    k=4
    K4=OMdataFrame%>% filter(EID==k, Freq>=sigma)
    #===========================================================================
    #Function to extract first four words
    string_fun_4 <- function(x) {
      ul = unlist(strsplit(x, split = ","))[1:4]
      paste(ul,collapse=",")
    }
    #===========================================================================
    # Find ordered match
    Seq=Seq%>% filter(Diseases>=4)
    totalPats=nrow(Seq)
    totalPats_4=totalPats
    Seq=Seq%>% group_by(SID)%>% mutate(four=string_fun_4(ITEMS))
    S4=Seq%>% ungroup()%>%distinct(four)%>%select(four)%>%arrange(four)
    
    sup_4=as.data.frame(table(Seq$four))
    
    colnames(sup_4)=c('Rule', 'support')
    sup_4_for5=sup_4
    sup_4_for5=as.data.frame(sup_4_for5)
    sup_4$Rule=sub(",([^,]*)$"," =>\\1", sup_4$Rule)#last comma
    sup_4=as.data.frame(sup_4)
    #===========================================================================
    #ADD X
    X4=as.data.frame(table(Seq$three))
    sup_4_for5$Rule=as.character(sup_4_for5$Rule)
    sup_4_for5=sup_4_for5%>% rowwise()%>% mutate(onlythree=string_fun_3(Rule))
    
    sup_4_for5=X4%>% rename(onlythree=Var1)%>% left_join(sup_4_for5)
    
    sup_4=cbind(sup_4, sup_4_for5$Freq)
    colnames(sup_4)[3]='X_Freq'
    sup_4$X_Freq=as.numeric(sup_4$X_Freq)
    #===========================================================================
    #ADD Y
    #Function to extract y4 alone one word
    string_fun_y4 <- function(x) {
      ul = unlist(strsplit(x, split = ","))[4:4]
      paste(ul,collapse=",")
    }
    #===========================================================================
    y4=S4%>% select(four)%>%rowwise()%>%mutate(ITEMS=string_fun_y4(four))
    y4=y4%>% ungroup()%>%select(ITEMS)
    y4=left_join(y4,K4)
    y4=y4%>% rename(Y_Freq=Freq)
    sup_4=cbind(sup_4,y4$Y_Freq)
    colnames(sup_4)[4]='Y_Freq'
    #===========================================================================
    #Confidence, Lift
    sup_4=as.data.frame(sup_4)
    sup_4$X_Freq=as.numeric(sup_4$X_Freq)
    sup_4=sup_4%>%mutate(x_sup=X_Freq/totalPats)
    
    sup_4$Y_Freq=as.numeric(sup_4$Y_Freq)
    sup_4=sup_4%>%mutate(y_sup=Y_Freq/totalPats)
    
    sup_4$support=as.numeric(sup_4$support)
    sup_4=sup_4%>%mutate(XY_sup=support/totalPats)
    
    sup_4=sup_4%>%mutate(Confidence=XY_sup/x_sup)
    sup_4=sup_4%>%mutate(Lift=Confidence/y_sup)
    sup_4=sup_4%>% filter(support>=1)
    sup_4=sup_4%>%mutate(Patterns_of='Four')
    #===========================================================================
    # Five item set
    k=5
    K5=OMdataFrame%>% filter(EID==k, Freq>=sigma)
    #===========================================================================
    #Function to extract first five words
    string_fun_5 <- function(x) {
      ul = unlist(strsplit(x, split = ","))[1:5]
      paste(ul,collapse=",")
    }
    #===========================================================================
    # Find ordered match
    Seq=Seq%>% filter(Diseases>=5)
    totalPats=nrow(Seq)
    totalPats_5=totalPats
    Seq=Seq%>% group_by(SID)%>% mutate(five=string_fun_5(ITEMS))
    S5=Seq%>% ungroup()%>%distinct(five)%>%select(five)%>%arrange(five)
    
    sup_5=as.data.frame(table(Seq$five))
    
    colnames(sup_5)=c('Rule', 'support')
    sup_5_for6=sup_5
    sup_5_for6=as.data.frame(sup_5_for6)
    sup_5$Rule=sub(",([^,]*)$"," =>\\1", sup_5$Rule)#last comma
    sup_5=as.data.frame(sup_5)
    #===========================================================================
    #ADD X
    X5=as.data.frame(table(Seq$four))
    sup_5_for6$Rule=as.character(sup_5_for6$Rule)
    sup_5_for6=sup_5_for6%>% rowwise()%>% mutate(onlyfour=string_fun_4(Rule))
    
    sup_5_for6=X5%>% rename(onlyfour=Var1)%>% left_join(sup_5_for6)
    
    sup_5=cbind(sup_5, sup_5_for6$Freq)
    colnames(sup_5)[3]='X_Freq'
    sup_5$X_Freq=as.numeric(sup_5$X_Freq)
    #===========================================================================
    #ADD Y
    #Function to extract y5 alone one word
    string_fun_y5 <- function(x) {
      ul = unlist(strsplit(x, split = ","))[5:5]
      paste(ul,collapse=",")
    }
    #===========================================================================
    y5=S5%>% select(five)%>%rowwise()%>%mutate(ITEMS=string_fun_y5(five))
    y5=y5%>% ungroup()%>%select(ITEMS)
    y5=left_join(y5,K5)
    y5=y5%>% rename(Y_Freq=Freq)
    sup_5=cbind(sup_5,y5$Y_Freq)
    colnames(sup_5)[4]='Y_Freq'
    #===========================================================================
    #Confidence, Lift
    sup_5=as.data.frame(sup_5)
    sup_5$X_Freq=as.numeric(sup_5$X_Freq)
    sup_5=sup_5%>%mutate(x_sup=X_Freq/totalPats)
    
    sup_5$Y_Freq=as.numeric(sup_5$Y_Freq)
    sup_5=sup_5%>%mutate(y_sup=Y_Freq/totalPats)
    
    sup_5$support=as.numeric(sup_5$support)
    sup_5=sup_5%>%mutate(XY_sup=support/totalPats)
    
    sup_5=sup_5%>%mutate(Confidence=XY_sup/x_sup)
    sup_5=sup_5%>%mutate(Lift=Confidence/y_sup)
    sup_5=sup_5%>% filter(support>=1)
    sup_5=sup_5%>%mutate(Patterns_of='Five')
    #===========================================================================
    #FINAL RESULTS
    Result=rbind(sup_2,sup_3,sup_4,sup_5)
    Result=data.frame(Result)
    Result=Result%>% rename(Count=support)
    Result=Result%>% rename(Support=XY_sup)
    Result
  })
  #===============================Tutorial======================================
  output$demodata <-
    downloadHandler(
      filename = function () {
        paste("SampleData_SERENDIP.csv", sep = "\t")
      },
      
      content = function(file) {
        
        write.csv(demo, file, row.names = F)
      }
    )
  #===================================== Master button==========================
  output$Master_results=renderUI({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    dfs=DATA_format()
    if(dfs=='Data format check passed...'){
      downloadButton("MasterData", "Download mined patterns")
    } else {''}
    
  })
  #===================================== Master download========================
  output$MasterData <-
    downloadHandler(
      filename = function () {
        paste("SERENDIP_Results.csv", sep = "\t")
      },
      
      content = function(file) {
        
        write.csv(Master_data(), file, row.names = F)
      }
    )
  #==================================Viewing Results============================
  output$viewresults=renderDT({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    dfs=DATA_format()
    if(dfs=='Data format check passed...'){
      df=Master_data()
      df=df%>% select(Rule,Count, Support, Confidence, Lift,Patterns_of)
      df$Confidence=specify_decimal(df$Confidence,4)
      df$Lift=specify_decimal(df$Lift,4)
      df$Support=specify_decimal(df$Support,4)
      df=as.data.frame(df)
      datatable(df,filter = "top", options = list(pageLength = 10, dom = 'tip'),caption = 'Patterns') %>%
        formatStyle('Rule',  color = 'black', fontWeight = 'bold')
    } else {''}
  })
  
  #======================================HOW TO=================================
  #how to for Apriori and & Serendip
  observeEvent(input$howto_1, {
    showModal(modalDialog(
      HTML('<img src="howto.png">'),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  #how to for Spade
  observeEvent(input$howto_2, {
    showModal(modalDialog(
      HTML('<img src="howtospade.png">'),
      easyClose = TRUE,
      footer = NULL
    ))
  }) 
  
  #how to for Serendip
  observeEvent(input$howto_3, {
    showModal(modalDialog(
      HTML('<img src="howto.png">'),
      easyClose = TRUE,
      footer = NULL
    ))
  }) 
  
  ##==============================================================================
  #                                         SPADE
  ##==============================================================================
  
  #=========================================Input check===========================
  spade_dataFormat=reactive({
    spade_inFile <- input$file3
    if (is.null(spade_inFile))
      return(NULL)
    spade_transactions <- read.csv(spade_inFile$datapath)
    required_columns <- c('id', 'date','item')
    column_names <- colnames(spade_transactions)
    max_columns <- 3
    
    shiny::validate(
      need(ncol(spade_transactions) <= max_columns, "Your data has too many columns"),
      need(all(required_columns %in% column_names), "data preparation Stoped! Incorrect column (S) Please visit documentation page for details")
    )
    'Data format check passed...'
  })
  
  #=================================Spade Master Data prep =======================
  spade_master_data_prep = reactive({
    
    spade_inFile <- input$file3
    if (is.null(spade_inFile))
      return(NULL)
    spade_dfs=spade_dataFormat()
    if(spade_dfs=='Data format check passed...'){
      transactions = read.csv(spade_inFile$datapath)
      transactions[transactions==""]<-NA
      transactions=drop_na(transactions)
      transactions=transactions %>% dplyr::select(id, date, item)
      
      # Aggregate
      trans_sequence <- transactions %>%
        group_by(id, date) %>%
        summarize(
          SIZE = dplyr::n(),
          item = paste(as.character(item), collapse = ',')
        )
      
      #Date processing
      trans_sequence$date=as.numeric(as.character(as.Date(trans_sequence$date, format = "%d/%m/%Y"), format="%Y%m%d"))
      setDT(trans_sequence)[, group_no := .GRP, by = id]
      trans_sequence = trans_sequence[,c(5,2,3,4)]
      names(trans_sequence) = c("sequenceID", "eventID", "SIZE", "items")
      trans_sequence <- data.frame(lapply(trans_sequence, as.factor))
      trans_sequence <- trans_sequence[order(trans_sequence$sequenceID, trans_sequence$eventID),]
      trans_sequence$eventID <-as.numeric(trans_sequence$eventID)
      trans_sequence$eventID = abs(trans_sequence$eventID)
      sapply(trans_sequence, class)
      trans_sequence
      
      
    } else {'Data preparation stoped!'}
    
  })
  
  #============================ Spade data prep download button=================
  output$Spade_prep_dn_btn_ui=renderUI({
    spade_inFile <- input$file3
    if (is.null(spade_inFile))
      return(NULL)
    spade_dfs=spade_dataFormat()
    if(spade_dfs=='Data format check passed...'){
      downloadButton("spade_MasterData_prep_btn", "Download prepared data")
    } else {''}
    
  })
  #=========================Spade Master data prep download=====================
  output$spade_MasterData_prep_btn <-
    downloadHandler(
      filename = function () {
        paste("Spade_prepared_data.txt")
      },
      
      content = function(file) {
        
        write.table(spade_master_data_prep(), file, sep=";", row.names = FALSE, col.names = FALSE, quote = FALSE)
      }
    )

  #===================================Spade Frequency=============================
  output$spade_frquency = renderPlot({
    
    spade_inFile <- input$file3
    if (is.null(spade_inFile))
      return(NULL)
    spade_dfs=spade_dataFormat()
    if(spade_dfs=='Data format check passed...'){
      retail = read.csv(spade_inFile$datapath)
      retail[retail==""]<-NA
      retail=drop_na(retail)
      retail=retail %>% dplyr::select(id, date, item)
      retail = retail %>% distinct()
      transactionData = retail %>% group_by(id, date) %>% summarise_all(funs(paste(na.omit(.), collapse = ",")))
      
      transactionData$id = NULL
      transactionData$date = NULL
      colnames(transactionData) <- c("items")
      items <- strsplit(as.character(transactionData$items), ",")
      trans <- as(items, "transactions")
      
      # frequency plot
      arules::itemFrequencyPlot(trans, topN = 10,
                                col = brewer.pal(8, 'Pastel2'),
                                main = 'Absolute Item Frequency Plot',
                                type = "absolute",
                                ylab = "Frequency (absolute)")
      
      
    } else {'No plot generated'}
    
  })
  
  #===============================Spade result table==========================

  output$Spaderesults= renderDataTable({
    
    spd_file = input$spdPrpd
    
    if (is.null(spd_file))
      return(NULL)
    
    #testread = fread(spd_file$datapath)
    
    trans_matrix = read_baskets(spd_file$datapath, sep = ";", info = c("sequenceID","eventID","SIZE"))
    
    s1 = cspade(trans_matrix, parameter = list(support = 0.1), control = list(verbose = TRUE))
    #s1.df <- as(s1, "data.frame")
    
    r1 = as(ruleInduction(s1, confidence = 0.5, control = list(verbose = TRUE)), "data.frame")
    
    datatable(r1,filter = "top", options = list(pageLength = 10, dom = 'tip'),caption = 'Result')
  })
  
  
  
  
##==============================================================================
  #                                         APRIORI
##==============================================================================

#=========================================Input check===========================
  Apri_dataFormat=reactive({
    Apri_inFile <- input$file2
    if (is.null(Apri_inFile))
      return(NULL)
    Apri_transactions <- read.csv(Apri_inFile$datapath)
    required_columns <- c('id', 'date','item')
    column_names <- colnames(Apri_transactions)
    max_columns <- 3
    
    shiny::validate(
      need(ncol(Apri_transactions) <= max_columns, "Your data has too many columns"),
      need(all(required_columns %in% column_names), "Mining Stoped! Incorrect column (S) Please visit documentation page for details")
    )
    'Data format check passed...'
  })

#=====================================Apriori Master Data=======================
  Apri_master_data = reactive({
    
    Apri_inFile <- input$file2
    if (is.null(Apri_inFile))
      return(NULL)
    Apri_dfs=Apri_dataFormat()
    if(Apri_dfs=='Data format check passed...'){
      retail = read.csv(Apri_inFile$datapath)
      retail[retail==""]<-NA
      retail=drop_na(retail)
      retail=retail %>% dplyr::select(id, date, item)
      retail = retail %>% distinct()

      transactionData = retail %>% group_by(id, date) %>% summarise_all(funs(paste(na.omit(.), collapse = ",")))
      
      # Remove ID & date
      transactionData$id = NULL
      transactionData$date = NULL
      
      #Rename column to items
      colnames(transactionData) <- c("items")
      
      # no write and save approach
      items <- strsplit(as.character(transactionData$items), ",")
      trans <- as(items, "transactions")
      
      rules2 <- apriori(trans, parameter = list(supp=0.001, conf=0.001,maxlen=4,minlen=2))
      rules2 <- sort(rules2, by='confidence', decreasing = TRUE)
      # Remove redundant rule    
      rules2 <- rules2[!is.redundant(rules2)]
      rules_dt2 <- data.table( lhs = labels( lhs(rules2) ), 
                               rhs = labels( rhs(rules2) ), 
                               quality(rules2) )[ order(-lift), ]
      rules_dt2
      
    } else {'Mining stoped!'}
    
  })
  
  #============================== APRIORI download button=======================
  output$Apri_dn_btn_ui=renderUI({
    Apri_inFile <- input$file2
    if (is.null(Apri_inFile))
      return(NULL)
    Apri_dfs=Apri_dataFormat()
    if(Apri_dfs=='Data format check passed...'){
      downloadButton("Apri_MasterDatabtn", "Download mined patterns")
    } else {''}
    
  })
  #=========================APRIORI Master data download========================
  output$Apri_MasterDatabtn <-
    downloadHandler(
      filename = function () {
        paste("Apriori_Results.csv", sep = "\t")
      },
      
      content = function(file) {
        
        write.csv(Apri_master_data(), file, row.names = F)
      }
    )
  
  #===============================Apriori result table==========================
  output$Apri_results=renderDT({
    Apri_inFile <- input$file2
    if (is.null(Apri_inFile))
      return(NULL)
    Apri_dfs=Apri_dataFormat()
    if(Apri_dfs=='Data format check passed...'){
      aPriData= Apri_master_data()
      datatable(aPriData,filter = "top", options = list(pageLength = 10, dom = 'tip'),caption = 'Result')
    } else {''}
  })
  
  #===================================Apriori Frequency=========================
 output$Apri_frquency = renderPlot({
    
    Apri_inFile <- input$file2
    if (is.null(Apri_inFile))
      return(NULL)
    Apri_dfs=Apri_dataFormat()
    if(Apri_dfs=='Data format check passed...'){
      retail = read.csv(Apri_inFile$datapath)
      retail[retail==""]<-NA
      retail=drop_na(retail)
      retail=retail %>% dplyr::select(id, date, item)
      retail = retail %>% distinct()
      transactionData = retail %>% group_by(id, date) %>% summarise_all(funs(paste(na.omit(.), collapse = ",")))
      
      transactionData$id = NULL
      transactionData$date = NULL
      colnames(transactionData) <- c("items")
      items <- strsplit(as.character(transactionData$items), ",")
      trans <- as(items, "transactions")
      
      # frequency plot
      arules::itemFrequencyPlot(trans, topN = 10,
                                col = brewer.pal(8, 'Pastel2'),
                                main = 'Absolute Item Frequency Plot',
                                type = "absolute",
                                ylab = "Frequency (absolute)")
      
      
    } else {'No plot generated'}
    
  })
  #===================================Apriori network=============================
  output$Apri_net = renderPlot({
    
    Apri_inFile <- input$file2
    if (is.null(Apri_inFile))
      return(NULL)
    Apri_dfs=Apri_dataFormat()
    if(Apri_dfs=='Data format check passed...'){
      retail = read.csv(Apri_inFile$datapath)
      retail[retail==""]<-NA
      retail=drop_na(retail)
      retail=retail %>% dplyr::select(id, date, item)
      retail = retail %>% distinct()
      
      transactionData = retail %>% group_by(id, date) %>% summarise_all(funs(paste(na.omit(.), collapse = ",")))
      
      # Remove ID & date
      transactionData$id = NULL
      transactionData$date = NULL
      
      #Rename column to items
      colnames(transactionData) <- c("items")
      
      # no write and save
      items <- strsplit(as.character(transactionData$items), ",")
      trans <- as(items, "transactions")
      
      rules2 <- apriori(trans, parameter = list(supp=0.001, conf=0.001,maxlen=4,minlen=2))
      rules2 <- sort(rules2, by='confidence', decreasing = TRUE)
      topRules <- rules2[1:10]
      plot(topRules, method="graph")
      
      
     # 
    } else {''}
  })
  
  #=================================Github link=================================
  url <- a("Github", href="https://github.com/alxv/disease-pattern-miner/")
  output$gitlink <- renderUI({
    tagList("Default support threshold = 0.001, confidence = 0.001.Please use local copy of DPM for customization:", url)
  })
  
  output$gitlink2 <- renderUI({
    tagList("Default support threshold = 0.001, confidence = 0.001.Please use local copy of DPM for customization:", url)
  })
  
  output$gitlink3 <- renderUI({
    tagList("Default support threshold = 0.001, confidence = 0.001.Please use local copy of DPM for customization:", url)
  })
  
  ##############################################################################  
}