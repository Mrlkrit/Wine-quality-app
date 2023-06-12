server <- function(input, output, session){
  
  val <- reactiveValues(df_data = NULL)
  #read data
  observeEvent(input$file, {val$df_data <- read.csv(input$file$datapath,header = input$header)
  updateSelectInput(session, "col_select",choices = colnames(val$df_data),selected = "")
  updateSelectInput(session, "col_select_train",choices = colnames(val$df_data),selected = "")
  updateSelectInput(session, "hist_select",choices = colnames(val$df_data),selected = "")
  })
  #display data
  output$table <- renderDT(datatable(val$df_data,editable = TRUE,rownames = FALSE))
  #edit row data
  observeEvent(input$table_cell_edit, {
    row  <- input$table_cell_edit$row
    col <- input$table_cell_edit$col
    val$df_data[row, col] <- input$table_cell_edit$value
  })
  #delete selected rows rows
  observeEvent(input$deleteRows,{
    if (!is.null(input$table_rows_selected)) {
      val$df_data <- val$df_data[-as.numeric(input$table_rows_selected),]
    }
  })
  #clearing data
  observeEvent(input$clear_data,{
    if (!(input$header)){
      val$df_data[val$df_data == ""] <- NA} 
    
    val$df_data<-na.omit(val$df_data)
    val$df_data<-distinct(val$df_data)
  })
  # change column name
  observeEvent(input$rename_col_button,{
    colnames(val$df_data)[which(colnames(val$df_data) == input$col_select)] <- input$new_col_name
    updateSelectInput(session, "col_select",choices = colnames(val$df_data),selected = "")
    updateSelectInput(session, "hist_select",choices = colnames(val$df_data),selected = "")
    updateSelectInput(session, "col_select_train",choices = colnames(val$df_data),selected = "")
  })
  #delete columns
  observeEvent(input$deleteCols,{
    val$df_data <- val$df_data[-c(which(colnames(val$df_data) == input$col_select))]
    updateSelectInput(session, "col_select",choices = colnames(val$df_data),selected = "")
    updateSelectInput(session, "hist_select",choices = colnames(val$df_data),selected = "")
    updateSelectInput(session, "col_select_train",choices = colnames(val$df_data),selected = "")
  })
  #print summary
  output$summary <- renderPrint({
    summary(val$df_data)
  })
  # hide show summary
  observeEvent(input$hideshow_summary, {
    toggle("summary")
  })
  #download data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(val$df_data, file)
    })
  #plot histogram
  output$histogram <- renderPlot({
    req(input$hist_select)
    req(val$df_data)
    hist(as.numeric(unlist(val$df_data[input$hist_select])), xlab = paste(input$hist_select), ylab = paste("count"), main = paste("Histogram of ", input$hist_select) )
    
  })
  
  
  #render correlation
  output$correlation <- renderPlot({
    req(val$df_data)
    M <- cor(val$df_data)
    corrplot(M, order = 'AOE', addCoef.col = 'black', 
             cl.pos = 'r', col = COL2('PRGn'),tl.cex = 1,number.cex = 0.75)
    toggle("correlation")
  })
  
  #hideshow correlation
  observeEvent(input$hideshow_correlation, {
    toggle("correlation")
  })
  #hideshow histogram
  observeEvent(input$hideshow_histogram, {
    toggle("histogram")
  })
  mod <- reactiveValues(model = NULL)
  #train model
  observeEvent(input$start_train,{
    req(val$df_data)
    req(input$seed)
    req(input$tree)
    val$df_data[,c(input$col_select_train)]<- as.factor(val$df_data[,c(input$col_select_train)])
    samples <- sample(nrow(val$df_data),as.numeric(input$split_ratio)*nrow(val$df_data))
    train <- val$df_data[samples, ]
    test <- val$df_data[-samples, ]
    set.seed(input$seed)
    q <- input$col_select_train
    model <- randomForest(train[,!(colnames(train)==input$col_select_train)]  , train[,c(input$col_select_train)], ntree = as.numeric(input$tree), na.action = na.omit)
    prediction <- predict(model, newdata = test)
    
    output$confusion <- renderDT({model$confusion})
    output$predict <- renderTable({table(prediction, test[,c(input$col_select_train)])})
    
    output$importance_classifier <- renderDT({importance(model)})
    output$result <- renderText({paste("Final accuracy:", sum(prediction==test[,c(input$col_select_train)])/nrow(test), sep = " ")})
    mod$model = model
    
    
  })
  
  observeEvent(input$own_data,{
    req(mod$model)
    my_data <- head(val$df_data,1) 
    
    mod$my_data <- my_data
    toggle("confusion")
    toggle("predict")
    toggle("importance_classifier")
    toggle("result")
    
  })
  
  output$mydata_table <- renderDT(datatable(mod$my_data,editable = TRUE,rownames = FALSE))
  
  observeEvent(input$check_wine,{
    pred <- predict(mod$model, newdata = mod$my_data)
    output$my_predict <- renderTable({pred})
  })
  
  output$help <-renderText({adr<-paste("	Po uruchomieniu aplikacji użytkownik widzi prosty oraz intuicyjny interfejs graficzny. 
                                  Ma możliwość skorzystania z trzech zakładek Dataset, Visualization, Prediction oraz Help.",
                                       "W zakładce Dataset najważniejszą funkcją jest możliwość wczytania pliku z pamięci komputera.
	                                Gdy plik zostanie już wczytany, możliwe jest wykonywanie operacji na danych.",
                                       "Program pozwala usuwać rzędy, kolumny, edytować zawarte w nich dane, zmieniać nazwy kolumn oraz usuwać niepotrzebne, niepoprawne dane.",
                                       "Wyświetla także summary, czyli podstawowe informacje o wczytanych danych. Wyświetlona tabela z danymi umożliwia pokazanie ich w sposób rosnący lub malejący.",
                                       "Dodatkową funkcją jest możliwość wyszukiwania konkretnych fraz w zbiorze danych. Po przetworzeniu danych dodatkowo możliwe jest ich pobranie.",
                                       "Warto wspomnieć, iż przed przejściem do działań w kolejnej zakładce, czyli w Visualization, należy wyczyścić dane za pomocą przycisku Clear,",
                                       "celem usunięcia danych niekompletnych, które mogą wpłynąć negatywnie na działanie programu.",
                                       "W zakładce Visualization możliwe jest wyświetlenie macierzy korelacji oraz histogramu dla poszczególnych kolumn.",
                                       "Można za pomoca przycisków Hide/Show pokazać bądź ukryć wspomniane wykresy. W celu wyświetlenia macierzy korelacji należy usunąć wartości klasy character.",
                                       "W zakładce Prediction możliwe jest uzyskanie oceny jakości wina. W zakładce tej istnieje możliwość ustawienia współczynnika,",
                                       "który definiuje się jako stosunek danych treningowych do sumy wszystkich danych, zarówno treningowych jak i testowych.",
                                       "Domyślnie wynosi on 0.7, czyli 70% danych treningowych i 30% testowych. Można także wybrać kolumnę, względem której trenujemy dane.",
                                       "Są tu także 2 pola tekstowe, w które można wpisać Seed, czyli ziarno, oraz Tree number, czyli ilość drzew losowych.",
                                       "Następnie można użyć przycisków do trenowania modelu i uzyskiwania predykcji.",sep = '\n')
  adr[1]
  })
}