# Biblioteca para o auxilio da manipulação de dados

# Plot dos menores preços de cada competidor por produto
toPlot_preco_competidor <- function(comp_prices, prod_id){
  
  melhorCompetidor <- group_by(comp_prices, PROD_ID, COMPETITOR) %>%
    summarise(min(COMPETITOR_PRICE)) %>%
    filter(PROD_ID == prod_id)
  
  colnames(melhorCompetidor) <- c("PROD_ID", "COMPETITOR", "COMPETITOR_PRICE")
  min_price <- min(melhorCompetidor$COMPETITOR_PRICE)
  max_price <- max(melhorCompetidor$COMPETITOR_PRICE)
  
  ggplot(melhorCompetidor, aes(x=reorder(COMPETITOR, -COMPETITOR), y=COMPETITOR_PRICE)) + 
    geom_bar(stat="identity") + 
    labs(y='', x='', title = paste0("Produto", " ", prod_id)) +
    theme_classic() + 
    theme(panel.background=element_blank()) + 
    geom_bar(data = filter(melhorCompetidor, COMPETITOR_PRICE == max_price), 
             stat="identity", colour="red", fill="red") + 
    geom_bar(data = filter(melhorCompetidor, COMPETITOR_PRICE == min_price), 
             stat="identity", colour="blue", fill="blue") 
}

# DataFrame com a quantidade de vezes que um competidor teve o menor/maior preço
toPlot_preco_competidor_df <- function(comp_prices){
  produtos_id <- unique(comp_prices$PROD_ID)
  
  df_melhor_preco <- data.frame()
  df_pior_preco <- data.frame()
  for (produto in produtos_id) {
    melhorCompetidor <-  group_by(comp_prices, PROD_ID, COMPETITOR) %>%
      summarise(min(COMPETITOR_PRICE)) %>%
      filter(PROD_ID == produto)
    
    colnames(melhorCompetidor) <- c("PROD_ID", "COMPETITOR", "COMPETITOR_PRICE")
    min_price <- min(melhorCompetidor$COMPETITOR_PRICE)
    max_price <- max(melhorCompetidor$COMPETITOR_PRICE)
    
    df_melhor_preco  <- rbind(df_melhor_preco, filter(melhorCompetidor, COMPETITOR_PRICE == min_price))
    df_pior_preco  <- rbind(df_pior_preco, filter(melhorCompetidor, COMPETITOR_PRICE == max_price))
  }
  
  rbind(data.frame(as.data.frame(table(df_melhor_preco$COMPETITOR)), 
             Preco = as.factor("Melhor")), 
             data.frame(as.data.frame(table(df_pior_preco$COMPETITOR)), 
                        Preco = as.factor("Pior")))
}

# Função que retonra o data frame com timestamp por colunas 
col_timestamp <- function(sales){
  sales$date <- as.Date(as.character(sales$DATE_ORDER), format = "%Y-%m-%d")
  sales$ano <- as.numeric(format(sales$date, format = '%Y'))
  sales$mes <- as.numeric(format(sales$date, format = '%m'))
  sales$dia <- as.numeric(format(sales$date, format = '%j'))
  sales$semana <- as.numeric(format(sales$date, format = '%W'))
  sales$dia_da_semana <- format(sales$date, format = '%a')
  sales$dia_da_semana_num <- format(sales$date, format = '%u')
  sales
} 

# Função que retonra o data frame com timestamp por colunas 
col_timestamp_comp <- function(comp_prices){
  comp_prices$date <- as.POSIXct(as.character(comp_prices$DATE_EXTRACTION), format = "%Y-%m-%d %H:%M:%S")
  comp_prices$dia <- as.numeric(format(comp_prices$date, format = '%j'))
  comp_prices$hora <- as.numeric(format(comp_prices$date, format = '%H'))  
  comp_prices$mes <- as.numeric(format(comp_prices$date, format = '%m'))
  comp_prices$semana <- as.numeric(format(comp_prices$date, format = '%W'))
  comp_prices$dia_da_semana_num <- as.numeric(format(comp_prices$date, format = '%u'))
  comp_prices
} 

# Função que cria um data frame com todos os dias e produtos possíveis
complet_df <- function(sales){
  df <- data.frame()
  for (i in min(sales$dia):max(sales$dia)){
      df <- rbind(df, c("P1", i))
  }
}

# Função que cria uma coluna da quantidade de vendas do dia anterior
qty_dia_anterior <- function(sales){
  aux <- sales["qty"]
  aux <- rbind(0, aux)
  aux <- data.frame(aux[-nrow(aux), ]) 
  
  colnames(aux) <- "qty_anterior"
  sales <- cbind(sales, aux)
}



