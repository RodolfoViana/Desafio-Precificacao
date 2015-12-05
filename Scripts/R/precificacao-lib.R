# Biblioteca para o auxilio da manipulação de dados

# Plot dos menores preços de cada competidor por produto
toPlot_preco_competidor <- function(comp_prices, prod_id){
  
  melhorCompetidor <-  group_by(comp_prices, PROD_ID, COMPETITOR) %>%
    summarise(min(COMPETITOR_PRICE)) %>%
    filter(PROD_ID== prod_id)
  
  colnames(melhorCompetidor) <- c("PROD_ID", "COMPETITOR", "COMPETITOR_PRICE")
  min_price <- min(melhorCompetidor$COMPETITOR_PRICE)
  max_price <- max(melhorCompetidor$COMPETITOR_PRICE)
  
  ggplot(melhorCompetidor, aes(x=reorder(COMPETITOR, -COMPETITOR), y=COMPETITOR_PRICE)) + 
    geom_bar(stat="identity") + 
    labs(y='Preço', x='Competidor', title = paste0("Menore Preços por Competidor Para o Produto", " ", prod_id)) +
    theme_classic() + 
    theme(panel.background=element_blank()) + 
    geom_bar(data = filter(melhorCompetidor, COMPETITOR_PRICE == max_price), stat="identity", colour="red", fill="red") + 
    geom_bar(data = filter(melhorCompetidor, COMPETITOR_PRICE == min_price), stat="identity", colour="blue", fill="blue") 
}

# DataFrame com a quantidade de vezes que um competidor teve o menor/maior preço
toPlot_preco_competidor <- function(comp_prices){
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
