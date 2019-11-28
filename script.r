source('./r_files/flatten_HTML.r')

############### Library Declarations ###############
libraryRequireInstall("TSA")
libraryRequireInstall("forecast")
libraryRequireInstall("ggplot2")
libraryRequireInstall("dplyr")
libraryRequireInstall("lubridate")
libraryRequireInstall("anytime")
libraryRequireInstall("dygraphs")
libraryRequireInstall("magrittr")

####################################################

################### Actual code ####################

#conversão de tipos
Values$usu_cmporc <- anytime(Values$usu_cmporc) 


##agrupamento mensal
orc_agrup <- Values %>% group_by(month=floor_date(usu_cmporc, "month")) %>% 
  summarise(amount=sum(usu_valrea))

##transformação dos dados

#1) separação dos conjuntos de teste e treino

#dados de treinamento:
dados_treino <- orc_agrup %>% slice(1:(n()-2))


#2) transformação logarítmica

#montar time series
dados_treino_ts <- ts(
  dados_treino$amount,
  start = c(year(min(dados_treino$month)),month(min(dados_treino$month))), 
  end = c(year(max(dados_treino$month)),month(max(dados_treino$month))),
  frequency = 12)

log_dados_treino <- log(dados_treino_ts)


## Modelo 

# função para selecionar melhores parametros para o modelo
best_sarima_model <- function(train_data,p,q,P,Q,d=1,D=1,s=12){
  
  current_best_model = arimax(
    train_data, order = c(0,0,0), 
    seasonal = list(order=c(0,0,0), period=12),
    method = 'ML')
  
  best_model_aic = Inf
  best_model_bic = Inf
  best_model_hqic = Inf
  best_model_order = c(0,0,0)
  models = list()
  
  for (p_ in p) {
    for (q_ in q) {
      for (P_ in P) {
        for (Q_ in Q) {
          tryCatch(
            {
              no_of_lower_metrics = 0
              model = arimax(
                train_data, order = c(p_,d,q_), 
                seasonal = list(order=c(P_,D,Q_), period=s),
                method = 'ML')
              models = c(models,model)
              if (AIC(model) <= best_model_aic){
                no_of_lower_metrics = no_of_lower_metrics+1}
              if (BIC(model) <= best_model_bic){
                no_of_lower_metrics = no_of_lower_metrics+1}
              if (no_of_lower_metrics >= 2) {
                best_model_aic = round(AIC(model),0)
                best_model_bic = round(BIC(model),0)
                best_model_order = c(p_,d,q_,P_,D,Q_,s)
                current_best_model = model
                models = c(models,model)
              }
            },
            error = function(e){invisible()}
          )
        }
      }
    }
  }
  
  return (current_best_model)
}

#aplicação do modelo selecionado
best_model = best_sarima_model(train_data = log_dados_treino,p=range(0,2),q=range(0,2),P=range(0,2),Q=range(0,2))


### visualização
interval_value_formatter <- "function(num, opts, seriesName, g, row, col) {
  value = g.getValue(row, col);
  if(value[0] != value[2]) {
    lower = Dygraph.numberValueFormatter(value[0], opts);
    upper = Dygraph.numberValueFormatter(value[2], opts);
    return '[' + lower + ', ' + upper + ']';
  } else {
    return Dygraph.numberValueFormatter(num, opts);
  }
}"

## dados
orc_agrup_ts <- ts(
  orc_agrup$amount,
  start = c(year(min(orc_agrup$month)),month(min(orc_agrup$month))), 
  end = c(year(max(orc_agrup$month)),month(max(orc_agrup$month))),
  frequency = 12)
log_orc_agrup_ts <- log(orc_agrup_ts)


## based on: https://towardsdatascience.com/how-to-create-better-interactive-forecast-plots-using-r-and-dygraph-29bdd7146066
p <- log_orc_agrup_ts %>%
    forecast(model = best_model, h = Values$projecao_meses)%>%
    {cbind(
      actuals=round(exp(.$x)/1000000,3),
      forecast_mean=round(exp(.$mean)/1000000,3),
      lower_95=round(exp(.$lower[,"95%"])/1000000,3), 
      upper_95=round(exp(.$upper[,"95%"])/1000000,3),
      lower_80=round(exp(.$lower[,"80%"])/1000000,3), 
      upper_80=round(exp(.$upper[,"80%"])/1000000,3))} %>%
      dygraph(ylab = "Valor Realizado (R$) - em Milhoes") %>%
      dyAxis("y", valueFormatter = interval_value_formatter) %>%
      dySeries("actuals", color = "#002C47", label = "Realizado",drawPoints = TRUE, pointSize = 3,strokeWidth = 2) %>%
      dySeries("forecast_mean", color = "#EC6826", label = "Forecast",drawPoints = TRUE, pointSize = 3, strokeWidth = 2) %>%
      dySeries(c("lower_80", "forecast_mean", "upper_80"),
           label = "Intervalo Conf. - 80%", color = "#EC6826") %>%
      dySeries(c("lower_95", "forecast_mean", "upper_95"),
           label = "Intervalo Conf. - 95%", color = "#EC6826") %>%
      dyLegend(labelsSeparateLines=TRUE) %>%
      dyRangeSelector() %>%
      dyOptions(digitsAfterDecimal = 1) %>%
      dyCSS(textConnection(".dygraph-legend {background-color: rgba(255, 255, 255, 0.5) !important; }"))

####################################################

############# Create and save widget ###############

internalSaveWidget(p,'out.html')
####################################################
