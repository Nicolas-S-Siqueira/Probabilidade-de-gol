# Carregando os pacotes
library(tidyverse)
library(jsonlite)
library(tidymodels)
library(glmnet)
library(tidyposterior)

# Lendo os dados
xg_data.h2o <- read_csv("dados_modelo_futebol.csv")

# Transformando em tibble
xg_data.h2o  <- as_tibble(xg_data.h2o)

# Transformando as variáveis character em fatores
xg_data.h2o <-  xg_data.h2o %>%  mutate(across(where(is_character), as_factor))

# Salvando o objeto em 2 nomes diferentes
xg_data <- xg_data.h2o

# Olhando os objetos
xg_data.h2o %>%  glimpse
xg_data %>%  glimpse


### Análise exploratória ###

# Comparação da distribuição do pass_length com a variável resposta
xg_data %>%
  select(shot_outcome_name, pass_length) %>% 
  ggplot(aes(x = pass_length, fill = shot_outcome_name)) +
  geom_density(alpha = 0.5)

# Comparação da distribuição do my_pass_angle com a variável resposta
xg_data %>%
  select(shot_outcome_name, my_pass_angle) %>% 
  ggplot(aes(x = my_pass_angle, fill = shot_outcome_name)) +
  geom_density(alpha = 0.5)


# Comparação da contagem do pass_height_name com a variável resposta
xg_data %>%
  select(shot_outcome_name, pass_height_name) %>% 
  ggplot(aes(x = pass_height_name, fill = shot_outcome_name)) +
  geom_bar(alpha = 1)


# Comparação da distribuição do pass_speed_kmph com a variável resposta
xg_data %>%
  select(shot_outcome_name, pass_speed_kmph) %>% 
  ggplot(aes(x = pass_speed_kmph, fill = shot_outcome_name)) +
  geom_density(alpha = 0.5)

# Comparação da distribuição do shot_length com a variável resposta
xg_data %>%
  select(shot_outcome_name, shot_length) %>% 
  ggplot(aes(x = shot_length, fill = shot_outcome_name)) +
  geom_density(alpha = 0.5)

# Comparação da distribuição do shot_angle com a variável resposta
xg_data %>%
  select(shot_outcome_name, shot_angle) %>% 
  ggplot(aes(x = shot_angle, fill = shot_outcome_name)) +
  geom_density(alpha = 0.5)


# Comparação da contagem do gk_in_goal com a variável resposta
xg_data %>%
  select(shot_outcome_name, gk_in_goal) %>% 
  ggplot(aes(x = gk_in_goal, fill = shot_outcome_name)) +
  geom_bar(alpha = 1)


# Comparação da distribuição do pressure com a variável resposta
xg_data %>%
  select(shot_outcome_name, pressure) %>% 
  ggplot(aes(x = pressure, fill = shot_outcome_name)) +
  geom_density(alpha = 0.5)


# Comparação da contagem do def_in_way com a variável resposta
xg_data %>%
  select(shot_outcome_name, def_in_way) %>% 
  ggplot(aes(x = def_in_way, fill = shot_outcome_name)) +
  geom_bar(alpha = 1)


# Comparação da histograma do var_angles com a variável resposta
xg_data %>%
  select(shot_outcome_name, var_angles) %>% 
  ggplot(aes(x = var_angles, fill = shot_outcome_name)) +
  geom_histogram(alpha = 0.5, position = "identity")


### Velocidade dos passes de acordo com as distâncias ###

# Passes até 20 metros
xg_data %>%
  select(pass_speed_kmph, pass_length, pass_height_name) %>%
  filter(pass_length <= 20) %>% 
  group_by(pass_height_name) %>%
  summarise(mean(pass_speed_kmph))

# Passes até entre 20 e 40 metros
xg_data %>%
  select(pass_speed_kmph, pass_length, pass_height_name) %>%
  filter(pass_length > 20 & pass_length <= 40) %>% 
  group_by(pass_height_name) %>%
  summarise(mean(pass_speed_kmph))

# Passes com mais de 40 metros
xg_data %>%
  select(pass_speed_kmph, pass_length, pass_height_name) %>%
  filter(pass_length > 40) %>% 
  group_by(pass_height_name) %>%
  summarise(mean(pass_speed_kmph))


# Retirando os NA's
xg_data.h2o <- drop_na(xg_data.h2o)
xg_data <- drop_na(xg_data)


# Dividindo em treino e teste
# Seed pra garantir reprodutibilidade
set.seed(8217)
split <- initial_split(xg_data, prop = 0.8, strata = shot_outcome_name) 
train <- training(split)
test <- testing(split)

# Cross-validation pra tunar o modelo
set.seed(39947)
folds <- vfold_cv(data = train, v = 10, repeats = 1, strata = shot_outcome_name) 


# Pré-processamento com recipes
rec <- recipe(shot_outcome_name ~ 
                pass_length + 
                my_pass_angle +
                pass_height_name +
                pass_speed_kmph +
                shot_length +
                shot_angle +
                gk_in_goal +
                pressure +
                def_in_way +
                var_angles, data = train) %>% 
  
  # Deixar os dados desbalanceados propositalmente para que as probabilidades não fiquem com valores mais altos do que deveriam.
  # O ideal é que a soma da probabilidade de cada finalização seja igual ao total real de gols.
  # Fazer apenas "scale" nos dados e não normalização para que os zeros permaneçam como zeros.
  
  step_scale(all_numeric(), -all_outcomes()) %>% 
  step_dummy(all_nominal(), -all_outcomes()) #hot encoding em todas as variáveis explicativas nominais.

# Ver como vão ficar os dados
rec %>%  prep() %>%  juice() %>%  View


# Modelagem com parsnip
library(glmnet)

# mixture = 0 significa que é uma regressão ridge (sem lasso)
# Todas as variáveis devem ser consideradas
glm_spec <- logistic_reg(penalty = tune(), mixture = 0) %>% 
  set_engine("glmnet") %>% 
  set_mode("classification")

# Métricas consideradas
glm_metrics <- metric_set(roc_auc, pr_auc, accuracy, mn_log_loss)

# Grade de parâmetros pra tunar ( apenas o "penalty" nesse caso)
glm_params <- parameters(glm_spec)

# Montando o grid com 20 opções de valores menos os valores a cima de 0.2
glm_grid <- grid_regular(glm_params,
                         levels = 20,
                         filter = penalty < 0.2
)
glm_grid


# Tunar o modelo nos folds
glm_res <- tune_grid(glm_spec,
                     preprocessor = rec,
                     resamples = folds,
                     grid = glm_grid,
                     metrics = glm_metrics
)

# Selecionar o modelo que tem o melhor valor de "mean_log_loss"
best_params <- glm_res %>% select_best("mn_log_loss")

# Finalizando o modelo escolhido
best_glm <-  finalize_model(glm_spec, parameters = best_params)

# Finalizando a receita escolhida
best_rec <- finalize_recipe(rec, parameters = best_params)

# Aplicando o modelo e a receita aos dados
last_glm <- last_fit(best_glm,
                     preprocessor =  best_rec,
                     split = split,
                     metrics = glm_metrics
)


# Previsões nos dados de teste
last_glm %>% 
  collect_predictions()

last_glm %>% 
  collect_predictions() %>% 
  select(.pred_Goal, .pred_class, shot_outcome_name) %>% 
  View()

last_glm %>% 
  collect_predictions() %>% 
  select(.pred_Goal, .pred_class, shot_outcome_name) %>% 
  conf_mat(truth = shot_outcome_name,
           estimate = .pred_class)

last_glm %>%
  collect_predictions() %>% 
  select(.pred_Goal) %>%  sum


# Criar um workflow pra poder salvar e reutilizar pra fazer previsões de novos valores

# Criação do workflow com o modelo e receita escolhidos
wf <- workflow() %>% 
  add_recipe(best_rec) %>% 
  add_model(best_glm)

# Aplicação do workflow aos dados de treino
fit_glm <- fit(wf, train)

# Previsões, nos dados de teste, feitas usando o objeto anterior
predict(object = fit_glm,
        new_data = test,
        type = "prob") %>% 
  select(.pred_Goal) %>%  sum
  
# Criando nova observação (Situação de finalização)
obs <- tibble(pass_length = 26,
              my_pass_angle = 1.36,
              pass_height_name = "Ground Pass",
              pass_speed_kmph = 62.27,
              shot_length = 12.4,
              shot_angle = 0.572,
              gk_in_goal = TRUE,
              pressure = 0.131,
              def_in_way = 0,
              var_angles = 0) 

# Prevendo a probabilidade de gol da nova observação usando o modelo 
predict(object = fit_glm,
        new_data = obs,
        type = "prob")[1] %>%  as.numeric()

# SALVANDO O MODELO:
# save(fit_glm, file =  "fit_glm.RData")

# PARA CARREGAR:
# load("fit_glm.RData")

