# Carregando os pacotes
library(tidyverse)
library(jsonlite)
library(skimr)
library(GGally)
library(vip)
library(SBpitch)
library(tidymodels)
library(corrplot)
library(glmnet)
library(tidyposterior)


# Competições
# Importar um arquivo json como um dataframe
competitions <- jsonlite::fromJSON(txt = "C:\\Users\\Nicol\\Desktop\\Friends of tracking\\StatsBombData\\open-data-master\\data\\competitions.json")

# Campeonatos disputados
competitions %>%  View()

# Ler as partidas
#### Pra carregar vários arquivos de uma vez
match.files <- list.files(path="C:\\Users\\Nicol\\Desktop\\Friends of tracking\\StatsBombData\\open-data-master\\data\\matches",
                          full.names = TRUE,recursive = TRUE)

# Carregando os arquivos e armazenando num dataframe
matches <- purrr::map_df(match.files, jsonlite::fromJSON)

# Total de partidas
matches %>%  pull(match_id) %>%  length()
matches %>%  pull(match_id) %>% unique() %>% length()

# Desaninhar as colunas que estão como dataframe
matches <-  matches %>%
  flatten() %>%
  unnest_wider(home_team.managers, names_sep = ".") %>%
  unnest_wider(away_team.managers, names_sep = ".") %>%
  unnest_wider(home_team.managers.country, names_sep = ".") %>%
  unnest_wider(away_team.managers.country, names_sep = ".")


# Eventos
events.files <- list.files(path="C:\\Users\\Nicol\\Desktop\\Friends of tracking\\StatsBombData\\open-data-master\\data\\events",
                           full.names = TRUE)

# Id's de todas as partidas
# É possível pegar apenas partidas que atendam alguma condição usando o "filer"
ids_champions <- matches %>% 
  #filter(competition.competition_id == ??) %>% 
  pull(match_id) %>% 
  as.character()  

path <- "C:\\Users\\Nicol\\Desktop\\Friends of tracking\\StatsBombData\\open-data-master\\data\\events\\"
ext <- ".json"

# Como o id da partida é o nome do arquivo dá pra ler os arquivos assim.
champions_matches_ids <- map_chr(ids_champions, function(x) paste(path, x, ext, sep = ""))

# Dados em uma lista pra ficar separado por partida e ser possível incluir
# os ids de cada partida.
events.champions <- map(champions_matches_ids, fromJSON, flatten = TRUE)

# Criar uma coluna com o id da partida em cada dataframe
events.champions <- map2(events.champions, ids_champions, function(x,ids_champions) mutate(x, match.id = ids_champions))

# Juntar tudo em um dataframe.
events.champions <-  events.champions %>%  bind_rows()

# Ver os elementos do dataframe que são listas
events.champions %>%  select(where(is_list)) %>% glimpse

# Ver algumas observações da lista "shot.freeze_frame"
events.champions$shot.freeze_frame %>%  unique %>%  head(2)

# Olhar os dados
events.champions %>%  glimpse


# Pegar os eventos de finalizações
shots <- events.champions %>% 
  select(id, duration, type.name, starts_with("shot."), location, under_pressure, out) %>% 
  filter(type.name == "Shot") %>% 
  rename(shot.duration = duration, shot.location = location)

# Pegar todos os passes que resultaram em finalização e os id's das finalizações.
passes <- events.champions %>% 
  select(starts_with("pass."), duration, location) %>% 
  filter(is.na(pass.assisted_shot_id) == FALSE) %>% 
  rename(id = pass.assisted_shot_id, pass.duration = duration, pass.location = location)

# Adicionar a informação dos passes que geraram as finalizações
xg_data <- left_join(shots, passes, by = "id")

# Velocidade do passe de jarda por segundo para kilômetro por hora
xg_data <- xg_data %>% mutate(pass.speed.kmph = (pass.length/pass.duration)*3.29184)

# Olhar os dados
xg_data %>%  glimpse

# Retirando variáveis que não serão utilizadas
xg_data <- xg_data %>% select(-c(id, shot.duration, type.name,
                                 shot.end_location, shot.key_pass_id,
                                 shot.one_on_one, shot.aerial_won,
                                 shot.redirect, shot.type.id,
                                 shot.deflected, shot.technique.id,
                                 shot.body_part.name, shot.body_part.id,
                                 shot.technique.name, out, pass.switch, 
                                 pass.through_ball, pass.shot_assist,
                                 pass.cross, 
                                 pass.aerial_won, pass.inswinging, 
                                 pass.cut_back, pass.miscommunication, 
                                 pass.straight, pass.outswinging, pass.goal_assist, 
                                 pass.height.id, pass.type.id, pass.type.name,
                                 pass.body_part.id, pass.body_part.name,
                                 pass.outcome.id, pass.outcome.name, 
                                 pass.technique.id, pass.technique.name,
                                 pass.backheel, pass.deflected, pass.no_touch, 
                                 pass.duration
))


# Transformar as listas com as coord's x e y em colunas separadas no dataframe
xg_data <-  xg_data %>% 
  unnest_wider(shot.location, names_sep = ".") %>% 
  unnest_wider(pass.location, names_sep = ".") %>%
  unnest_wider(pass.end_location, names_sep = ".")

# Tudo que for coordenada y tem q ser (80 - valor) pra espelhar os valores já que StatsBomob considera
# o topo do campo sendo 0,0 e embaixo sendo 0,80.
xg_data <- xg_data %>% 
  mutate(across(ends_with(".2"), function(x) 80-x ))


# Função pra calcular a distâncio do chute dada a coordenada
LengthToGoal <- function(x,y) {
  CatAdj <- abs(x - 120)
  CatOp <- abs(y - 40)   # 120,40 é o centro do gol
  Hip <- sqrt(CatAdj^2 + CatOp^2)
  
  return(Hip)
}

# Função pra calcular o ângulo do chute dada a coordenada
AngToGoal <- function(x,y) {
  CatAdj <- abs(x - 120)
  CatOp <- abs(y - 40)   # 120,40 é o centro do gol
  Hip <- sqrt(CatAdj^2 + CatOp^2)
  
  CatOp/Hip 
  
  Ang <-  asin(CatOp/Hip)
  
  return(Ang)# O valor tá em radiano
  # Se quiser o ângulo em graus multiplica o resultado por 180/pi
}


# Função do pacote SBPitch pra criar o gráfico de um campo de futebol
# Passes que reultaram em chute pra gol
create_Pitch(grass_colour = "#538032", 
             line_colour =  "#ffffff", 
             background_colour = "#538032", 
             goaltype = "box") +
  geom_point(data = xg_data, aes(x = pass.location.1, y = pass.location.2), color = "darkgrey") +
  geom_point(data = xg_data, aes(x = pass.end_location.1, y = pass.end_location.2), color = "red") +
  geom_segment(data = xg_data,
               aes(x = pass.location.1, y = pass.location.2, xend = pass.end_location.1, yend = pass.end_location.2),
               color = "lightblue",
               arrow = arrow(type = "open",
                             length = unit(0.2, "cm")))


# Adicionando distância e ângulo do chute
xg_data <- xg_data %>%
  mutate(shot.length = LengthToGoal(shot.location.1, shot.location.2)) %>% 
  mutate(shot.angle = AngToGoal(shot.location.1, shot.location.2))

# Olhar os dados
xg_data %>%
  glimpse


# Função pra checar se o goleiro e/ou os defensores estão
# no triângulo formado pelo local do chute e as 2 traves
is_in_triangle <- function(x,y) {
  
  shot = c(x,y)
  post1 = c(120, 44)
  post2 = c(120, 36)
  
  ang.coef.1 = (post1[2] - shot[2])/(post1[1] - shot[1])
  ang.coef.2 = (post2[2] - shot[2])/(post2[1] - shot[1])
  
  #linha1
  y1 =  ang.coef.1*(xis - shot[1]) + shot[2]
  
  #linha2
  y2 =  ang.coef.2*(xis - shot[1]) + shot[2]
}

# Função pra checar se o goleiro e/ou os defensores estão
# no triângulo formado pelo local do chute e as 2 traves
teste <-  function(def.X, def.Y, shot.X, shot.Y, post1.X, post1.Y, post2.X, post2.Y){
  # Testar se o defensor está numa posição q poderia bloquear uma bola em direção ao gol

  post1.X = 120
  post1.Y = 44
  post2.X = 120
  post2.Y = 36
  
  #testar se está abaixo da linha formada pelo ponto do chute e trave de cima
  test1 = ((shot.X - def.X)*(post1.Y - def.Y) - (shot.Y - def.Y)*(post1.X - def.X) < 0)
  
  #testar se está a cima da linha formada pelo ponto do chute e trave de baixo
  test2 = ((shot.X - def.X)*(post2.Y - def.Y) - (shot.Y - def.Y)*(post2.X - def.X) > 0)
  
  return(test1 == TRUE & test2 == TRUE);
}

# Função pra calcular a distâncio do defensor para o local do chute
# dada a coordenada
LengthToDef <- function(shot.x, shot.y, def.x, def.y) {
  CatAdj <- abs(shot.x - def.x)
  CatOp <- abs(shot.y - def.y)   
  Hip <- sqrt(CatAdj^2 + CatOp^2)
  
  limit = 2
  
  return(list(Hip < limit,Hip))
}

# Ângulos dos bloqueadores
# Usar essa função pra calcular a variância entre os ângulos e com isso ver
# a possibilidade da bola ser bloqueada pelos defensores.
block <- function(shot.x, shot.y, def.x, def.y) {
  
  post1.X = 120
  post1.Y = 44
  
  dist1 = sqrt((shot.x - def.x)^2 + (shot.y - def.y)^2) 
  dist2 = sqrt((shot.x - post1.X)^2 + (shot.y - post1.Y)^2) 
  dist3 = sqrt((def.x - post1.X)^2 + (def.y - post1.Y)^2) 
  
  ang = acos((dist1^2 + dist2^2 - dist3^2)/(2*dist1*dist2))
  
  return(ang)
}


# Tirar os NULL's na lista do shot.freeze_frame (toda a linha)
xg_data <- xg_data %>%  
  filter( lengths(shot.freeze_frame) > 0 )


# Função que recebe um elemento do shot.freeze.frame e faz o espelhamento
# no segundo elemento da coluna location
# e retorna o elemento completo
espelhar <- function(df) {
  
  df$location <- modify(df$location, function(x) {x[2] <- 80-x[2]; x})
  
  return(df)
  
}


# Aplicando a função anterior a todos os elementos de shot.freeze.frame
xg_data$shot.freeze_frame <-  modify(xg_data$shot.freeze_frame, espelhar)


# A função recebe o dataframe que é um elemento da lista.
# Tem que ser usada em conjunto com o "map" se quiser percorrer a lista toda.
# A funcao retorna um valor pra cada elemento da lista
gk_goal <- function(x,y,z) { 
  #x é o shot.location.1
  #y é o shot.location.2
  #z é o shot.freeze_frame
  
  shot.X <- x
  shot.Y <- y
  post1.X = 120
  post1.Y = 44 + 1 # Adicionando um pouco por causa da curva da bola e por causa do tempo que o goleiro tem pra se mover durante a trajetória do chute
  post2.X = 120
  post2.Y = 36 - 1 # Adicionando um pouco por causa da curva da bola e por causa do tempo que o goleiro tem pra se mover durante a trajetória do chute
  
  # Posicao x do goleiro  
  def.X <-   z %>%
    dplyr::filter(teammate == FALSE & position.id == 1) %>%
    dplyr::pull(location) %>%
    purrr::pluck(1) %>% # Esse 1 é o 1º jogador da coluna location (de possíveis 10 pro caso dos defensores. ou o único pro caso do goleiro)
    purrr::pluck(1) # Esse 1 indica a coordenada x
  
  # Posicao y do goleiro
  def.Y <-   z %>%
    dplyr::filter(teammate == FALSE & position.id == 1) %>%
    dplyr::pull(location) %>%
    purrr::pluck(1) %>% # Esse 1 é o 1º jogador da coluna location (de possíveis 10 pro caso dos defensores. ou o único pro caso do goleiro)
    purrr::pluck(2) # Esse 1 indica a coordenada x 
  
  # Testar se está abaixo da linha formada pelo ponto do chute e trave de cima
  test1 = ((shot.X - def.X)*(post1.Y - def.Y) - (shot.Y - def.Y)*(post1.X - def.X) < 0)
  
  # Testar se está a cima da linha formada pelo ponto do chute e trave de baixo
  test2 = ((shot.X - def.X)*(post2.Y - def.Y) - (shot.Y - def.Y)*(post2.X - def.X) > 0)
  
  return(test1 == TRUE & test2 == TRUE);
  
}

# Aplicando a função anterior aos dados
gk_in_goal <- pmap(list(xg_data$shot.location.1,
                        xg_data$shot.location.2,
                        xg_data$shot.freeze_frame), ~gk_goal(..1, ..2, ..3))

# Trocar os NULL's por NA's pra não sumir observações quando fizer o unlist
gk_in_goal[lengths(gk_in_goal) == 0] <- NA

gk_in_goal <- gk_in_goal %>%  unlist

# Adicionando o que foi calculado numa coluna de mesmo nome
xg_data <- xg_data %>% mutate(gk_in_goal = gk_in_goal)


# Calcular a pressão no finalizador. Considera o total de jogadores num raio
# de 10 metros e suas distâncias.
qtd_def_2m <- function(x,y,z) { 
  #x é o shot.location.1
  #y é o shot.location.2
  #z é o shot.freeze_frame
  
  shot.X <- x
  shot.Y <- y
  
  # Posicao x de cada defensor  
  def.X <-   z %>%
    dplyr::filter(teammate == FALSE & position.id != 1) %>%
    dplyr::pull(location) %>%
    map(1) # map quando usado na forma map(lista, n) extrai o n-ésimo valor de cada elemento da lista
  
  # Posicao y de cada defensor
  def.Y <-   z %>%
    dplyr::filter(teammate == FALSE & position.id != 1) %>%
    dplyr::pull(location) %>%
    map(2) # map quando usado na forma map(lista, n) extrai o n-ésimo valor de cada elemento da lista
  
  valores <- map2(def.X, def.Y, function(def.X, def.Y){
    
    CatAdj <- abs(shot.X - def.X)
    CatOp <- abs(shot.Y - def.Y)   
    Hip <- sqrt(CatAdj^2 + CatOp^2)
    
    limit = 10
    
    return(Hip)
    
  }
  )
  
  limit = 10
  
  # Retira as distâncias que são 10m ou maiores e soma o inverso das outras distâncias
  # Quanto menor a distância e quanto mais jogadores maior o pressure
  # 1 no denominador pra garantir que não dê infinito
  # pressão que cada jogador exerce varia entre 0 e 1
  pressure <- map(valores, function(x) {if_else(x<limit, 1/(1+x), 0)}) %>%  unlist %>%  sum
  
  return(pressure)
}

# Aplicando a função aos dados
dists <- pmap_dbl(list(xg_data$shot.location.1,
                       xg_data$shot.location.2,
                       xg_data$shot.freeze_frame), ~qtd_def_2m(..1, ..2, ..3))

# Adicionando a nova variável aos dados
xg_data <- xg_data %>% mutate(pressure = dists)


# Calcula o total de defensores no ângulo de finalização
qtd_def_in_way <- function(x,y,z) { 
  #x é o shot.location.1
  #y é o shot.location.2
  #z é o shot.freeze_frame
  
  shot.X <- x
  shot.Y <- y
  post1.X = 120
  post1.Y = 44 + 1   # Adicionando um pouco por causa da curva da bola e por causa do tempo que os defensores têm pra se mover durante a trajetória do chute
  post2.X = 120
  post2.Y = 36 - 1   # Adicionando um pouco por causa da curva da bola e por causa do tempo que os defensores têm pra se mover durante a trajetória do chute
  
  # Posicao x dos defensores
  def.X <-   z %>%
    dplyr::filter(teammate == FALSE & position.id != 1) %>%
    dplyr::pull(location) %>%
    purrr::map(1) 
  
  # Posicao y dos defensores
  def.Y <-   z %>%
    dplyr::filter(teammate == FALSE & position.id != 1) %>%
    dplyr::pull(location) %>%
    purrr::map(2)
  
  valores <- map2(def.X, def.Y, function(def.X, def.Y){
    
    shot.X <- x
    shot.Y <- y
    post1.X = 120
    post1.Y = 44 + 1 # Adicionando um pouco por causa da curva da bola e por causa do tempo que os defensores têm pra se mover durante a trajetória do chute
    post2.X = 120
    post2.Y = 36 - 1 # Adicionando um pouco por causa da curva da bola e por causa do tempo que os defensores têm pra se mover durante a trajetória do chute
    
    # Testar se está abaixo da linha formada pelo ponto do chute e trave de cima
    test1 = ((shot.X - def.X)*(post1.Y - def.Y) - (shot.Y - def.Y)*(post1.X - def.X) < 0)
    
    # Testar se está a cima da linha formada pelo ponto do chute e trave de baixo
    test2 = ((shot.X - def.X)*(post2.Y - def.Y) - (shot.Y - def.Y)*(post2.X - def.X) > 0)
    
    return(test1 == TRUE & test2 == TRUE);
  }
  )
  
  valores <-  valores %>%  unlist
  
  return(sum(valores)) # Soma de um vetor lógico retorna a quantidade de TRUE's
}

# Aplicando a função aos dados
qtd <- pmap_int(list(xg_data$shot.location.1,
                     xg_data$shot.location.2,
                     xg_data$shot.freeze_frame), ~qtd_def_in_way(..1, ..2, ..3))

# Adicionando a nova variável aos dados
xg_data <- xg_data %>% mutate(def.in.way = qtd)


# Função pra calcular a variância entre os ângulos e com isso ver
# a possibilidade da bola ser bloqueada pelos defensores.
ang_blocks <- function(x,y,z) { 
  #x é o shot.location.1
  #y é o shot.location.2
  #z é o shot.freeze_frame
  
  shot.X <- x
  shot.Y <- y
  post1.X = 120
  post1.Y = 44 + 1
  
  # Posição x do defensor  
  def.X <-   z %>%
    dplyr::filter(teammate == FALSE & position.id != 1) %>%
    dplyr::pull(location) %>%
    purrr::map(1) 
  
  # Posição y do defensor
  def.Y <-   z %>%
    dplyr::filter(teammate == FALSE & position.id != 1) %>%
    dplyr::pull(location) %>%
    purrr::map(2)
  
  valores <- map2_dbl(def.X, def.Y, function(def.X, def.Y){
    
    shot.X <- x
    shot.Y <- y
    post1.X = 120
    post1.Y = 44 + 1 
    post2.X = 120
    post2.Y = 36 - 1 
    
    # Testar se está abaixo da linha formada pelo ponto do chute e trave de cima
    test1 = ((shot.X - def.X)*(post1.Y - def.Y) - (shot.Y - def.Y)*(post1.X - def.X) < 0)
    
    # Testar se está a cima da linha formada pelo ponto do chute e trave de baixo
    test2 = ((shot.X - def.X)*(post2.Y - def.Y) - (shot.Y - def.Y)*(post2.X - def.X) > 0)
    
    ang <-  NA
    
    if (test1 == TRUE & test2 == TRUE) {
     
      dist1 = sqrt((shot.X - def.X)^2 + (shot.Y - def.Y)^2) 
      dist2 = sqrt((shot.X - post1.X)^2 + (shot.Y - post1.Y)^2) 
      dist3 = sqrt((def.X - post1.X)^2 + (def.Y - post1.Y)^2) 
      
      ang <-   acos((dist1^2 + dist2^2 - dist3^2)/(2*dist1*dist2)) 
    }
    
    return(ang)
  }
  )

  angs <- var(valores, na.rm = TRUE)
  
  return(ifelse(test = is.na(angs), yes = 0, no = angs)) # Soma de um vetor lógico retorna a quantidade de TRUE's
}

# Aplicando a função aos dados
blocks.ang <- pmap_dbl(list(xg_data$shot.location.1,
                            xg_data$shot.location.2,
                            xg_data$shot.freeze_frame), ~ang_blocks(..1, ..2, ..3))

# Transformando os NA's (que é quando existem menos de 2 defensores no ângulo
# de finalização) em zero.
blocks.ang[is.na(blocks.ang)] <- 0

# Adicionando a variável aos dados
xg_data <- xg_data %>%  mutate(var.angles = blocks.ang)


# Função que calcula o Ângulo do passe em relação ao jogador que recebe e o centro do gol.
# Quanto menor o ângulo mais a bola deve mudar a trajetória em relação a um chute no centro do gol
# Quanto maior o ângulo menos a bola deve mudar a trajetória em relação a um chute no centro do gol
three.points.angle <- function(shot.x, shot.y, def.x, def.y) {
  #shot: local do chute
  #post: centro do gol
  #def: local do passe
  
  post1.X = 120
  post1.Y = 40 
  
  dist1 = sqrt((shot.x - def.x)^2 + (shot.y - def.y)^2) 
  dist2 = sqrt((shot.x - post1.X)^2 + (shot.y - post1.Y)^2) 
  dist3 = sqrt((def.x - post1.X)^2 + (def.y - post1.Y)^2) 
  
  ang = acos((dist1^2 + dist2^2 - dist3^2)/(2*dist1*dist2))
  
  return(ang)
}

# Aplicando a função aos dados e adicionando a nova variável
xg_data <-  xg_data %>%
  mutate(my.pass.angle = three.points.angle(shot.location.1, shot.location.2, pass.location.1, pass.location.2))


#Olhar os dados
xg_data %>%  glimpse


# Juntar todos os chute que não resultaram em gol em uma única categoria "No_Goal"
xg_data <- xg_data %>%
  mutate(shot.outcome.name = as_factor(shot.outcome.name)) %>%
  mutate(shot.outcome.name = fct_collapse(shot.outcome.name, No_Goal = c("Blocked",
                                                                         "Off T",
                                                                         "Saved",
                                                                         "Post",
                                                                         "Wayward",
                                                                         "Saved to Post",
                                                                         "Saved Off Target")))

# Olhar os dados
xg_data %>%  glimpse


# Pegando só finalizações de primeira, com o pé e que foram originados de passes.
# Rebotes não contam
xg_data <-  xg_data %>%
  filter(shot.first_time == TRUE & is.na(pass.location.1) == FALSE)

# Deixar os nomes com mesmo padrão
xg_data <- janitor::clean_names(xg_data)

# Escolhendo apenas as variáveis importantes
xg_data.h2o <- xg_data %>%  select(shot_outcome_name,
                                   pass_length, 
                                   my_pass_angle,
                                   pass_height_name,
                                   pass_speed_kmph,
                                   shot_length,
                                   shot_angle,
                                   gk_in_goal,
                                   pressure,
                                   def_in_way,
                                   var_angles) %>% mutate(pass_height_name = as_factor(pass_height_name))

# SALVAR O DATAFRAME EM UM ARQUIVO CSV
# write_csv(xg_data.h2o, "dados_modelo_futebol.csv")
