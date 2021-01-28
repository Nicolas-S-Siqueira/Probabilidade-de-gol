
library(tidyverse)
library(tidymodels)
library(glmnet)
library(shinyBS) # tooltip
library(shinydashboard) # valueBox
library(shinyWidgets) # useshinydashboard()



# Carregar o modelo
load("fit_glm.RData")


# Criar campo de futebol
pitch_plot <-  SBpitch::create_Pitch(grass_colour = "#065400",
                                     line_colour =  "#ffffff",
                                     background_colour = "#065400",
                                     goaltype = "box")


# Funções
#

# Função pra calcular a distancio do passe dada a coordenada
passLength <- function(pass.x, pass.y, shot.x, shot.y) {
  CatAdj <- abs(shot.x - pass.x)
  CatOp <- abs(shot.y - pass.y)   
  Hip <- sqrt(CatAdj^2 + CatOp^2)
  
  return(Hip)
}


# Função pra calcular o ângulo entre o local do passe, local do chute e o centro do gol
three.points.angle <- function(shot.x, shot.y, pass.x, pass.y) {
  # Shot: local do chute
  # Pass: origem do passe
  # GCenter: centro do gol 
  
  GCenter.X = 120
  GCenter.Y = 40 
  
  dist1 = sqrt((shot.x - pass.x)^2 + (shot.y - pass.y)^2) 
  dist2 = sqrt((shot.x - GCenter.X)^2 + (shot.y - GCenter.Y)^2) 
  dist3 = sqrt((pass.x - GCenter.X)^2 + (pass.y - GCenter.Y)^2) 
  
  
  ang = acos((dist1^2 + dist2^2 - dist3^2)/(2*dist1*dist2))
  
  return(ang)
}



# Função pra calcular a distancio do chute para o centro do gol dada a coordenada
LengthToGoal <- function(x,y) {
  CatAdj <- abs(x - 120)
  CatOp <- abs(y - 40)   #120,40 é o centro do gol
  Hip <- sqrt(CatAdj^2 + CatOp^2)
  
  return(Hip)
}



# Função pra calcular o angulo do chute dada a coordenada
AngToGoal <- function(x,y) {
  CatAdj <- abs(x - 120)
  CatOp <- abs(y - 40)   #120,40 é o centro do gol
  Hip <- sqrt(CatAdj^2 + CatOp^2)
  
  CatOp/Hip 
  
  Ang <-  asin(CatOp/Hip)
  
  return(Ang)# Valor em radiano
  # Se quiser o ângulo em graus multiplica o resultado por 180/pi
}





#Calcular a pressão no finalizador. Considera o total de jogadores num raio
# de 10 metros e suas distâncias.
qtd_def_10m <- function(shot.x, shot.y, def.x, def.y) { 
  
  
  
  
  
  
  valores <- purrr::map2(def.x, def.y, function(def.x, def.y){
    
    CatAdj <- abs(shot.x - def.x)
    CatOp <- abs(shot.y - def.y)   
    Hip <- sqrt(CatAdj^2 + CatOp^2)
    
    
    
    return(Hip)
    
    
    
  }
  )
  
  
  
  limit = 10
  
  
  
  # Retira as distancias que são 10m ou maiores e soma o inverso das outras distancias
  # quanto menor a distância e quanto mais jogadores, maior o "pressure".
  # O "1" no denominador é pra garantir que não dê infinito.
  # A pressão que cada jogador exerce varia entre 0 e 1.
  pressure <- purrr::map(valores, function(x) {if_else(x<limit, 1/(1+x), 0)}) %>%  unlist %>%  sum
  
  
  return(pressure)
}




# Calcula o total de defensores no ângulo de finalização
qtd_def_in_way <- function(shot.x, shot.y, def.x, def.y) { 
  
  
  
  
  valores <- purrr::map2(def.x, def.y, function(def.x, def.y){
    
    
    post1.x = 120
    post1.y = 44 + 1 # adicionando um pouco por causa da curva da bola e por causa do tempo que os defensores têm pra se mover durante a trajetória do chute
    post2.x = 120
    post2.y = 36 - 1 # adicionando um pouco por causa da curva da bola e por causa do tempo que o goleiro tem pra se mover durante a trajetoria do chute
    
    
    #testar se está abaixo da linha formada pelo ponto do chute e trave de cima
    test1 = ((shot.x - def.x)*(post1.y - def.y) - (shot.y - def.y)*(post1.x - def.x) < 0)
    
    #testar se está a cima da linha formada pelo ponto do chute e trave de baixo
    test2 = ((shot.x - def.x)*(post2.y - def.y) - (shot.y - def.y)*(post2.x - def.x) > 0)
    
    return(test1 == TRUE & test2 == TRUE);
  }
  )
  
  valores <-  valores %>%  unlist
  
  return(sum(valores)) #soma de um vetor lógico retorna a quantidade de TRUE's
}










#Função pra calcular a variancia entre os ângulos e com isso ver
# a possibilidade da bola ser bloqueada pelos defensores.
ang_blocks <- function(shot.x, shot.y, def.x, def.y) { 
  
  
  valores <- purrr::map2_dbl(def.x, def.y, function(def.x, def.y){
    
    
    
    
    post1.x = 120
    post1.y = 44 + 1 
    post2.x = 120
    post2.y = 36 - 1 
    
    
    #testar se está abaixo da linha formada pelo ponto do chute e trave de cima
    test1 = ((shot.x - def.x)*(post1.y - def.y) - (shot.y - def.y)*(post1.x - def.x) < 0)
    
    #testar se está a cima da linha formada pelo ponto do chute e trave de baixo
    test2 = ((shot.x - def.x)*(post2.y - def.y) - (shot.y - def.y)*(post2.x - def.x) > 0)
    
    
    
    ang <-  NA
    
    
    
    
    
    if (test1 == TRUE & test2 == TRUE) {
      
      
      
      
      
      
      dist1 = sqrt((shot.x - def.x)^2 + (shot.y - def.y)^2) 
      dist2 = sqrt((shot.x - post1.x)^2 + (shot.y - post1.y)^2) 
      dist3 = sqrt((def.x - post1.x)^2 + (def.y - post1.y)^2) 
      
      
      ang <-   acos((dist1^2 + dist2^2 - dist3^2)/(2*dist1*dist2)) 
    }
    
    
    
    
    return(ang)
  }
  )

  
  angs <- var(valores, na.rm = TRUE)
  
  
  #se for NA retorna 0
  return(ifelse(test = is.na(angs), yes = 0, no = angs)) #soma de um vetor lógico retorna a quantidade de TRUE's
}









library(shiny)




# Códigos da interface
ui <- fluidPage( 
  tabsetPanel(type = "pills",
              
              
              # Aba "Probabilidade"
              tabPanel("Probabilidade", icon = icon("dice-two"),
                       
                       
                       
                       
                       sidebarLayout(
                         
                         sidebarPanel(width = 3, 
                                      style = "
                            
                            background-color: WhiteSmoke;
                            height: 705px;
                            border-color: grey;
                            border-top-color: white;
                            border-left-color: white;
                            border-width: 3px
                           ",
                                      
                                      # Aproximação
                                      sliderInput(inputId = "zoom_x",
                                                  label = "Aproximação",
                                                  value = 70, min = 0, max = 100),
                                      
                                      shinyBS::bsTooltip("zoom_x", "Ajusta a parte do campo exibida na imagem", placement = "bottom"),
                                      
                                      
                                      # Posição do goleiro
                                      radioButtons(inputId = "gk_in_goal", 
                                                   label = "Posição do goleiro",
                                                   choices = c("No gol" = TRUE,
                                                               "Fora do Gol" = FALSE)
                                      ),
                                      
                                      # Descrição: posição do goleiro
                                      shinyBS::bsTooltip("gk_in_goal", "Avalia se está dentro ou fora da parte clara do campo (formada a partir do ângulo do chute).", placement = "bottom"),
                                      
                                      
                                      # Passe - Velocidade
                                      uiOutput(outputId = "tip"),
                                      
                                      
                                      
                                      
                                      # Altura do passe
                                      radioButtons(inputId = "pass_height", 
                                                   label = "Altura do passe",
                                                   choices = c("Rasteiro" = "Ground Pass",
                                                               "Médio" = "Low Pass",
                                                               "Aéreo" = "High Pass")
                                      ),
                                      
                                      # Descrição: altura do passe
                                      shinyBS::bsTooltip("pass_height",
                                                         "  <b>Rasteiro:</b> rente ao gramado <br><br>    <b>Médio:</b> no ponto mais alto da trajetória, a bola sobe à uma altura mais baixa que a do ombro <br><br>   <b>Aéreo:</b> no ponto mais alto da trajetória, a bola sobe à uma altura mais alta que a do ombro",
                                                         placement = "bottom")
                                      
                                      
                                      
                         ),
                         
                         mainPanel( 
                           useShinydashboard(),
                           
                           fluidRow(
                             
                             # ValueBox: qualidade da chance criada
                             column(12, uiOutput(outputId = "quali"))
                           ),
                           
                           tags$hr(),
                           
                           tags$br(),
                           
                           fluidRow(
                             
                             # Botão "Calcular"
                             column(5, actionButton(inputId = "calc",
                                                    label = "Calcular",
                                                    icon = icon("calculator"),
                                                    style = "
                              font-weight: bolder;
                              height: 100px;
                              width: 250px;
                              border-width: 5px;
                              background-color: whitesmoke
                              ")      
                             ),
                             
                             
                             # ValueBox: probabilidade de gol
                             column(7, uiOutput(outputId = "prob"))
                             
                           ),
                           
                           # Gráfico do campo na vertical
                           plotOutput(outputId = "halfpitch")
                           
                           
                                      
                         )
                         
                         
                       )
              ),
              
              
              
              # Aba "Posições"
              tabPanel("Posições", icon = icon("search-location"),
                       
                       navlistPanel(
                         
                         # Item "Tamanho dos jogadores"
                         tabPanel("Tamanho dos jogadores",  icon = icon("users"),
                                  
                                  # Gráfico do campo na horizontal
                                  plotOutput("pitch0", height = "485px"),
                                  
                                  
                                  
                                  # Ícone dos jogadores
                                  sliderInput(inputId = "player_size",
                                              label = "Ícones dos jogadores",
                                              value = 8, min = 2, max = 16)
                                  
                                  
                                  
                         ),
                         
                         
                         # Item "Finalizador"
                         tabPanel("Finalizador",  icon = icon("crosshairs"), 
                                  
                                  
                                  # Gráfico do campo na horizontal
                                  plotOutput("pitch1", click = "click_1", height = "485px")
                                  
                                  
                                  
                                  
                                  
                         ),
                         
                         # Item "Origem do passe"
                         tabPanel("Origem do passe",  icon = icon("people-arrows"),
                                  
                                  
                                  
                                  # Gráfico do campo na horizontal
                                  plotOutput("pitch2", click = "click_2", height = "485px")
                         ),
                         
                         
                         
                         # Item "Defensor 0"
                         tabPanel("Defensor 0", icon = icon("user-shield"),
                                  
                                  
                                  # Gráfico do campo na horizontal
                                  plotOutput("pitch3", click = "click_3", height = "485px"),
                                  
                                  #Botão para retirar o defensor
                                  actionButton(inputId = "remove0",
                                               label = "Remover",
                                               icon = icon("user-slash"),
                                               style = "height: 75px;
                                              width: 150px;
                                              font-weight: bolder;
                                              border-width: 3px
                                     ")
                         ),
                         
                         # Item "Defensor 1"
                         tabPanel("Defensor 1", icon = icon("user-shield"),
                                  
                                  
                                  # Gráfico do campo na horizontal
                                  plotOutput("pitch4", click = "click_4", height = "485px"),
                                  
                                  #Botão para retirar o defensor
                                  actionButton(inputId = "remove1",
                                               label = "Remover",
                                               icon = icon("user-slash"),
                                               style = "height: 75px;
                                              width: 150px;
                                              font-weight: bolder;
                                              border-width: 3px")
                                  #)
                                  
                         ),
                         
                         # Item "Defensor 2"
                         tabPanel("Defensor 2", icon = icon("user-shield"),
                                  
                                  # Gráfico do campo na horizontal
                                  plotOutput("pitch5", click = "click_5", height = "485px"),
                                  
                                  #Botão pra retirar o defensor
                                  actionButton(inputId = "remove2",
                                               label = "Remover",
                                               icon = icon("user-slash"),
                                               style = "height: 75px;
                                              width: 150px;
                                              font-weight: bolder;
                                              border-width: 3px")
                                  
                         ),
                         
                         # Item "Defensor 3"
                         tabPanel("Defensor 3", icon = icon("user-shield"),
                                  
                                  # Gráfico do campo na horizontal
                                  plotOutput("pitch6", click = "click_6", height = "485px"),
                                  
                                  #Botão pra retirar o defensor
                                  actionButton(inputId = "remove3",
                                               label = "Remover",
                                               icon = icon("user-slash"),
                                               style = "height: 75px;
                                              width: 150px;
                                              font-weight: bolder;
                                              border-width: 3px")
                                  
                         ),
                         
                         # Item "Defensor 4"
                         tabPanel("Defensor 4", icon = icon("user-shield"),
                                  
                                  # Gráfico do campo na horizontal
                                  plotOutput("pitch7", click = "click_7", height = "485px"),
                                  
                                  #Botão pra retirar o defensor
                                  actionButton(inputId = "remove4",
                                               label = "Remover",
                                               icon = icon("user-slash"),
                                               style = "height: 75px;
                                              width: 150px;
                                              font-weight: bolder;
                                              border-width: 3px")
                                  
                         ),
                         
                         # Item "Defensor 5"
                         tabPanel("Defensor 5", icon = icon("user-shield"),
                                  
                                  # Gráfico do campo na horizontal
                                  plotOutput("pitch8", click = "click_8", height = "485px"),
                                  
                                  #Botão pra retirar o defensor
                                  actionButton(inputId = "remove5",
                                               label = "Remover",
                                               icon = icon("user-slash"),
                                               style = "height: 75px;
                                              width: 150px;
                                              font-weight: bolder;
                                              border-width: 3px")
                                  
                         ),
                         
                         # Item "Defensor 6"
                         tabPanel("Defensor 6", icon = icon("user-shield"),
                                  
                                  # Gráfico do campo na horizontal
                                  plotOutput("pitch9", click = "click_9", height = "485px"),
                                  
                                  #Botão pra retirar o defensor
                                  actionButton(inputId = "remove6",
                                               label = "Remover",
                                               icon = icon("user-slash"),
                                               style = "height: 75px;
                                              width: 150px;
                                              font-weight: bolder;
                                              border-width: 3px")
                                  
                         ),
                         
                         # Item "Defensor 7"
                         tabPanel("Defensor 7", icon = icon("user-shield"),
                                  
                                  # Gráfico do campo na horizontal
                                  plotOutput("pitch10", click = "click_10", height = "485px"),
                                  
                                  #Botão pra retirar o defensor
                                  actionButton(inputId = "remove7",
                                               label = "Remover",
                                               icon = icon("user-slash"),
                                               style = "height: 75px;
                                              width: 150px;
                                              font-weight: bolder;
                                              border-width: 3px")
                                  
                         ),
                         
                         # Item "Defensor 8"
                         tabPanel("Defensor 8", icon = icon("user-shield"),
                                  
                                  # Gráfico do campo na horizontal
                                  plotOutput("pitch11", click = "click_11", height = "485px"),
                                  
                                  #Botão pra retirar o defensor
                                  actionButton(inputId = "remove8",
                                               label = "Remover",
                                               icon = icon("user-slash"),
                                               style = "height: 75px;
                                              width: 150px;
                                              font-weight: bolder;
                                              border-width: 3px")
                                  
                         ),
                         
                         # Item "Defensor 9"
                         tabPanel("Defensor 9", icon = icon("user-shield"),
                                  
                                  # Gráfico do campo na horizontal
                                  plotOutput("pitch12", click = "click_12", height = "485px"),
                                  
                                  #Botão pra retirar o defensor
                                  actionButton(inputId = "remove9",
                                               label = "Remover",
                                               icon = icon("user-slash"),
                                               style = "height: 75px;
                                              width: 150px;
                                              font-weight: bolder;
                                              border-width: 3px")
                                  
                         )
                         
                       )
                       
              ),
              
              
              
              
              
              
              # Aba "Exemplos"
              tabPanel("Exemplos", icon = icon("lightbulb"),
                       
                       navlistPanel(
                         
                         # Item "Situação 1"
                         tabPanel("Situação 1", icon = icon("bookmark"),
                                  
                                  
                                  
                                  fluidRow(
                                    
                                    column(8,tags$h1("Passe para trás")),
                                    
                                    # Botão "Vincular"
                                    column(4, actionButton(inputId = "asensio",
                                                           label = "Vincular",
                                                           icon = icon("link"),
                                                           style = "width: 200px;
                                              height: 60px;
                                              font-weight: bolder;
                                              border-width: 3px"))
                                    
                                  ),
                                  
                                  
                                  
                                  tags$hr(),
                                  
                                  # GIF
                                  fluidRow(
                                    column(width  = 12, offset = 1,
                                           tags$image(src = "uefa_asensio.gif", height = 360, width = 640)
                                    )
                                  ),
                                  
                                  tags$br(),
                                  tags$br(),
                                  tags$br(),
                                  tags$hr(),
                                  
                                  
                                  # Fonte 1
                                  tags$h6("Imagens retiradas do canal da",
                                          tags$a(href = "https://www.youtube.com/uefatv/about", "UEFA", target = "_blank"),
                                          "no youtube"),
                                  
                                  # Fonte 2
                                  tags$h6("Gifs criados no site",
                                          tags$a(href = "https://www.gifs.com",
                                                 "gifs.com",
                                                 target = "_blank")),
                                 
                         ),
                         
                         # Item "Situação 2"
                         tabPanel("Situação 2", icon = icon("bookmark"),
                                  
                                  fluidRow(
                                    
                                    column(8,tags$h1("Cruzamento alto vindo de trás")),
                                    
                                    # Botão "Vincular"
                                    column(4, actionButton(inputId = "bale",
                                                           label = "Vincular",
                                                           icon = icon("link"),
                                                           style = "width: 200px;
                                              height: 60px;
                                              font-weight: bolder;
                                              border-width: 3px"))
                                    
                                  ),
                                  tags$hr(),
                                  
                                  # GIF
                                  fluidRow(
                                    column(width  = 12, offset = 1,
                                           tags$image(src = "uefa_bale.gif", height = 360, width = 640)
                                    )
                                  ),
                                  
                                  
                                  tags$br(),
                                  tags$br(),
                                  tags$br(),
                                  tags$hr(),
                                  
                                  
                                  # Fonte 1
                                  tags$h6("Imagens retiradas do canal da",
                                          tags$a(href = "https://www.youtube.com/uefatv/about",
                                                 "UEFA",
                                                 target = "_blank"),
                                          "no youtube"),
                                  
                                  # Fonte 2
                                  tags$h6("Gifs criados no site",
                                          tags$a(href = "https://www.gifs.com",
                                                 "gifs.com",
                                                 target = "_blank")),
                                   
                                  
                                  
                                  
                                  
                                  
                         ),
                         
                         # Item "Situação 3"
                         tabPanel("Situação 3", icon = icon("bookmark"),
                                  
                                  fluidRow(
                                    
                                    column(8,tags$h1("Cruzamento rasteiro pela frente")),
                                    
                                    # Botão "Vincular"
                                    column(4, actionButton(inputId = "carrasco",
                                                           label = "Vincular",
                                                           icon = icon("link"),
                                                           style = "width: 200px;
                                              height: 60px;
                                              font-weight: bolder;
                                              border-width: 3px"))
                                    
                                  ),
                                  
                                  tags$hr(),
                                  
                                  # GIF
                                  fluidRow(
                                    column(width  = 12, offset = 1,
                                           tags$image(src = "uefa_carrasco.gif", height = 360, width = 640)
                                    )
                                  ),
                                  
                                  
                                  tags$br(),
                                  tags$br(),
                                  tags$br(),
                                  tags$hr(),
                                  
                                  # Fonte 1
                                  tags$h6("Imagens retiradas do canal da",
                                          tags$a(href = "https://www.youtube.com/uefatv/about",
                                                 "UEFA",
                                                 target = "_blank"),
                                          "no youtube"),
                                  
                                  # Fonte 2
                                  tags$h6("Gifs criados no site",
                                          tags$a(href = "https://www.gifs.com",
                                                 "gifs.com",
                                                 target = "_blank")),
                                  
                                  
                                  
                         ),
                         
                         # Item "Situação 4"
                         tabPanel("Situação 4", icon = icon("bookmark"),
                                  
                                  
                                  fluidRow(
                                    
                                    column(8,tags$h1("Cruzamento rasteiro por trás")),
                                    
                                    # Botão "Vincular"
                                    column(4, actionButton(inputId = "cr7",
                                                           label = "Vincular",
                                                           icon = icon("link"),
                                                           style = "width: 200px;
                                              height: 60px;
                                              font-weight: bolder;
                                              border-width: 3px"))
                                    
                                  ),
                                  
                                  tags$hr(),
                                  
                                  # GIF
                                  fluidRow(
                                    column(width  = 12, offset = 1,
                                           tags$image(src = "uefa_cr7.gif", height = 360, width = 640)
                                    )
                                  ),
                                  
                                  
                                  tags$br(),
                                  tags$br(),
                                  tags$br(),
                                  tags$hr(),
                                  
                                  # Fonte 1
                                  tags$h6("Imagens retiradas do canal da",
                                          tags$a(href = "https://www.youtube.com/uefatv/about",
                                                 "UEFA",
                                                 target = "_blank"),
                                          "no youtube"),
                                  
                                  # Fonte 2
                                  tags$h6("Gifs criados no site",
                                          tags$a(href = "https://www.gifs.com",
                                                 "gifs.com",
                                                 target = "_blank")),
                                  
                                  
                         ),
                         
                         # Item "Situação 5"
                         tabPanel("Situação 5", icon = icon("bookmark"),
                                  
                                  
                                  fluidRow(
                                    
                                    column(8,tags$h1("Passe longo em profundidade")),
                                    
                                    # Botão "Vincular"
                                    column(4, actionButton(inputId = "crespo",
                                                           label = "Vincular",
                                                           icon = icon("link"),
                                                           style = "width: 200px;
                                              height: 60px;
                                              font-weight: bolder;
                                              border-width: 3px"))
                                    
                                  ),
                                  
                                  tags$hr(),
                                  
                                  # GIF
                                  fluidRow(
                                    column(width  = 12, offset = 1,
                                           tags$image(src = "uefa_crespo.gif", height = 360, width = 640)
                                    )
                                  ),
                                  
                                  
                                  tags$br(),
                                  tags$br(),
                                  tags$br(),
                                  tags$hr(),
                                  
                                  # Fonte 1
                                  tags$h6("Imagens retiradas do canal da",
                                          tags$a(href = "https://www.youtube.com/uefatv/about",
                                                 "UEFA",
                                                 target = "_blank"),
                                          "no youtube"),
                                  
                                  # Fonte 2
                                  tags$h6("Gifs criados no site",
                                          tags$a(href = "https://www.gifs.com",
                                                 "gifs.com",
                                                 target = "_blank")),
                                  
                                  
                                  
                         ),
                         
                         # Item "Situação 6"
                         tabPanel("Situação 6", icon = icon("bookmark"),
                                  
                                  
                                  fluidRow(
                                    
                                    column(8,tags$h1("Cruzamento médio vindo do lado")),
                                    
                                    # Botão "Vincular"
                                    column(4, actionButton(inputId = "maldini",
                                                           label = "Vincular",
                                                           icon = icon("link"),
                                                           style = "width: 200px;
                                              height: 60px;
                                              font-weight: bolder;
                                              border-width: 3px"))
                                    
                                  ),
                                  
                                  tags$hr(),
                                  
                                  # GIF
                                  fluidRow(
                                    column(width  = 12, offset = 1,
                                           tags$image(src = "uefa_maldini.gif", height = 360, width = 640)
                                    )
                                  ),
                                  
                                  
                                  tags$br(),
                                  tags$br(),
                                  tags$br(),
                                  tags$hr(),
                                  
                                  # Fonte 1
                                  tags$h6("Imagens retiradas do canal da",
                                          tags$a(href = "https://www.youtube.com/uefatv/about",
                                                 "UEFA",
                                                 target = "_blank"),
                                          "no youtube"),
                                  
                                  # Fonte 2
                                  tags$h6("Gifs criados no site",
                                          tags$a(href = "https://www.gifs.com",
                                                 "gifs.com",
                                                 target = "_blank")),
                                                   
                                  
                         ),
                         
                         # Item "Situação 7"
                         tabPanel("Situação 7", icon = icon("bookmark"),
                                  
                                  
                                  fluidRow(
                                    
                                    column(8,tags$h1("Passe curto em profundidade")),
                                    
                                    # Botão "Vincular"
                                    column(4, actionButton(inputId = "rooney",
                                                           label = "Vincular",
                                                           icon = icon("link"),
                                                           style = "width: 200px;
                                              height: 60px;
                                              font-weight: bolder;
                                              border-width: 3px"))
                                    
                                  ),
                                  
                                  tags$hr(),
                                  
                                  # GIF
                                  fluidRow(
                                    column(width  = 12, offset = 1,
                                           tags$image(src = "uefa_rooney.gif", height = 360, width = 640)
                                    )
                                  ),
                                  
                                  
                                  tags$br(),
                                  tags$br(),
                                  tags$br(),
                                  tags$hr(),
                                  
                                  # Fonte 1
                                  tags$h6("Imagens retiradas do canal da",
                                          tags$a(href = "https://www.youtube.com/uefatv/about",
                                                 "UEFA",
                                                 target = "_blank"),
                                          "no youtube"),
                                  
                                  
                                  # Fonte 2
                                  tags$h6("Gifs criados no site",
                                          tags$a(href = "https://www.gifs.com",
                                                 "gifs.com",
                                                 target = "_blank")),
                                          
                                  
                                  
                                  
                                  
                         ),
                         
                         
                         # Item "Remover jogadores"
                         tabPanel("Remover jogadores", icon = icon("trash-alt"),
                                  
                                  tags$h1("Remover e selecionar posições manualmente"),
                                  tags$hr(),
                                  
                                  # Botão "Recomeçar"
                                  fluidRow(
                                    column(width  = 12, offset = 1,
                                           actionButton(inputId = "remove_all",
                                                        label = "Recomeçar",
                                                        icon = icon("refresh"),
                                                        style = "width: 640px;
                                              height: 360px;
                                              font-weight: bolder;
                                              border-width: 3px")
                                    )
                                  )
                                  
                                  
                                  
                         )
                         
                         
                       )
                       
              ),
              
             
              
              # Aba "Instruções"
              tabPanel("Instruções", icon = icon("question-circle"),
                       
                       
                       # Visão Geral
                       h1(strong("Visão geral")),
                       hr(),
                       
                       h3(p("Este aplicativo tem como objetivo medir a qualidade da chance criada em um jogo de futebol. Para uma maior precisão,
              serão consideradas apenas finalizações feitas com o pé, de primeira e originadas de um passe."), p("Você pode escolher a posição
              dos jogadores no momento da finalização e de qual local o passe foi originado, além de características do passe (a origem
              do passe é extraída da posição do passador no momento do passe)."), p("Depois que a situação de jogo estiver da forma
              desejada você pode clicar no botão", strong("Calcular"), "para descobrir a probabilidade de uma chance naquela situação 
              resultar em gol.")),
                       br(),
                       br(),
                       br(),
                       br(),
                       
                       
                       # Posições
                       h1(strong("Posições")),
                       hr(),
                       
                       h3("Na aba", strong("Posições"), "é possível atribuir a posição do finalizador, da origem do passe e de cada um dos 10 jogadores
              de linha da equipe adversária. Basta clicar na posição desejada no campo que o jogador será adicionado ou movido até a posição selecionada."),
                       br(),
                       br(),
                       br(),
                       
                       
                       # Probabilidade
                       h1(strong("Probabilidade")),
                       hr(),
                       
                       h3("Na aba", strong("Probabilidade,"), " além de inserir as últimas informações relacionadas à situação de jogo, é onde se calcula a
              qualidade da chance criada."),
                       br(),
                       br(),
                       br(),
                       
                       
                       # Exemplos
                       h1(strong("Exemplos")),
                       hr(),
                       
                       h3("Na aba", strong("Exemplos"), "encontram-se diversas situações reais de jogo. Você pode vincular essas situações ao aplicativo e fazer
                alterações à sua escolha ou calcular a probabilidade diretamente."),
                       br(),
                       br(),
                       br(),
                       br(),
                       
                       
                       # Dicas
                       h1(strong("Dicas")),
                       hr(),
                       
                       h3(tags$li("Com este aplicativo é possível recriar manualmente finalizações de uma partida de sua escolha
              ou criar cenários hipotéticos. Porém, para o segundo caso, é importante levar em conta que o aplicativo considera que o passe sempre chega
              ao companheiro mesmo que na prática isso seja improvável. Situações como passe longo e rasteiro
              ou passe longo e lento dificilmente chegariam ao companheiro em um jogo real.")),
                       
                       h3(tags$li("Para finalizações de cabeça ou qualquer outra parte do corpo, não é recomendada a utilização deste aplicativo
                        uma vez que a previsão será comprometida.")),
                       
                       h3(tags$li("Caso queira criar um cenário em que o chute não foi de primeira, uma boa abordagem seria 
              considerar o último toque do condutor da bola como um passe para si próprio. Rebotes também podem ser considerados passes,
                        incluindo local de origem, velocidade e altura.")),
                       
                       h3(tags$li("Você pode calcular as chances de um time e seu adversário numa partida
                       e somar as probabilidades para ter o número esperado de gols de ambos. Dessa forma, consegue-se uma
                                   ideia melhor do desempenho de cada equipe se comparado a olhar o placar ou simplesmente o total de finalizações de cada equipe.",
                                  p("Mas também é apenas uma aproximação afinal o modelo não considera uma bola que o atacante é desarmado em frente ao goleiro
                                  no último momento, não considera o corte do zagueiro que quase vira um gol contra nem a bola com o gol aberto que o atacante não alcança
                                    por centímetros."))),
                       
                       h3(tags$li("Apenas defensores a menos de 10 metros do finalizador e/ou que pudesse bloquear uma possível bola em direção ao gol são
              considerados no cálculo. Sendo assim, não é necessário especificar a posição dos 10 defensores.",
              p("Como referência, a distancia do círculo central pro centro do campo é de 9 metros e 15 centímetros."))),
                       
                       h3(tags$li("Cálculos desse tipo também são úteis pra medir a qualidade da finaliação de um jogador.
                         Caso muitos dados estejam disponíveis, pode-se comparar a quantidade esperada de gols, 
                         somando as probabilidades, com a quantidade real."))
                       
                       
                       
              ),
              
              
              
                       
              # Aba "Sobre o aplicativo"
              tabPanel("Sobre o aplicativo", icon = icon("info-circle"),
                       
                       hr(),
                       
                       # Texto e logo do StatsBomb
                       h3("Todos os dados utilizados foram fornecidos por", tags$a(tags$image(src = "statsbomb-logo.jpg", height = 105*0.3, width = 755*0.3),
                                                                                  href = "https://statsbomb.com/",
                                                                                  target = "_blank")),
                       
                       # Texto e logo do GitHub
                       h3("Acesse esses dados em ", tags$a(tags$image(src = "github-logo.jpg", height = 109*0.2, width = 366*0.2),
                                                       href = "https://github.com/statsbomb/open-data",
                                                       target = "_blank")),
                       
                       # Texto e logo do GitHub
                       h3("Acesse o código fonte do aplicativo em ", tags$a(tags$image(src = "github-logo.jpg", height = 109*0.2, width = 366*0.2),
                                                                           href = "https://github.com/Nicolas-S-Siqueira/Probabilidade-de-gol",
                                                                           target = "_blank")),
                       
                       hr(),
                       
                       br(),
                       br(),
                       br(),
                       
                       # Sobre os dados
                       h1("Sobre os dados"),
                       
                       hr(),
                       
                       
                       h4("Foram considerados dados de 878 partidas entre 2003 e 2020 incluindo os seguintes campeonatos: Liga dos campeões,
                          Super liga feminina da Inglaterra (FA WSL), Copa do mundo, Campeonato espanhol, Campeonato de futebol feminino dos Estados Unidos,
                          Campeonato inglês e Copa do mundo de futebol feminino.", p("Todos os gols na aba", strong("Exemplos"), "
                          aconteceram em finais de Liga dos campeões e estão no conjunto de dados.")),
                       
                       br(),
                       
                       # Limitações conhecidas
                       h1("Limitações conhecidas"),
                       
                       hr(),
                       
                       h4(tags$li("É fornecida nos dados a altura do passe que resulta na finalização mas não se sabe a altura da bola no momento do chute que seria uma informação mais relevante.")),
                         
                         h4(tags$li("O ângulo do corpo do finalizador e demais jogadores no momento do chute não é conhecido.")),
                           
                           h4(tags$li("A velocidade e direção dos jogadores no momento imediatamente anterior à finalização não são conhecidas.")),
                             
                             h4(tags$li("Não é levada em conta a qualidade do finalizador nem do goleiro.")),
                             
                               h4(tags$li("O cálculo da velocidade do passe desconsidera a curva e a altura da bola. 
                                           Considera apenas o tempo gasto pra sair do local de origem e chegar no ponto da finalização.")),
                             
                                  h4(tags$li("Foram utilizadas 878 partidas para a criação desse aplicativo. Caso fossem utilizados mais dados, a probabilidade
                                             seria ainda mais confiável.")),
                       
                       
              )
              
              
  )
)

# Códigos do servidor
server <- function(input, output, session) {
  
  # Valores que vão ser reconhecidos pelos cliques
  click_fin <- reactiveValues()
  
  # Valores que vão ser reconhecidos pelos cliques
  click_pass <- reactiveValues()
  
  # Valores que vão ser reconhecidos pelos cliques
  click_adv <- reactiveValues()
  
  #outras características do passe
  pass <- reactiveValues()
  
  
  # Posição do goleiro
  gk <- reactiveValues()
  
  
  # valores dos pontos nos gráficos (será usado pra criar os gráficos)
  pch <- reactiveValues()
  
  # Iniciar com passador e finalizador que sempre estarão presente nos gráficos
  pch$valores <- c("P", "F")
  
  
  # Sempre que um novo clique for dado no gráfico ou o botão recomeçar for selecionado fazer:
  observeEvent(c(click_adv$x0, click_adv$x1, click_adv$x2,
                 click_adv$x3, click_adv$x4, click_adv$x5,
                 click_adv$x6, click_adv$x7,
                 click_adv$x8, click_adv$x9, input$remove_all), {
                   
                   pch$valores <- c("P", "F")
                   
                   # Checar se o valor do click é NULL. Se não for adiciona ao "pch"
                   if (length(click_adv$x0) != 0) { pch$valores <- c("0", pch$valores) }
                   if (length(click_adv$x1) != 0) { pch$valores <- c("1", pch$valores) }
                   if (length(click_adv$x2) != 0) { pch$valores <- c("2", pch$valores) }
                   if (length(click_adv$x3) != 0) { pch$valores <- c("3", pch$valores) }
                   if (length(click_adv$x4) != 0) { pch$valores <- c("4", pch$valores) }
                   if (length(click_adv$x5) != 0) { pch$valores <- c("5", pch$valores) }
                   if (length(click_adv$x6) != 0) { pch$valores <- c("6", pch$valores) }
                   if (length(click_adv$x7) != 0) { pch$valores <- c("7", pch$valores) }
                   if (length(click_adv$x8) != 0) { pch$valores <- c("8", pch$valores) }
                   if (length(click_adv$x9) != 0) { pch$valores <- c("9", pch$valores) }
                   

                   
                   # Ordenar os números mas não as letras
                   # A ordem deve ser P, F , 0, 1, ..., 9
                   if (length(pch$valores) > 2) {
                     
                     pch$valores <- sort(pch$valores, partial = 1:(length(pch$valores) - 2)) 
                     
                   }
                   
                   
                   
                   
                   
                 })
  
  
  # Cor dos pontos nos gráficos (será usado pra criar os gráficos)
  color <- reactiveValues()
  
  # Iniciar com um azul pro finalizador e um azul pro marcador que sempre estarão presentes nos gráficos
  color$valores <- c("blue", "blue")
  
  
  # Sempre que um novo clique for dado no gráfico ou o botão recomeçar for selecionado fazer:
  observeEvent(c(click_adv$x0, click_adv$x1, click_adv$x2,
                 click_adv$x3, click_adv$x4, click_adv$x5,
                 click_adv$x6, click_adv$x7,
                 click_adv$x8, click_adv$x9, input$remove_all), {
                   
                   color$valores <- c("blue", "blue")
                   
                   # Checar se o valor do click é NULL. Se não for adiciona ao "color"
                   if (length(click_adv$x0) != 0) { color$valores <- c("red", color$valores) }
                   if (length(click_adv$x1) != 0) { color$valores <- c("red", color$valores) }
                   if (length(click_adv$x2) != 0) { color$valores <- c("red", color$valores) }
                   if (length(click_adv$x3) != 0) { color$valores <- c("red", color$valores) }
                   if (length(click_adv$x4) != 0) { color$valores <- c("red", color$valores) }
                   if (length(click_adv$x5) != 0) { color$valores <- c("red", color$valores) }
                   if (length(click_adv$x6) != 0) { color$valores <- c("red", color$valores) }
                   if (length(click_adv$x7) != 0) { color$valores <- c("red", color$valores) }
                   if (length(click_adv$x8) != 0) { color$valores <- c("red", color$valores) }
                   if (length(click_adv$x9) != 0) { color$valores <- c("red", color$valores) }
                   
                   
                   
                 })
  
  
  
  

  
  # Criar um texto que dependa de outro valor nesse caso a altura do passe
  text <- reactiveValues()
  
  # Iniciar o valor do texto pro passe rasteiro
  text$pass <- "<h6>Velocidade média: passe <b>rasteiro</b></h6>  passe longo: vel média rasteiro    passe curto: vel média rasteiro   cruzamento : vel média rasteiro"
  
  # Modificar o texto de acordo com a altura do passe selecionada
  observeEvent(input$pass_height, {
    passe_rasteiro <- "<h6>Velocidade média: passe <b>rasteiro</b></h6>  até 20 metros: 39 Km/h   <br>    entre 20 e 40 metros: 52 Km/h <br>   mais que 40 metros : 55 Km/h"
    passe_medio <- "<h6>Velocidade média: passe <b>médio</b></h6>  até 20 metros: 38 Km/h <br>    entre 20 e 40 metros: 65 Km/h <br>   mais que 40 metros : 70 Km/h"
    passe_alto <- "<h6>Velocidade média: passe <b>aéreo</b></h6>  até 20 metros: 33 Km/h <br>   entre 20 e 40 metros: 59 Km/h <br>   mais que 40 metros : 63 Km/h"
    
    if (input$pass_height == "Ground Pass") { text$pass <- passe_rasteiro }
    if (input$pass_height == "Low Pass") { text$pass <- passe_medio }
    if (input$pass_height == "High Pass") { text$pass <- passe_alto }
    
  })
  
  # Atualizando o valor da posição do goleiro com o que foi imputado pelo usuário
  observeEvent(input$gk_in_goal, {
    gk$position <- input$gk_in_goal
    
  })
  
  # Valor inicial para a velocidade do passe
  pass$speed <- 50
  
  # Atualizando o valor da velocidade do passe com o que foi imputado pelo usuário
  observeEvent(input$pass_speed_kmph, {
    pass$speed <- input$pass_speed_kmph
    
  })
  
  
  
  # Atualizando o valor da altura do passe com o que foi imputado pelo usuário
  observeEvent(input$pass_height, {
    pass$height <- input$pass_height
    
  })
  
  
  # Valor inicial para o local da finalização (centro do campo)
  click_fin$x <- 60.0001
  click_fin$y <- 40
  
  
  # Atualizar o local da finalização de acordo com o clique no gráfico
  observeEvent(input$click_1, {
    click_fin$x <- input$click_1[[1]]
    click_fin$y <- input$click_1[[2]]
  })
  
  # Valor inicial para o local da origem do passe (centro do campo)
  click_pass$x <- 60
  click_pass$y <- 40
  
  # Atualizar o local da origem do passe de acordo com o clique no gráfico
  observeEvent(input$click_2, {
    click_pass$x <- input$click_2[[1]]
    click_pass$y <- input$click_2[[2]]
  })
  
  # Clique da coordenada do adversário 0
  observeEvent(input$click_3, {
    click_adv$x0 <- input$click_3[[1]]
    click_adv$y0 <- input$click_3[[2]]
  })
  
  # Clique da coordenada do adversário 1
  observeEvent(input$click_4, {
    click_adv$x1 <- input$click_4[[1]]
    click_adv$y1 <- input$click_4[[2]]
  })
  
  # Clique da coordenada do adversário 2
  observeEvent(input$click_5, {
    click_adv$x2 <- input$click_5[[1]]
    click_adv$y2 <- input$click_5[[2]]
  })
  
  # Clique da coordenada do adversário 3
  observeEvent(input$click_6, {
    click_adv$x3 <- input$click_6[[1]]
    click_adv$y3 <- input$click_6[[2]]
  })
  
  # Clique da coordenada do adversário 4
  observeEvent(input$click_7, {
    click_adv$x4 <- input$click_7[[1]]
    click_adv$y4 <- input$click_7[[2]]
  })
  
  # Clique da coordenada do adversário 5
  observeEvent(input$click_8, {
    click_adv$x5 <- input$click_8[[1]]
    click_adv$y5 <- input$click_8[[2]]
  })
  
  # Clique da coordenada do adversário 6
  observeEvent(input$click_9, {
    click_adv$x6 <- input$click_9[[1]]
    click_adv$y6 <- input$click_9[[2]]
  })
  
  # Clique da coordenada do adversário 7
  observeEvent(input$click_10, {
    click_adv$x7 <- input$click_10[[1]]
    click_adv$y7 <- input$click_10[[2]]
  })
  
  # Clique da coordenada do adversário 8
  observeEvent(input$click_11, {
    click_adv$x8 <- input$click_11[[1]]
    click_adv$y8 <- input$click_11[[2]]
  })
  
  # Clique da coordenada do adversário 9
  observeEvent(input$click_12, {
    click_adv$x9 <- input$click_12[[1]]
    click_adv$y9 <- input$click_12[[2]]
  })
  
  
  
  
  # Remover defensor 0 quando seu botão for selecionado
  observeEvent(input$remove0, {
    click_adv$x0 <- NULL
    click_adv$y0 <- NULL
  })
  
  # Remover defensor 1 quando seu botão for selecionado
  observeEvent(input$remove1, {
    click_adv$x1 <- NULL
    click_adv$y1 <- NULL
  })
  
  # Remover defensor 2 quando seu botão for selecionado
  observeEvent(input$remove2, {
    click_adv$x2 <- NULL
    click_adv$y2 <- NULL
  })
  
  # Remover defensor 3 quando seu botão for selecionado
  observeEvent(input$remove3, {
    click_adv$x3 <- NULL
    click_adv$y3 <- NULL
  })
  
  # Remover defensor 4 quando seu botão for selecionado
  observeEvent(input$remove4, {
    click_adv$x4 <- NULL
    click_adv$y4 <- NULL
  })
  
  # Remover defensor 5 quando seu botão for selecionado
  observeEvent(input$remove5, {
    click_adv$x5 <- NULL
    click_adv$y5 <- NULL
  })
  
  # Remover defensor 6 quando seu botão for selecionado
  observeEvent(input$remove6, {
    click_adv$x6 <- NULL
    click_adv$y6 <- NULL
  })
  
  # Remover defensor 7 quando seu botão for selecionado
  observeEvent(input$remove7, {
    click_adv$x7 <- NULL
    click_adv$y7 <- NULL
  })
  
  # Remover defensor 8 quando seu botão for selecionado
  observeEvent(input$remove8, {
    click_adv$x8 <- NULL
    click_adv$y8 <- NULL
  })
  
  # Remover defensor 9 quando seu botão for selecionado
  observeEvent(input$remove9, {
    click_adv$x9 <- NULL
    click_adv$y9 <- NULL
  })
  
  
  # gráfico para escolher o tamanho do ícone dos jogadores
  output$pitch0 <- renderPlot({
    pitch_plot +
      
      # Círculos
      geom_point(aes(x = c(click_adv$x0, click_adv$x1, click_adv$x2,
                           click_adv$x3, click_adv$x4, click_adv$x5,
                           click_adv$x6, click_adv$x7, click_adv$x8,
                           click_adv$x9, click_pass$x, click_fin$x),
                     y = c(click_adv$y0, click_adv$y1, click_adv$y2,
                           click_adv$y3, click_adv$y4, click_adv$y5,
                           click_adv$y6, click_adv$y7, click_adv$y8,
                           click_adv$y9, click_pass$y, click_fin$y)),
                 shape = 21, color = color$valores, fill = "white",
                 size = input$player_size) +
      
      # Letras e números
      geom_point(aes(x = c(click_adv$x0, click_adv$x1, click_adv$x2,
                           click_adv$x3, click_adv$x4, click_adv$x5,
                           click_adv$x6, click_adv$x7, click_adv$x8,
                           click_adv$x9, click_pass$x, click_fin$x),
                     y = c(click_adv$y0, click_adv$y1, click_adv$y2,
                           click_adv$y3, click_adv$y4, click_adv$y5,
                           click_adv$y6, click_adv$y7, click_adv$y8,
                           click_adv$y9, click_pass$y, click_fin$y)),
                 pch = pch$valores,
                 color = color$valores,  size = input$player_size/2)
    
  })
  
  
  # Gráfico que reconhece a coordenada do local da finalização
  output$pitch1 <- renderPlot({
    pitch_plot +
      geom_point(aes(x = c(click_adv$x0, click_adv$x1, click_adv$x2,
                           click_adv$x3, click_adv$x4, click_adv$x5,
                           click_adv$x6, click_adv$x7, click_adv$x8,
                           click_adv$x9, click_pass$x, click_fin$x),
                     y = c(click_adv$y0, click_adv$y1, click_adv$y2,
                           click_adv$y3, click_adv$y4, click_adv$y5,
                           click_adv$y6, click_adv$y7, click_adv$y8,
                           click_adv$y9, click_pass$y, click_fin$y)),
                 shape = 21, color = color$valores, fill = "white",
                 size = input$player_size) +
      
      geom_point(aes(x = c(click_adv$x0, click_adv$x1, click_adv$x2,
                           click_adv$x3, click_adv$x4, click_adv$x5,
                           click_adv$x6, click_adv$x7, click_adv$x8,
                           click_adv$x9, click_pass$x, click_fin$x),
                     y = c(click_adv$y0, click_adv$y1, click_adv$y2,
                           click_adv$y3, click_adv$y4, click_adv$y5,
                           click_adv$y6, click_adv$y7, click_adv$y8,
                           click_adv$y9, click_pass$y, click_fin$y)),
                 pch = pch$valores, 
                 color = color$valores,  size = input$player_size/2)    
  })
  
  # Gráfico que reconhece a coordenada do local da origem do passe
  output$pitch2 <- renderPlot({
    pitch_plot +
      
      geom_point(aes(x = c(click_adv$x0, click_adv$x1, click_adv$x2,
                           click_adv$x3, click_adv$x4, click_adv$x5,
                           click_adv$x6, click_adv$x7, click_adv$x8,
                           click_adv$x9, click_pass$x, click_fin$x),
                     y = c(click_adv$y0, click_adv$y1, click_adv$y2,
                           click_adv$y3, click_adv$y4, click_adv$y5,
                           click_adv$y6, click_adv$y7, click_adv$y8,
                           click_adv$y9, click_pass$y, click_fin$y)),
                 shape = 21, color = color$valores, fill = "white",
                 size = input$player_size) +
      
      geom_point(aes(x = c(click_adv$x0, click_adv$x1, click_adv$x2,
                           click_adv$x3, click_adv$x4, click_adv$x5,
                           click_adv$x6, click_adv$x7, click_adv$x8,
                           click_adv$x9, click_pass$x, click_fin$x),
                     y = c(click_adv$y0, click_adv$y1, click_adv$y2,
                           click_adv$y3, click_adv$y4, click_adv$y5,
                           click_adv$y6, click_adv$y7, click_adv$y8,
                           click_adv$y9, click_pass$y, click_fin$y)),
                 pch = pch$valores,
                 color = color$valores,  size = input$player_size/2)
  })
  
  # Gráfico que reconhece a coordenada do defensor 0
  output$pitch3 <- renderPlot({
    pitch_plot +
      
      geom_point(aes(x = c(click_adv$x0, click_adv$x1, click_adv$x2,
                           click_adv$x3, click_adv$x4, click_adv$x5,
                           click_adv$x6, click_adv$x7, click_adv$x8,
                           click_adv$x9, click_pass$x, click_fin$x),
                     y = c(click_adv$y0, click_adv$y1, click_adv$y2,
                           click_adv$y3, click_adv$y4, click_adv$y5,
                           click_adv$y6, click_adv$y7, click_adv$y8,
                           click_adv$y9, click_pass$y, click_fin$y)),
                 shape = 21, color = color$valores, fill = "white",
                 size = input$player_size) +
      
      geom_point(aes(x = c(click_adv$x0, click_adv$x1, click_adv$x2,
                           click_adv$x3, click_adv$x4, click_adv$x5,
                           click_adv$x6, click_adv$x7, click_adv$x8,
                           click_adv$x9, click_pass$x, click_fin$x),
                     y = c(click_adv$y0, click_adv$y1, click_adv$y2,
                           click_adv$y3, click_adv$y4, click_adv$y5,
                           click_adv$y6, click_adv$y7, click_adv$y8,
                           click_adv$y9, click_pass$y, click_fin$y)),
                 pch = pch$valores,
                 color = color$valores,  size = input$player_size/2)
  })
  
  # Gráfico que reconhece a coordenada do defensor 1
  output$pitch4 <- renderPlot({
    pitch_plot +
      
      geom_point(aes(x = c(click_adv$x0, click_adv$x1, click_adv$x2,
                           click_adv$x3, click_adv$x4, click_adv$x5,
                           click_adv$x6, click_adv$x7, click_adv$x8,
                           click_adv$x9, click_pass$x, click_fin$x),
                     y = c(click_adv$y0, click_adv$y1, click_adv$y2,
                           click_adv$y3, click_adv$y4, click_adv$y5,
                           click_adv$y6, click_adv$y7, click_adv$y8,
                           click_adv$y9, click_pass$y, click_fin$y)),
                 shape = 21, color = color$valores, fill = "white",
                 size = input$player_size) +
      
      geom_point(aes(x = c(click_adv$x0, click_adv$x1, click_adv$x2,
                           click_adv$x3, click_adv$x4, click_adv$x5,
                           click_adv$x6, click_adv$x7, click_adv$x8,
                           click_adv$x9, click_pass$x, click_fin$x),
                     y = c(click_adv$y0, click_adv$y1, click_adv$y2,
                           click_adv$y3, click_adv$y4, click_adv$y5,
                           click_adv$y6, click_adv$y7, click_adv$y8,
                           click_adv$y9, click_pass$y, click_fin$y)),
                 pch = pch$valores,
                 color = color$valores,  size = input$player_size/2)
  })
  
  # Gráfico que reconhece a coordenada do defensor 2
  output$pitch5 <- renderPlot({
    pitch_plot +
      
      geom_point(aes(x = c(click_adv$x0, click_adv$x1, click_adv$x2,
                           click_adv$x3, click_adv$x4, click_adv$x5,
                           click_adv$x6, click_adv$x7, click_adv$x8,
                           click_adv$x9, click_pass$x, click_fin$x),
                     y = c(click_adv$y0, click_adv$y1, click_adv$y2,
                           click_adv$y3, click_adv$y4, click_adv$y5,
                           click_adv$y6, click_adv$y7, click_adv$y8,
                           click_adv$y9, click_pass$y, click_fin$y)),
                 shape = 21, color = color$valores, fill = "white",
                 size = input$player_size) +
      
      geom_point(aes(x = c(click_adv$x0, click_adv$x1, click_adv$x2,
                           click_adv$x3, click_adv$x4, click_adv$x5,
                           click_adv$x6, click_adv$x7, click_adv$x8,
                           click_adv$x9, click_pass$x, click_fin$x),
                     y = c(click_adv$y0, click_adv$y1, click_adv$y2,
                           click_adv$y3, click_adv$y4, click_adv$y5,
                           click_adv$y6, click_adv$y7, click_adv$y8,
                           click_adv$y9, click_pass$y, click_fin$y)),
                 pch = pch$valores,
                 color = color$valores,  size = input$player_size/2)
  })
  
  
  # Gráfico que reconhece a coordenada do defensor 3
  output$pitch6 <- renderPlot({
    pitch_plot +
      
      geom_point(aes(x = c(click_adv$x0, click_adv$x1, click_adv$x2,
                           click_adv$x3, click_adv$x4, click_adv$x5,
                           click_adv$x6, click_adv$x7, click_adv$x8,
                           click_adv$x9, click_pass$x, click_fin$x),
                     y = c(click_adv$y0, click_adv$y1, click_adv$y2,
                           click_adv$y3, click_adv$y4, click_adv$y5,
                           click_adv$y6, click_adv$y7, click_adv$y8,
                           click_adv$y9, click_pass$y, click_fin$y)),
                 shape = 21, color = color$valores, fill = "white",
                 size = input$player_size) +
      
      geom_point(aes(x = c(click_adv$x0, click_adv$x1, click_adv$x2,
                           click_adv$x3, click_adv$x4, click_adv$x5,
                           click_adv$x6, click_adv$x7, click_adv$x8,
                           click_adv$x9, click_pass$x, click_fin$x),
                     y = c(click_adv$y0, click_adv$y1, click_adv$y2,
                           click_adv$y3, click_adv$y4, click_adv$y5,
                           click_adv$y6, click_adv$y7, click_adv$y8,
                           click_adv$y9, click_pass$y, click_fin$y)),
                 pch = pch$valores,
                 color = color$valores,  size = input$player_size/2)
  })
  
  
  # Gráfico que reconhece a coordenada do defensor 4
  output$pitch7 <- renderPlot({
    pitch_plot +
      
      geom_point(aes(x = c(click_adv$x0, click_adv$x1, click_adv$x2,
                           click_adv$x3, click_adv$x4, click_adv$x5,
                           click_adv$x6, click_adv$x7, click_adv$x8,
                           click_adv$x9, click_pass$x, click_fin$x),
                     y = c(click_adv$y0, click_adv$y1, click_adv$y2,
                           click_adv$y3, click_adv$y4, click_adv$y5,
                           click_adv$y6, click_adv$y7, click_adv$y8,
                           click_adv$y9, click_pass$y, click_fin$y)),
                 shape = 21, color = color$valores, fill = "white",
                 size = input$player_size) +
      
      geom_point(aes(x = c(click_adv$x0, click_adv$x1, click_adv$x2,
                           click_adv$x3, click_adv$x4, click_adv$x5,
                           click_adv$x6, click_adv$x7, click_adv$x8,
                           click_adv$x9, click_pass$x, click_fin$x),
                     y = c(click_adv$y0, click_adv$y1, click_adv$y2,
                           click_adv$y3, click_adv$y4, click_adv$y5,
                           click_adv$y6, click_adv$y7, click_adv$y8,
                           click_adv$y9, click_pass$y, click_fin$y)),
                 pch = pch$valores,
                 color = color$valores,  size = input$player_size/2)
  })
  
  
  # Gráfico que reconhece a coordenada do defensor 5
  output$pitch8 <- renderPlot({
    pitch_plot +
      
      geom_point(aes(x = c(click_adv$x0, click_adv$x1, click_adv$x2,
                           click_adv$x3, click_adv$x4, click_adv$x5,
                           click_adv$x6, click_adv$x7, click_adv$x8,
                           click_adv$x9, click_pass$x, click_fin$x),
                     y = c(click_adv$y0, click_adv$y1, click_adv$y2,
                           click_adv$y3, click_adv$y4, click_adv$y5,
                           click_adv$y6, click_adv$y7, click_adv$y8,
                           click_adv$y9, click_pass$y, click_fin$y)),
                 shape = 21, color = color$valores, fill = "white",
                 size = input$player_size) +
      
      geom_point(aes(x = c(click_adv$x0, click_adv$x1, click_adv$x2,
                           click_adv$x3, click_adv$x4, click_adv$x5,
                           click_adv$x6, click_adv$x7, click_adv$x8,
                           click_adv$x9, click_pass$x, click_fin$x),
                     y = c(click_adv$y0, click_adv$y1, click_adv$y2,
                           click_adv$y3, click_adv$y4, click_adv$y5,
                           click_adv$y6, click_adv$y7, click_adv$y8,
                           click_adv$y9, click_pass$y, click_fin$y)),
                 pch = pch$valores,
                 color = color$valores,  size = input$player_size/2)
  })
  
  
  # Gráfico que reconhece a coordenada do defensor 6
  output$pitch9 <- renderPlot({
    pitch_plot +
      
      geom_point(aes(x = c(click_adv$x0, click_adv$x1, click_adv$x2,
                           click_adv$x3, click_adv$x4, click_adv$x5,
                           click_adv$x6, click_adv$x7, click_adv$x8,
                           click_adv$x9, click_pass$x, click_fin$x),
                     y = c(click_adv$y0, click_adv$y1, click_adv$y2,
                           click_adv$y3, click_adv$y4, click_adv$y5,
                           click_adv$y6, click_adv$y7, click_adv$y8,
                           click_adv$y9, click_pass$y, click_fin$y)),
                 shape = 21, color = color$valores, fill = "white",
                 size = input$player_size) +
      
      geom_point(aes(x = c(click_adv$x0, click_adv$x1, click_adv$x2,
                           click_adv$x3, click_adv$x4, click_adv$x5,
                           click_adv$x6, click_adv$x7, click_adv$x8,
                           click_adv$x9, click_pass$x, click_fin$x),
                     y = c(click_adv$y0, click_adv$y1, click_adv$y2,
                           click_adv$y3, click_adv$y4, click_adv$y5,
                           click_adv$y6, click_adv$y7, click_adv$y8,
                           click_adv$y9, click_pass$y, click_fin$y)),
                 pch = pch$valores,
                 color = color$valores,  size = input$player_size/2)  
  })
  
  
  # Gráfico que reconhece a coordenada do defensor 7
  output$pitch10 <- renderPlot({
    pitch_plot +
      
      geom_point(aes(x = c(click_adv$x0, click_adv$x1, click_adv$x2,
                           click_adv$x3, click_adv$x4, click_adv$x5,
                           click_adv$x6, click_adv$x7, click_adv$x8,
                           click_adv$x9, click_pass$x, click_fin$x),
                     y = c(click_adv$y0, click_adv$y1, click_adv$y2,
                           click_adv$y3, click_adv$y4, click_adv$y5,
                           click_adv$y6, click_adv$y7, click_adv$y8,
                           click_adv$y9, click_pass$y, click_fin$y)),
                 shape = 21, color = color$valores, fill = "white",
                 size = input$player_size) +
      
      geom_point(aes(x = c(click_adv$x0, click_adv$x1, click_adv$x2,
                           click_adv$x3, click_adv$x4, click_adv$x5,
                           click_adv$x6, click_adv$x7, click_adv$x8,
                           click_adv$x9, click_pass$x, click_fin$x),
                     y = c(click_adv$y0, click_adv$y1, click_adv$y2,
                           click_adv$y3, click_adv$y4, click_adv$y5,
                           click_adv$y6, click_adv$y7, click_adv$y8,
                           click_adv$y9, click_pass$y, click_fin$y)),
                 pch = pch$valores,
                 color = color$valores,  size = input$player_size/2)  
  })
  
  
  # Gráfico que reconhece a coordenada do defensor 8
  output$pitch11 <- renderPlot({
    pitch_plot +
      
      geom_point(aes(x = c(click_adv$x0, click_adv$x1, click_adv$x2,
                           click_adv$x3, click_adv$x4, click_adv$x5,
                           click_adv$x6, click_adv$x7, click_adv$x8,
                           click_adv$x9, click_pass$x, click_fin$x),
                     y = c(click_adv$y0, click_adv$y1, click_adv$y2,
                           click_adv$y3, click_adv$y4, click_adv$y5,
                           click_adv$y6, click_adv$y7, click_adv$y8,
                           click_adv$y9, click_pass$y, click_fin$y)),
                 shape = 21, color = color$valores, fill = "white",
                 size = input$player_size) +
      
      geom_point(aes(x = c(click_adv$x0, click_adv$x1, click_adv$x2,
                           click_adv$x3, click_adv$x4, click_adv$x5,
                           click_adv$x6, click_adv$x7, click_adv$x8,
                           click_adv$x9, click_pass$x, click_fin$x),
                     y = c(click_adv$y0, click_adv$y1, click_adv$y2,
                           click_adv$y3, click_adv$y4, click_adv$y5,
                           click_adv$y6, click_adv$y7, click_adv$y8,
                           click_adv$y9, click_pass$y, click_fin$y)),
                 pch = pch$valores,
                 color = color$valores,  size = input$player_size/2)  
  })
  
  
  # Gráfico que reconhece a coordenada do defensor 9
  output$pitch12 <- renderPlot({
    pitch_plot +
      
      geom_point(aes(x = c(click_adv$x0, click_adv$x1, click_adv$x2,
                           click_adv$x3, click_adv$x4, click_adv$x5,
                           click_adv$x6, click_adv$x7, click_adv$x8,
                           click_adv$x9, click_pass$x, click_fin$x),
                     y = c(click_adv$y0, click_adv$y1, click_adv$y2,
                           click_adv$y3, click_adv$y4, click_adv$y5,
                           click_adv$y6, click_adv$y7, click_adv$y8,
                           click_adv$y9, click_pass$y, click_fin$y)),
                 shape = 21, color = color$valores, fill = "white",
                 size = input$player_size) +
      
      geom_point(aes(x = c(click_adv$x0, click_adv$x1, click_adv$x2,
                           click_adv$x3, click_adv$x4, click_adv$x5,
                           click_adv$x6, click_adv$x7, click_adv$x8,
                           click_adv$x9, click_pass$x, click_fin$x),
                     y = c(click_adv$y0, click_adv$y1, click_adv$y2,
                           click_adv$y3, click_adv$y4, click_adv$y5,
                           click_adv$y6, click_adv$y7, click_adv$y8,
                           click_adv$y9, click_pass$y, click_fin$y)),
                 pch = pch$valores,
                 color = color$valores,  size = input$player_size/2) 
  })
  
  ##########################################################################
  
  
  # Gráfico vertical da aba "Probabilidade"
  output$halfpitch <- renderPlot({
    
    # Transparência da seta do passe depende da altura do passe
    if (pass$height == "Ground Pass") { alpha1 <- 0.3 }
    if (pass$height == "Low Pass") { alpha1 <- 0.6 }
    if (pass$height == "High Pass") { alpha1 <- 1 }
    
    # Cor da seta do passe depende da velocidade do passe
    color_position <- as.integer(pass$speed)
    
    post1_x <- 120
    post1_y <- 36 - 1
    post2_x <- 120
    post2_y <- 44 + 1
    
    
    # Transparência do goleiro depende se ele está ou não no ângulo de finalização
    if (gk$position == FALSE) { alpha2 <- 0.3 }
    if (gk$position == TRUE) { alpha2 <- 1 }
    
    
    halfpitch <- pitch_plot + 
      
      # Triângulo formado pelo ângulo de finalização
      geom_polygon(aes(x = c(click_fin$x, post1_x, post2_x), y = c(click_fin$y, post1_y, post2_y)), fill = "#00FF33", alpha = 0.15) +
      
      # Círculos
      geom_point(aes(x = c(click_adv$x0, click_adv$x1, click_adv$x2,
                           click_adv$x3, click_adv$x4, click_adv$x5,
                           click_adv$x6, click_adv$x7, click_adv$x8,
                           click_adv$x9, click_pass$x, click_fin$x),
                     y = c(click_adv$y0, click_adv$y1, click_adv$y2,
                           click_adv$y3, click_adv$y4, click_adv$y5,
                           click_adv$y6, click_adv$y7, click_adv$y8,
                           click_adv$y9, click_pass$y, click_fin$y)),
                 shape = 21, color = color$valores, fill = "white",
                 size = input$player_size) +
      
      # Seta indicando a direção do passe
      geom_segment(aes(x = click_pass$x, y = click_pass$y,
                       xend = click_fin$x, yend = click_fin$y),
                   arrow = arrow(angle = 15, type = "closed"), size = 1.2, alpha = alpha1,
                   color = hcl.colors(n = 150, palette = "Zissou 1")[color_position]) +
      
      # Letras e números
      geom_point(aes(x = c(click_adv$x0, click_adv$x1, click_adv$x2,
                           click_adv$x3, click_adv$x4, click_adv$x5,
                           click_adv$x6, click_adv$x7, click_adv$x8,
                           click_adv$x9, click_pass$x, click_fin$x),
                     y = c(click_adv$y0, click_adv$y1, click_adv$y2,
                           click_adv$y3, click_adv$y4, click_adv$y5,
                           click_adv$y6, click_adv$y7, click_adv$y8,
                           click_adv$y9, click_pass$y, click_fin$y)),
                 pch = pch$valores,
                 color = color$valores,  size = input$player_size/2) +
      
      
      
      # Goleiro
      geom_point(aes(x = 120.5, y = 40), color = "red", fill = "white", shape = 24, size = 8, alpha = alpha2) +
      geom_point(aes(x = 120.5, y = 40), color = "red", pch = "G", size = 4, alpha = alpha2) +
      
      
      # espelhar os pontos
      coord_flip(xlim = c(input$zoom_x, 121.5)) + scale_y_reverse()
    
    # Mostrar o gráfico
    halfpitch
    
    
  })
  
  
  
  
  # Vincular a "Situação 1" ao aplicativo
  observeEvent(input$asensio, {
    
    click_fin$x <- 113
    click_fin$y <- 40
    click_pass$x <- 119
    click_pass$y <- 51
    click_adv$x0 <- NULL
    click_adv$y0 <- NULL
    click_adv$x1 <- 118
    click_adv$y1 <- 80 - 40
    click_adv$x2 <- 118
    click_adv$y2 <- 80 - 44 
    click_adv$x3 <- 111
    click_adv$y3 <- 80 - 40
    click_adv$x4 <- 112
    click_adv$y4 <- 80 - 17
    click_adv$x5 <- 108
    click_adv$y5 <- 80 - 30
    click_adv$x6 <- 119
    click_adv$y6 <- 80 - 30
    click_adv$x7 <- NULL
    click_adv$y7 <- NULL
    click_adv$x8 <- NULL
    click_adv$y8 <- NULL
    click_adv$x9 <- NULL
    click_adv$y9 <- NULL
    
    pass$speed <- 50.43957
    pass$height <- "Ground Pass"
    gk$position <- TRUE
    
    # Atualiza os valores dos marcadores na interface (opções e "sliders") para os valores vinculados
    updateSliderInput(session = session, inputId = "pass_speed_kmph", value = pass$speed)
    updateRadioButtons(session = session, inputId = "pass_height", selected = pass$height)
    updateRadioButtons(session = session, inputId = "gk_in_goal", selected = gk$position)
    
  })
  
  # Vincular a "Situação 2" ao aplicativo
  observeEvent(input$bale, {
    
    click_fin$x <- 104.7
    click_fin$y <- 41.9
    click_pass$x <- 99.1
    click_pass$y <- 65.2
    click_adv$x0 <- NULL
    click_adv$y0 <- NULL
    click_adv$x1 <- 109.6
    click_adv$y1 <- 80 - 39.3
    click_adv$x2 <- 107.7
    click_adv$y2 <- 80 - 42.9 
    click_adv$x3 <- 107.3
    click_adv$y3 <- 80 - 37.3
    click_adv$x4 <- 105
    click_adv$y4 <- 80 - 31.5
    click_adv$x5 <- 96.8
    click_adv$y5 <- 80 - 46.6
    click_adv$x6 <- 98.9
    click_adv$y6 <- 80 - 39.9
    click_adv$x7 <- 103.8
    click_adv$y7 <- 80 - 16.2
    click_adv$x8 <- 99.4
    click_adv$y8 <- 80 - 26
    click_adv$x9 <- 97.4
    click_adv$y9 <- 80 - 18.4
    pass$speed <- 70.02136
    pass$height <- "High Pass"
    gk$position <- TRUE
    
    # Atualiza os valores dos marcadores na interface (opções e "sliders") para os valores vinculados
    updateSliderInput(session = session, inputId = "pass_speed_kmph", value = pass$speed)
    updateRadioButtons(session = session, inputId = "pass_height", selected = pass$height)
    updateRadioButtons(session = session, inputId = "gk_in_goal", selected = gk$position)
  })
  
  
  # Vincular a "Situação 3" ao aplicativo
  observeEvent(input$carrasco, {
    
    click_fin$x <- 115
    click_fin$y <- 42
    click_pass$x <- 112
    click_pass$y <- 18
    click_adv$x0 <- NULL
    click_adv$y0 <- NULL
    click_adv$x1 <- 115
    click_adv$y1 <- 80 - 59
    click_adv$x2 <- 109
    click_adv$y2 <- 80 - 56 
    click_adv$x3 <- 100
    click_adv$y3 <- 80 - 60
    click_adv$x4 <- 98
    click_adv$y4 <- 80 - 54
    click_adv$x5 <- 106
    click_adv$y5 <- 80 - 48
    click_adv$x6 <- 113
    click_adv$y6 <- 80 - 38
    click_adv$x7 <- 114
    click_adv$y7 <- 80 - 38
    click_adv$x8 <- 113
    click_adv$y8 <- 80 - 45
    click_adv$x9 <- NULL
    click_adv$y9 <- NULL
    pass$speed <- 68.57213
    pass$height <- "Low Pass"
    gk$position <- TRUE
    
    # Atualiza os valores dos marcadores na interface (opções e "sliders") para os valores vinculados
    updateSliderInput(session = session, inputId = "pass_speed_kmph", value = pass$speed)
    updateRadioButtons(session = session, inputId = "pass_height", selected = pass$height)
    updateRadioButtons(session = session, inputId = "gk_in_goal", selected = gk$position)
  })
  
  # Vincular a "Situação 4" ao aplicativo
  observeEvent(input$cr7, {
    
    click_fin$x <- 105
    click_fin$y <- 36
    click_pass$x <- 108
    click_pass$y <- 22
    click_adv$x0 <- NULL
    click_adv$y0 <- NULL
    click_adv$x1 <- 113
    click_adv$y1 <- 80 - 54
    click_adv$x2 <- 99
    click_adv$y2 <- 80 - 51 
    click_adv$x3 <- 111
    click_adv$y3 <- 80 - 31
    click_adv$x4 <- 98
    click_adv$y4 <- 80 - 30
    click_adv$x5 <- 112
    click_adv$y5 <- 80 - 41
    click_adv$x6 <- 108
    click_adv$y6 <- 80 - 45
    click_adv$x7 <- 112
    click_adv$y7 <- 80 - 39
    click_adv$x8 <- NULL
    click_adv$y8 <- NULL
    click_adv$x9 <- NULL
    click_adv$y9 <- NULL
    pass$speed <- 54.16034
    pass$height <- "Ground Pass"
    gk$position <- TRUE
    
    # Atualiza os valores dos marcadores na interface (opções e "sliders") para os valores vinculados
    updateSliderInput(session = session, inputId = "pass_speed_kmph", value = pass$speed)
    updateRadioButtons(session = session, inputId = "pass_height", selected = pass$height)
    updateRadioButtons(session = session, inputId = "gk_in_goal", selected = gk$position)
  })
  
  
  # Vincular a "Situação 5" ao aplicativo
  observeEvent(input$crespo, {
    
    click_fin$x <- 101
    click_fin$y <- 43.8
    click_pass$x <- 57.9
    click_pass$y <- 25.8
    click_adv$x0 <- 95.3
    click_adv$y0 <- 80 - 37.2
    click_adv$x1 <- 93.1
    click_adv$y1 <- 80 - 46.8
    click_adv$x2 <- 90
    click_adv$y2 <- 80 - 54 
    click_adv$x3 <- NULL
    click_adv$y3 <- NULL
    click_adv$x4 <- NULL
    click_adv$y4 <- NULL
    click_adv$x5 <- NULL
    click_adv$y5 <- NULL
    click_adv$x6 <- NULL
    click_adv$y6 <- NULL
    click_adv$x7 <- NULL
    click_adv$y7 <- NULL
    click_adv$x8 <- NULL
    click_adv$y8 <- NULL
    click_adv$x9 <- NULL
    click_adv$y9 <- NULL
    pass$speed <- 57.09301
    pass$height <- "Low Pass"
    gk$position <- TRUE
    
    # Atualiza os valores dos marcadores na interface (opções e "sliders") para os valores vinculados
    updateSliderInput(session = session, inputId = "pass_speed_kmph", value = pass$speed)
    updateRadioButtons(session = session, inputId = "pass_height", selected = pass$height)
    updateRadioButtons(session = session, inputId = "gk_in_goal", selected = gk$position)
  })
  
  
  # Vincular a "Situação 6" ao aplicativo
  observeEvent(input$maldini, {
    
    click_fin$x <- 107.6
    click_fin$y <- 37.9
    click_pass$x <- 102.7
    click_pass$y <- 13.2
    click_adv$x0 <- 104.8
    click_adv$y0 <- 80 - 61.9
    click_adv$x1 <- 112.6
    click_adv$y1 <- 80 - 35.7
    click_adv$x2 <- 110.6
    click_adv$y2 <- 80 - 34.8
    click_adv$x3 <- 108.9
    click_adv$y3 <- 80 - 42.6
    click_adv$x4 <- 110.4
    click_adv$y4 <- 80 - 41
    click_adv$x5 <- 109.3
    click_adv$y5 <- 80 - 39.4
    click_adv$x6 <- 113.8
    click_adv$y6 <- 80 - 32.1
    click_adv$x7 <- 112.2
    click_adv$y7 <- 80 - 45.1
    click_adv$x8 <- 104.3
    click_adv$y8 <- 80 - 60.9
    click_adv$x9 <- 102.7
    click_adv$y9 <- 80 - 42.3
    pass$speed <- 100.0707
    pass$height <- "Low Pass"
    gk$position <- TRUE
    
    # Atualiza os valores dos marcadores na interface (opções e "sliders") para os valores vinculados
    updateSliderInput(session = session, inputId = "pass_speed_kmph", value = pass$speed)
    updateRadioButtons(session = session, inputId = "pass_height", selected = pass$height)
    updateRadioButtons(session = session, inputId = "gk_in_goal", selected = gk$position)
  })
  
  
  # Vincular a "Situação 7" ao aplicativo
  observeEvent(input$rooney, {
    
    click_fin$x <- 105
    click_fin$y <- 37
    click_pass$x <- 108
    click_pass$y <- 29
    click_adv$x0 <- 106
    click_adv$y0 <- 80 - 47
    click_adv$x1 <- 108
    click_adv$y1 <- 80 - 48
    click_adv$x2 <- 108
    click_adv$y2 <- 80 - 46 
    click_adv$x3 <- 109
    click_adv$y3 <- 80 - 39
    click_adv$x4 <- NULL
    click_adv$y4 <- NULL
    click_adv$x5 <- NULL
    click_adv$y5 <- NULL
    click_adv$x6 <- NULL
    click_adv$y6 <- NULL
    click_adv$x7 <- NULL
    click_adv$y7 <- NULL
    click_adv$x8 <- NULL
    click_adv$y8 <- NULL
    click_adv$x9 <- NULL
    click_adv$y9 <- NULL
    pass$speed <- 34.98023
    pass$height <- "Ground Pass"
    gk$position <- TRUE
    
    # Atualiza os valores dos marcadores na interface (opções e "sliders") para os valores vinculados
    updateSliderInput(session = session, inputId = "pass_speed_kmph", value = pass$speed)
    updateRadioButtons(session = session, inputId = "pass_height", selected = pass$height)
    updateRadioButtons(session = session, inputId = "gk_in_goal", selected = gk$position)
  })
  
  
  
  # Remover todos os defensores dos gráficos e voltar o finalizador, passador e velocidade do passe pros valores padrões
  observeEvent(input$remove_all, {
    
    click_fin$x <- 60.0001
    click_fin$y <- 40
    click_pass$x <- 60
    click_pass$y <- 40
    click_adv$x0 <- NULL
    click_adv$y0 <- NULL
    click_adv$x1 <- NULL
    click_adv$y1 <- NULL
    click_adv$x2 <- NULL
    click_adv$y2 <- NULL
    click_adv$x3 <- NULL
    click_adv$y3 <- NULL
    click_adv$x4 <- NULL
    click_adv$y4 <- NULL
    click_adv$x5 <- NULL
    click_adv$y5 <- NULL
    click_adv$x6 <- NULL
    click_adv$y6 <- NULL
    click_adv$x7 <- NULL
    click_adv$y7 <- NULL
    click_adv$x8 <- NULL
    click_adv$y8 <- NULL
    click_adv$x9 <- NULL
    click_adv$y9 <- NULL
    pass$speed <- 50 
    
    # Atualiza os valores dos marcadores na interface (opções e "sliders") para os valores padrões
    updateSliderInput(session = session, inputId = "pass_speed_kmph", value = pass$speed)
    updateRadioButtons(session = session, inputId = "pass_height", selected = pass$height)
    updateRadioButtons(session = session, inputId = "gk_in_goal", selected = gk$position)
  })
  
  # Criar um valor reativo pra probabilidade de gol
  probability <- reactiveValues()
  
  # Valor inicial pra probabilidade de gol
  probability$value <- 200
  
  # Valor inicial que aparecerá no ValueBox
  probability$pretty <- "Chance "
  
  # Aplicar o modelo à situação criada pelo usuário.
  # Sempre que o botão calcular for selecionado o modelo será aplicado.
  observeEvent(input$calc, {
    
    clicks_adv_x <- c(click_adv$x0, click_adv$x1, click_adv$x2, click_adv$x3, click_adv$x4, click_adv$x5, click_adv$x6, click_adv$x7, click_adv$x8, click_adv$x9)
    clicks_adv_y <- c(click_adv$y0, click_adv$y1, click_adv$y2, click_adv$y3, click_adv$y4, click_adv$y5, click_adv$y6, click_adv$y7, click_adv$y8, click_adv$y9)
    
    
    
    # Cálculo do pressure  
    pressure <- qtd_def_10m(shot.x = click_fin$x,               
                            shot.y = click_fin$y,
                            def.x = clicks_adv_x,
                            def.y = clicks_adv_y
    )
    
    # Cálculo do def_in_way
    def_in_way <- qtd_def_in_way(shot.x = click_fin$x,               
                                 shot.y = click_fin$y,
                                 def.x = clicks_adv_x,
                                 def.y = clicks_adv_y
    )
    
    # Cálculo do var_angles
    var_angles <- ang_blocks(shot.x = click_fin$x,               
                             shot.y = click_fin$y,
                             def.x = clicks_adv_x,
                             def.y = clicks_adv_y
    )
    
    # Cálculo do pass_length
    pass_length <- passLength(shot.x = click_fin$x,
                              shot.y = click_fin$y,
                              pass.x = click_pass$x, 
                              pass.y = click_pass$y
    )
    
    # Cálculo do my_pass_angle
    my_pass_angle <- three.points.angle(pass.x = click_pass$x, 
                                        pass.y = click_pass$y,
                                        shot.x = click_fin$x,
                                        shot.y = click_fin$y
    )
    
    
    
    
    # Cálculo do shot_length
    shot_length <- LengthToGoal(x = click_fin$x, y = click_fin$y)
    
    
    
    # Cálculo do shot_angle
    shot_angle <- AngToGoal(x = click_fin$x, y = click_fin$y)
    
    # Armazenando o valor da altura do passe
    pass_height_name <- pass$height
    
    # Armazenando o valor da velocidade do passe  
    pass_speed_kmph <- pass$speed
          
    # Armazenando o valor que indica se o goleiro está dentro ou fora do gol
    gk_in_goal <- gk$position  %>% as.logical()
      
    # Reunindo todas as informações disponíveis em um tibble (tipo de dataframe) nomeado
    #  apropriadamente para o modelo reconhecer as variáveis corretamente
    chance <- tibble(pass_length = pass_length,
                  my_pass_angle = my_pass_angle,
                  pass_height_name = pass_height_name,
                  pass_speed_kmph = pass_speed_kmph,
                  shot_length = shot_length,
                  shot_angle = shot_angle,
                  gk_in_goal = gk_in_goal,
                  pressure = pressure,
                  def_in_way = def_in_way,
                  var_angles = var_angles)
    
    
    
    
    
    # Garatir que a probabilidade dê 0 caso o chute ou o passe sejam originados de fora do campo
    if (click_fin$x >= 0 & click_fin$x <= 120 &
        click_fin$y >= 0 & click_fin$y <= 80  &
        click_pass$x >= 0 & click_pass$x <= 120 &
        click_pass$y >= 0 & click_pass$y <= 80) {
      
      # Aplicando o modelo e retirando apenas o valor da probabilidade
      probab <- stats::predict(object = fit_glm,
                               new_data = chance,
                               type = "prob")[1] %>%  as.numeric()*100
                                          
    }  else  { probab <- 0 }
    
    
    
    
    
    
    # Arredondando a probabilidade pra 2 casas decimais
    probab <- round(probab, digits = 2)
    probability$value <- round(probab, digits = 2)
    
    # Trocando o divisor de decimal de ponto para vírgula e adicionando o símbolo de percentual
    probability$pretty <- paste(prettyNum(probab, decimal.mark = ","), "%")
    
    
    
  })
  
  # Garantir que a probabilidade suma sempre que um valor relevante seja modificado
  observeEvent( c(click_adv$x0, click_adv$x1, click_adv$x2,
                  click_adv$x3, click_adv$x4, click_adv$x5,
                  click_adv$x6, click_adv$x7, click_adv$x8,
                  click_adv$x9, click_adv$y0, click_adv$y1,
                  click_adv$y2, click_adv$y3, click_adv$y4,
                  click_adv$y5, click_adv$y6, click_adv$y7,
                  click_adv$y8, click_adv$y9, click_fin$x,
                  click_fin$y, click_pass$x, click_pass$y,
                  pass$speed, pass$height, gk$position), {probability$pretty <- ""; probability$value <- 200})
  
  
  # Contruindo o ValueBox que irá mostrar o valor da probabilidade
  output$prob <-  renderUI({
    
    valueBox( value = probability$pretty,
              subtitle = "Probabilidade de gol",
              icon = icon("dice"),
              color = "blue",
              width = 13
    )
    
    
    
  })
  
  
  
  
  # Contruindo o ValueBox que irá mostrar a qualidade da chance criada
  # A cor e o texto dependem do valor da probabilidade
  output$quali <-  renderUI({
    
    if (probability$value == 200) { probability$quality <- "Chance "; color_quali <- "blue" } 
    if (probability$value == 0 ) { probability$quality <- "Perda da posse de bola!"; color_quali <- "blue"  }
    if (probability$value > 0 & probability$value < 1 ) { probability$quality <- "Chance péssima"; color_quali <- "red"  }
    if (probability$value > 1 & probability$value < 5 ) { probability$quality <- "Chance ruim"; color_quali <- "orange" }
    if (probability$value > 5 & probability$value < 15 ) { probability$quality <- "Chance regular"; color_quali <- "yellow" }
    if (probability$value > 15 & probability$value < 30 ) { probability$quality <- "Chance boa"; color_quali <- "lime" }
    if (probability$value > 30 & probability$value < 50 ) { probability$quality <- "Chance ótima"; color_quali <- "green" }
    if (probability$value > 50 & probability$value != 200) { probability$quality <- "Chance excelente"; color_quali <- "olive" }
    
    
    
    
    
    valueBox( value = probability$quality,
              subtitle = "Qualidade da chance criada",
              icon = icon("certificate"),
              color = color_quali,
              width = 13
    )
    
    
    
  })
  
  
  
  
  
  # Passe - Velocidade na aba "Probabilidade" e
  # Descrição: Passe - Velocidade
  # Teve que ser criado no servidor já que o valor da descrição (text$pass)
  #  depende do valor imputado pelo usuário em "Altura do passe"
  output$tip <- renderUI({
    
    tipify(el = sliderInput(inputId = "pass_speed_kmph",
                            label = "Passe - Velocidade",
                            value = isolate(pass$speed), min = 1, max = 150),
           title = text$pass,
           options = list(delay = as.numeric("500"))
           )
    
    
   
    
  })
  
  
  
}




# Rodar o aplicativo
shinyApp(ui = ui, server = server)

