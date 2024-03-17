library(shiny)
library(ggplot2)
library(gridExtra)
library(reshape2)

ui <- fluidPage(
  theme = shinythemes::shinytheme('lumen'),
  withMathJax(),
  fluidRow(
    column(12,offset = 4, titlePanel("Ondas de Choque - Gráfico Interativo"))
  ),
  fluidRow(
    column(12,
           tags$h4('Considere uma via com densidade de congestionamento \\(k_j\\) 
           veic/km, na qual ocorre uma interrupção de tráfego no km \\(x_{int}\\) 
           entre os minutos \\(t_i\\) e \\(t_f\\). Antes da interrupção, a velocidade,
           a densidade e o fluxo da corrente de tráfego são de \\(u_a\\) km/h, \\(k_a\\) veic/km
           e \\(q_a\\) veic/h, respectivamente. Os veículos retidos na interrupção aglomeram-se na
           densidade de congestionamento \\(k_j\\) e, assim que liberados, a corrente que se forma
           assume valores de velocidade, densidade e fluxo iguais a \\(u_a\\) km/h, \\(k_a\\) veic/km
           e \\(q_a\\) veic/h, respectivamente. Simule o gráfico das ondas de choque para
           diferentes valores destes parâmetros iniciais:')
    )
  ),
  fluidRow(
    column(4,
           sliderInput('xint',tags$h5('Local da interrupção (\\(x_{int}\\)), km da via:'),
                       25,35,30,1, width = "100%")
    ),
    column(4,
           sliderInput("tif", tags$h5('Períodos inicial (\\(t_i\\)) e final \\(t_f\\) da interrupção, em min:'),
                       min = 0, max = 20, value = c(5, 10), width = "100%")
    ),
    column(4,
           sliderInput('kj',tags$h5('Densidade de congestionamento (\\(k_j\\)), em veic/km:'),
                       80,100,90,1, width = "100%")
    )
  ),
  fluidRow(
    column(2,
           tags$h4('Antes', align = 'center'),
           hr(),
           sliderInput('ua',withMathJax(tags$h5('Velocidade (\\(u_a\\)), em km/h: ')),
                       50,70,60,1, width = "100%"),
           uiOutput("slider_ka", width = "100%"),
           sliderInput('qa',withMathJax(tags$h5('Fluxo (\\(q_a\\)), em veic/h: ')),
                       1500,2000,1500,10, width = "100%")
    ),
    
    column(8,
           plotOutput('sw')
    ),
    column(2,
           tags$h4('Depois', align = 'center'),
           hr(),
           sliderInput('ud',withMathJax(tags$h5('Velocidade (\\(u_d\\)), em km/h: ')),
                       50,70,50,1, width = "100%"),
           uiOutput("slider_kd"),
           sliderInput('qd',withMathJax(tags$h5('Fluxo (\\(q_d\\)), em veic/h: ')),
                       1500,2000,2000,10, width = "100%")
    )
    
  ),
  fluidRow(
    column(2, offset = 2,
           numericInput("n_veic", tags$h5("Quantos veículos mostrar?"), 
                        30, min = 10, max = 40, step = 10)
    ),
    column(8,
           tags$h4('Velocidade das ondas de choque'),
           uiOutput('usw')
    )
    
  ),
  fluidRow(
    
  )
)

server <- function(input, output, session){
  
  output$slider_ka <- renderUI({
    req(input$kj)
    sliderInput('ka',withMathJax(tags$h5('Densidade (\\(k_a\\)), em veic/km: ')),
                55,input$kj - 5,60,1, width = "100%")
  })
  
  output$slider_kd <- renderUI({
    req(input$kj)
    sliderInput('kd',withMathJax(tags$h5('Densidade (\\(k_d\\)), em veic/km: ')),
                55,input$kj -5,75,1, width = "100%")
  })
  
  output$sw <- renderPlot({
    req(input$kj); req(input$ka); req(input$kd); req(input$ua); req(input$ud)
    req(input$qa); req(input$qd); req(input$n_veic); req(input$xint); req(input$tif)
    # Parâmetros q, k, u antes (a) e depois (d) da interrupção:
    xint <- input$xint
    kj <- input$kj
    ua <- input$ua; ka <- input$ka; qa <- input$qa 
    ud <- input$ud; kd <- input$kd; qd <- input$qd
    n_veic <- input$n_veic
    # Velocidades das ondas de choque:
    usw12 <- qa/(ka - kj)
    usw23 <- qd/(kd - kj)
    usw24 <- 0 #Alterar aqui quando desenvolver modelo genérico
    usw13 <- (qa - qd)/(ka - kd)
    
    #Dissolução do pelotão
    t <- seq(0,0.5,0.0005)
    
    ti <- input$tif[1]/60
    tf <- input$tif[2]/60
    a12 <- usw12
    b12 <- xint - a12*ti
    a23 <- usw23
    b23 <- xint - a23*tf
    
    tp <- (b23 - b12)/(a12 - a23)
    xp <- usw23*tp + b23
    
    a24 <- usw24
    b24 <- xint #Alterar aqui quando desenvolver modelo genérico
    a13 <- usw13
    b13 <- xp - usw13*tp
    
    #Retas de cada veículo
    traf_lines <- function(bv, t){
      tI <- (b12 - bv)/(ua - usw12) #t da interseção c/ usw12
      xI <- usw12*tI + b12 #x da interseção c/ usw12]
      a1 <- ua; a2 <- 0
      b1 <- bv; b2 <- xI
      tII <- (b23 - b2)/(a2 - usw23) #t da interseção c/ usw23
      xII <- usw23*tII + b23 #x da interseção c/ usw23
      a3 <- ud;
      b3 <- xII - a3*tII
      if(tI < ti){
        xv <- a1*t + bv
      } else {
        if(usw12 <= usw23){
          xv <- ifelse(t < tI, a1*t + b1,
                       ifelse(t < tII, a2*t + b2, a3*t + b3))
        } else {
          if(usw13 > ud){
            if(tI < tp){
              tIII <- (b13 - b3)/(a3 - usw13);
              xIII <- a3*tIII + b3
              a4 <- ua
              b4 <- xIII - a4*tIII 
              xv <- ifelse(t < tI, a1*t + b1,
                           ifelse(t < tII, a2*t + b2,
                                  ifelse(t < tIII, a3*t + b3, a4*t + b4)))
            } else {
              xv <- ua*t + bv
            }
          } else if(usw13 < ua){
            if(tI < tp){
              xv <- ifelse(t < tI, a1*t + b1,
                           ifelse(t < tII, a2*t + b2, a3*t + b3))
            } else {
              tIV <- (b13 - bv)/(ua - usw13)
              xIV <- ua*tIV + bv
              a1 <- ua; a2 <- ud;
              b1 <- bv; b2 <- xIV - ud*tIV
              xv <- ifelse(t < tIV, a1*t + b1, a2*t + b2)
            }
          }  else if(usw13 >= ua & usw13 <= ud){
            if(tI < tp){
              xv <- ifelse(t < tI, a1*t + b1,
                           ifelse(t < tII, a2*t + b2, a3*t + b3))
            } else {
              xv <- ua*t + bv
            }
          } 
        }
      }
      return(xv)
    }
    
    
    ti12 <- ti; tf12 <- ifelse(a12 <= a23, max(t), tp)
    ti23 <- tf; tf23 <- ifelse(a12 <= a23, max(t), tp)
    ti24 <- ti; tf24 <- tf
    if(a12 > a23){
      ti13 <- tp
      tf13 <- max(t)
    }
    
    #AA: sw23, BB: sw12 e CC: sw24
    AA <- ifelse(t > tf23 | t < ti23, NA, a23*t + b23)
    BB <- ifelse(t > tf12 | t < ti12, NA, a12*t + b12)
    CC <- ifelse(t > tf24 | t < ti24, NA, a24*t + b24)
    if((a12 > a23) & !(usw13 >= ua & usw13 <= ud)){
      DD <- ifelse(t < tp, NA, a13*t + b13)
      df_sw <- data.frame(t = t, BB = BB, AA = AA, CC = CC, DD = DD)
    } else{
      df_sw <- data.frame(t = t, BB = BB, AA = AA, CC = CC)
    }
    
    df_sw <- melt(df_sw, id = 't')
    
    # Adicionando as linhas de tráfego
    
    veic <- seq(-.5*ua, 40, length.out = n_veic)
    
    df_obs <- as.data.frame(do.call(cbind,
                                    lapply(veic, function(x,t) traf_lines(x,t), t = t)))
    df_obs['t'] <- t
    df_obs <- melt(df_obs, id = 't')
    
    if((a12 > a23) & !(usw13 >= ua & usw13 <= ud)){
      ggplot() +
        geom_line(aes(x = t, y = value, color = variable), df_obs) +
        geom_line(aes(x = t, y = value, color = variable), df_sw, size = 1.2) +
        labs(x = 't (h)', y = 'Posição (km)', title = 'Gráfico - Ondas de Choque') +
        theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) +
        xlim(0,.5) +
        ylim(0,40) + 
        scale_color_manual('Onda de Choque', breaks = c('AA','BB','CC','DD'),
                           values=c(AA='red',BB='blue',CC ='green',DD='gold',
                                    rep('black', length(veic))))
    } else {
      ggplot() +
        geom_line(aes(x = t, y = value, color = variable), df_obs) +
        geom_line(aes(x = t, y = value, color = variable), df_sw, size = 1.2) +
        labs(x = 't (h)', y = 'Posição (km)', title = 'Gráfico - Ondas de Choque') +
        theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) +
        xlim(0,.5) +
        ylim(0,40) + 
        scale_color_manual('Onda de Choque', breaks = c('AA','BB','CC'),
                           values=c(AA='red',BB='blue',CC ='green',
                                    rep('black', length(veic))))
    }
  })
  
  output$usw <- renderUI({
    req(input$kj); req(input$ka); req(input$kd); req(input$ua); req(input$ud)
    req(input$qa); req(input$qd); req(input$n_veic); req(input$xint); req(input$tif)
    # Parâmetros q, k, u antes (a) e depois (d) da interrupção:
    kj <- input$kj
    ua <- input$ua; ka <- input$ka; qa <- input$qa 
    ud <- input$ud; kd <- input$kd; qd <- input$qd
    n_veic <- input$n_veic
    # Velocidades das ondas de choque:
    usw12 <- qa/(ka - kj)
    usw23 <- qd/(kd - kj)
    usw24 <- 0 #Alterar aqui quando desenvolver modelo genérico
    usw13 <- (qa - qd)/(ka - kd)
    #Linhas das ondas de choque:
    
    if((usw12 > usw23) & !(usw13 >= ua & usw13 <= ud)){
      withMathJax("\\(u_{sw}^{AA}\\) = ", round(usw23,2), 'km/h, ', '\\(u_{sw}^{BB}\\) = ', 
                  round(usw12,2), 'km/h, ', '\\(u_{sw}^{CC}\\) = ', round(usw24,2), ' km/h e ',
                  '\\(u_{sw}^{DD}\\) = ', round(usw13,2), ' km/h.')
    } else{
      withMathJax("\\(u_{sw}^{AA}\\) = ", round(usw23,2), 'km/h, ', '\\(u_{sw}^{BB}\\) = ', 
                  round(usw12,2), 'km/h',  ' e ', '\\(u_{sw}^{CC}\\) = ', round(usw24,2), ' km/h.')
      
    }
  })
  
}

shinyApp(ui = ui, server = server)
