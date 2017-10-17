library(shiny)
shinyServer(function(input, output) {
  library(boot)
  mtcars$mpgsp <- ifelse(mtcars$mpg - 20 > 0, mtcars$mpg - 20, 0)
  model1 <- lm(hp ~ mpg, data = mtcars)
  model2 <- lm(hp ~ mpg + mpgsp, data = mtcars)
  model3 <- lm(log(hp) ~  mpg, data = mtcars)
  # rather than form in model3 above, could use nonlinear fitting to fit exponential as below
  # model3 <- nls(hp ~ exp(a + b * mpg), data = mtcars, start = list(a = 1, b = 1))
  
  model1pred <- reactive({
    mpgInput <- input$sliderMPG
    predict(model1, newdata = data.frame(mpg = mpgInput))
  })
  model1boot <- reactive({
    mpgInput <- input$sliderMPG
    # function to obtain prediction of y-value for input x-value from fit of data 
    value_slider <- function(formula, data, indices) {
      d <- data[indices,] # allows boot to select sample 
      fit <- lm(formula, data=d)
      return(predict(fit, newdata = data.frame(mpg = mpgInput)))
    } 
    # bootstrapping with 500 replications 
    results <- boot(data=mtcars, statistic=value_slider, 
                    R=500, formula=hp~mpg)
    ci <- boot.ci(results, type="bca")
    return(ci$bca[4:5])
  })
  model2pred <- reactive({
    mpgInput <- input$sliderMPG
    predict(model2, newdata =
              data.frame(mpg = mpgInput,
                         mpgsp = ifelse(mpgInput - 20 > 0,
                                        mpgInput - 20, 0)))
  })
  model2boot <- reactive({
    mpgInput <- input$sliderMPG
    # function to obtain prediction of y-value for input x-value from fit of data 
    value_slider <- function(formula, data, indices) {
      d <- data[indices,] # allows boot to select sample 
      fit <- lm(formula, data=d)
      return(predict(fit, newdata = data.frame(mpg = mpgInput,
                                               mpgsp = ifelse(mpgInput - 20 > 0,
                                                              mpgInput - 20, 0))))
    } 
    # bootstrapping with 500 replications 
    results <- boot(data=mtcars, statistic=value_slider, 
                    R=500, formula=hp ~ mpg + mpgsp)
    ci <- boot.ci(results, type="bca")
    return(ci$bca[4:5])
  })
  model3pred <- reactive({
    mpgInput <- input$sliderMPG
    exp(predict(model3, newdata = data.frame(mpg = mpgInput)))
  })
  model3boot <- reactive({
    mpgInput <- input$sliderMPG
    # function to obtain prediction of y-value for input x-value from fit of data 
    value_slider <- function(formula, data, indices) {
      d <- data[indices,] # allows boot to select sample 
      # fit <- nls(formula, data=d, start = list(a = 1, b = 1))  # (if nonlinear fitting)
      fit <- lm(formula, data=d)
      return(exp(predict(fit, newdata = data.frame(mpg = mpgInput))))
    } 
    # bootstrapping with 500 replications 
    results <- boot(data=mtcars, statistic=value_slider, 
                    R=500, formula=log(hp) ~  mpg)
    ci <- boot.ci(results, type="bca")
    return(ci$bca[4:5])
  })
  
  output$plot1 <- renderPlot({
    mpgInput <- input$sliderMPG
    
    plot(mtcars$mpg, mtcars$hp, xlab = "Miles Per Gallon", 
         ylab = "Horsepower", bty = "n", pch = 16,
         xlim = c(10, 35), ylim = c(50, 350))
    if(input$showModel1){
      abline(model1, col = "red", lwd = 2)
      points(mpgInput, model1pred(), col = "red", pch = 16, cex = 2)
    }
    if(input$showModel2){
      model2lines <- predict(model2, newdata = data.frame(
        mpg = 10:35, mpgsp = ifelse(10:35 - 20 > 0, 10:35 - 20, 0)
      ))
      lines(10:35, model2lines, col = "blue", lwd = 2)
      points(mpgInput, model2pred(), col = "blue", pch = 16, cex = 2)
    }
    if(input$showModel3){
      model3lines <- exp(predict(model3, newdata = data.frame(
        mpg = 10:35      
      )))
      lines(10:35, model3lines, col = "green", lwd = 2)
      points(mpgInput, model3pred(), col = "green", pch = 16, cex = 2)
    }
    legend(25, 250, c("Lin", "Hoc", "Exp"), pch = 16, 
           col = c("red", "blue", "green"), bty = "n", cex = 1.2)
  })

  output$pred1 <- renderText({
    round(model1pred(),2)
  })
  output$boot1 <- renderText({
    c(round(model1boot(), 2), "(", round((model1boot()[2]-model1boot()[1])/model1pred(),2), ")")
  })
  output$pred2 <- renderText({
    round(model2pred(),2)
  })
  output$boot2 <- renderText({
    c(round(model2boot(), 2), "(", round((model2boot()[2]-model2boot()[1])/model2pred(),2), ")")
  })
  output$pred3 <- renderText({
    round(model3pred(),2)
  })
  output$boot3 <- renderText({
    c(round(model3boot(), 2), "(", round((model3boot()[2]-model3boot()[1])/model3pred(),2), ")")
  })
})