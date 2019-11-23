library(shiny)
shinyServer(function(input, output) {
        modelH <- lm(Height ~ Girth, data = trees)
        modelV <- lm(Volume ~ Girth, data = trees)
        predH <- reactive({
                predict(modelH, newdata = data.frame(Girth = input$slider))
        })
        predV <- reactive({
                predict(modelV, newdata = data.frame(Girth = input$slider))
        })
        output$plotH <- renderPlot({
                plot(trees$Girth, trees$Height, pch = 19, col = "green", 
                     cex = 1.5, xlab = "Diameter (inches)", 
                     ylab = "Height (feet)", 
                     main = "Black Cherry Tree Height vs Diameter", 
                     xlim = c(8, 21))
                abline(modelH, col = "orange", lwd = 3)
                abline(v = input$slider, lwd = 2, lty = 2, col = "red")
                abline(h = predH(), lwd = 2, lty = 2, col = "red")
        })
        output$predH <- renderText({
                round(predH(), 2)
        })
        output$plotV <- renderPlot({
                plot(trees$Girth, trees$Volume, pch = 19, col = "blue", 
                     cex = 1.5, xlab = "Diameter (inches)", 
                     ylab = "Volume (cubic feet)", 
                     main = "Black Cherry Tree Volume vs Diameter", 
                     xlim = c(8, 21), ylim = c(0, 80))
                abline(modelV, col = "orange", lwd = 3)
                abline(v = input$slider, lwd = 2, lty = 2, col = "red")
                abline(h = predV(), lwd = 2, lty = 2, col = "red")
        })
        output$predV <- renderText({
                round(predV(), 2)
        })
})