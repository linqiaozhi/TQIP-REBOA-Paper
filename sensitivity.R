library(ggplot2)


# We assume our confounder U has the following baseline relationships
Y_U0 = 0.2
U_A0 = 0.2

# Make up a confounder, B
B_A0 = U_A0
B_A1 = 0.2
Y_B0 = Y_U0
Y_B1 = 0.2
known_confounders  <- data.frame( x = B_A1, y = Y_B1, label = 'known confounder')


dev.off()
Y_A1 = 0.3
Y_A0 = 0.2
(d_abs <- Y_A1 - Y_A0)
(d_rel <- Y_A1 / Y_A0)
U_A1  <- seq(U_A0+0.01, 1, 0.001)
delta_abs  <- U_A1-U_A0
gamma_abs <- d_abs/delta_abs
Y_U1 <- gamma_abs+ Y_U0 
toplot_abs  <- data.frame( x= U_A1, y = Y_U1)
gamma_rel  <- (1-d_rel  + d_rel * U_A0-U_A1)/(d_rel*U_A0 - U_A1)
Y_U1 <- gamma_abs* Y_U0 
toplot_rel  <- data.frame( x= U_A1, y = Y_U1)
g <- ggplot( ) +  xlim(0,1) + ylim(0,1)  + geom_line(data = toplot_rel,mapping =  aes(x=x,y=y, color = 'relative')) +
    geom_line(data = toplot_abs,mapping =  aes(x=x,y=y, color = 'absolute')) + geom_point( data = known_confounders, aes(x=x, y=y, label = label)) + 
    xlab ('P( U = 1 | A = 1)\nRelationship of confounder with treatment') + ylab( 'P( Y =1 | U = 1)\nRelationship of confounder with outcome') + 
    ggtitle(sprintf('P(Y = 1 | A = 0) = %.2f,  P(Y = 1 | A=1) = %.2f\nRD: %.2f, RR: %.2f\nConfounder with P(U = 1 | A = 0) = %.2f and P(Y = 1 | U = 0) = %.2f', 
                    Y_A0, Y_A1,
                    d_abs, d_rel,
                    U_A0, Y_U0))
g 
