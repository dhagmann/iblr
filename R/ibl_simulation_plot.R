ibl_simulation_plot <- function(simulation, noptions, plot_option) {
    props <- t(apply(simulation,1, function(x) prop.table(tabulate(x,noptions))))
    plot(props[,plot_option],type="l",ylim=c(0,1),pch=16,col="blue")
    abline(h=0.5)
}