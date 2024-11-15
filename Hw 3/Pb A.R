library(readr)
library(ggplot2)
library(igraph)

colnames(dnc_data) <- c("recip1ID", "recip2ID", "nummsgs")

filtered_data <- dnc_data[dnc_data$recip1ID < dnc_data$recip2ID, c("recip1ID", "recip2ID", "nummsgs")]

g <- graph_from_data_frame(filtered_data, directed = FALSE)

degrees <- degree(g)
degree_dist <- table(degrees)

log_i <- log(as.numeric(names(degree_dist)))
log_mi <- log(as.numeric(degree_dist))


model <- lm(log_mi - log_i)
summary(model)

coefficients <- coef(model)
intercept <- coefficients[1]
slope <- coefficients[2]

plot <- ggplot(data.frae(log_i = log_i, log_mi = log_mi), aes(x = log_i, y = log_mi)) + geom.point() + geom_abline(intercept = intercept, slope = slope, color = "lightblue") + labs(title = "Plot of Degree Distribution", x = "log(i)", y = "log(m_i)") + theme_minimal()


print(plot)