library(lattice)
library(tidyverse)

hw <- theme_gray()+ theme(
  plot.title=element_text(hjust=0.5),
  plot.subtitle=element_text(hjust=0.5),
  plot.caption=element_text(hjust=-.5),
  
  strip.text.y = element_blank(),
  strip.background=element_rect(fill=rgb(.9,.95,1),
                                colour=gray(.5), linewidth =.2),
  
  panel.border=element_rect(fill=FALSE,colour=gray(.70)),
  panel.grid.minor.y = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.spacing.x = unit(0.10,"cm"),
  panel.spacing.y = unit(0.05,"cm"),
  
  # axis.ticks.y= element_blank()
  axis.ticks=element_blank(),
  axis.text=element_text(colour="black"),
  axis.text.y=element_text(margin=margin(0,3,0,3)),
  axis.text.x=element_text(margin=margin(-1,0,3,0))
)

Massachusetts <- c(52,25,10,7,6)
NewYork <- c(49,27,10,7,7)
DC <- c(45,28,12,8,7)
Wisconsin <- c(43,28,10,5,14)
California <- c(41,33,11,6,9)
Hawaii <- c(39,37,10,7,7)
Colarado <- c(34,36,13,6,11)
Florida <- c(33,25,13,22,7)
Texas <- c(32,25,26,7,10)
Mississippi <- c(32,16,37,7,8)

mat <- cbind(Massachusetts,NewYork,DC,Wisconsin,California,Hawaii,Colarado,Florida,Texas,Mississippi)
mat

type <- c("Apple Pie", "Pumpkin Pie", "Pecan Pie", "Key Lime Pie", "Cherry Pie")
City <- c("Massachusetts", "NewYork","DC","Wisconsin","California","Hawaii","Colarado","Florida","Texas","Mississippi")


colnames(mat) <- City
rownames(mat) <- type 
mat


oneColumn = dotplot(mat, groups = FALSE,
                    layout = c(5, 5), aspect = 0.7,
                    origin = 0,type = c("p","h"),
                    main = "Pies Americans Googled for in the last five years",
                    scales = list(x = list(tck = 0, alternating = FALSE)),
                    panel = function(...){
                      panel.fill(rgb(.9,.9,.9))
                      panel.grid(h = 0,v = -1,col = "white",lwd = 2)
                      panel.dotplot(col = rgb(0,.5,1),cex = 1.1,...)
                    }
)
oneColumn

mat_df <- as.data.frame(mat)
mat_df$City <- rownames(mat_df)

# Transform to long format
mat_long <- pivot_longer(mat_df, cols = -City, names_to = "Type", values_to = "Count")

# Create the scatter plot
scatter_plot <- ggplot(mat_long, aes(x = City, y = Count, color = Type)) +
  geom_point(size = 3) +  # Scatter points
  labs(title = "Searches for Pies by City",
       x = "Type of Pie",
       y = "Number of Searches",
       color = "City") +
  theme_minimal(base_size = 15) + 
  theme(plot.title = element_text(hjust = 0.5)) +  # Center title
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Angle x-axis text for readability

# Display the plot
print(scatter_plot)


# Create the scatter plot
scatter_plot <- ggplot(mat_long, aes(x = Type, y = Count, color = City)) +
  geom_point(size = 3) +  # Scatter points
  labs(title = "Searches for Pies by City",
       x = "Type of Pie",
       y = "Number of Searches",
       color = "City") +
  theme_minimal(base_size = 15) + 
  theme(plot.title = element_text(hjust = 0.5)) +  # Center title
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Angle x-axis text for readability

# Display the plot
print(scatter_plot)





