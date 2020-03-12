```{r}
plot(x = 0, y = 0, pch = 20, xlim = c(-180, 180), ylim = c(-180, 180))

draw.circle(x = 0, y =  0, radius = 90)

segments(x0 = -90, y0 = 0, x1 = 90, y1 = 0)

segments(x0 = 0, y0 = -180, x1 = 0, y1 = 180)

#x = cos.deg(90)
#y = sin.deg(90)

rx = cos.deg(66.55)
ry = sin.deg(66.55)

x;y

r = sqrt((x**2) + (y**2))

segments(x0 = 0, y0 = 0, x1 = r * x, y1 = r * y, col = "red")

#par(new = TRUE)

```

```{r}
library(plotrix)

# Creates empty plot and variables
plot(1,type="n",axes=F,xlim=c(-0.1,1.2),ylim=c(-0.1,1.2),xlab="",ylab="")
x1 = c(1,0)
x2 = c(0.4,0.7)

# X1
arrows(0,0,x1[1],x1[2])
text(1,-0.05,expression(x[1]),cex=1.5)

# X2
arrows(0,0,x2[1],x2[2])
text(0.4,0.75,expression(x[2]),cex=1.5)


alpha_angle= acos((x2%*%x1)/(sqrt(x2%*%x2)*sqrt(x1%*%x1)))

draw.arc(0,0,0.15,angle2=alpha_angle)

text(0.07,0.05,expression(alpha),cex=1.5)
```

```{r}
r = 3.8
angle = 66.55  + 270

#angle = 23.45 + 90

x = round(r * cos.deg(angle), 1)
y = round(r * sin.deg(angle), 1)

x;y
```