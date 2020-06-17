connexion=TRUE
x <- matrix(runif(100000000), nrow=500)

if (connexion==T) { sink("./sink-steps.txt", split=T) }
cat("1. Step 1\n")
if (connexion==T) {sink()}



for (i in 1:2) { x2 <- DAPAR2::matAdjStats(x)}
if (connexion==T) { sink("./sink-steps.txt",append=T, split=T) }
cat("2. Step 2\n")
if (connexion==T) {sink()}


for (i in 1:2) { x2 <- DAPAR2::matAdjStats(x)}
if (connexion==T) { sink("./sink-steps.txt",append=T, split=T) }
cat("3. Step 3\n")
if (connexion==T) {sink()}


for (i in 1:2) { x2 <- DAPAR2::matAdjStats(x)}
if (connexion==T) { sink("./sink-steps.txt",append=T, split=T) }
cat("4. Step 4\n")
if (connexion==T) {sink()}


hc <- plot(table(rpois(100, 5)), type = "h", col = "red", lwd = 10,
           main = "rpois(100, lambda = 5)")
if (connexion==T) { sink("./sink-steps.txt",append=T, split=T) }
cat("5. Making Plot\n")
if (connexion==T) {sink()}

