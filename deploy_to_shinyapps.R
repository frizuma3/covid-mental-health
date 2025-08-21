# Shiny deployment helper
# 1) Install rsconnect if needed:
# install.packages("rsconnect")

library(rsconnect)
# Fill these once from shinyapps.io Account settings → Tokens
# rsconnect::setAccountInfo(name='YOUR_NAME', token='YOUR_TOKEN', secret='YOUR_SECRET')

# Deploy from the project root (assuming this script lives at root)
rsconnect::deployApp(appDir = "ShinyApp")