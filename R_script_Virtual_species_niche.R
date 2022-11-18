
########################################################################################
#Code Author:
# Vitezslav Moudry - https://scholar.google.com/citations?user=aSI2lNEAAAAJ&hl=en&oi=ao
# Lukas Gabor - https://scholar.google.com/citations?user=pLQXY5wAAAAJ&hl=en&oi=ao

# Date: 17/11/2022
########################################################################################

# -----------------------------------------------------------
# Load packages
# -----------------------------------------------------------
    library(raster)
    library(virtualspecies)
    library (rgdal)
    library(ggplot2)
    library(PresenceAbsence)
# --------------------------------------------------------------------------------
    ## SPLIT FUNCTION 50% TRAINIG 50% TESTING
    splitdf <- function(dataframe, seed=NULL) {
      if (!is.null(seed)) set.seed(seed)
      index <- 1:nrow(dataframe)
      trainindex <- sample(index, trunc(length(index)*5/10))
      trainset <- dataframe[trainindex, ]
      testset <- dataframe[-trainindex, ]
      list(trainset=trainset,testset=testset)
    }
# --------------------------------------------------------------------------------  
    
# -----------------------------------------------------------
# Read data and create raster stack
# -----------------------------------------------------------
    
# Set path to the data
  setwd("PAthToYourData")

  ALS10 = "EBR_ALS_CHM_10m.tif"  
  ALS30 = "EBR_ALS_CHM_30m.tif"
  HRCH10 = "EBR_HRCH_10m.tif"
  GFCH30 = "EBR_GFCH_30m.tif"

# Environment for Virtual species creation 
  
  my1_environment_10 <- stack(ALS10)
  names(my1_environment_10) <- c("ALS10")
  my2_environment_10 <- stack(HRCH10)
  names(my2_environment_10) <- c("HRCH10")
  
  my1_environment_30 <- stack(ALS30)
  names(my1_environment_30 ) <- c("ALS30")
  my2_environment_30  <- stack(GFCH30)
  names(my2_environment_30 ) <- c("GFCH30")
  
# Response of the species to the environmental variables
  my1_parameters_10 <- formatFunctions(ALS10 = c(fun = 'dnorm', mean = 10, sd = 8))
  my1_parameters_30 <- formatFunctions(ALS30 = c(fun = 'dnorm', mean = 10, sd = 8))
    
# Generate environmental suitability
  my1_virtualspecies_10 <- generateSpFromFun(raster.stack = my1_environment_10, parameters = my1_parameters_10, plot = FALSE, rescale.each.response = FALSE)
  my1_virtualspecies_30 <- generateSpFromFun(raster.stack = my1_environment_30, parameters = my1_parameters_30, plot = FALSE, rescale.each.response = FALSE)

# Plot the response
  plotResponse(my1_virtualspecies_10)
  plotResponse(my1_virtualspecies_30)

# ENVIRONMENT for Virtual species EVALUATION (RASTER STACK)
# VERSION FOR 10m
  Raster_stack_10 <- stack(ALS10, HRCH10)
  names(Raster_stack_10) <- c("ALS10","HRCH10")
# VERSION FOR 30m
  Raster_stack_30 <- stack(ALS30, GFCH30)
  names(Raster_stack_30) <- c("ALS30","GFCH30")

# Define variables
# Sample size (Number of sampled presences and absences)
  N <- 800
  
# Alfa and Beta parameters are used to determine the shape of the logistic curve that is used for the 
# conversion of environmental suitability to presence-absence.
  BETA = 0.65 # Controls the inflection point
  ALPHA = -0.15 # drives the "slope" of the curve

# Empty variables
  AUC_ALL_LiDAR_10  <- numeric(0)
  AUC_ALL_LiDAR_30  <- numeric(0)
  AUC_ALL_ALS <- numeric(0)
  NEW_Test_dat_EVALUATE_HRCH10 <- numeric(0)
  NEW_Test_dat_EVALUATE_GFCH30 <- numeric(0)
  NEW_Test_dat_EVALUATE_ALS <- numeric(0)
    
# --------------------------------------------------------------------------------
#################################################################################
# LOOP
#################################################################################
# --------------------------------------------------------------------------------

for(i in 1:25) {

# Conversion to presence-absence
PArastr_10 <- convertToPA(my1_virtualspecies_10, beta = BETA, alpha = ALPHA, plot = FALSE)
PArastr_30 <- convertToPA(my1_virtualspecies_30, beta = BETA, alpha = ALPHA, plot = FALSE)


# Sampling of 'presence-absence' data
PA_sampling_10 <- sampleOccurrences(PArastr_10, n = N, type = "presence-absence", sample.prevalence = 0.5, plot = FALSE)
PA_sampling_30 <- sampleOccurrences(PArastr_30, n = N, type = "presence-absence", sample.prevalence = 0.5, plot = FALSE)


#-----------------------------------------------------------------
# Export sampled presence-absence data and environmental variables 
#-----------------------------------------------------------------
  ID <- 1:N
  
  Presence_Absence_XY_10 <- PA_sampling_10$sample.points[, c( "x", "y",  "Observed")]
  Extract_stack10 <- extract(Raster_stack_10, PA_sampling_10$sample.points[, c( "x", "y")])
  
  DATA_10 <- data.frame(ID, Presence_Absence_XY_10, Extract_stack10)

  Presence_Absence_XY_30 <- PA_sampling_30$sample.points[, c( "x", "y",  "Observed")]
  Extract_stack30 <- extract(Raster_stack_30, PA_sampling_30$sample.points[, c( "x", "y")])
  
  DATA_30 <- data.frame(ID, Presence_Absence_XY_30, Extract_stack30)
  
#-----------------------------------------------------
## SPLIT SAMPLES 50% TRAINIG 50% TESTING
#-----------------------------------------------------

splits_10 <- splitdf(DATA_10)
str(splits_10)
# save the training and testing sets as data frames
Train_10 <- splits_10$trainset
Test_10 <- splits_10$testset

splits_30 <- splitdf(DATA_30)
str(splits_30)
# save the training and testing sets as data frames
Train_30 <- splits_30$trainset
Test_30 <- splits_30$testset

# --------------------------------------------------------------------------------
#################################################################################
# GLM Modelling 10 m using HRCH map
#################################################################################
# --------------------------------------------------------------------------------
   Model_10 <- glm(Observed~HRCH10+I(HRCH10^2), family= binomial, data=Train_10)
   summary(Model_10)
  
# --------------------------------
# Model evaluation (AUC)  
# --------------------------------

   #Prediction
    predict_10 <- predict(Model_10, newdata=Test_10, type="response") 
    Predicted_10 <- as.vector(predict_10)

    DAT_GRAF_10 <- data.frame (Test_10$ID, Test_10$ALS10, Test_10$Observed, Predicted_10, rep(1))
    colnames(DAT_GRAF_10) <- c("ID", "ALS", "Observed","Predicted", "Version")

   #Calculate AUC 
    DAT_10 <- data.frame(Test_10$ID, Test_10$Observed, Predicted_10)
    DAT_10<- na.omit(DAT_10) # There are few gaps (NoData cells) in HRCH
    AUC_L_10 <- auc(DAT_10, st.dev = TRUE, which.model = 1, na.rm = FALSE);AUC_L_10
    AUC_ALL_LiDAR_10 <- rbind(AUC_ALL_LiDAR_10,AUC_L_10)

# -------------------------------------------
# Prepare data for plotting response function
# -------------------------------------------

    NEW_Test_dat_10 <- data.frame(seq(0, 70, 0.1))
    NEW_Test_dat_10 <- setNames (NEW_Test_dat_10, c("HRCH10"))
   #Prediction
    predict_HRCH10_2 <- predict(Model_10, newdata=NEW_Test_dat_10, type="response")
    predict_HRCH10_2 <- as.vector(predict_HRCH10_2)
    NEW_Test_dat_EVALUATE_HRCH10 <- cbind(NEW_Test_dat_EVALUATE_HRCH10, predict_HRCH10_2)

# --------------------------------------------------------------------------------
#################################################################################
# GLM Modelling 30 m using GFCH 
#################################################################################
# --------------------------------------------------------------------------------
Model_30 <- glm(Observed~GFCH30+I(GFCH30^2), family= binomial, data=Train_30)
summary(Model_30)

# --------------------------------
# Model evaluation (AUC)  
# --------------------------------

  #Prediction
   predict_30 <- predict(Model_30, newdata=Test_30, type="response") 
   Predicted_30 <- as.vector(predict_30)

   DAT_GRAF_30 <- data.frame (Test_30$ID, Test_30$ALS30, Test_30$Observed, Predicted_30, rep(2))
   colnames(DAT_GRAF_30) <- c("ID", "ALS", "Observed","Predicted", "Version")

  #Calculate AUC
   DAT_30 <- data.frame(Test_30$ID, Test_30$Observed, Predicted_30)
   DAT_30<- na.omit(DAT_30) # There are few gaps (NoData cells) in GFCH
   AUC_L_30 <- auc(DAT_30, st.dev = TRUE, which.model = 1, na.rm = FALSE);AUC_L_30
   AUC_ALL_LiDAR_30 <- rbind(AUC_ALL_LiDAR_30,AUC_L_30)

   # -------------------------------------------
   # Prepare data for plotting response function
   # -------------------------------------------

   NEW_Test_dat_30 <- data.frame(seq(0, 70, 0.1))
   NEW_Test_dat_30 <- setNames (NEW_Test_dat_30, c("GFCH30"))
  #Prediction
   predict_GFCH30_2 <- predict(Model_30, newdata=NEW_Test_dat_30, type="response")
   predict_GFCH30_2 <- as.vector(predict_GFCH30_2)
   NEW_Test_dat_EVALUATE_GFCH30 <- cbind(NEW_Test_dat_EVALUATE_GFCH30, predict_GFCH30_2)

# --------------------------------------------------------------------------------
#################################################################################
# GLM Modelling 10 m ALS DATA
#################################################################################
# --------------------------------------------------------------------------------
Model1_ALS10 <- glm(Observed~ALS10+I(ALS10^2),family= binomial, data=Train_10)
summary(Model1_ALS10)
    
# --------------------------------
# Model evaluation (AUC)  
# --------------------------------

  #Prediction
   predict_ALS <- predict(Model1_ALS10, newdata=Test_10, type="response") 
   Predicted_ALS <- as.vector(predict_ALS)

   DAT_ALS_GRAF <- data.frame (Test_10$ID, Test_10$ALS10, Test_10$Observed, Predicted_ALS, rep(3))
   colnames(DAT_ALS_GRAF) <- c("ID", "ALS", "Observed","Predicted","Version")

  #Calculate AUC
   DAT_ALS <- data.frame (Test_10$ID, Test_10$Observed, Predicted_ALS )
   AUC_ALS <- auc(DAT_ALS, st.dev = TRUE, which.model = 1, na.rm = FALSE);AUC_ALS
   AUC_ALL_ALS <- rbind(AUC_ALL_ALS,AUC_ALS)

   # -------------------------------------------
   # Prepare data for plotting response function
   # -------------------------------------------

   NEW_Test_dat_ALS <- setNames (NEW_Test_dat_10, c("ALS10"))
  #Prediction
   predict_ALS_2 <- predict(Model1_ALS10, newdata=NEW_Test_dat_ALS , type="response") 
   Predicted_ALS_2 <- as.vector(predict_ALS_2)
   NEW_Test_dat_EVALUATE_ALS <- cbind(NEW_Test_dat_EVALUATE_ALS, Predicted_ALS_2)

}   # END OF LOOP

  # -------------------------------------------
  # Calculate mean AUC and Standard deviation
  # -------------------------------------------
    mean(AUC_ALL_ALS$AUC)    
    mean(AUC_ALL_LiDAR_30$AUC) 
    mean(AUC_ALL_LiDAR_10$AUC)  

    sd(AUC_ALL_ALS$AUC)    
    sd(AUC_ALL_LiDAR_30$AUC) 
    sd(AUC_ALL_LiDAR_10$AUC) 

  # ---------------------------------------------
  # Plot estimated response functions (Figure 2c)
  # ---------------------------------------------
    # This plot shows the probability of species occurrence with respect to canopy height or habitat heterogeneity
    # estimated with generalized linear models using ALS canopy height, GFCH map, and HRCH map, respectively. 
    # The shaded areas represent the regions delimited by the 5th–95th percentiles of the estimated 
    # probability of occurrence obtained from 25 simulations. Black lines show the “true” relationships.   
    
         
  #Calculate 0.05; 0.50; and 0.95 percentiles
   P50_ALS <- apply(NEW_Test_dat_EVALUATE_ALS,1,quantile,probs=c(.50))
   P95_ALS <- apply(NEW_Test_dat_EVALUATE_ALS,1,quantile,probs=c(.95))
   P05_ALS <- apply(NEW_Test_dat_EVALUATE_ALS,1,quantile,probs=c(.05))
   
   P50_HRCH10 <- apply(NEW_Test_dat_EVALUATE_HRCH10,1,quantile,probs=c(.50))
   P95_HRCH10 <- apply(NEW_Test_dat_EVALUATE_HRCH10,1,quantile,probs=c(.95))
   P05_HRCH10 <- apply(NEW_Test_dat_EVALUATE_HRCH10,1,quantile,probs=c(.05))
  
   P50_GFCH30 <- apply(NEW_Test_dat_EVALUATE_GFCH30,1,quantile,probs=c(.50))
   P95_GFCH30 <- apply(NEW_Test_dat_EVALUATE_GFCH30,1,quantile,probs=c(.95)) 
   P05_GFCH30 <- apply(NEW_Test_dat_EVALUATE_GFCH30,1,quantile,probs=c(.05))
  
  #Create Data frame
   
   Response_ <- cbind(NEW_Test_dat_10, P50_ALS, P05_ALS, P95_ALS, P50_HRCH10, P05_HRCH10, P95_HRCH10, P50_GFCH30, P05_GFCH30, P95_GFCH30)
   Response_ <- setNames (Response_, c("ALS","Fit_ALS", "Lower_ALS", "Upper_ALS","Fit_HRCH", "Lower_HRCH", "Upper_HRCH","Fit_GFCH", "Lower_GFCH", "Upper_GFCH"))
   
  #Original response function of the species to the environmental variables
   par(bty = "n", cex = 1.3)
   x <-  seq(0, 70, length = 400)
   y <-dnorm(x=x, mean = 10, sd = 8)
   y <- ((y - min(y)) / (max(y) - min(y)))
  #Probabilistic conversion of Environmental Suitability into Probability of Occurrence
   y2 <- 1/(1+exp((y-BETA)/ALPHA))
   
  #Plot
   ggplot(NULL, aes(x = slope, y = as.numeric(ocupied))) +
     geom_ribbon(data = Response_, aes(ymin = Lower_ALS, ymax = Upper_ALS, x = ALS),
                 fill = "#7570b3", alpha = 0.4, inherit.aes = FALSE) +
     geom_line(colour="#7570b3", size = 0.5, data = Response_, aes(y = Fit_ALS, x = ALS)) +
     geom_ribbon(data = Response_, aes(ymin = Lower_HRCH, ymax = Upper_HRCH, x = ALS),
                 fill = "#d95f02", alpha = 0.4, inherit.aes = FALSE) +
     geom_line(colour="#d95f02", size = 0.5,data = Response_, aes(y = Fit_HRCH, x = ALS)) +
     geom_ribbon(data = Response_, aes(ymin = Lower_GFCH, ymax = Upper_GFCH, x = ALS),
                 fill = "#1b9e77", alpha = 0.4, inherit.aes = FALSE) +
     geom_line(colour="#1b9e77", size = 0.5,data = Response_, aes(y = Fit_GFCH, x = ALS)) +
     geom_line(data = Response_, aes(x=ALS, y = Lower_ALS), size = 0.5, color = "#7570b3", linetype="dashed") +
     geom_line(data = Response_,aes(x=ALS, y = Upper_ALS), size = 0.5, color = "#7570b3", linetype="dashed") +
     geom_line(data = Response_, aes(x=ALS, y = Lower_HRCH), size = 0.5, color = "#d95f02", linetype="dashed") +
     geom_line(data = Response_,aes(x=ALS, y = Upper_HRCH), size = 0.5, color = "#d95f02", linetype="dashed")+
     geom_line(data = Response_, aes(x=ALS, y = Lower_GFCH), size = 0.5, color = "#1b9e77", linetype="dashed") +
     geom_line(data = Response_,aes(x=ALS, y = Upper_GFCH), size = 0.5, color = "#1b9e77", linetype="dashed")+
     geom_line(aes(x=x, y = y2), size = 1.5, color = "black")+
     xlim(c(0,70))+
     labs(y = "Probability of occurence", x = "Canopy height [m]")+
     theme(panel.grid.major = element_blank(), axis.title.y=element_text(vjust=1),
           axis.title=element_text(colour="black", size=18),
           axis.text=element_text(colour="black", size=18),axis.line = element_line(colour = "black"),
           panel.border = element_rect(colour = "black", fill=NA, size=0.5))
