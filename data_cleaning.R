library(dplyr)
library(readxl)
library(forcats)

#Read in Data
injury_data <- read_excel("Ped_Injury.xlsx")
#####################################################################Data Cleaning#############################################################################

#Remove meaningless features
injury_data <- subset(injury_data, select = -c(Sno., 
                                               ProjectSiteId, 
                                               CrashId, 
                                               VehicleId, 
                                               PEDESTRIAN_PersonId.x, 
                                               NETWORK_MedianType, 
                                               VEHICLE_Vehicle.Model.Year, 
                                               VEHICLE_Total.Lanes.In.Roadway, 
                                               VEHICLE_Roadway.Alignment, 
                                               VEHICLE_Traffic.Control.Device.Type, 
                                               DRIVER_Age.y, 
                                               NETWORK_SpeedChangeLane, 
                                               NETWORK_LaneWidth, 
                                               `Front bumper height`,
                                               PEDESTRIAN_Injury_Severity_KA2.BC1.O0))

#Convert all categorical features to factors
injury_data <- injury_data %>% 
  mutate(across(-c(PEDESTRIAN_Age.x, NETWORK_NumberOfThruLanes), as.factor))

#Convert all quantitative features to numerics
injury_data$PEDESTRIAN_Age.x <- as.numeric(injury_data$PEDESTRIAN_Age.x)
injury_data$NETWORK_NumberOfThruLanes <- as.numeric(injury_data$NETWORK_NumberOfThruLanes)

###Clean ALL Predictors

#PEDESTRIAN_Condition.at.Time.of.Crach
#Add NA to the unknown category
injury_data <- injury_data %>%
  mutate(PEDESTRIAN_Condition.at.Time.of.Crash = fct_na_value_to_level(PEDESTRIAN_Condition.at.Time.of.Crash, level = "Unknown"))
#Add Other and Not Applicable to the Unknown category
injury_data <- injury_data %>%
  mutate(PEDESTRIAN_Condition.at.Time.of.Crash = recode(PEDESTRIAN_Condition.at.Time.of.Crash, 
                                                        "Other" = "Unknown",
                                                        "Not Applicable" = "Unknown"))
#Collapse appropriate categories and rename
injury_data$PEDESTRIAN_Condition.at.Time.of.Crash <- fct_collapse(injury_data$PEDESTRIAN_Condition.at.Time.of.Crash,
                                                                  Normal = c("Apparently Normal"),
                                                                  Intoxicated = c("Under the Influence of Medications/Drugs/Alcohol"),
                                                                  Emotional = c("Emotional (depressed, angry, disturbed, etc.)"))


#PEDESTRIAN_Actions.Prior.to.Crash
#Add NA to the unknown category
injury_data <- injury_data %>%
  mutate(PEDESTRIAN_Actions.Prior.to.Crash = fct_na_value_to_level(PEDESTRIAN_Actions.Prior.to.Crash, level = "Unknown"))
#Add Other, In Roadway - Other, and None to Unknown category
injury_data <- injury_data %>%
  mutate(PEDESTRIAN_Actions.Prior.to.Crash = recode(PEDESTRIAN_Actions.Prior.to.Crash, 
                                                    "In Roadway - Other" = "Unknown",
                                                    "Other" = "Unknown",
                                                    "None" = "Unknown"))
#Collapse and rename appropriate categories
injury_data$PEDESTRIAN_Actions.Prior.to.Crash <- fct_collapse(injury_data$PEDESTRIAN_Actions.Prior.to.Crash,
                                                              Crossing = c("Crossing Roadway", "Waiting to Cross Roadway"),
                                                              `Moving Along Roadway` = c("Walking/Cycling along Roadway Against Traffic (In or Adjacent to Travel Lane)", "Walking/Cycling along Roadway Along Traffic (In or Adjacent to Travel Lane)", "Walking/Cycling on Sidewalk", "Adjacent to Roadway (e.g., Shoulder, Median)"),
                                                              `Working in Trafficway` = c("Working in Trafficway for Incident Response"))
#PEDESTRIAN_Actions.At.Time.Of.Crash
#Add NA to the unknown category
injury_data <- injury_data %>%
  mutate(PEDESTRIAN_Actions.At.Time.Of.Crash = fct_na_value_to_level(PEDESTRIAN_Actions.At.Time.Of.Crash, level = "Unknown"))
#Add Other, Not Applicable, and Not Visible to unknown
injury_data <- injury_data %>%
  mutate(PEDESTRIAN_Actions.At.Time.Of.Crash = recode(PEDESTRIAN_Actions.At.Time.Of.Crash,
                                                      "Other" = "Unknown",
                                                      "Not Visible (Dark Clothing, No Lighting, etc.)" = "Unknown",
                                                      "Not Applicable" = "Unknown"))

#PEDESTRIAN_Location.At.Time.Of.Crash
#Add NA to the Unknown category
injury_data <- injury_data %>%
  mutate(PEDESTRIAN_Location.At.Time.Of.Crash = fct_na_value_to_level(PEDESTRIAN_Location.At.Time.Of.Crash, level = "Unknown"))
#Add Other to unknown
injury_data <- injury_data %>%
  mutate(PEDESTRIAN_Location.At.Time.Of.Crash = recode(PEDESTRIAN_Location.At.Time.Of.Crash,
                                                       "Other" = "Unknown"))
#Collapse and rename appropriate categories
injury_data$PEDESTRIAN_Location.At.Time.Of.Crash <- fct_collapse(injury_data$PEDESTRIAN_Location.At.Time.Of.Crash,
                                                                 Intersection = c("Intersection-Marked Crosswalk", "Intersection-Unmarked Crosswalk", "Intersection – Other"),
                                                                 `Midblock / Travelway` = c("Travel Lane-Other Location", "Midblock-Marked Crosswalk", "Shoulder/Roadside", "Median/Crossing Island"),
                                                                 `Off-road / Non-traffic facility` = c("Sidewalk", "Shared-Use Path or Trail", "Non-Trafficway Area", "Driveway Access"),
                                                                 `Bicycle facility` = c("Bicycle Lane", "Sharrow"))
#PEDESTRIAN_Distracted
#Add NA to the Unknown category
injury_data <- injury_data %>%
  mutate(PEDESTRIAN_Distracted = fct_na_value_to_level(PEDESTRIAN_Distracted, level = "Unknown if Distracted"))
#Combine all categories into Not Distracted, Distracted, Unknown if Distracted
injury_data$PEDESTRIAN_Distracted <- fct_other(injury_data$PEDESTRIAN_Distracted, keep = c("Not Distracted", "Unknown if Distracted", other_level = "Distracted"))
#Rename Other to Distracted
injury_data <- injury_data %>%
  mutate(PEDESTRIAN_Distracted = recode(PEDESTRIAN_Distracted,
                                        "Other" = "Distracted"))

#CRASH_Route.Class
#Add NA to the Unknown Cateogry
injury_data <- injury_data %>%
  mutate(CRASH_Route.Class = fct_na_value_to_level(CRASH_Route.Class, level = "Unknown"))

#CRASH_Light.Condition
#Add Other to Unknown
injury_data <- injury_data %>%
  mutate(CRASH_Light.Condition = recode(CRASH_Light.Condition,
                                        "Other" = "Unknown"))

#CRASH_Road.Surface.Condition
#Add Other to Unknown
injury_data <- injury_data %>%
  mutate(CRASH_Road.Surface.Condition = recode(CRASH_Road.Surface.Condition,
                                               "Other" = "Unknown"))
#CRASH_CRASH.Specific.Location
#Add Other to Unknown 
injury_data <- injury_data %>%
  mutate(CRASH_Crash.Specific.Location = recode(CRASH_Crash.Specific.Location,
                                                "Other" = "Unknown"))

#CRASH_Highway.Type..FHWA
#Add NA to Unknown
injury_data <- injury_data %>%
  mutate(CRASH_Highway.Type..FHWA. = fct_na_value_to_level(CRASH_Highway.Type..FHWA., level = "Unknown"))

#Vehicle_Roadway.Grade
#Add NA to Unknown
injury_data <- injury_data %>%
  mutate(VEHICLE_Roadway.Grade = fct_na_value_to_level(VEHICLE_Roadway.Grade, level = "Unknown"))

#Vehicle_Body.Type
#Add NA to Unknown
injury_data <- injury_data %>%
  mutate(VEHICLE_Body.Type = fct_na_value_to_level(VEHICLE_Body.Type, level = "Unknown"))
#Add Other to Unknown
injury_data <- injury_data %>%
  mutate(VEHICLE_Body.Type = recode(VEHICLE_Body.Type,
                                    "Other" = "Unknown"))

#DRIVER_Gender.y
#Add NA to Unknown
injury_data <- injury_data %>%
  mutate(DRIVER_Gender.y = recode(DRIVER_Gender.y,
                                  "NA" = "Unknown"))

#DRIVER_Condition.at.Time.of.Crash.y
#Add NA to the unknown category
injury_data <- injury_data %>%
  mutate(DRIVER_Condition.at.Time.of.Crash.y = fct_na_value_to_level(DRIVER_Condition.at.Time.of.Crash.y, level = "Unknown"))
#Add Other and Not Applicable to the Unknown category
injury_data <- injury_data %>%
  mutate(DRIVER_Condition.at.Time.of.Crash.y = recode(DRIVER_Condition.at.Time.of.Crash.y, 
                                                      "Other" = "Unknown",
                                                      "Not Applicable" = "Unknown",
                                                      "NA" = "Unknown"))

#DRIVER_Speeding.Related.y
#Add NA to the Unknown category
injury_data <- injury_data %>%
  mutate(DRIVER_Speeding.Related.y = fct_na_value_to_level(DRIVER_Speeding.Related.y, level = "Unknown"))
#Convert the feature to the categories Speeding, Not Speeding, Unknown
injury_data <- injury_data %>%
  mutate(DRIVER_Speeding.Related.y = recode(DRIVER_Speeding.Related.y,
                                            "Exceeded Speed Limit" = "Speeding",
                                            "NA" = "Unknown",
                                            "No" = "Not Speeding",
                                            "Racing" = "Speeding",
                                            "Too Fast for Conditions" = "Speeding"))

#Driver_Driver.Actions.1.y
#Add NA to the Unknown category
injury_data <- injury_data %>%
  mutate(DRIVER_Driver.Actions.1.y = fct_na_value_to_level(DRIVER_Driver.Actions.1.y, level = "Unknown"))
#Add NA and Not Applicable to Unknwon
injury_data <- injury_data %>%
  mutate(DRIVER_Driver.Actions.1.y = recode(DRIVER_Driver.Actions.1.y,
                                            "NA" = "Unknown",
                                            "Not Applicable" = "Unknown"))

#DRVIER_Driver.Distracted.By.y
#Add NA to the Unknown category
injury_data <- injury_data %>%
  mutate(DRIVER_Driver.Distracted.By.y = fct_na_value_to_level(DRIVER_Driver.Distracted.By.y, level = "Unknown if Distracted"))
#Combine the feature into categories Distracted, Not Distracted, Unkown if Distracted
injury_data <- injury_data %>%
  mutate(DRIVER_Driver.Distracted.By.y = recode(DRIVER_Driver.Distracted.By.y,
                                                "Manually Operating an Electronic Communication Device (texting, typing, dialing)" = "Distracted",
                                                "NA" = "Unknown if Distracted",
                                                "Other Activity, Electronic Device" = "Distracted",
                                                "Outside the Vehicle (includes unspecified external distractions)" = "Distracted",
                                                "Passenger" = "Distracted",
                                                "Talking on Hand-Held Electronic Device" = "Distracted",
                                                "Talking on Hands-Free Electronic Device" = "Distracted",
                                                "Other Inside the Vehicle (eating, personal hygiene, etc.)" = "Distracted"))

#NETWORK_NumberOfThruLanes
#Add NA to Unknown
injury_data$NETWORK_NumberOfThruLanes <- as.factor(injury_data$NETWORK_NumberOfThruLanes)
injury_data <- injury_data %>%
  mutate(NETWORK_NumberOfThruLanes = fct_na_value_to_level(NETWORK_NumberOfThruLanes, level = "Unknown"))

#VIN_bodyclass
#Create Unknown category
injury_data <- injury_data %>%
  mutate(VIN_bodyclass = recode(VIN_bodyclass,
                                "0" = "Unknown",
                                "NA" = "Unknown"))

###Clean Response Variable (PEDESTRIAN_Injury.Status is the response variable)

#aggregate the feature such that it only has the categories Severe/Fatal Injury and No/Minor Injury

# Filter out minor and possible injuries so there are only severe/fatal injruies and no injuries
injury_data <-  injury_data %>% filter(PEDESTRIAN_Injury.Status != "Suspected Minor Injury (B)" & PEDESTRIAN_Injury.Status != "Possible Injury (C)")
injury_data <- injury_data %>%
  mutate(PEDESTRIAN_Injury.Status = recode(PEDESTRIAN_Injury.Status,
                                           "Fatal Injury (K)" = "Suspected Serious/Fatal Injury (A/K)",
                                           "Suspected Serious Injury (A)" = "Suspected Serious/Fatal Injury (A/K)"))
injury_data$PEDESTRIAN_Injury.Status <- droplevels(injury_data$PEDESTRIAN_Injury.Status)

#Remove any remaining null values
injury_data <- na.omit(injury_data)

