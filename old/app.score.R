app.score = function(tech, case){
  # Description
  # This function calculates the the product of the app.profile containing the scores for all attributes
  # Usage
  # app.score(technology,case)
  # Input:
  # tech: a technology from the technology list, e.g. techlist$septic.tank
  # case: a case from the case list, e.g. arbaminch
  # Output:
  # app.score
  prod(app.profile(tech, case))
}
