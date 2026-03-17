library(mlr3)

MeasureRegrCCC = R6::R6Class(
  "MeasureRegrCCC",
  inherit = mlr3::MeasureRegr,
  public = list(
    initialize = function() {
      super$initialize(
        id = "regr.ccc",
        range = c(-1, 1),
        minimize = FALSE,
        predict_type = "response"
      )
    }
  ),
  private = list(
    .score = function(prediction, ...) {
      truth = prediction$truth
      response = prediction$response
      # Lin's CCC
      rho = stats::cor(truth, response, use = "complete.obs")
      mx  = mean(truth,    na.rm = TRUE)
      my  = mean(response, na.rm = TRUE)
      vx  = stats::var(truth,    na.rm = TRUE)
      vy  = stats::var(response, na.rm = TRUE)
      (2 * rho * sqrt(vx) * sqrt(vy)) / (vx + vy + (mx - my)^2)
    }
  )
)

msr_ccc = MeasureRegrCCC$new()

# usage:
# rr = resample(task, learner, rsmp("cv", folds = 5))
# rr$aggregate(list(msr_ccc, mlr3::msr("regr.pearson"), mlr3::msr("regr.mae")))
