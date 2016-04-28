# FUNCTIONS
##
#


# functions for using meta-regression within glmulti by W. Veichtbauer
# (http://www.metafor-project.org/doku.php/tips:model_selection_with_glmulti)
#
# function to incorporate meta regression into glmulti
# altered by N. Kinlock to work with rma.mv files
rma.glmulti <- function(formula, data, V, random,...) {
  do.call("rma.mv", list(as.formula(paste(deparse(formula))), V = as.name(V), random = as.name(random), data = data,  method = "ML", ...))
}

rma.glmulti2 <- function(formula, data, ...) {
  rma(as.formula(paste(deparse(formula))), vi, data=data, method="ML", ...)
}

# get glmulti to handle rma.mv objects (metafor) again by W. Veichtbauer
setOldClass("rma.mv")
setMethod('getfit', 'rma.mv', function(object, ...) {
  if (object$knha == FALSE) {
    cbind(estimate = coef(object), se = sqrt(diag(vcov(object))), df = 100000)
  } else {
    cbind(estimate = coef(object), se = sqrt(diag(vcov(object))), df = object$k - object$p)
  }
})
