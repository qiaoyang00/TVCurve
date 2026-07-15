## Deploy with following structure:
## .
## ├── ui.R
## ├── server.R
## ├── auth.txt
## ├── deploy/
## │   ├── real_data.rds
## │   ├── simu_data.rds
## │   ├── simu_gridsearch.rds
## │   └── More Examples TDCox.Rmd
## └── www/
##     └── TVCurve_1080p.mp4
##
## Introduction of files:
## R files: ui.R, server.R.
## R data files: deploy/real_data.rds, deploy/simu_data.rds, deploy/simu_gridsearch.rds.
## R markdown file: deploy/More Examples TDCox.Rmd
## Video: www/TVCurve_1080p.mp4

require(shiny)
require(shinydashboard)
require(shinyWidgets)
require(shinyjs)
require(shinyauthr)
require(shinymeta)  # also need: shinyAce, clipr
require(shinyAce)
require(clipr)
require(DT)
require(tidyverse)
require(scales)
require(metR)
require(survival)
require(survminer)
require(nleqslv)
require(knitr)
require(pryr)

# Still not working on shinyapps.io, requires xclip package installed
Sys.setenv(CLIPR_ALLOW = T)
clip_icon <- if (clipr_available()) "clipboard" else NULL
cat("clipboard_available:", clipr_available(), "\n\n")

options(shiny.sanitize.errors = T)
options(DT.options = list(pageLength = 25, autoWidth = T, #scrollX = T,
                          dom = "lrtip", buttons = c("csv", "excel")))
theme_set(theme_bw() + theme(axis.text = element_text(size = rel(0.8))))

# User authentication
user_base <- read.table("auth.txt", header = T, sep = "\t")


# Rounds numeric columns in a data frame to a specified number of digits using either round or signif.
fun_roundDf <- function(x, digits, type = c("round", "signif")) {
  type <- match.arg(type)
  nums <- sapply(x, is.numeric)
  x[, nums] <- switch(type, round = round, signif = signif)(x[, nums], digits = digits)
  x
}


# Calculates bias between two sets of values, x and y, with optional relative or percentage adjustments.
fun_bias <- function(x, y, relative = T, percent = T) {
  (x - y) / (relative * (y - 1) + 1) * ifelse(relative & percent, 100, 1)
}


# Rounds values in x to the nearest “ceiling” digit based on a specified decimal precision.
fun_ceiling <- function(x, d = 1) {
  y <- 10 ^ (floor(log10(x)) - d)
  ceiling(x / y) * y
}


# Converts numeric values in a matrix or data frame to a character format with three significant digits.
fun_coefMat <- function(x) {
  x <- as.matrix(x)  # if (is.data.frame(x))
  x[, ] <- sprintf("%.3g", x)
  x
}


# Evaluates an expression and ignores specific errors based on a function or regex pattern in f.
# If the specified error occurs, the function returns a default value instead of stopping the code execution.
fun_IgnoreError <- function(expr, return = NULL, f = function(...) F, ...) {
  eval.parent(
    substitute({
      res <- try(expr, silent = T)
      if (!inherits(res, "try-error")) res else {
        err <- attr(res, "condition")
        cm <- conditionMessage(err)
        print(cm)
        cond <- if (is.character(f)) grepl(f, cm) else rlang::as_function(f)(cm, ...)
        if (cond) return else stop(err)
      }
    })
  )
}


# Wrap a specified function to a call object.
fun_wrapFunction <- function(fun = NULL, funname = NULL) {
  if (is.null(fun)) fun <- match.fun(funname)
  if (is.null(funname)) funname <- deparse(substitute(fun, environment()))
  call("<-", as.name(funname), call("function", formals(fun), body(fun)))
}


# Generates survival plots using ggsurvplot and customizes the plot with optional risk tables, time breaks, and axis limits.
fun_survplot <- function(fit, data, break.time.by = NULL, xlim = NULL, risk.table = T,
                         title = NULL, ...) {
  f <- function(x) if (length(x) > 0 && any(!is.na(x))) x
  ggsurvplot(fit, data, conf.int = T, title = title, linewidth = 0.5, # surv.median.line = "hv",
             risk.table = risk.table, risk.table.y.text.col = F, censor = F,
             break.time.by = f(break.time.by), xlim = f(xlim), legend.title = "", ...) %++%
    (theme_classic() + theme(legend.position = "top"))
}


# Prepares data for time-dependent Cox regression by merging data at event and time-dependent covariate points.
fun_helper_tmerge <- function(data, vars) {
  select(data, all_of(unlist(vars, use.names = F))) %>%
    rename(id = !!sym(vars$id)) %>%
    tmerge(., ., id, event = event(get(vars$time), get(vars$status)),
           tdc = tdc(get(vars$XTtime), get(vars$XTstatus))) %>%
    select(id, tstart, tstop, event, tdc) %>%
    mutate(across(tdc, ~ replace_na(., 0))) %>%
    rename(!!sym(vars$status) := event, !!sym(vars$XTstatus) := tdc)
}


# Plots time-dependent Cox model predictions by simulating new survival data at specific times.
fun_helper_tdplot <- function(model, vars, maxtime, newtime) {
  flag_notime <- length(newtime) == 0
  if (flag_notime) newtime <- Inf
  l <- length(newtime)
  fitdata <- data.frame(id = 1:(l + 1), time = maxtime, status = c(1, rep(0, l)),
                        XTstatus = c(0, rep(1, l)), XTtime = c(NA, newtime)) %>%
    rename_with(~ unlist(vars)[.]) %>% fun_helper_tmerge(vars)
  if (flag_notime) fitdata <- fitdata[1, ]
  fit <- survfit(model, newdata = fitdata, id = id)
  list(fit = fit, data = fitdata)
}


# Creates a Kaplan-Meier plot with confidence intervals, cumulative hazards, and strata adjustments.
fun_helper_kmplot <- function(fit, data, vars) {
  d <- filter(data, !!sym(vars$XTstatus) == 1)
  t <- lapply(c(censor = 0, event = 1), function(i) d$tstop[d[[vars$status]] == i])
  q <- qnorm((1 + fit$conf.int) / 2)
  m <- data.frame(time = sort(unique(c(d$tstart, d$tstop)))) %>% rowwise %>%
    mutate(n.risk = sum(d$tstart < time & time <= d$tstop),
           n.event = sum(t$event == time), n.censor = sum(t$censor == time)) %>% ungroup %>%
    mutate(surv = cumprod(replace_na(1 - n.event / n.risk, 1)),
           std.err = sqrt(cumsum(replace_na(n.event / n.risk / (n.risk - n.event), 0))),
           lower = surv / exp(std.err * q), upper = pmin(surv * exp(std.err * q), 1),
           cumhaz = cumsum(replace_na(n.event / n.risk, 0)),
           std.chaz = sqrt(cumsum(replace_na(n.event / n.risk ^ 2, 0))))
  for (name in names(m)) fit[[name]] <- c(fit[[name]][1:fit$strata[1]], m[[name]])
  fit$strata[2] <- nrow(m)
  fit
}


# Fit a standard Cox proportional hazards model with optional coefficients, confidence intervals, and p-values.
fun_modelCox <- function(data, vars, only.coef = F, ...) {
  formula <- formula(sprintf("Surv(%s,%s)~%s", vars$time, vars$status, vars$XTstatus))
  model <- coxph(formula, data, timefix = F, iter.max = 100)
  model$call$formula <- eval(model$call$formula)
  coef <- with(summary(model), data.frame(coefficients[1, -5, drop = F], conf.int[1, 3:4, drop = F],
                                          pvalue = sctest[3], check.names = F))
  if (only.coef) return(coef)
  fit <- survfit(formula, data = data)
  fit$call$formula <- eval(fit$call$formula)
  list(model = model, coef = coef, plot = list(fit = fit, data = data))
}


# Fit a landmark Cox model, allowing selection of time points to adjust for time-varying covariates.
# landmark = quantile(data[data[, vars$XTstatus] == 1, vars$XTtime], 1)
# The landmark method selects a fixed time after cohort entry as the landmark
# Patients on study at the landmark are classified according their exposure status
# at the landmark and are then followed from the landmark regardless of subsequent changes in exposure status
# Patients who die or whose data are censored before the landmark time are excluded from the analysis
# filter(XT_time <= landmark | is.na(XT_time)) %>% filter(!(time < landmark & event == 1))
fun_modelLMCox <- function(data, vars, only.coef = F, landmark = NULL, ...) {
  survdata <- data %>% filter(!!sym(vars$time) >= landmark | !!sym(vars$status) == 0) %>%
    mutate(across(vars$XTtime, ~ replace(., . > landmark, NA)),
           across(vars$XTstatus, ~ 1 - is.na(!!sym(vars$XTtime))))
  formula <- formula(sprintf("Surv(%s,%s)~%s", vars$time, vars$status, vars$XTstatus))
  model <- coxph(formula, survdata, timefix = F, iter.max = 100)
  model$call$formula <- eval(model$call$formula)
  coef <- with(summary(model), data.frame(coefficients[1, -5, drop = F], conf.int[1, 3:4, drop = F],
                                          pvalue = sctest[3], check.names = F))
  if (only.coef) return(coef)
  fit <- survfit(formula, data = survdata)
  fit$call$formula <- eval(fit$call$formula)
  list(model = model, coef = coef, plot = list(fit = fit, data = survdata))
}


# Fit a time-dependent Cox model using fun_helper_tmerge to handle start and stop times for events.
fun_modelTDCox <- function(data, vars, only.coef = F, newtime = NULL, ...) {
  survdata <- fun_helper_tmerge(data, vars)
  formula <- formula(sprintf("Surv(tstart,tstop,%s)~%s", vars$status, vars$XTstatus))
  model <- coxph(formula, survdata, id = id, timefix = F, iter.max = 100)
  model$call$formula <- eval(model$call$formula)
  coef <- with(summary(model), data.frame(coefficients[1, -5, drop = F], conf.int[1, 3:4, drop = F],
                                          pvalue = sctest[3], check.names = F))
  if (only.coef) return(coef)
  plot <- fun_helper_tdplot(model = model, vars = vars,
                            maxtime = max(data[, vars$time], na.rm = T), newtime = newtime)
  list(model = model, coef = coef, plot = plot)
}


# Fit a Cox model with a Kaplan-Meier curve, using fun_helper_kmplot for visualization.
fun_modelKMCox <- function(data, vars, only.coef = F, ...) {
  survdata <- fun_helper_tmerge(data, vars)
  formula <- formula(sprintf("Surv(tstart,tstop,%s)~%s", vars$status, vars$XTstatus))
  model <- coxph(formula, survdata, id = id, timefix = F, iter.max = 100)
  model$call$formula <- eval(model$call$formula)
  coef <- with(summary(model), data.frame(coefficients[1, -5, drop = F], conf.int[1, 3:4, drop = F],
                                          pvalue = sctest[3], check.names = F))
  if (only.coef) return(coef)
  fit <- survfit(formula, data = survdata, id = id)
  fit$call$formula <- eval(fit$call$formula)
  fit <- fun_helper_kmplot(fit = fit, data = survdata, vars = vars)
  list(model = model, coef = coef, plot = list(fit = fit, data = survdata))
}


# Summarizes model coefficients in a table, useful for comparing multiple models.
fun_summaryTable <- function(models, names = NULL) {
  `rownames<-`(fun_coefMat(do.call(rbind, lapply(models, `[[`, "coef"))),
               if (is.null(names)) names(models) else names)
}


# Plots multiple survival curves from different models.
fun_summaryPlot <- function(plots, names = NULL, break.time.by = NULL, xlim = NULL) {
  pp <- mapply(function(plot, title) {
    fun_survplot(plot$fit, plot$data, break.time.by, xlim, risk.table = F, title = title)$plot
  }, plot = plots, title = if (is.null(names)) names(plots) else names, SIMPLIFY = F, USE.NAMES = F)
  ggfortify:::autoplot.list(pp)
}


# Simulates survival data based on specified parameters, including treatment and event times, Weibull parameters, and hazard ratios. CDF of weibull distribution: 1 - exp(-(lambda * x) ^ alpha)
# M: monte carlo replication size
# N: sample size in each replicate
# maxtimeT, maxtimeE: censoring time, i.e. maximum time threshold (days)
# alphaT, lambdaT: shape and rate of Weibull distribution for treatment time
# alphaE, lambdaE: shape and rate of Weibull distribution for event time
# beta: log hazard ratio
# N_good: at least number of good data from the first case
# Note: larger lambda leads to smaller time.
fun_simulateData <- function(M, N, maxtimeT, maxtimeE, alphaT, lambdaT, alphaE, lambdaE, beta,
                             N_good = N, set.seed = F, verbose = F, ...) {
  if (set.seed) {
    .seed <- .Random.seed
    on.exit(.Random.seed <<- .seed)  # assign(".Random.seed", .seed, envir = globalenv())
    set.seed(set.seed)
  }

  alpha0 <- -1.14
  c <- c1 <- exp(alpha0)
  c2 <- exp(alpha0 + beta)
  idx_good <- seq_len(N_good)
  dt_list <- list()

  for (i in seq_len(M * 1e3)) {
    u1 <- runif(N)
    u2 <- runif(N)
    b <- (-log(1 - u1) / c) ^ (1 / alphaT) / lambdaT  # treatment time
    temp <- (lambdaE * b) ^ alphaE
    flag <- u2 >= 1 - exp(-c1 * temp)  # TRUE for 2nd line in the paper
    t <- (-log(1 - u2) / ifelse(flag, c2, c1) + (1 - c1 / c2) * temp * flag) ^ (1 / alphaE) / lambdaE  # event time
    dt <- mutate(data.frame(timeE = t, timeT = b, id = 1:N),
                 timeT = ifelse(timeT > maxtimeT, Inf, timeT),
                 event = as.numeric(timeE <= maxtimeE), time = pmin(timeE, maxtimeE),
                 XT_value = as.numeric(timeT < time), XT_time = ifelse(XT_value, timeT, NA))[, -(1:2)]
    if (any(dt$event[idx_good] == 1) && any(dt$XT_value[idx_good] == 0) && any(dt$XT_value[idx_good] == 1)) {
      # at least one event, one time-dependent event and one time-dependent censored
      dt_list <- c(dt_list, list(dt))
      if (length(dt_list) == M) break
    }
  }

  if (verbose) cat(sprintf("\tGenerate %d data sets in %d tries (%.4g%%).\n", length(dt_list), i, length(dt_list) / i * 100))
  dt_list
}


# Creates various plot types (line, boxplot, violin) for visualizing simulation results.
fun_simulatePlot <- function(data, which = c("line", "boxplot", "violin"), x, y, group = model, color = model,
                             xlab = "true hazard ratio", ylab = NULL, size = NA_real_, position = c("identity", "dodge"), ...) {
  which <- match.arg(which)
  position <- match.arg(position)
  width <- switch(which, line = 0.2, boxplot =, violin = 0.5)  # boxplot width and dodge width
  pos <- switch(position, identity = position_identity(), dodge = position_dodge(width))
  geom <- switch(
    which,
    line = list(geom_line(aes(group = !!enquo(group)), position = pos),
                geom_point(size = replace_na(size, 1), position = pos)),
    boxplot = list(geom_boxplot(width = width, size = replace_na(size, .5), na.rm = T, show.legend = F, ...),
                   stat_summary(aes(group = !!enquo(group)), geom = "line", position = pos, fun = median, na.rm = T)),
    violin = list(geom_violin(trim = F, na.rm = T, show.legend = F, ...),
                  geom_boxplot(width = .1, size = replace_na(size, .5), na.rm = T, show.legend = F, outlier.shape = NA, ...),
                  geom_hline(yintercept = 0, linetype = "dashed"))
  )
  ggplot(data, aes(x = factor(!!enquo(x)), y = !!enquo(y), color = !!enquo(color))) +
    geom + labs(x = xlab, y = ylab, color = NULL) + theme(legend.position = "bottom")
}


# Estimates Weibull parameters and derivatives for Cox regression based on time-varying covariates, using nleqslv to solve equations.
fun_est_param <- function(data, vars) {
  est_weibull <- function(x) {
    # MME
    c <- log(mean(x ^ 2)) - log(mean(x) ^ 2)
    shape <- uniroot(function(y) 2 * lgamma(1 + 1 / y) - lgamma(1 + 2 / y) + c, c(1e-18, 1e18))$root
    rate <- gamma(1 + 1 / shape) / mean(x)
    c(shape = shape, rate = rate)
    # MLE
    # est <- MASS::fitdistr(x, "weibull", start = list(shape = shape, scale = 1 / rate))$estimate
    # c(shape = est[[1]], rate = 1 / est[[2]])
  }

  # Derivative of (kappaA, lambdaA, kappaT, lambdaT, beta)
  # x: value of (shape & rate) of treatment, (shape & rate) of event, log hazard ratio
  deriv_loglik <- function(x, data, vars) {
    kA <- x[1]; lA <- x[2]; kT <- x[3]; lT <- x[4]; b <- x[5]
    Y <- data[[vars$time]]
    R <- data[[vars$XTstatus]]
    D <- data[[vars$status]]
    A <- replace_na(data[[vars$XTtime]], 1e9)

    kappaA <- sum(R * (1 / kA + log(lA * A) - (lA * A) ^ kA * log(lA * A)) - (1 - R) * (lA * Y) ^ kA * log(lA * Y))
    lambdaA <- sum(R * (1 - (lA * A) ^ kA) - (1 - R) * (lA * Y) ^ kA)
    kappaT <- sum(D * (1 / kT + log(lT * Y)) - R * ((1 - exp(b)) * (lT * A) ^ kT * log(lT * A) + exp(b) * (lT * Y) ^ kT * log(lT * Y)) - (1 - R) * (lT * Y) ^ kT * log(lT * Y))
    lambdaT <- sum(D - R * ((1 - exp(b)) * (lT * A) ^ kT + exp(b) * (lT * Y) ^ kT) - (1 - R) * (lT * Y) ^ kT)
    beta <- sum(R * (D - exp(b) * ((lT * Y) ^ kT - (lT * A) ^ kT)))

    # A <- data$time_tvc
    # res1 <- sum(R * (1 / kA + log(lA * A) - (lA * A) ^ kA * log(lA * A)), na.rm = T) - sum((1 - R) * (lA * Y) ^ kA * log(lA * Y))
    # res2 <- sum(R * kA / lA * (1 - (lA * A) ^ kA), na.rm = T) - sum((1 - R) * kA / lA * (lA * Y) ^ kA)
    # res3 <- sum(D * (1 / kT + log(lT * Y))) - sum(R * ((1 - exp(b)) * (lT * A) ^ kT * log(lT * A) + exp(b) * (lT * Y) ^ kT * log(lT * Y)), na.rm = T) - sum((1 - R) * (lT * Y) ^ kT * log(lT * Y))
    # res4 <- sum(D * kT / lT) - sum(R * kT / lT * ((1 - exp(b)) * (lT * A) ^ kT + exp(b) * (lT * Y) ^ kT), na.rm = T) - sum((1 - R) * kT / lT * (lT * Y) ^ kT)
    # res5 <- sum(R * (D - exp(b) * ((lT * Y) ^ kT - (lT * A) ^ kT)), na.rm = T)
    c(kappaA, lambdaA, kappaT, lambdaT, beta)
  }

  suppressWarnings({
    init <- setNames(c(est_weibull(na.omit(data[[vars$XTtime]])), est_weibull(data[[vars$time]]), 0),
                     c("alphaT", "lambdaT", "alphaE", "lambdaE", "beta"))
    est <- nleqslv(init, deriv_loglik, data = data, vars = vars, control = list(maxit = 1e3))$x
  })
  list(initial = init, estimate = est)
}


code_global_setup <- metaAction({
  name <- list(model = c("Naive Cox", "Landmark Cox", "Time-dependent Cox", "Time-dependent Cox"),
               plot = c("Naive KM", "Landmark KM", "Smith-Zee", "Extended KM"))
  fun_model <- setNames(list(fun_modelCox, fun_modelLMCox, fun_modelTDCox, fun_modelKMCox), name$model)
})

if (file.exists("deploy/simu_data.rds")) {
  simu_ref <- readRDS("deploy/simu_data.rds")
  plot_simu_reference <- sapply(c("alpha", "lambda", "N"), function(which) {
    file <- simu_ref[[which]] %>% mutate(bias = coef - log(`exp(beta)`)) %>% filter(!is.na(coef) & abs(coef) < 10)
    # file %>% summarise(.by = alphaT:model, n(), overflow = sum(abs(coef) > 10, na.rm = T), na = sum(is.na(coef))) %>% print(n = 100)
    formula <- switch(which, alpha = alphaT ~ alphaE, lambda = lambdaT ~ lambdaE,
                      N = alphaT + alphaE + lambdaT + lambdaE ~ N)
    info <- select(file[1, ], 1:7, -starts_with(if (which == "N") c("alpha", "lambda", "N") else which, ignore.case = F))

    plot_bias <- fun_simulatePlot(file, "boxplot", x = `exp(beta)`, y = bias, ylab = "bias of log hazard ratio",
                                  position = "dodge", outlier.size = .3) +
      facet_grid(formula, labeller = label_both, scales = "free_y")
    plot_pval <- fun_simulatePlot(file, "boxplot", x = `exp(beta)`, y = pvalue, ylab = "pvalue",
                                  position = "dodge", outlier.size = .3) +
      geom_hline(yintercept = 0.05, linetype = "dashed") +
      facet_grid(formula, labeller = label_both) + ylim(0, 1)
    plot_rej <- group_by(file, across(1:"exp(beta)")) %>%
      summarise(rej = mean(pvalue < 0.05), .groups = "drop") %>%
      fun_simulatePlot("line", x = `exp(beta)`, y = rej, ylab = "rejection rate at significance level 0.05", position = "dodge") +
      geom_hline(yintercept = 0.05, linetype = "dashed") +
      facet_grid(formula, labeller = label_both) + ylim(0, 1)
    plot_cov <- group_by(file, across(1:"exp(beta)")) %>%
      summarise(cov = mean(`exp(beta)` >= `lower .95` & `exp(beta)` <= `upper .95`, na.rm = T), .groups = "drop") %>%
      fun_simulatePlot("line", x = `exp(beta)`, y = cov, ylab = "coverage rate at significance level 0.05", position = "dodge") +
      geom_hline(yintercept = 0.95, linetype = "dashed") +
      facet_grid(formula, labeller = label_both) + ylim(0, 1)

    list(info = info, bias = plot_bias, pval = plot_pval, rej = plot_rej, cov = plot_cov)
  }, simplify = F, USE.NAMES = T)
}
if (file.exists("deploy/real_data.rds")) {
  real_example <- readRDS("deploy/real_data.rds")
  real_example <- with(real_example, {
    tab <- fun_summaryTable(list(cox, lmcox, tdcox), names = paste(c("×", "?", "√"), name$model[-4]))
    pp <- fun_summaryPlot(lapply(list(cox, lmcox, tdcox, kmcox), `[[`, "plot"),
                          names = sprintf("%s %s", c("×", "?", "√", "√"), name$plot))
    label <- c("no", "at 50%", "at 75%")
    pp[[3]] <- pp[[3]] + scale_color_discrete(labels = label) + scale_fill_discrete(labels = label)
    list(table = tab, plot = pp)
  })
}
if (file.exists("deploy/simu_gridsearch.rds")) {
  # file.path("deploy", if (Sys.info()[[1]] == "Darwin") "simu_gridsearch_compress.rds" else "simu_gridsearch.rds")
  simu_gridsearch <- readRDS("deploy/simu_gridsearch_compress.rds")
}
data_example <- fun_simulateData(M = 1, N = 20, maxtimeT = 5, maxtimeE = 5, alphaT = 1, lambdaT = 1e-1,
                                 alphaE = 1, lambdaE = 1e-1, beta = 0, set.seed = 1)[[1]]



######
server <- function(input, output, session) {
  # output$test <- renderPrint(getCurrentOutputInfo())
  autoInvalidate <- reactiveTimer(1000)
  output$test <- renderText({
    autoInvalidate()
    a <- pryr::mem_used()
    print(a)
    x <- pryr::parenvs(rlang::current_env())
    env <- x[[length(x) - 2]]
    for (b in setdiff(ls(env), lsf.str(env))) {
      cat(sprintf("%-30s:", b))
      size <- tryCatch(pryr::object_size(get(b, envir = env), env = env), error = function(e) NULL)
      print(size)
    }
    cat(strrep("-", 80), "\n")
    capture.output(a)
  })
  output$debug <- renderUI({
    req(credentials()$user_auth)
    list(
      textAreaInput("debug_codeinput", label = NULL, value = "require(tidyverse)\n",
                    width = "100%", height = "400px", resize = "vertical"),
      verbatimTextOutput("debug_codeoutput"),
      verbatimTextOutput("debug_sessionInfo")
    )
  })
  output$debug_sessionInfo <- renderPrint(sessionInfo())
  output$debug_codeoutput <- renderPrint(eval(parse(text = input$debug_codeinput)))

  plotwidth <- function(x, ratio = 1) function() session$clientData[[paste0("output_", x, "_width")]] * ratio
  observe(toggle("sidebarPanel")) %>% bindEvent(input$toggleSidebar, ignoreInit = T)

  #### User Login & Logout ####
  credentials <- loginServer("login", data = user_base, user_col = user, pwd_col = password,
                             log_out = reactive(logout_init()))
  logout_init <- logoutServer("logout", active = reactive(credentials()$user_auth))
  output$user_auth <- reactive(credentials()$user_auth)
  outputOptions(output, "user_auth", suspendWhenHidden = F)
  observe({
    showNotification("You have full access to the source code.", duration = 4, type = "message")
  }) %>% bindEvent(req(credentials()$user_auth))


  #### Source Code Setup ####
  code_pkg_setup <- metaAction({
    library(tidyverse)
    library(survival)
    library(survminer)
  })



  #### Real Data Analysis ####
  ##### 1. Upload Data #####
  output$example <- renderDT(fun_roundDf(data_example, 4, "round"), options = list(pageLength = 10))
  output$example_download <- downloadHandler(
    "example.csv", content = function(file) write.csv(data_example, file, row.names = F))

  data_real <- metaReactive({
    req(input$datafile)
    validate(need(tools::file_ext(input$datafile$name) == "csv", "Please upload a csv file."))
    data <- read.csv(input$datafile$datapath, header = input$datafile_header, stringsAsFactors = F)
    unlink(input$datafile$datapath, recursive = T, force = T)
    data
  })

  output$data_real <- renderDT(fun_roundDf(data_real(), 4, "signif"), options = list(scrollX = T))
  output$ui_vars <- renderUI({
    var_all <- names(data_real())
    cls_all <- sapply(data_real(), class)
    idx_num <- sapply(data_real(), is.numeric)
    var_num <- var_all[idx_num]
    cls_num <- cls_all[idx_num]
    list(
      h4("Choose variables:"),
      pickerInput("var_id", "Id", choices = var_all, choicesOpt = list(subtext = cls_all), options = list(size = 10)),
      helpText("Time-to-event covariate (TTE):"),
      pickerInput("var_status", "Event status", choices = var_all, choicesOpt = list(subtext = cls_all), options = list(size = 10)),
      pickerInput("var_time", "Event time", choices = var_num, choicesOpt = list(subtext = cls_num), options = list(size = 10)),
      helpText("Time-varying covariate (TVC):"),
      pickerInput("var_XTstatus", "TVC status", choices = var_all, choicesOpt = list(subtext = cls_all), options = list(size = 10)),
      pickerInput("var_XTtime", "TVC time", choices = var_num, choicesOpt = list(subtext = cls_num), options = list(size = 10)),
      helpText("Event status: binary variable (0 or 1), coded as 0=censored or 1=event occured.", br(),
               "Event time: time of event occured or censored.", br(),
               "TVC status: status of time-varying covariate (0 or 1), coded as 0=always 0, status didn't change or 1=status changed from 0 to 1.", br(),
               "TVC time: time to time-varying covariate change.")
    )
  })

  observe({
    req(data_real())
    updatePickerInput(session, "var_id", choices = names(data_real()), selected = "subject")
    updatePickerInput(session, "var_status", choices = names(data_real()), selected = "event")
    updatePickerInput(session, "var_time", choices = names(data_real()), selected = "time")
    updatePickerInput(session, "var_XTstatus", choices = names(data_real()), selected = "XT_value")
    updatePickerInput(session, "var_XTtime", choices = names(data_real()), selected = "XT_time")
  }) %>% bindEvent(input$hack)

  # update data and variables after clicking "run"
  dt <- metaReactive2({
    req(input$run)
    isolate(metaExpr({
      list(data = ..(data_real()),
           vars = list(id = ..(input$var_id), status = ..(input$var_status), time = ..(input$var_time),
                       XTstatus = ..(input$var_XTstatus), XTtime = ..(input$var_XTtime)))
    }))
  })

  observe({
    req(dt())
    maxtime <- fun_ceiling(max(dt()$data[, dt()$vars$time]))
    updateSliderInput(session, "plot_xlim", max = maxtime, value = c(0, maxtime))
  })


  ##### 2. Naive cox model and KM curve #####
  cox <- metaReactive2({
    req(dt())
    cat("Run cox\n")
    metaExpr(fun_modelCox(..(dt())$data, ..(dt())$vars))
  })
  cox_plot <- metaReactive2({
    req(cox())
    metaExpr(with(..(cox())$plot, fun_survplot(fit, data, ..(input$plot_breaktime), ..(input$plot_xlim), risk.table = F)))
  })

  output$table_cox <- metaRender(renderTable, {
    fun_coefMat(..(cox())$coef)
  }, striped = T, hover = T, rownames = T, caption.placement = "top", caption = "Coefficients")
  output$plot_cox <- metaRender(renderPlot, {
    ..(cox_plot()) %++% theme(text = element_text(size = ..(input$plot_fontsize)))
  }, height = plotwidth("plot_cox", 0.6))
  output$plot_cox_download <- downloadHandler("Naive KM plot.pdf", content = function(file)
    ggsave(file, survminer:::.build_ggsurvplot(cox_plot()), width = 10, height = 7))

  observe({
    req(credentials()$user_auth, dt())
    ec <- newExpansionContext()
    ec$substituteMetaReactive(data_real, function() {
      metaExpr(read.csv(..(input$datafile$name), header = ..(input$datafile_header), stringsAsFactors = F))
    })
    code <- expandChain(
      code_pkg_setup(),
      "# Function definitions",
      fun_wrapFunction(fun_modelCox), fun_wrapFunction(fun_survplot), fun_wrapFunction(fun_coefMat),
      "# Change to your csv file location",
      output$table_cox(), output$plot_cox(), .expansionContext = ec)
    displayCodeModal(code, clip = clip_icon, wordWrap = T)
  }) %>% bindEvent(input$code_cox)


  ##### 3. Landmark cox model and KM curve #####
  output$other_lmcox_text1 <- renderText({
    sprintf("For %d%%: number of events occures before landmark threshold.", round(input$other_lmcox_prob * 100))
  })
  output$other_lmcox_text2 <- renderText({
    sprintf("For %d%%: number of subjects with time-varing covariate changing status after landmark threshold.",
            round(input$other_lmcox_prob * 100))
  })
  temp_lmcox <- metaReactive2({
    req(dt(), input$other_lmcox_prob)
    metaExpr(
      local({
        data <- ..(dt())$data
        vars <- ..(dt())$vars
        q <- quantile(data[, vars$XTtime], ..(input$other_lmcox_prob), na.rm = T, names = F)
        d <- list(filter(data, !!sym(vars$time) < q), filter(data, !!sym(vars$XTtime) > q)) %>%
          lapply(function(x) cbind(select(x, all_of(unlist(vars)))[1:nrow(x), ], threshold = q))
        list(quantile = q, landmark = replace_na(as.numeric(..(input$other_lmcox_landmark)), q), data = d)
      }))
  })
  output$other_lmcox_table1 <- renderTable(temp_lmcox()$data[[1]])
  output$other_lmcox_table2 <- renderTable(temp_lmcox()$data[[2]])

  lmcox <- metaReactive2({
    req(dt(), temp_lmcox())
    cat("Run lmcox\n")
    metaExpr(fun_modelLMCox(..(dt())$data, ..(dt())$vars, landmark = ..(temp_lmcox())$landmark))
  })
  lmcox_plot <- metaReactive2({
    req(lmcox())
    metaExpr(with(..(lmcox())$plot, fun_survplot(fit, data, ..(input$plot_breaktime), ..(input$plot_xlim))))
  })

  output$table_lmcox <- metaRender(renderTable, {
    fun_coefMat(..(lmcox())$coef)
  }, striped = T, hover = T, rownames = T, caption.placement = "top", caption = "Coefficients")
  output$plot_lmcox <- metaRender(renderPlot, {
    ..(lmcox_plot()) %++% theme(text = element_text(size = ..(input$plot_fontsize)))
  }, height = plotwidth("plot_lmcox", 0.6))
  output$plot_lmcox_download <- downloadHandler("Landmark KM plot.pdf", content = function(file)
    ggsave(file, survminer:::.build_ggsurvplot(lmcox_plot()), width = 10, height = 7))

  observe({
    req(credentials()$user_auth, dt())
    ec <- newExpansionContext()
    ec$substituteMetaReactive(data_real, function() {
      metaExpr(read.csv(..(input$datafile$name), header = ..(input$datafile_header), stringsAsFactors = F))
    })
    code <- expandChain(
      code_pkg_setup(),
      "# Function definitions",
      fun_wrapFunction(fun_modelLMCox), fun_wrapFunction(fun_survplot), fun_wrapFunction(fun_coefMat),
      "# Change to your csv file location",
      output$table_lmcox(), output$plot_lmcox(), .expansionContext = ec)
    displayCodeModal(code, clip = clip_icon, wordWrap = T)
  }) %>% bindEvent(input$code_lmcox)


  ##### 4. Time-dependent cox model and Smith-Zee curve #####
  temp_tdcox <- metaReactive2({
    req(dt())
    metaExpr(quantile(..(dt())$data[, ..(dt())$vars$XTtime],
                      as.numeric(..(input$other_tdcox_prob)), na.rm = T, names = F))
  })
  output$other_tdcox_quantile <- renderUI({
    req(temp_tdcox())
    HTML(paste0(format(100 * as.numeric(input$other_tdcox_prob), trim = T, digits = 1, nsmall = 1),
                "% quantile: ", temp_tdcox(), collapse = "<br/>"))
  })

  tdcox <- metaReactive2({
    req(dt(), temp_tdcox())
    cat("Run tdcox\n")
    metaExpr(fun_modelTDCox(..(dt())$data, ..(dt())$vars, newtime = ..(temp_tdcox())))
  })
  tdcox_plot <- metaReactive2({
    req(tdcox())
    metaExpr(with(..(tdcox())$plot, fun_survplot(fit, data, ..(input$plot_breaktime), ..(input$plot_xlim),
                                                 risk.table = F, legend.labs = ..(input$other_tdcox_legend))))
  })

  output$table_tdcox <- metaRender(renderTable, {
    fun_coefMat(..(tdcox())$coef)
  }, striped = T, hover = T, rownames = T, caption.placement = "top", caption = "Coefficients")
  output$plot_tdcox <- metaRender(renderPlot, {
    ..(tdcox_plot()) %++% theme(text = element_text(size = ..(input$plot_fontsize)))
  }, height = plotwidth("plot_tdcox", 0.6))
  output$plot_tdcox_download <- downloadHandler("Smith-Zee plot.pdf", content = function(file)
    ggsave(file, survminer:::.build_ggsurvplot(tdcox_plot()), width = 10, height = 7))

  observe({
    req(credentials()$user_auth, dt())
    ec <- newExpansionContext()
    ec$substituteMetaReactive(data_real, function() {
      metaExpr(read.csv(..(input$datafile$name), header = ..(input$datafile_header), stringsAsFactors = F))
    })
    code <- expandChain(
      code_pkg_setup(),
      "# Function definitions",
      fun_wrapFunction(fun_helper_tmerge), fun_wrapFunction(fun_helper_tdplot),
      fun_wrapFunction(fun_modelTDCox), fun_wrapFunction(fun_survplot), fun_wrapFunction(fun_coefMat),
      "# Change to your csv file location",
      output$table_tdcox(), output$plot_tdcox(), .expansionContext = ec)
    displayCodeModal(code, clip = clip_icon, wordWrap = T)
  }) %>% bindEvent(input$code_tdcox)


  ##### 5. Extended cox model and KM curve #####
  kmcox <- metaReactive2({
    req(dt())
    cat("Run kmcox\n")
    metaExpr(fun_modelKMCox(..(dt())$data, ..(dt())$vars))
  })
  kmcox_plot <- metaReactive2({
    req(kmcox())
    metaExpr(with(..(kmcox())$plot, fun_survplot(fit, data, ..(input$plot_breaktime), ..(input$plot_xlim))))
  })

  output$table_kmcox <- metaRender(renderTable, {
    fun_coefMat(..(kmcox())$coef)
  }, striped = T, hover = T, rownames = T, caption.placement = "top", caption = "Coefficients")
  output$plot_kmcox <- metaRender(renderPlot, {
    ..(kmcox_plot()) %++% theme(text = element_text(size = ..(input$plot_fontsize)))
  }, height = plotwidth("plot_kmcox", 0.6))
  output$plot_kmcox_download <- downloadHandler("Extended KM plot.pdf", content = function(file)
    ggsave(file, survminer:::.build_ggsurvplot(kmcox_plot()), width = 10, height = 7))

  observe({
    req(credentials()$user_auth, dt())
    ec <- newExpansionContext()
    ec$substituteMetaReactive(data_real, function() {
      metaExpr(read.csv(..(input$datafile$name), header = ..(input$datafile_header), stringsAsFactors = F))
    })
    code <- expandChain(
      code_pkg_setup(),
      "# Function definitions",
      fun_wrapFunction(fun_helper_tmerge), fun_wrapFunction(fun_helper_kmplot),
      fun_wrapFunction(fun_modelKMCox), fun_wrapFunction(fun_survplot), fun_wrapFunction(fun_coefMat),
      "# Change to your csv file location",
      output$table_kmcox(), output$plot_kmcox(), .expansionContext = ec)
    displayCodeModal(code, clip = clip_icon, wordWrap = T)
  }) %>% bindEvent(input$code_kmcox)


  ##### 6. Model Summary #####
  output$table_model_summary <- renderTable({
    req(cox(), lmcox(), tdcox(), kmcox())
    cat("Run summary\n")
    fun_summaryTable(list(cox(), lmcox(), tdcox()), names = paste(c("×", "?", "√"), name$model[-4]))
  }, striped = T, hover = T, rownames = T, caption.placement = "top", caption = "Coefficients of Models")
  output$plot_model_summary <- renderPlot({
    req(cox(), lmcox(), tdcox(), kmcox())
    fun_summaryPlot(lapply(list(cox(), lmcox(), tdcox(), kmcox()), function(x) x$plot),
                    names = sprintf("%s %s", c("×", "?", "√", "√"), name$plot),
                    break.time.by = input$plot_breaktime, xlim = input$plot_xlim) +
      theme(text = element_text(size = input$plot_fontsize))
  }, height = plotwidth("plot_model_summary", 0.7))


  ##### 7. Model Estimation #####
  res_est <- metaReactive2({
    req(dt())
    metaExpr(fun_est_param(..(dt())$data, ..(dt())$vars))
  }, inline = T)
  output$table_est <- renderTable({
    req(res_est())
    rbind(initial = res_est()$initial, estimate = res_est()$estimate)
  }, striped = T, hover = T, rownames = T, digits = 5,
  caption.placement = "top", caption = "Initial Values and Estimates of Parameters")

  # Parameter Input
  est_simu_pars <- reactive({
    req(res_est())
    pars <- as.list(res_est()$estimate)
    print(list(M = input$est_simu_M, N = input$est_simu_N, maxtimeT = input$est_simu_maxtime,
               maxtimeE = input$est_simu_maxtime, alphaT = pars$alphaT, lambdaT = pars$lambdaT,
               alphaE = pars$alphaE, lambdaE = pars$lambdaE, expbeta = c(1, exp(pars$beta))))
  }) %>% bindEvent(input$run_est_simu)
  data_est_simu <- reactive({
    req(est_simu_pars())
    .seed <- .Random.seed
    sapply(set_names(est_simu_pars()$expbeta), function(x) {
      assign(".Random.seed", .seed, envir = globalenv())
      do.call(fun_simulateData, `$<-`(est_simu_pars(), beta, log(x)))
    }, simplify = F)
  })

  res_est_simu <- reactive({
    l <- sapply(data_est_simu(), function(x) median(sapply(x, `[[`, simu_vars$XTtime), na.rm = T))
    lapply(fun_model[1:3], function(f) {
      mapply(function(x, y) bind_rows(lapply(x, function(d) f(d, simu_vars, only.coef = T, landmark = y))),
             x = data_est_simu(), y = l, SIMPLIFY = F)
    })
  })

  res_est_simu_summary <- reactive({
    m <- bind_rows(lapply(res_est_simu(), bind_rows, .id = "expbeta"), .id = "model") %>% filter(expbeta != 1)
    coef <- cbind(within(est_simu_pars(), rm(expbeta)), `rownames<-`(m, NULL)) %>%
      mutate(model = relevel(factor(model), name$model[1]), expbeta = as.numeric(expbeta), bias = coef - log(expbeta))
    avg <- filter(coef, !is.na(`exp(coef)`)) %>% group_by(across(1:expbeta)) %>%
      summarise(`Avgerage Estimate` = median(`exp(coef)`, na.rm = T),
                `Rejection Rate` = mean(pvalue < 0.05, na.rm = T),
                `Coverage Rate` = mean(expbeta >= `lower .95` & expbeta <= `upper .95`, na.rm = T), .groups = "drop") %>%
      select(model, last_col(2):last_col())
    list(coef = coef, avg = avg)
  })

  # Summary
  output$table_est_simu_summary <- renderTable({
    res_est_simu_summary()$avg
  }, striped = T, hover = T, digits = 3, caption.placement = "top", caption = "Est. Hazard Ratio under Different Models")

  output$plot_est_simu_summary_bias <- renderPlot({
    fun_simulatePlot(res_est_simu_summary()$coef, "boxplot", x = expbeta, y = bias,
                     ylab = "bias of log hazard ratio", position = "dodge", outlier.size = 1) +
      ggtitle(sprintf("Model Comparison\n(alphaT=%.3g, lambdaT=%.3g, alphaE=%.3g, lambdaE=%.3g)",
                      est_simu_pars()$alphaT, est_simu_pars()$lambdaT, est_simu_pars()$alphaE, est_simu_pars()$lambdaE)) +
      theme(text = element_text(size = input$plot_fontsize))
  }, height = plotwidth("plot_est_simu_summary_bias", 0.7))
  output$plot_est_simu_summary_est <- renderPlot({
    req(data_est_simu())
    res <- lapply(data_est_simu(), function(x) lapply(x, function(d) {
      fun_est_param(d, simu_vars)$estimate %>%
        setNames(paste0(names(.), "_est")) %>%
        c(beta_td_est = fun_modelTDCox(d, simu_vars, only.coef = T)$coef)
    }) %>% bind_rows(.id = "rep")) %>%
      bind_rows(.id = "expbeta") %>%
      cbind(within(est_simu_pars(), rm(expbeta)), .) %>%
      mutate(expbeta = as.numeric(expbeta),
             alphaT_bias = fun_bias(alphaT_est, alphaT, relative = F),
             lambdaT_bias = fun_bias(lambdaT_est, lambdaT, relative = F),
             alphaE_bias = fun_bias(alphaE_est, alphaE, relative = F),
             lambdaE_bias = fun_bias(lambdaE_est, lambdaE, relative = F),
             beta_bias = fun_bias(beta_est, log(expbeta), relative = F),
             beta_td_bias = fun_bias(beta_td_est, log(expbeta), relative = F)
      ) %>% pivot_longer(ends_with("bias")) %>%
      mutate(name = factor(name, levels = unique(name)))
    fun_simulatePlot(res, "violin", x = factor(expbeta), y = value, group = NA, color = NULL, ylab = "bias") +
      facet_wrap("name", ncol = 2, scales = "free_y",
                 labeller = function(x) mutate(x, across(everything(), ~ str_remove(., "_bias$")))) +
      theme(text = element_text(size = input$plot_fontsize))
  }, height = plotwidth("plot_est_simu_summary_est", 1))

  observe({
    req(credentials()$user_auth, dt())
    ec <- newExpansionContext()
    ec$substituteMetaReactive(data_real, function() {
      metaExpr(read.csv(..(input$datafile$name), header = ..(input$datafile_header), stringsAsFactors = F))
    })
    code <- expandChain(
      quote({
        library(tidyverse)
        library(nleqslv)
      }),
      "# Function definitions", fun_wrapFunction(fun_est_param),
      "# Change to your csv file location", res_est(), .expansionContext = ec)
    displayCodeModal(code, clip = clip_icon, wordWrap = T)
  }) %>% bindEvent(input$code_est)


  ##### 8. Real Data Example #####
  output$table_realdata_example <- renderTable(real_example$table, striped = T, hover = T, rownames = T,
                                               caption.placement = "top", caption = "Coefficients of Models")
  output$plot_realdata_example <- renderPlot(real_example$plot, height = plotwidth("plot_realdata_example", 0.7))



  #### Simulation ####
  ##### 1. Simulate Data #####
  code_simu_setup <- metaAction(simu_vars <- list(id = "id", status = "event", time = "time",
                                                  XTstatus = "XT_value", XTtime = "XT_time"))
  simu_pars <- metaReactive2({
    req(input$run_simu)
    print(isolate(metaExpr(
      list(M = ..(input$simu_M), N = ..(input$simu_N),
           maxtimeT = ..(input$simu_maxtime), maxtimeE = ..(input$simu_maxtime),
           alphaT = ..(input$simu_alphaT), lambdaT = ..(input$simu_lambdaT),
           alphaE = ..(input$simu_alphaE), lambdaE = ..(input$simu_lambdaE),
           expbeta = as.numeric(..(input$simu_expbeta))))
    ))
  })
  data_simu <- metaReactive2({
    req(simu_pars())
    metaExpr(
      local({
        .seed <- .Random.seed
        sapply(set_names(..(simu_pars())$expbeta), function(x) {
          assign(".Random.seed", .seed, envir = globalenv())
          do.call(fun_simulateData, `$<-`(..(simu_pars()), beta, log(x)))
        }, simplify = F)
      }))
  })
  simu_landmark <- metaReactive({
    val <- as.numeric(..(input$other_simu_landmark_value))
    switch(..(input$other_simu_landmark_type),
           "percentage" = lapply(pmin(1, val), function(q) sapply(..(data_simu()), function(x)
             quantile(sapply(x, `[[`, simu_vars$XTtime), q, na.rm = T, names = F))),
           "fixed value" = val)
  }, localize = T)

  res_simu_1 <- metaReactive({  # naive model, and time-dependent model
    lapply(fun_model[c(1, 3)], function(f) {
      lapply(..(data_simu()), function(x) bind_rows(lapply(x, function(d) f(d, simu_vars, only.coef = T))))
    })
  }, localize = T)
  res_simu_2 <- metaReactive({  # landmark model
    temp <- switch(..(input$other_simu_landmark_type),
                   "percentage" = label_percent()(as.numeric(..(input$other_simu_landmark_value))),
                   "fixed value" = ..(input$other_simu_landmark_value))
    lapply(setNames(..(simu_landmark()), sprintf("%s (%s)", name$model[2], temp)), function(l) {
      mapply(function(x, y) bind_rows(lapply(x, function(d) fun_model[[2]](d, simu_vars, only.coef = T, landmark = y))),
             x = ..(data_simu()), y = l, SIMPLIFY = F)
    })
  }, localize = T)
  res_simu_summary <- metaReactive({
    m <- bind_rows(lapply(c(..(res_simu_1()), ..(res_simu_2())), bind_rows, .id = "expbeta"), .id = "model")
    coef <- cbind(within(..(simu_pars()), rm(expbeta)), `rownames<-`(m, NULL)) %>%
      mutate(model = relevel(factor(model), name$model[1]), expbeta = as.numeric(expbeta), bias = coef - log(expbeta))
    avg <- filter(coef, !is.na(`exp(coef)`)) %>% group_by(across(1:expbeta)) %>%
      summarise(AvgEst = median(`exp(coef)`, na.rm = T), RejRate = mean(pvalue < 0.05, na.rm = T),
                CovRate = mean(expbeta >= `lower .95` & expbeta <= `upper .95`, na.rm = T), .groups = "drop") %>%
      pivot_longer(c(AvgEst, RejRate, CovRate)) %>%
      pivot_wider(id_cols = c(model, name), names_from = expbeta, values_from = value, names_glue = "HazardRatio={expbeta}")
    list(coef = coef, avg = avg)
  }, localize = T)


  ###### 1.1 Summary ######
  output$table_simu_summary_stat <- metaRender(renderTable, {
    bind_rows(lapply(..(data_simu()), bind_rows, .id = "rep"), .id = "expbeta") %>% group_by(expbeta, rep) %>%
      summarise(across(c(simu_vars$status, simu_vars$XTstatus), mean),
                across(c(simu_vars$time, simu_vars$XTtime), ~ mean(!is.na(.) & . <= ..(simu_pars())$maxtimeT / 2)),
                .groups = "drop_last") %>% select(-rep) %>%
      summarise(across(everything(), ~ do.call(sprintf, c("%s (%s-%s)", as.list(label_percent()(c(mean(.), range(.)))))))) %>%
      mutate(expbeta = paste0("HazardRatio=", expbeta)) %>% column_to_rownames("expbeta") %>%
      rename_with(~ c("event", "treatment") %>% c(sprintf("%s within %s days", ., ..(simu_pars())$maxtimeT / 2))) %>% t
  }, striped = T, hover = T, rownames = T, localize = T)

  output$plot_simu_summary_histogram <- metaRender2(renderPlot, {
    req(data_simu(), simu_landmark())
    metaExpr(
      local({
        temp <- list(E = ..(simu_pars())$maxtimeE * 1.2, T = ..(simu_pars())$maxtimeT * 1.2)
        data_hist <- lapply(..(data_simu()), function(dt)
          lapply(simu_vars[c("time", "XTtime")], function(i) c(sapply(dt, `[[`, i)))) %>%
          bind_rows(.id = "HazardRatio") %>%
          mutate(across(time, ~ replace_na(., temp$E)), across(XTtime, ~ replace_na(.,  temp$T)))
        data_landmark <- switch(
          ..(input$other_simu_landmark_type),
          "percentage" = pivot_longer(rownames_to_column(bind_cols(lapply(..(simu_landmark()), data.frame)),
                                                         "HazardRatio"), -HazardRatio),
          "fixed value" = data.frame(value = ..(simu_landmark())))

        pp1 <- ggplot(data_hist, aes(time)) + xlab("event time") +
          scale_x_continuous(breaks = ~ c(pretty_breaks(5)(.), temp$E),
                             labels = ~ replace(., . == temp$E, paste0(">", ..(simu_pars())$maxtimeE)))
        pp2 <- ggplot(data_hist, aes(XTtime)) + xlab("treatment time") +
          scale_x_continuous(breaks = ~ c(pretty_breaks(5)(.), temp$T),
                             labels = ~ replace(., . == temp$T, paste0(">", ..(simu_pars())$maxtimeT)))
        ggfortify:::autoplot.list(list(pp1, pp2), ncol = 1) +
          geom_histogram(aes(y = after_stat(width * density)), bins = 20, color = "black", fill = "white", na.rm = T) +
          geom_vline(aes(xintercept = value), data_landmark, color = "red") +
          facet_wrap("HazardRatio", nrow = 1, labeller = label_both) +
          scale_y_continuous(labels = label_percent()) + labs(y = NULL) +
          theme(text = element_text(size = ..(input$plot_fontsize)))
      }))
  }, height = plotwidth("plot_simu_summary_histogram", 0.5))

  output$table_simu_summary_avgest <- renderTable({
    subset(res_simu_summary()$avg, name == "AvgEst", select = -name)
  }, striped = T, hover = T, digits = 3, caption.placement = "top", caption = "Median of Est. Hazard Ratio")
  output$table_simu_summary_rejrate <- renderTable({
    subset(res_simu_summary()$avg, name == "RejRate", select = -name)
  }, striped = T, hover = T, digits = 3, caption.placement = "top", caption = "Rejection Rate")
  output$table_simu_summary_covrate <- renderTable({
    subset(res_simu_summary()$avg, name == "CovRate", select = -name)
  }, striped = T, hover = T, digits = 3, caption.placement = "top", caption = "Coverage Rate")

  output$plot_simu_summary_bias <- metaRender(renderPlot, {
    fun_simulatePlot(..(res_simu_summary())$coef, "boxplot", x = expbeta, y = bias,
                     ylab = "bias of log hazard ratio", position = "dodge", outlier.size = 1) +
      ggtitle(sprintf("Model Comparison\n(alphaT=%.3g, lambdaT=%.3g, alphaE=%.3g, lambdaE=%.3g)",
                      ..(simu_pars())$alphaT, ..(simu_pars())$lambdaT,
                      ..(simu_pars())$alphaE, ..(simu_pars())$lambdaE)) +
      theme(text = element_text(size = ..(input$plot_fontsize)))
  }, height = plotwidth("plot_simu_summary_bias", 0.7), localize = T)
  output$plot_simu_summary_est <- metaRender2(renderPlot, {
    req(data_simu())
    metaExpr(
      local({
        res <- lapply(..(data_simu()), function(x) lapply(x, function(d) {
          fun_est_param(d, simu_vars)$estimate %>%
            setNames(paste0(names(.), "_est")) %>%
            c(beta_td_est = fun_modelTDCox(d, simu_vars, only.coef = T)$coef)
        }) %>% bind_rows(.id = "rep")) %>%
          bind_rows(.id = "expbeta") %>%
          cbind(within(..(simu_pars()), rm(expbeta)), .) %>%
          mutate(expbeta = as.numeric(expbeta),
                 alphaT_bias = fun_bias(alphaT_est, alphaT, relative = F),
                 lambdaT_bias = fun_bias(lambdaT_est, lambdaT, relative = F),
                 alphaE_bias = fun_bias(alphaE_est, alphaE, relative = F),
                 lambdaE_bias = fun_bias(lambdaE_est, lambdaE, relative = F),
                 beta_bias = fun_bias(beta_est, log(expbeta), relative = F),
                 beta_td_bias = fun_bias(beta_td_est, log(expbeta), relative = F)
          ) %>% pivot_longer(ends_with("bias")) %>%
          mutate(name = factor(name, levels = unique(name)))
        fun_simulatePlot(res, "violin", x = factor(expbeta), y = value, group = NA, color = NULL, ylab = "bias") +
          facet_wrap("name", ncol = 2, scales = "free_y",
                     labeller = function(x) mutate(x, across(everything(), ~ str_remove(., "_bias$")))) +
          theme(text = element_text(size = ..(input$plot_fontsize)))
      }))
  }, height = plotwidth("plot_simu_summary_est", 1))


  ###### 1.2 Show case ######
  output$ui_simu_showcase <- renderUI({
    req(data_simu(), simu_pars())
    list(
      pickerInput("simu_showcase_expbeta", "Select hazard ratio", choices = names(data_simu()),
                  choicesOpt = list(subtext = paste("replication size:", lengths(data_simu())))),
      numericInput("simu_showcase_M", "Select replicate number", value = 1, min = 1, max = simu_pars()$M)
    )
  })

  data_simu_showcase <- reactive({
    req(data_simu(), input$simu_showcase_expbeta, input$simu_showcase_M)
    data_simu()[[input$simu_showcase_expbeta]][[input$simu_showcase_M]]
  })
  output$simu_showcase_data <- renderDT(fun_roundDf(data_simu_showcase(), 4, "signif"), options = list(pageLength = 10))
  res_simu_showcase <- reactive({
    req(data_simu(), simu_landmark())
    lapply(fun_model, function(f) f(data_simu_showcase(), simu_vars, landmark = simu_landmark()[[1]][1]))
  })

  output$table_simu_showcase_summary <- renderTable({
    fun_summaryTable(res_simu_showcase()[-4])
  }, striped = T, hover = T, rownames = T, caption.placement = "top", caption = "Coefficients of Models")
  output$table_simu_showcase <- renderTable({
    fun_summaryTable(res_simu_showcase()[-4])
  }, striped = T, hover = T, rownames = T, caption.placement = "top", caption = "Coefficients of Models")
  output$table_simu_showcase_est <- renderTable({
    res <- fun_est_param(data_simu_showcase(), simu_vars)
    rbind(initial = res$initial, estimate = res$estimate)
  }, striped = T, hover = T, rownames = T, digits = 5, caption.placement = "top", caption = "Model Parameter Estimation")

  output$plot_simu_showcase <- renderPlot({
    res <- res_simu_showcase()
    q <- quantile(data_simu_showcase()[, simu_vars$XTtime], .5, na.rm = T, names = F)
    res[[3]]$plot <- fun_helper_tdplot(res[[3]]$model, simu_vars, max(data_simu_showcase()[, simu_vars$time]), q)
    fun_summaryPlot(lapply(res, '[[', "plot"), names = sprintf("%s (%s)", name$plot, name$model)) +
      theme(text = element_text(size = input$plot_fontsize))
  }, height = plotwidth("plot_simu_showcase", 0.7))

  ##### 2. Reference case #####

  ###### 2.1 Grid Search ######
  output$simu_reference_grid_setting <- renderTable(simu_gridsearch[1, 1:3], display = rep("fg", 4))

  lapply(c("bias", "cov", "mse"), function(which) {
    lapply(c("alpha", "lambda"), function(var) {
      name <- sprintf("simu_reference_grid_%s_%s", which, var)
      name_plot <- paste0("plot_", name)
      name_ui <- paste0("ui_", name)
      pars <- sprintf("%s%s", switch(var, alpha = "lambda", lambda = "alpha"), c("T", "E"))
      labels <- sprintf("\\(\\%s_%s\\)", switch(var, alpha = "lambda", lambda = "alpha"), c("T", "E"))
      inputIds <- sprintf("grid_%s_%s_%s", which, var, pars)

      output[[name_ui]] <- renderUI({
        choices <- lapply(simu_gridsearch[pars], function(x) sort(unique(x)))
        list(
          tagList(
            tags$style(type = "text/css",
                       "#big_slider1 .irs-grid-text {font-size: 10px} #big_slider2 .irs-grid-text {font-size: 10px}"),
            div(id = "big_slider1",
                withMathJax(sliderTextInput(inputIds[1], labels[1], choices = choices[[1]],
                                            # selected = switch(var, alpha = max, lambda = min)(choices[[1]]),
                                            selected = max(choices[[1]]) / 2,
                                            grid = T, width = "80%"))),
            div(id = "big_slider2",
                withMathJax(sliderTextInput(inputIds[2], labels[2], choices = choices[[2]],
                                            # selected = switch(var, alpha = min, lambda = max)(choices[[2]]),
                                            selected = max(choices[[2]]) / 2,
                                            grid = T, width = "80%")))
          )
        )
      })
      gc()

      output[[name_plot]] <- renderPlot({
        vals <- lapply(inputIds, function(i) input[[i]])
        req(vals[[1]], vals[[2]])
        dt_filter <- filter(simu_gridsearch, !!sym(pars[1]) == vals[[1]], !!sym(pars[2]) == vals[[2]])
        dgt <- floor(log10(max(abs(dt_filter[[which]]))))
        pp <- ggplot(dt_filter, aes(!!!syms(c(x = paste0(var, "T"), y = paste0(var, "E"), z = which)))) +
          geom_contour_fill(breaks = AnchorBreaks(0), bins = 8, global.breaks = F) +
          geom_contour2(aes(label = round(after_stat(level), 2 - dgt)), breaks = AnchorBreaks(0),
                        bins = 8, size = 0.1, label_size = 3, global.breaks = F, skip = 2) +
          switch(which, bias = scale_fill_divergent(low = "orangered", high = "dodgerblue", trans = "pseudo_log",
                                                    limits = function(x) c(-1, 1) * max(abs(x))),
                 cov = scale_fill_gradient(low = "orange", high = "skyblue", limits = 0:1),  # hcl(0, 80, 100), hcl(0, 80, 50)
                 mse = scale_fill_distiller(direction = 1, trans = "pseudo_log")) +
          facet_grid(model ~ beta, labeller = label_bquote(cols = "true logHR:" ~ .(beta))) +
          labs(fill = "value") + coord_fixed() +
          theme_bw() + theme(text = element_text(size = input$plot_fontsize))

        if (which == "cov") {
          g <- ggplotGrob(pp)
          # gt <- function(x, pattern) gtable::gtable_filter(x, pattern, trim = F)$grobs[[1]]
          # color <- last(gt(gt(gt(g, "guide"), "guide"), "bar")$raster)  # extract color of lowest values in legend
          new <- grid::pathGrob(c(1, 1, 21, 21) / 22, c(1, 21, 21, 1) / 22, rule = "evenodd",
                                default.units = "native", name = "NULL", gp = grid::gpar(col = NA, fill = "orange"))
          for (i in grep("panel", g$layout$name))
            if (length(grep("path", grid::childNames(g$grobs[[i]]))) == 0)
              g$grobs[[i]] <- grid::setGrob(g$grobs[[i]], "NULL", new, grep = T)
          ggplotify::as.ggplot(g)
        } else pp
      }, height = plotwidth(name_plot, 0.6))
      gc()
    })
  })


  ###### 2.2 Other Comparison (alpha, lambda, N) ######
  lapply(c("alpha", "lambda", "N"), function(which) {
    res <- plot_simu_reference[[which]]
    output[[paste0("simu_reference_param_", which)]] <- renderTable(res$info, display = rep("fg", ncol(res$info) + 1))

    name <- paste0("plot_simu_reference_bias_", which)
    output[[name]] <- renderPlot({
      res$bias + theme(text = element_text(size = input$plot_fontsize))
    }, height = plotwidth(name, ifelse(which == "N", 1.3, 0.7)))

    name <- paste0("plot_simu_reference_pval_", which)
    output[[name]] <- renderPlot({
      res$pval + theme(text = element_text(size = input$plot_fontsize))
    }, height = plotwidth(name, ifelse(which == "N", 1.3, 0.7)))

    name <- paste0("plot_simu_reference_rej_", which)
    output[[name]] <- renderPlot({
      res$rej + theme(text = element_text(size = input$plot_fontsize))
    }, height = plotwidth(name, ifelse(which == "N", 1.1, 0.6)))

    name <- paste0("plot_simu_reference_cov_", which)
    output[[name]] <- renderPlot({
      res$cov + theme(text = element_text(size = input$plot_fontsize))
    }, height = plotwidth(name, ifelse(which == "N", 1.1, 0.6)))
  })


  ###### 2.3 Source code ######
  output$simu_reference_grid_download <- downloadHandler("simu_gridsearch.rds", content = function(file) saveRDS(get0("simu_gridsearch"), file))
  output$simu_reference_alpha_download <- downloadHandler("simu_data.rds", content = function(file) saveRDS(get0("simu_ref"), file))
  output$simu_reference_lambda_download <- downloadHandler("simu_data.rds", content = function(file) saveRDS(get0("simu_ref"), file))
  output$simu_reference_n_download <- downloadHandler("simu_data.rds", content = function(file) saveRDS(get0("simu_ref"), file))

  observe({
    req(credentials()$user_auth, simu_pars())
    code <- expandChain(
      code_pkg_setup(),
      quote({
        library(scales)
        library(ggfortify)
        library(nleqslv)
      }),
      "# Function definitions",
      fun_wrapFunction(fun_simulateData), fun_wrapFunction(fun_simulatePlot),
      fun_wrapFunction(fun_helper_tmerge),
      fun_wrapFunction(fun_helper_tdplot), fun_wrapFunction(fun_helper_kmplot),
      fun_wrapFunction(fun_est_param), fun_wrapFunction(fun_bias),
      fun_wrapFunction(fun_modelCox), fun_wrapFunction(fun_modelLMCox),
      fun_wrapFunction(fun_modelTDCox), fun_wrapFunction(fun_modelKMCox),
      "# Simulate data", code_global_setup(), code_simu_setup(), invisible(data_simu()),
      "# Compute summary statistics",
      output$table_simu_summary_stat(), invisible(res_simu_summary()), quote(res_simu_summary$avg),
      "# Draw histogram", output$plot_simu_summary_histogram(),
      "# Draw boxplot", output$plot_simu_summary_bias(),
      "# Draw violin plot", output$plot_simu_summary_est())
    displayCodeModal(code, clip = clip_icon, wordWrap = T)
  }) %>% bindEvent(input$code_simu_summary)

  observe({
    req(credentials()$user_auth)
    code <- expandChain(
      quote({
        library(tidyverse)
        library(metR)
      }),
      "# Function definitions", fun_wrapFunction(fun_simulatePlot),
      "# Load saved data",
      quote({
        simu_ref <- readRDS("simu_data.rds")
        simu_gridsearch <- readRDS("simu_gridsearch.rds")
      }),
      "# Draw grid search comparison",
      "# Draw comparison plots")
    displayCodeModal(code, clip = clip_icon, wordWrap = T)
  }) %>% bindEvent(input$code_simu_reference)
}
