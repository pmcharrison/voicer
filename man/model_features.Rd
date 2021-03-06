% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-features.R
\name{model_features}
\alias{model_features}
\title{Model features}
\usage{
model_features(x, features = attr(x, "features"),
  formula = linear_formula(x, features), keep_model = TRUE,
  eval_model = TRUE, perm_int = TRUE, verbose = TRUE)
}
\arguments{
\item{x}{A corpus's feature analysis as created by
\code{\link{get_corpus_features}}.}

\item{features}{(Character vector)
Features to model, corresponding to columns of \code{x}.
By default this is extracted from the metadata of \code{x},
and corresponds to all computed features.}

\item{formula}{(Formula)
The formula used to predict chord voicings.
The default is created by \code{\link{linear_formula}},
and corresponds to a simple linear combination of the features
specified in the \code{features} argument.}

\item{keep_model}{(Logical scalar)
Determines whether the returned object should include the fitted model
from \code{\link[mclogit]{mclogit}},
which can be rather large.}

\item{eval_model}{(Logical scalar)
Whether to compute and return model evaluation metrics.}

\item{perm_int}{(Logical scalar)
Whether to compute and return permutation-based feature importance metrics
(also known as model reliance metrics).}

\item{verbose}{(Logical scalar)
Whether to show progress messages during the computation.}
}
\value{
An object of class \code{voicer_mod} with the following slots:
\itemize{
\item \code{features} -
A character vector listing the features used to fit the model.
\item \code{formula} -
A character vector corresponding to the formula used to fit the model.
\item \code{weights} -
A \code{\link[tibble]{tibble}} describing the fitted weights.
This tibble has five columns:
\code{feature} (the name of the feature, or more generally the effect),
\code{estimate} (the estimate of the corresponding regression weight),
\code{std_err} (the standard error of the regression weight),
\code{z} (the z-statistic associated with the regression weight),
and \code{p} (the p-value associated with the regression weight).
\item \code{mod} -
A fitted model as created by \code{\link[mclogit]{mclogit}}.
\item \code{eval} -
Evaluation metrics for the fitted model.
This is a list with two components.
\code{summary} is a \code{\link[tibble]{tibble}} listing six model statistics:
\code{probability}, the mean probability assigned to the observed voicing;
\code{accuracy}, the mean accuracy when predicting the correct voicing;
\code{info_content}, the mean log probability of the observed voicing (base 2);
\code{num_options}, the mean number of candidate voicings for each chord;
\code{abs_rank}, the mean absolute rank of the chosen voicing
within the list of candidates as ranked by assigned probability;
\code{pct_rank}, the mean percentile rank of the chosen voicing
within the list of candidates as ranked by assigned probability.
\code{perm_int} is a \code{\link[tibble]{tibble}} listing permutation importances
(also known as model reliances) by feature,
with the permutation importance metrics mapping to
the columns of \code{summary}.
}
}
\description{
This function fits a sequential conditional logit model
that predicts chord voicings from chord features.
}
