#
#
#  Devel NOTES ONLY.R
#
#'
#' KEY INSIGHT:
#'   . The target is a feature that works well with the benefit of foresight
#'     Foresight may be 1, 5, 21 or whatever number of days.  The
#'     feature should have look back with enough hindsight to be stable
#'     e.g. Maybe a minimum of a 5:1 hindsight:foresight ratio, or higher?
#'     If no hindsight, then we are predicting future returns and that's too
#'     noisy for the ML algorithm to figure out a reasonable pattern.
#'
#'  . The target can be momentum, an sma, or a ratio of smas and momentum, or
#'    really anything else including a mixture of momentums and/or higher
#'    moments.
#'
#'  . The key point is the target feature must have a compelling equity curve
#'    with foresight with few switches (so it's tradable), low MDD and compelling
#'    CAGR.  This need is traded off with the need to be predictable so an ML
#'    model with reasonable prediction accuracy (as measured by y vs. ypred) can
#'    be developed.
#'
#'  . Then, copy that same feature in the ML model, but adjusted for an alignment
#'    in time.  This means that both target (looking forward beyond time t) and
#'    feature must start on the same day in the time series.  For instance, if
#'    Nlags = -10 (10 day foresight), and we use an sma50_sma200 for the target,
#'    then the feature should be sma40_sma190 so it aligns in time.  This implies
#'    all is known up to time t, and the model is only predicting for the extra
#'    10 days.  NOTE:  This is clear for momentums, but not sure it applies to
#'    SMA ratios like this.  Test to see.
#'
#'  . In addition to modulating the feature with sd ratios to get some foresight
#'    based on volatility, look into adding a feature based on interest rates
#'    (TNX and 3 months T bills to get a sense of the yield curve).  Do some
#'    analysis to see how good of a predictor this may be, and whether
#'    the higher moments can be helpful (first test to see if there is
#'    autocorrelation of higher moments in TNX, and if so, it can be helpful to
#'    predict future value of TNX and, indirectly, the market index if TNX foresight
#'    is helpful)
#'
#' FUNCTIONS IMPROVEMENTS
#'  . Add + and - to make_features
#'  . Add offsets to make_features, so that a ratio around one can be used as a
#'    predictor - similar to make_featurecurve
#'
#'
