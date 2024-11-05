\name{jarpa}
\alias{jarpa}
\title{This function to analysis Just-About-Right data for each sample}
\usage{
jarpa(data, col.l = NA, percent_consum = 20, meandrop_line=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ A dataframe of JAR data have sensory descriptors and overall liking
}
  \item{col.l}{
The Position of the Liking variable
}
  \item{percent_consum}{
The minimum percentage of number consumer for apply Penalty analysis
}
  \item{meandrop_line}{
The default of meandrop line when plot figure Mean drops with Percent for each sensory attribute
}
}

\author{
LE Tuan Phuc phuc.letuan@hust.edu.vn
}

\examples{
jarpa(data, 1)
}

