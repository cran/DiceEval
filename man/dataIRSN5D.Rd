\name{dataIRSN5D}
\docType{data}
\alias{dataIRSN5D}
\title{5D benchmark from nuclear criticality safety assessments}
\description{Nuclear criticality safety assessments are based on an optimization process to search for safety-penalizing physical conditions in a given range of parameters of a system involving fissile materials.

In the following examples, the criticality coefficient (namely k-effective or keff) models the nuclear chain reaction trend:

- keff > 1 is an increasing neutrons production leading to an uncontrolled chain reaction potentially having deep consequences on safety,

- keff = 1 means a stable neutrons population as required in nuclear reactors,

- keff < 1 is the safety state required for all unused fissile materials, like for fuel storage.

Besides its fissile materials geometry and composition, the criticality of a system is widely sensitive to physical parameters like water density, geometrical perturbations or structure materials (like concrete) characteristics. Thereby, a typical criticality safety assessment is supposed to verify that k-effective cannot reach the critical value of 1.0 (in practice the limit value used is 0.95) for given hypothesis on these parameters.

The benchmark system is an assembly of four fuel rods contained in a reflecting hull. Regarding criticality safety hypothesis, the main parameters are the uranium enrichment of fuel (namely "e", U235 enrichment, varying in [0.03, 0.07]), the rods assembly geometrical characteristics (namely "p", the pitch between rods, varying in [1.0, 2.0] cm and "l", the length of fuel rods, varying in [10, 60] cm), the water density inside the assembly (namely "b", varying in [0.1, 0.9]) , and the hull reflection characteristics (namely "r", reflection coefficient, varying in [0.75, 0.95]).

In this criticality assessment, the MORET (Fernex et al., 2005) Monte Carlo simulator is used to estimate the criticality coefficient of the fuel storage system using these parameters (among other) as numerical input,. The output k-effective is returned as a Gaussian density which standard deviation is setup to be negligible regarding input parameters sensitivity.
}

\references{
Fernex F., Heulers L, Jacquet O., Miss J. and Richet Y. (2005) \emph{The MORET 4B Monte Carlo code - New features to treat complex criticality systems}, M&C International Conference on Mathematics and Computation Supercomputing, Reactor Physics and Nuclear and Biological Application, Avignon, 12/09/2005
}
\usage{data(dataIRSN5D)}
\format{a data frame with 50 observations (lines) and 6 columns. Columns 1 to 5 correspond to the design of experiments for the input variables ("b","e","p","r" and "l") and the last column the value of the output "keff".}
\author{Y. Richet}
\source{IRSN (Institut de Radioprotection et de Sûreté Nucléaire)}
\keyword{datasets}