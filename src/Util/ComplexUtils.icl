implementation module Util.ComplexUtils
import StdEnv


//for adding complex numbers
instance + Complex
where
	(+) a b = { real = a.real + b.real , imaginary = a.imaginary + b.imaginary }

//for subtracting complex numbers
instance - Complex
where
	(-) a b = { real = a.real - b.real , imaginary = a.imaginary - b.imaginary }

//for multiplying complex numbers
instance * Complex
where
	(*) a b = { real = a.real * b.real - a.imaginary * b.imaginary ,
				imaginary = a.real*b.imaginary + a.imaginary*b.real }

//to find a conjugate of complex number
conjugate::Complex->Complex
conjugate a = {real = a.real , imaginary = -1.0*a.imaginary }

// for dividing complex numbers
instance / Complex
where
	(/) a b = { real = (a * (conjugate b)).real / (b*(conjugate b)).real,
					imaginary = (a * (conjugate b)).imaginary / (b*(conjugate b)).real }


