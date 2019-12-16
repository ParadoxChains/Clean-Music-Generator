definition module Util.complexUtils

:: Complex = {real :: Real, imaginary :: Real}

//for adding complex numbers
instance + Complex

//for subtracting complex numbers
instance - Complex

//for multiplying complex numbers
instance * Complex

//to find a conjugate of complex number
conjugate::Complex->Complex

// for dividing complex numbers
instance / Complex