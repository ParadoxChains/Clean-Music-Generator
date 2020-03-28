definition module Util.complexUtils

:: Complex = {real :: Real, imaginary :: Real}

/*
Name: instance +
Input: record: Complex 
Output: record: Complex
*/

instance + Complex


/*
Name: instance -
Input: record: Complex 
Output: record: Complex
*/

instance - Complex


/*
Name: instance *
Input: record: Complex 
Output: record: Complex
*/

instance * Complex

/*
Name: instance /
Input: record: Complex 
Output: record: Complex
*/
instance / Complex

/*
Name: conjugate
Input: record: Complex 
Output: record: Complex
*/
conjugate::Complex->Complex