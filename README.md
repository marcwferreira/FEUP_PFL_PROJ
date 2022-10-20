# FEUP_PFL_PROJ

This project is a small program to calculate different functions with polynomials. Done for the subject of PFL at FEUP.

## Main Functionalities

### The module consists of four function:

> #### **POLYNOMIAL NORMALIZATION** <br>
> - NAME <br>
> &nbsp; normPoli - normalize polynomial
> - SYNOPSIS <br>
> &nbsp; normPoli [polynomial string]
> - DESCRIPTION <br>
> &nbsp; Modifies a polynomial to the polynomial norm form. <br>
> &nbsp; Mandatory arguments: <br>
> &nbsp; &nbsp; &nbsp; <**polynomial**> <br>
> &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; String of polynomial to be normalized <br>
> &nbsp; Return value<br>
> &nbsp; &nbsp; &nbsp; This function returns a string of normalized polynomial.
> - EXAMPLES <br>
> &nbsp; Polynomial already in normal form - **normPoli "x"**<br>
> &nbsp; &nbsp; **result** "x"
> <br><br>
> &nbsp; Polynomial with one term - **normPoli "2x^0"**<br>
> &nbsp; &nbsp; **result** "2x"
> <br><br>
> &nbsp; Equal terms in polynomial - **normPoli "2x^2+3x^2"**<br>
> &nbsp; &nbsp; **result** "5x^2"
> <br><br>
> &nbsp; Term with same pair (variable, exponent) -  **normPoli "2x^2*x^2"**<br>
> &nbsp; &nbsp; **result** "2x^4"
> <br><br>
> &nbsp; Polynomial term with variables not sorted - **normPoli "2y*x^2"**<br>
> &nbsp; &nbsp; **result** "2x^2*y"
> <br><br>
> &nbsp; Polynomial with terms not sorted - **normPoli "2y*x^2"**<br>
> &nbsp; &nbsp; **result** "2x^2*y"
> <br><br>
> &nbsp; These examples can be mixed to form more complex examples <br>
> - AUTHORS <br>
> &nbsp; Written by **Andre Costa** and **Marcos Ferreira**.

<br>

> #### **ADDITION OF POLYNOMIALS** <br>
> - NAME <br>
> &nbsp; addPolis - Sum two polynomials
> - SYNOPSIS <br>
> &nbsp; sumPolis [polynomial string] [polynomial string]
> - DESCRIPTION <br>
> &nbsp; Modifies a polynomial to the polynomial norm form. <br>
> &nbsp; Mandatory arguments: <br>
> &nbsp; &nbsp; &nbsp; <**polynomial**> <br>
> &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; First polynomial <br>
> &nbsp; &nbsp; &nbsp; <**polynomial**> <br>
> &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; Second polynomial <br>
> &nbsp; Return value<br>
> &nbsp; &nbsp; &nbsp; This function returns a string of the sum result.
> - EXAMPLES <br>
> &nbsp; Addition with empty polynomial - **addPolis &nbsp; "x" &nbsp; " "**<br>
> &nbsp; &nbsp; **result** "x"
> <br><br>
> &nbsp; Addition of numbers (polynomial with x^0 only) - **addPolis &nbsp; "3" &nbsp; "2"**<br>
> &nbsp; &nbsp; **result** "5"
> <br><br>
> &nbsp; These examples can be mixed to form more complex examples <br>
> - AUTHORS <br>
> &nbsp; Written by **Andre Costa** and **Marcos Ferreira**.

