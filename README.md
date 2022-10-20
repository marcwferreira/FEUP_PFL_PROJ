# FEUP_PFL_PROJ

>This project is a small program to calculate different functions with polynomials. 
>Done for the subject of PFL at FEUP.

## Main Functionalities

### The module consists of 4 function:

> #### **POLYNOMIAL NORMALIZATION** <br>
> - NAME: <br>
> &nbsp; normPoli - normalize polynomial
> - SYNOPSIS: <br>
> &nbsp; normPoli [polynomial string]
> - DESCRIPTION: <br>
> &nbsp; Modifies a polynomial to the polynomial norm form. <br>
> &nbsp; Mandatory arguments: <br>
> &nbsp; &nbsp; &nbsp; <**polynomial**> <br>
> &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; String of polynomial to be normalized <br>
> &nbsp; Return value<br>
> &nbsp; &nbsp; &nbsp; This function returns a string of normalized polynomial.
> - EXAMPLES: <br>
> &nbsp; Polynomial already in normal form - **normPoli "x"**<br>
> &nbsp; &nbsp; **result** "x"
> <br><br>
> &nbsp; Polynomial with one term - **normPoli "2x^0"**<br>
> &nbsp; &nbsp; **result** "2"
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
> 
>- AUTHORS: <br>
> &nbsp; Written by **Andre Costa** and **Marcos Ferreira**.

<br>

> #### **ADDITION OF POLYNOMIALS** <br>
> - NAME: <br>
> &nbsp; addPolis - Sum two polynomials
> - SYNOPSIS: <br>
> &nbsp; sumPolis [polynomial string] [polynomial string]
> - DESCRIPTION: <br>
> &nbsp; Sums the first polynomial to the second one. <br>
> &nbsp; Mandatory arguments: <br>
> &nbsp; &nbsp; &nbsp; <**polynomial**> <br>
> &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; First polynomial <br>
> &nbsp; &nbsp; &nbsp; <**polynomial**> <br>
> &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; Second polynomial <br>
> &nbsp; Return value<br>
> &nbsp; &nbsp; &nbsp; This function returns a string of the sum result.
> - EXAMPLES: <br>
> &nbsp; Addition with empty polynomial - **addPolis &nbsp; "x" &nbsp; " "**<br>
> &nbsp; &nbsp; **result** "x"
> <br><br>
> &nbsp; Addition of numbers (polynomial with x^0 only) - **addPolis &nbsp; "3" &nbsp; "2"**<br>
> &nbsp; &nbsp; **result** "5"
> <br><br>
>&nbsp; Addition with variables with no expoent - **addPolis &nbsp; "3x" &nbsp; "2y"**<br>
> &nbsp; &nbsp; **result** "3x + 2y"
> <br><br>
>&nbsp; Addition of opposites polynomial - **addPolis &nbsp; "3x^2" &nbsp; "-3x^2"**<br>
> &nbsp; &nbsp; **result** "0"
> <br><br>
>&nbsp; Addition of variables with same expoents - **addPolis &nbsp; "3x^2" &nbsp; "2x^2"**<br>
> &nbsp; &nbsp; **result** "5x^2"
> <br><br>
> &nbsp; These examples can be mixed to form more complex examples <br>
> - AUTHORS: <br>
> &nbsp; Written by **Andre Costa** and **Marcos Ferreira**.

> #### **MULTIPLICATION OF POLYNOMIALS** <br>
> - NAME: <br>
> &nbsp; multPolis - Multiply two polynomials
> - SYNOPSIS: <br>
> &nbsp; multPolis [polynomial string] [polynomial string]
> - DESCRIPTION: <br>
> &nbsp; Muliplys the first polynomial to the second one. <br>
> &nbsp; Mandatory arguments: <br>
> &nbsp; &nbsp; &nbsp; <**polynomial**> <br>
> &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; First polynomial <br>
> &nbsp; &nbsp; &nbsp; <**polynomial**> <br>
> &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; Second polynomial <br>
> &nbsp; Return value<br>
> &nbsp; &nbsp; &nbsp; This function returns a string of the mult result.
> - EXAMPLES: <br>
> &nbsp; Multiply with empty polynomial - **multPolis &nbsp; "x" &nbsp; ""**<br>
> &nbsp; &nbsp; **result** ""
> <br><br>
> &nbsp; Multiply with 0 - **multPolis &nbsp; "3x^2" &nbsp; "0"**<br>
> &nbsp; &nbsp; **result** "0"
> <br><br>
> &nbsp; Multiply only numbers - **multPolis &nbsp; "3" &nbsp; "6"**<br>
> &nbsp; &nbsp; **result** "18"
> <br><br>
> &nbsp; Multiply with same variables (sum expoents) - **multPolis &nbsp; "3x" &nbsp; "6x"**<br>
> &nbsp; &nbsp; **result** "18x^2"
> <br><br>
> &nbsp; Multiply that needs a sum operation after - **multPolis &nbsp; "3x^2*y + 3y" &nbsp; "6y + 6x^2*y"**<br>
> &nbsp; &nbsp; **result** "18x^4*y^2 + 36x^2*y^2 + 18y^2"
> <br><br>
> &nbsp; These examples can be mixed to form more complex examples <br>
> - AUTHORS: <br>
> &nbsp; Written by **Andre Costa** and **Marcos Ferreira**.

> #### **DERIVATION OF POLYNOMIALS** <br>
> - NAME: <br>
> &nbsp; derivPoli - Derivation of a polynomial by a variable
> - SYNOPSIS: <br>
> &nbsp; derivPoli [Term] [polynomial string]
> - DESCRIPTION: <br>
> &nbsp; Derivs a polynomial by a variable. <br>
> &nbsp; Mandatory arguments: <br>
> &nbsp; &nbsp; &nbsp; <**Term**> <br>
> &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; variable <br>
> &nbsp; &nbsp; &nbsp; <**polynomial**> <br>
> &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; Polynomial <br>
> &nbsp; Return value<br>
> &nbsp; &nbsp; &nbsp; This function returns a string of the mult result.
> - EXAMPLES: <br>
> &nbsp; Derivation of empty polynomial - **derivPoli &nbsp; ""**<br>
> &nbsp; &nbsp; **result** ""
> <br><br>
> &nbsp; These examples can be mixed to form more complex examples <br>
> - AUTHORS: <br>
> &nbsp; Written by **Andre Costa** and **Marcos Ferreira**.

> duvidas:
> se derivarmos um polinómio e dermos uma variável que este não tenha o que retorna?
> rever o que eu fiz