*&---------------------------------------------------------------------*
*& Report Z4WEEK_FUNCTION01
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z4WEEK_FUNCTION01.

DATA: N1 TYPE I,
      N2 TYPE I,
      S1 TYPE I.

N1 = 1.
N2 = 2.

CALL FUNCTION 'ZPLUS'
  EXPORTING
    num1          = N1
    num2          = N2
  IMPORTING
    SUM1          = S1
.

WRITE : N1, '+', N2, '=', S1.