CLASS zcl_bdc_screen DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_bdc_operation .

    ALIASES bdc_line
      FOR zif_bdc_operation~bdc_line .

    METHODS constructor
      IMPORTING
        !screen TYPE n .
    METHODS in_program
      IMPORTING
        !program TYPE csequence .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_BDC_SCREEN IMPLEMENTATION.


  METHOD constructor.
    bdc_line-dynpro = screen.
    bdc_line-dynbegin = 'X'.
  ENDMETHOD.


  METHOD in_program.
    bdc_line-program = program.
  ENDMETHOD.
ENDCLASS.
