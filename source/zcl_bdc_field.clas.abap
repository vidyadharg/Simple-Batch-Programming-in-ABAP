class ZCL_BDC_FIELD definition
  public
  final
  create public .

public section.

  interfaces ZIF_BDC_OPERATION .

  aliases BDC_LINE
    for ZIF_BDC_OPERATION~BDC_LINE .

  methods CONSTRUCTOR
    importing
      !FIELD type CSEQUENCE .
  methods WITH
    importing
      !VALUE type CSEQUENCE .
  methods ROW
    importing
      !ROW_NUM type N
    returning
      value(SELF) type ref to ZCL_BDC_FIELD .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_BDC_FIELD IMPLEMENTATION.


  METHOD constructor.
    bdc_line-fnam = field.
  ENDMETHOD.


  METHOD row.
    bdc_line-fnam = bdc_line-fnam && row_num.
  ENDMETHOD.


  METHOD with.
    bdc_line-fval = value.
  ENDMETHOD.
ENDCLASS.
