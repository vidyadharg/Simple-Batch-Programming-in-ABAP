class ZCL_BDC_TRANSACTION definition
  public
  final
  create private

  global friends ZCL_BDC_RUN .

public section.
  PROTECTED SECTION.
private section.

  data TCODE type TCODE .
  data OPERATIONS type ZTT_BDC_OPERATIONS .
  data OPTIONS type CTU_PARAMS .
  data MESSAGES type ZTT_BDC_MESSAGES .

  class-methods NEW_TRANSACTION
    importing
      !TCODE type CSEQUENCE
    returning
      value(BDC) type ref to ZCL_BDC_TRANSACTION .
  methods SET_MESSAGES
    importing
      !IT_MESSAGES type ZTT_BDC_MESSAGES .
*  data SESSION type BINPT value '' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_BDC_TRANSACTION IMPLEMENTATION.


  METHOD NEW_TRANSACTION.
    CREATE OBJECT bdc.

    bdc->tcode = tcode.
    bdc->options-updmode = 'S'. "Synchronous update (mandatory for BDC)
  ENDMETHOD.


  METHOD set_messages.
    APPEND LINES OF it_messages TO messages.
  ENDMETHOD.
ENDCLASS.
