*&---------------------------------------------------------------------*
*& Report  Z_BDC_EXAMPLE
*&
*&---------------------------------------------------------------------*
*& This example shows how to call a transaction using ZCL_BDC_SESSION.
*&
*&---------------------------------------------------------------------*

REPORT z_bdc_example.

INCLUDE bdcrecx1_s.
PARAMETERS p_thresh  TYPE i DEFAULT 100.  "threshold transactions per session
PARAMETERS pobj TYPE programm OBLIGATORY DEFAULT 'CL_SPFLI_PERSISTENT'.

CLASS demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS main.
ENDCLASS.

CLASS demo IMPLEMENTATION.
  METHOD main.
    DATA lv_bdc_selscrn TYPE zst_bdc_selscrn.
*    DATA(app_log) = zcl_log_factory=>create_log( object = 'ABAPUNIT'
*                                         subobject = 'LOGGER'
*                                          desc = 'SMARTFORMS Text Module' ).

    lv_bdc_selscrn-session = session.   " SM35 session
    lv_bdc_selscrn-group = group.       " session name
    lv_bdc_selscrn-user = user.         " User
    lv_bdc_selscrn-keep = keep.         " Keep session
    lv_bdc_selscrn-holddate = holddate. " Lock date
    lv_bdc_selscrn-e_group = e_group.   " Error sessn
    lv_bdc_selscrn-e_user = e_user.     " Error session User
    lv_bdc_selscrn-e_keep = e_keep.     " Keep Error session
    lv_bdc_selscrn-e_hdate = e_hdate.   " Error session lock date
    lv_bdc_selscrn-nodata = nodata.     " NO DATA
    lv_bdc_selscrn-thresh = p_thresh.   " Threshold transan per session

    DATA(bdc) = NEW zcl_bdc_run( lv_bdc_selscrn ).
*  i_session = session *  i_group = group *  i_user = user *  i_keep = keep *  i_holddate = e_hdate ).


    bdc->new_transaction( 'SE24' ).
    bdc->go_to_screen( '1000' )->in_program( 'SAPLSEOD' ).
    bdc->focus_on( 'SEOCLASS-CLSNAME' ).
    bdc->fill( 'SEOCLASS-CLSNAME' )->with( pobj ).
    bdc->set_okcode( '=WB_DISPLAY' ).

    bdc->run_bdc(
      EXPORTING
        screen      = ctumode "zcl_bdc_run=>screen_show_err_only
        screen_size = zcl_bdc_run=>size_current
*      terminate_at_commit = 'X'
     IMPORTING et_messages = DATA(li_messages) ).

*    app_log->add( li_messages ).

    CLEAR li_messages[].
    bdc->close_bdc( IMPORTING et_messages = li_messages ).
*    app_log->add( li_messages ).

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  demo=>main( ).

END-OF-SELECTION.

*  app_log->fullscreen( ).
