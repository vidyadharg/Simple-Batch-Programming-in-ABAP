class ZCL_BDC_RUN definition
  public
  final
  create public .

public section.

  types:
    t_group TYPE c LENGTH 12 .

  data MESSAGES type ZTT_BDC_MESSAGES read-only .
  constants SCREEN_SHOW type CTU_MODE value 'A' ##NO_TEXT.
  constants SCREEN_SHOW_ERR_ONLY type CTU_MODE value 'E' ##NO_TEXT.
  constants SCREEN_SHOW_BREAKPOINT type CTU_MODE value 'P' ##NO_TEXT.
  constants SCREEN_HIDE_ALL type CTU_MODE value 'N' ##NO_TEXT.
  constants SIZE_STANDARD type CTU_DEFSZE value 'X' ##NO_TEXT.
  constants SIZE_CURRENT type CTU_DEFSZE value ' ' ##NO_TEXT.
  data BDC_TRANSACTION type ref to ZCL_BDC_TRANSACTION read-only .

  methods CONSTRUCTOR
    importing
      !I_BDC_SELSCRN type ZST_BDC_SELSCRN .
  methods RUN_BDC
    importing
      !SCREEN type CTU_MODE default SCREEN_SHOW
      !SCREEN_SIZE type CTU_DEFSZE default SIZE_STANDARD
      !IS_BATCH_INPUT type ABAP_BOOL default ABAP_TRUE
      !IS_BATCH_INPUT_AT_END type ABAP_BOOL default ABAP_TRUE
      !TERMINATE_AT_COMMIT type ABAP_BOOL default ABAP_TRUE
    exporting
      !ET_MESSAGES type ZTT_BDC_MESSAGES .
  methods NEW_TRANSACTION
    importing
      !TCODE type CSEQUENCE .
  methods FILL
    importing
      !FIELD type CSEQUENCE
    returning
      value(FIELD_OBJ) type ref to ZCL_BDC_FIELD .
  methods GO_TO_SCREEN
    importing
      !SCREEN type N
    returning
      value(SCREEN_OBJ) type ref to ZCL_BDC_SCREEN .
  methods FOCUS_ON
    importing
      !FIELD type CSEQUENCE .
  methods SET_OKCODE
    importing
      !OKCODE type CSEQUENCE .
  methods SELECT
    importing
      !FIELD type CSEQUENCE .
  methods DESELECT
    importing
      !FIELD type CSEQUENCE .
  methods CLOSE_BDC
    exporting
      !ET_MESSAGES type ZTT_BDC_MESSAGES .
  PROTECTED SECTION.
private section.

  data:
    t_bdc_transactions TYPE STANDARD TABLE OF REF TO zcl_bdc_transaction .
  data BDC_SELSCRN type ZST_BDC_SELSCRN .
  data GR_STRING type STRING .
  data GV_GROUP_OPENED type XFELD .    "error session opened (' ' or 'X')
  data GV_SESSION_CNT type I .
  data GV_THRESH type I .

  methods GET_SESSION_NAME
    importing
      !I_ADD_CNT type XFLD default ''
    returning
      value(RV_GROUP) type APQI-GROUPID .
  methods RUN_SESSION
    importing
      !I_GROUPID type APQI-GROUPID .
  methods OPEN_SESSION .
  methods INSERT_SESSION .
  methods CLOSE_SESSION
    exporting
      !ET_MESSAGES type ZTT_BDC_MESSAGES .
  methods RUN_TRANSACTION
    importing
      !SCREEN type CTU_MODE default SCREEN_SHOW
      !SCREEN_SIZE type CTU_DEFSZE default SIZE_STANDARD
      !IS_BATCH_INPUT type ABAP_BOOL default ABAP_TRUE
      !IS_BATCH_INPUT_AT_END type ABAP_BOOL default ABAP_TRUE
      !TERMINATE_AT_COMMIT type ABAP_BOOL default ABAP_TRUE .
  methods ADD_MSG
    importing
      !IT_MESSAGES type ZTT_BDC_MESSAGES optional .
ENDCLASS.



CLASS ZCL_BDC_RUN IMPLEMENTATION.


  METHOD add_msg.
    DATA :
      li_messages TYPE ZTT_BDC_MESSAGES,
      lw_bdc_messages TYPE bdcmsgcoll.

    IF it_messages IS SUPPLIED.
*      APPEND LINES OF it_messages TO messages.
       bdc_transaction->SET_MESSAGES( it_messages ).
    ELSE.
      IF sy-msgid IS NOT INITIAL AND sy-msgno IS  NOT INITIAL.

        lw_bdc_messages-msgtyp = sy-msgty.
*lw_bdc_messages-MSGSPRA =
        lw_bdc_messages-msgid = sy-msgid.
        lw_bdc_messages-msgnr = sy-msgno.
        lw_bdc_messages-msgv1 = sy-msgv1.
        lw_bdc_messages-msgv2 = sy-msgv2.
        lw_bdc_messages-msgv3 = sy-msgv3.
        lw_bdc_messages-msgv4 = sy-msgv4.
        APPEND lw_bdc_messages TO li_messages.
        bdc_transaction->SET_MESSAGES( li_messages ).
        APPEND LINES OF li_messages to messages.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD close_bdc.
    IF bdc_selscrn-session EQ 'X'.
      close_session( IMPORTING ET_MESSAGES = ET_MESSAGES ).
    ELSE.
      IF gv_group_opened = 'X'.
        CALL FUNCTION 'BDC_CLOSE_GROUP'.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD close_session.

    DATA(lv_group) = get_session_name( ).
*   close batchinput group
    CALL FUNCTION 'BDC_CLOSE_GROUP'.

    IF sy-subrc EQ 0.
      MESSAGE s004(zbdc) WITH lv_group sy-subrc INTO gr_string. add_msg( ).
    ELSE.
      IF sy-msgty IS NOT INITIAL.
        add_msg( ).
      ENDIF.
      MESSAGE e004(zbdc) WITH lv_group  sy-subrc INTO gr_string. add_msg( ).
    ENDIF.

      run_session( lv_group ).

      APPEND LINES OF messages to et_messages.

  ENDMETHOD.


  METHOD constructor.

    bdc_selscrn = i_bdc_selscrn.

  ENDMETHOD.


  METHOD deselect.
    fill( field )->with( space ).
  ENDMETHOD.


  METHOD fill.
    CREATE OBJECT field_obj EXPORTING field = field.
    APPEND field_obj TO bdc_transaction->operations.
  ENDMETHOD.


  METHOD focus_on.
    fill( 'BDC_CURSOR' )->with( field ).
  ENDMETHOD.


  METHOD get_session_name.

    IF i_add_cnt IS SUPPLIED.
      ADD 1 TO gv_session_cnt.
    ENDIF.

    rv_group =  bdc_selscrn-group && '_' && gv_session_cnt.
  ENDMETHOD.


  METHOD go_to_screen.
    CREATE OBJECT screen_obj EXPORTING screen = screen.
    APPEND screen_obj TO bdc_transaction->operations.
  ENDMETHOD.


  METHOD insert_session.

    DATA: bdc_tab TYPE TABLE OF bdcdata,
          bdc_op  TYPE REF TO zif_bdc_operation.

    LOOP AT bdc_transaction->operations INTO bdc_op.
      APPEND bdc_op->bdc_line TO bdc_tab.
    ENDLOOP.

    CALL FUNCTION 'BDC_INSERT'
      EXPORTING
        tcode     = bdc_transaction->tcode
      TABLES
        dynprotab = bdc_tab.
    IF sy-subrc EQ 0.
      MESSAGE s003(zbdc) WITH sy-subrc sy-index INTO gr_string. add_msg( ).
    ELSE.
      IF sy-msgty IS NOT INITIAL.
        add_msg( ).
      ENDIF.
      MESSAGE e003(zbdc) WITH sy-subrc sy-index INTO gr_string. add_msg( ).
    ENDIF.
  ENDMETHOD.


  METHOD new_transaction.
    bdc_transaction = zcl_bdc_transaction=>new_transaction( tcode ).
    APPEND bdc_transaction TO t_bdc_transactions.
  ENDMETHOD.


  METHOD open_session.

    DATA(lv_group) = get_session_name( 'X' ). "'X' - increment counter

    MESSAGE s001(zbdc) WITH lv_group INTO gr_string. add_msg( ).

*   Open batchinput group
    CALL FUNCTION 'BDC_OPEN_GROUP'
      EXPORTING
        client   = sy-mandt
        group    = lv_group
        user     = bdc_selscrn-user
        keep     = bdc_selscrn-keep
        holddate = bdc_selscrn-holddate.
    IF sy-subrc EQ 0.
      MESSAGE s002(zbdc) WITH sy-subrc INTO gr_string. add_msg( ).
    ELSE.
      IF sy-msgty IS NOT INITIAL.
        add_msg( ).
      ENDIF.
      MESSAGE e002(zbdc) WITH sy-subrc INTO gr_string. add_msg( ).
    ENDIF.

  ENDMETHOD.


  METHOD run_bdc.

    CHECK bdc_transaction IS NOT INITIAL.

    IF bdc_selscrn-session EQ 'X'.
      IF gv_thresh EQ 0.
        open_session( ).
      ENDIF.

      IF gv_thresh GE bdc_selscrn-thresh AND bdc_selscrn-thresh IS NOT INITIAL.
        CLEAR gv_thresh. "reset transaction counter
        close_session( ). "close last session
        open_session( ).  "open new session
      ENDIF.
      ADD 1 TO gv_thresh.
      insert_session( ).

    ELSE.

      run_transaction(
        EXPORTING
          screen = screen "ZCL_BDC_TRANSACTION=>screen_show_err_only
          screen_size = screen_size
          is_batch_input = is_batch_input
          is_batch_input_at_end = is_batch_input_at_end
          terminate_at_commit = terminate_at_commit ).

    ENDIF.

    et_messages[] = bdc_transaction->messages[].

*    DATA :
*      lv_thresh TYPE i.
*
*    IF bdc_selscrn-session EQ 'X'.
*      open_session( ).
*
*      LOOP AT t_bdc_transactions INTO bdc_transaction.
*        IF lv_thresh GE bdc_selscrn-thresh AND bdc_selscrn-thresh IS NOT INITIAL.
*          CLEAR lv_thresh. "reset transaction counter
*          close_session( ). "close last session
*          open_session( ).  "open new session
*        ENDIF.
*        ADD 1 TO lv_thresh.
*        insert_session( ).
*      ENDLOOP.
*
*      close_session( ).
*      run_session( ).
*    ELSE.
*      LOOP AT t_bdc_transactions INTO bdc_transaction.
*
*        run_transaction(
*          EXPORTING
*            screen = screen "ZCL_BDC_TRANSACTION=>screen_show_err_only
*            screen_size = screen_size
*            is_batch_input = is_batch_input
*            is_batch_input_at_end = is_batch_input_at_end
*            terminate_at_commit = terminate_at_commit ).
*
*      ENDLOOP.
*      IF gv_group_opened = 'X'.
*        close_session( ).
*      ENDIF.
*    ENDIF.
  ENDMETHOD.


  METHOD run_session.

    IF bdc_selscrn-sub_sess EQ 'X'.
      SUBMIT rsbdcsub AND RETURN
        WITH mappe = i_groupid   "Session
        WITH z_verarb = 'X'              "New
        WITH von      =  sy-datum        "From date
        WITH bis      =  sy-datum        "To date
        WITH fehler = ' '                "Incorrect
        WITH logall = 'X'.               "Extended log
      MESSAGE s005(zbdc) WITH i_groupid INTO gr_string. add_msg( ).
    ENDIF.
  ENDMETHOD.


  METHOD run_transaction.
    DATA :
      l_subrc           LIKE sy-subrc,
      li_messages       TYPE ztt_bdc_messages,
      options           TYPE ctu_params,
      l_auth_check_text TYPE string,
      lx_auth_check     TYPE REF TO cx_root,
      bdc_tab           TYPE TABLE OF bdcdata,
      bdc_op            TYPE REF TO zif_bdc_operation.

    CHECK bdc_transaction IS NOT INITIAL.

    options-dismode = screen.
    options-updmode = 'S'. "Synchronous Update
    options-defsize = screen_size.
    options-racommit = boolc( terminate_at_commit = abap_false ).
    options-nobinpt = boolc( is_batch_input = abap_false ).
    options-nobiend = boolc( is_batch_input_at_end = abap_false ).


    LOOP AT bdc_transaction->operations INTO bdc_op.
      IF bdc_op->bdc_line-fval EQ bdc_selscrn-nodata AND "NODATA indicator
         bdc_op->bdc_line-dynbegin NE 'X'.
      ELSE.
        APPEND bdc_op->bdc_line TO bdc_tab.
      ENDIF.
    ENDLOOP.

    REFRESH li_messages.
    TRY.
* call transaction using
        CALL TRANSACTION bdc_transaction->tcode WITH AUTHORITY-CHECK USING bdc_tab
              OPTIONS FROM options
              MESSAGES INTO li_messages.

      CATCH cx_sy_authorization_error INTO lx_auth_check.
*     Authorization missing for user when executing transaction
        l_auth_check_text = lx_auth_check->get_text( ).
        MESSAGE e000(zbdc) WITH bdc_transaction->tcode l_auth_check_text INTO gr_string. add_msg( ).
        sy-subrc = 99.
    ENDTRY.
    l_subrc = sy-subrc.

    IF sy-subrc = 1001.
      "Processing Error.
      IF sy-msgty = 'S' AND sy-msgid = '00' AND sy-msgno = '344' AND
         sy-msgv1 = 'SAPMSSY3' AND sy-msgv2 = '0131'.
        "Breakpoint reached and 'hide screens' specified.
      ENDIF.

      IF l_subrc <> 0 AND l_subrc <> 99 AND bdc_selscrn-e_group <> space.
        IF gv_group_opened = ' '.
          CALL FUNCTION 'BDC_OPEN_GROUP'
            EXPORTING
              client   = sy-mandt
              group    = bdc_selscrn-e_group
              user     = bdc_selscrn-e_user
              keep     = bdc_selscrn-e_keep
              holddate = bdc_selscrn-e_hdate.
          gv_group_opened = 'X'.
        ENDIF.

        CALL FUNCTION 'BDC_INSERT'
          EXPORTING
            tcode     = bdc_transaction->tcode
          TABLES
            dynprotab = bdc_tab.

      ENDIF.
    ENDIF.

    add_msg( li_messages ).
*    bdc_transaction->SET_MESSAGES( li_messages ).

  ENDMETHOD.


  METHOD select.
    fill( field )->with( 'X' ).
  ENDMETHOD.


  METHOD set_okcode.
    fill( 'BDC_OKCODE' )->with( okcode ).
  ENDMETHOD.
ENDCLASS.
