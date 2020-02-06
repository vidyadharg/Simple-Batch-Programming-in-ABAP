*&---------------------------------------------------------------------*
*& Report ZBDCREC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbdcrec.
TABLES: sscrfields.

TYPES :
  BEGIN OF gt_outtab,
    groupid  TYPE apqi-groupid,
    creator  TYPE apqi-creator,
    credate  TYPE apqi-credate,
    cretime  TYPE apqi-cretime,
    transcnt TYPE apqi-transcnt,
    msgcnt   TYPE apqi-msgcnt,
    qid      TYPE apq_quid,
  END OF gt_outtab,
  gtt_outtab TYPE  STANDARD TABLE OF gt_outtab.

TYPES :
  gt_source  TYPE tdline, "string,
  gtt_source TYPE STANDARD TABLE OF gt_source.

DATA :
  gi_source     TYPE gtt_source,
  gr_table      TYPE REF TO cl_salv_table,
  doc_container TYPE REF TO cl_gui_docking_container,
  go_textedit   TYPE REF TO cl_gui_textedit,
  gv_tabnam_old TYPE  dd03m-tabname,
  gi_outtab     TYPE gtt_outtab.
* local class to handle semantic checks
CLASS lcl_event_receiver DEFINITION DEFERRED.

DATA: g_event_receiver TYPE REF TO lcl_event_receiver.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION .  "for event handling

  PUBLIC SECTION.
    METHODS:

      on_double_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column,

      on_link_click FOR EVENT link_click OF cl_salv_events_table
        IMPORTING row column.

ENDCLASS.                    "lcl_event_receiver DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD on_double_click.
    PERFORM generate_code USING row.
  ENDMETHOD.                    "on_double_click

  METHOD on_link_click.
    PERFORM generate_code USING row.
  ENDMETHOD.                    "on_single_click

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(13) TEXT-002.
PARAMETERS: p_grpid LIKE d0100-mapn DEFAULT '*'.

SELECTION-SCREEN COMMENT 29(3) TEXT-003.
PARAMETERS: pfrdt TYPE d0100-von.
SELECTION-SCREEN COMMENT 45(4) TEXT-004.
PARAMETERS: ptodt TYPE d0100-bis.

SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN COMMENT 1(13) TEXT-005.
PARAMETERS: pcrea TYPE d0100-creator DEFAULT '*'.
SELECTION-SCREEN END OF LINE.

*SELECTION-SCREEN PUSHBUTTON /10(20) TEXT-002 USER-COMMAND cl1.

SELECTION-SCREEN END OF BLOCK b1.


AT SELECTION-SCREEN." ON s_sel.

  CASE sscrfields.
    WHEN 'CL1'.
*      PERFORM generate_code USING lv_row.
    WHEN ''.
      PERFORM record_select USING p_grpid
                                  pfrdt
                                  ptodt
                                  pcrea.

      PERFORM generate_code USING 1.

      gr_table->refresh( ).
  ENDCASE.

INITIALIZATION.
  pfrdt = sy-datum - 90.
  ptodt = sy-datum.

  PERFORM record_select USING p_grpid
                              pfrdt
                              ptodt
                              pcrea.
  PERFORM f_init.


START-OF-SELECTION.
*  PERFORM generate_code.


FORM record_select USING    p_groupid
                            p_von
                            p_bis
                            p_creator.

  DATA:
    li_apqi   TYPE STANDARD TABLE OF apqi,
    lw_apqi   TYPE apqi,
    lw_outtab TYPE gt_outtab.

  IF p_groupid = space.
    p_groupid = '*'.
  ENDIF.
  IF p_creator = space.
    p_creator = '*'.
  ENDIF.
  CALL FUNCTION 'BDC_OBJECT_SELECT'
    EXPORTING
      name            = p_groupid
      date_from       = p_von
      date_to         = p_bis
      session_creator = p_creator
    TABLES
      apqitab         = li_apqi
    EXCEPTIONS
      OTHERS          = 1.
  IF sy-subrc <> 0.
    MESSAGE a604(ms) WITH 'BDC_OBJECT_SELECT' sy-subrc.
  ELSE.
    CLEAR gi_outtab.
    LOOP AT li_apqi INTO lw_apqi.
      lw_outtab-groupid = lw_apqi-groupid.
      lw_outtab-creator = lw_apqi-creator.
      lw_outtab-credate = lw_apqi-credate.
      lw_outtab-cretime = lw_apqi-cretime.
      lw_outtab-transcnt = lw_apqi-transcnt.
      lw_outtab-msgcnt = lw_apqi-msgcnt.
      lw_outtab-qid = lw_apqi-qid.
      APPEND lw_outtab TO gi_outtab.
    ENDLOOP.
  ENDIF.
ENDFORM.


FORM f_init.
  DATA :
    lw_source  TYPE gt_source.

  IF doc_container IS INITIAL.

    CREATE OBJECT doc_container
      EXPORTING
        repid     = sy-repid
        dynnr     = sy-dynnr
        side      = doc_container->dock_at_right
        extension = 650.

    IF go_textedit  IS INITIAL.
      CREATE OBJECT go_textedit
        EXPORTING
          wordwrap_mode          = cl_gui_textedit=>wordwrap_at_fixed_position
          wordwrap_position      = 132
          parent                 = doc_container
        EXCEPTIONS
          error_cntl_create      = 1
          error_cntl_init        = 2
          error_cntl_link        = 3
          error_dp_create        = 4
          gui_type_not_supported = 5
          OTHERS                 = 6.
      IF sy-subrc <> 0.
        MESSAGE e010(ad) WITH 'Error setting up screen'.
      ENDIF.

      lw_source = 'Source code:'.
      APPEND lw_source TO gi_source.

      go_textedit->set_toolbar_mode( toolbar_mode = 0 ).
      go_textedit->set_statusbar_mode( statusbar_mode = 0 ).
      go_textedit->set_readonly_mode( readonly_mode = 1 ).
      go_textedit->set_text_as_r3table( table = gi_source ).
    ENDIF.

***alv
*  repid = sy-repid.
*  variant-report = sy-repid.
*  variant-username = sy-uname.
*  layout-zebra =
*  layout-EDIT =
*    layout-edit_mode =
*    layout-no_toolbar  = layout-cwidth_opt = 'X'.
*    layout-stylefname = 'CELLTAB'.
*    PERFORM fill_celltab.

    CREATE OBJECT doc_container
      EXPORTING
        repid     = sy-repid
        dynnr     = sy-dynnr
        side      = doc_container->dock_at_bottom "dock_at_right
        extension = 185.

*... �2 create an ALV table
    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container    = doc_container
            container_name = 'CONTAINER'
          IMPORTING
            r_salv_table   = gr_table
          CHANGING
            t_table        = gi_outtab ).
      CATCH cx_salv_msg.                                "#EC NO_HANDLER
    ENDTRY.

    DATA: gr_display   TYPE REF TO cl_salv_display_settings.
    gr_display = gr_table->get_display_settings( ).
    gr_display->set_list_header( 'SHDB Recordings' ).

*... set the columns technical
    DATA: lr_columns TYPE REF TO cl_salv_columns,
          lr_column  TYPE REF TO cl_salv_column_table.

    lr_columns = gr_table->get_columns( ).
    lr_columns->set_optimize( 'X' ).

*... §4 set hotspot column
    TRY.
        lr_column ?= lr_columns->get_column( 'GROUPID' ).
        lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
*      lr_column->set_icon( if_salv_c_bool_sap=>true ).
*      lr_column->set_long_text( 'HOTSPOT' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

*... §6 register to the events of cl_salv_table
    DATA: lr_events TYPE REF TO cl_salv_events_table.

    lr_events = gr_table->get_event( ).

    CREATE OBJECT g_event_receiver .

*... §6.2 register to the event DOUBLE_CLICK
    SET HANDLER g_event_receiver->on_double_click FOR lr_events.
*... §6.3 register to the event LINK_CLICK
    SET HANDLER g_event_receiver->on_link_click FOR lr_events.

*... �7 selections
    DATA: lr_selections TYPE REF TO cl_salv_selections,
          lt_rows       TYPE salv_t_row.

    lr_selections = gr_table->get_selections( ).

*... �7.1 set selection mode
*    lr_selections->set_selection_mode( if_salv_c_selection_mode=>single ).
    lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

*... §7.3 set selected rows.
    APPEND 1 TO lt_rows.
    lr_selections->set_selected_rows( lt_rows ).

    PERFORM generate_code USING 1.

*... �8 display the table
    gr_table->display( ).

  ENDIF.

ENDFORM.


FORM get_selections
  CHANGING pc_row   TYPE i.

  DATA: lr_selections TYPE REF TO cl_salv_selections.
  DATA: lt_rows   TYPE salv_t_row.

  gr_table->refresh( ).

  lr_selections = gr_table->get_selections( ).
  lt_rows = lr_selections->get_selected_rows( ).

*... Zeile
  pc_row = VALUE #( lt_rows[ 1 ] DEFAULT 1 ).

ENDFORM.                    " get_selections


FORM generate_code USING
      pc_row    TYPE i.
  DATA :
    lw_source TYPE gt_source,

    lw_outtab TYPE gt_outtab.

  DATA:
    li_dynprotab    TYPE STANDARD TABLE OF bdcdata,
    li_DYNPRO_FIELDS TYPE STANDARD TABLE OF BDCDF,
    lw_DYNPRO_FIELDS type BDCDF,
    lw_dynprotab TYPE bdcdata,
    lv_DYNPRO_FIELDS_INDEX TYPE i,
    tcode        LIKE tstc-tcode.

  DATA:
    lv_quid TYPE apq_quid.

  DEFINE append_cod.
    APPEND &1 TO gi_source.
    CLEAR lw_source.
  END-OF-DEFINITION.

  CLEAR gi_source[].

  lw_outtab = VALUE gt_outtab( gi_outtab[ pc_row ] DEFAULT lw_outtab ).

* get records *********************************************************
  CALL FUNCTION 'BDC_OBJECT_READ'
    EXPORTING
      queue_id         = lw_outtab-qid
    TABLES
      dynprotab        = li_dynprotab
    EXCEPTIONS
      not_found        = 1
      system_failure   = 2
      invalid_datatype = 3
      OTHERS           = 4.
  IF sy-subrc >< 0.
    MESSAGE s627(ms) WITH lv_quid. EXIT.
  ENDIF.


  CALL FUNCTION 'BDC_DYNPROTAB_GET_FIELDS'
       TABLES
            DYNPROTAB    = li_dynprotab
            DYNPROFIELDS = li_DYNPRO_FIELDS.


* same lines for all records ------------------------------------------
* ***report <report>
  append_cod 'Report zreportname'.
  append_cod '       no standard page heading line-size 255.'.
  append_cod ''.
  append_cod '* Include bdcrecx1_s:'.
  append_cod '* The call transaction using is called WITH AUTHORITY-CHECK!'.
  append_cod '* If you have own auth.-checks you can use include bdcrecx1 instead.'.

* ***include bdcrecxx.
* ***include bdcrecx1.    "since release 4.5
* ***include bdcrecx1_s.  "since release 7.50 SP 09 and 7.51 SP 04
  append_cod 'include bdcrecx1_s.'.

PERFORM source_file_line TABLES li_DYNPROTAB li_DYNPRO_FIELDS.

append_cod ''.
append_cod 'SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-T01.'.
append_cod 'PARAMETERS:'.
append_cod '  pfile TYPE string OBLIGATORY,'.
append_cod '  p_thresh  TYPE i DEFAULT 100.  "threshold # trans per session'.
append_cod 'SELECTION-SCREEN END OF BLOCK b1.'.
append_cod ''.
append_cod 'AT SELECTION-SCREEN ON VALUE-REQUEST FOR pfile.'.
append_cod '*file_open_dialog'.
append_cod '*cl_gui_frontend_services=>file_open_dialog( ).'.
append_cod ''.

  append_cod  'INITIALIZATION.'.
  append_cod ''.
  append_cod  '*  DATA(app_log) = zcl_log_factory=>create_log( object = ''ZFICO'''.
  append_cod  '*                                               subobject = '''''.
  append_cod  '*                                               desc = '''' )'.
* ***start-of-selection.
  append_cod 'START-OF-SELECTION.'.
  append_cod ''.
  append_cod '*  PERFORM  UPLOAD_FILE.'.
  append_cod ''.
  append_cod '  PERFORM  run_bdc.'.
  append_cod ''.
  append_cod 'END-OF-SELECTION.'.
  append_cod ''.
  append_cod '*  app_log->fullscreen( ).'.
  append_cod ''.
  append_cod 'FORM  run_bdc.'.
  append_cod '  DATA :'.
  append_cod '    lv_bdc_selscrn TYPE zst_bdc_selscrn,'.
  append_cod '    lw_file        TYPE gt_file.'.
  append_cod ''.
  append_cod '    lv_bdc_selscrn-session = session.   " SM35 session'.
  append_cod '    lv_bdc_selscrn-group = group.       " session name'.
  append_cod '    lv_bdc_selscrn-user = user.         " User '.
  append_cod '    lv_bdc_selscrn-keep = keep.         " Keep session'.
  append_cod '    lv_bdc_selscrn-holddate = holddate. " Lock date'.
  append_cod '    lv_bdc_selscrn-e_group = e_group.   " Error sessn'.
  append_cod '    lv_bdc_selscrn-e_user = e_user.     " Error session User'.
  append_cod '    lv_bdc_selscrn-e_keep = e_keep.     " Keep Error session'.
  append_cod '    lv_bdc_selscrn-e_hdate = e_hdate.   " Error session lock date'.
  append_cod '    lv_bdc_selscrn-nodata = nodata.     " NO DATA '.
  append_cod '    lv_bdc_selscrn-thresh = p_thresh.   " Threshold transan per session'.
  append_cod ''.
  append_cod '    bdc = NEW zcl_bdc_run( i_bdc_selscrn = lv_bdc_selscrn ).'.
  append_cod ''.
  append_cod '    LOOP AT gi_file INTO lw_file.'.
  append_cod '      PERFORM f_bdc USING lw_file.'.
  append_cod ''.
  append_cod '      bdc->run_bdc('.
  append_cod '          EXPORTING'.
  append_cod '            screen      = ctumode "zcl_bdc_run=>screen_show_err_only'.
  append_cod '            screen_size = zcl_bdc_run=>size_current'.
  append_cod '*          terminate_at_commit = ''X'' ).'.
  append_cod '          IMPORTING et_messages = data(li_messages) ).'.
  append_cod ''.
  append_cod '*      app_log->add( li_messages ).'.
  append_cod ''.
  append_cod '    ENDLOOP.'.
  append_cod ''.
  append_cod '  CLEAR li_messages[].'.
  append_cod '  bdc->close_bdc( IMPORTING et_messages = li_messages ).'.
  append_cod '*  app_log->add( li_messages ).'.
  append_cod ''.
  append_cod 'ENDFORM.  "run_bdc'.
  append_cod ''.
  append_cod 'FORM f_bdc USING pw_file TYPE gt_file.'.

  LOOP AT li_dynprotab INTO lw_dynprotab.
    CASE lw_dynprotab-dynbegin.
*     new transaction -------------------------------------------------
      WHEN 'T'.
*       save tcode for next transaction
        tcode = lw_dynprotab-fnam.
        IF NOT tcode IS INITIAL.
          append_cod ''.
*         ***perform bdc_transaction using dynprotab-fnam.
          lw_source = '  bdc->new_transaction(''' && tcode && ''').' .
          append_cod lw_source.
        ENDIF.
*     new dynpro ------------------------------------------------------
      WHEN 'X'.
*       ***bdc->go_to_screen( '0100' )->in_program( 'SAPMSSFO' ).
        append_cod ''.
        lw_source =
        |  bdc->go_to_screen( '| &&
        lw_dynprotab-dynpro &&
        |' )->in_program( '| &&
        lw_dynprotab-program &&
        |' ).|.
        append_cod lw_source.
*     dynpro field ----------------------------------------------------
      WHEN space.
*       ***perform bdc_field using <dynprotab-fnam> <dynprotab-fval>.
        CHECK lw_dynprotab-fnam <> 'BDC_SUBSCR'.

        CASE lw_dynprotab-fnam.
          WHEN 'BDC_CURSOR'.
*bdc->focus_on( 'RB_TX' ).
            lw_source =
            |  bdc->focus_on( '| &&
            lw_dynprotab-fval &&
            |' ).|.
            append_cod lw_source.

          WHEN 'BDC_OKCODE'.
*bdc->set_okcode( '=RB' ).
            lw_source =
            |  bdc->set_okcode( '| &&
            lw_dynprotab-fval &&
            |' ).|.
            append_cod lw_source.

          WHEN OTHERS.

    ADD 1 TO lv_DYNPRO_FIELDS_INDEX.
    READ TABLE li_DYNPRO_FIELDS INDEX lv_DYNPRO_FIELDS_INDEX INTO lw_DYNPRO_FIELDS.

*bdc->fill( 'SSFSCREEN-TNAME' )->with( stnam-low ).
            lw_source =
            |  bdc->fill( '| &&
            lw_dynprotab-fnam &&
            |' )->with( pw_file-| &&
            lw_DYNPRO_FIELDS-RECFIELD &&
            | ). "| &&
            lw_dynprotab-fval.
            append_cod lw_source.
        ENDCASE.
    ENDCASE.
  ENDLOOP.

  append_cod ''.
  append_cod 'ENDFORM.  "f_bdc'.

  go_textedit->set_text_as_r3table( table = gi_source ).

ENDFORM.


FORM source_file_line
  TABLES DYNPROTAB DYNPRO_FIELDS.


DATA:
  lw_DYNPRO_FIELDS TYPE BDCDF,
  lw_source type gt_source,

  L_DFIES     LIKE DFIES,
  L_TABNAME   LIKE DCOBJDEF-NAME,
  L_FIELDNAME LIKE DFIES-LFIELDNAME,
  L_DUMMY     LIKE DFIES-LFIELDNAME.

  append_cod ''.
  append_cod 'TYPES:'.
  append_cod '  BEGIN OF gt_file,'.


  LOOP AT DYNPRO_FIELDS INTO lw_DYNPRO_FIELDS.
*   *** <field_n>(<length>)
    CLEAR L_DFIES.
    IF lw_DYNPRO_FIELDS-FIELDNAME CA '-'.
*     create dataelement comment line
      SPLIT lw_DYNPRO_FIELDS-FIELDNAME AT '-'
            INTO L_TABNAME
                 L_FIELDNAME.
      SPLIT L_FIELDNAME AT '('
            INTO L_FIELDNAME
                 L_DUMMY.
      CALL FUNCTION 'DDIF_FIELDINFO_GET'
           EXPORTING
                TABNAME        = L_TABNAME
*               fieldname      = l_fieldname
*               LANGU          = SY-LANGU
                LFIELDNAME     = L_FIELDNAME
*               ALL_TYPES      = ' '
           IMPORTING
*               X030L_WA       =
*               DDOBJTYPE      =
                DFIES_WA       = L_DFIES
*          TABLES
*               DFIES_TAB      =
           EXCEPTIONS
                NOT_FOUND      = 1
                INTERNAL_ERROR = 2
                OTHERS         = 3.
      IF SY-SUBRC <> 0.
        CLEAR L_DFIES.
      ENDIF.
    ENDIF.

*    CONCATENATE '    ' lw_DYNPRO_FIELDS-RECFIELD
*                '(' lw_DYNPRO_FIELDS-LENGTH ')' ', "'
*                L_DFIES-SCRTEXT_L
*                ' * data element: ' L_DFIES-ROLLNAME
*                INTO lw_source.

    lw_source = |    | && lw_DYNPRO_FIELDS-RECFIELD &&
    |(| && lw_DYNPRO_FIELDS-LENGTH && |), "|
     && L_DFIES-SCRTEXT_L &&
     | " data element: |  && L_DFIES-ROLLNAME.

    APPEND_cod lw_source.
  ENDLOOP.

* *** end   of record.

  append_cod '  END OF gt_file,'.
  append_cod '  gtt_file TYPE STANDARD TABLE OF gt_file.'.
  append_cod ''.
  append_cod 'DATA :'.
  append_cod '  bdc     TYPE REF TO zcl_bdc_run,'.
  append_cod '  gi_file TYPE gtt_file.'.
  append_cod ''.
endform.
