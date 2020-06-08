    CLASS-METHODS popup
      IMPORTING
        iv_repo_url TYPE string
      CHANGING
        cv_user     TYPE string
        cv_pass     TYPE string.

    CLASS-METHODS on_screen_init.
    CLASS-METHODS on_screen_output.
    CLASS-METHODS on_screen_event
      IMPORTING
        iv_ucomm TYPE sy-ucomm.

  PRIVATE SECTION.
    CLASS-DATA gv_confirm TYPE abap_bool.
    CLASS-METHODS enrich_title_by_hostname
      IMPORTING
        iv_repo_url TYPE string.

ENDCLASS.

CLASS lcl_password_dialog IMPLEMENTATION.

  METHOD popup.

    CLEAR p_pass.
    p_url      = iv_repo_url.
    p_user     = cv_user.
    gv_confirm = abap_false.
    enrich_title_by_hostname( iv_repo_url ).

    CALL SELECTION-SCREEN c_dynnr STARTING AT 5 5 ENDING AT 60 8.

    IF gv_confirm = abap_true.
      cv_user = p_user.
      cv_pass = p_pass.
    ELSE.
      CLEAR: cv_user, cv_pass.
    ENDIF.

    CLEAR: p_url, p_user, p_pass.

  ENDMETHOD.

  METHOD on_screen_init.
    s_title = 'Login'     ##NO_TEXT.
    s_url   = 'Repo URL'  ##NO_TEXT.
    s_user  = 'User'      ##NO_TEXT.
    s_pass  = 'Password'  ##NO_TEXT.
  ENDMETHOD.

  METHOD on_screen_output.
    DATA lt_ucomm TYPE TABLE OF sy-ucomm.

    ASSERT sy-dynnr = c_dynnr.

    LOOP AT SCREEN.
      IF screen-name = 'P_URL'.
        screen-input       = '0'.
        screen-intensified = '1'.
        screen-display_3d  = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name = 'P_PASS'.
        screen-invisible   = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

    " Program RSSYSTDB, GUI Status %_CSP
    PERFORM set_pf_status IN PROGRAM rsdbrunt IF FOUND.
    APPEND 'NONE' TO lt_ucomm.  "Button Check
    APPEND 'SPOS' TO lt_ucomm.  "Save as Variant

    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = sy-pfkey
      TABLES
        p_exclude = lt_ucomm.

    IF p_user IS NOT INITIAL.
      SET CURSOR FIELD 'P_PASS'.
    ENDIF.

  ENDMETHOD.

  METHOD on_screen_event.
    ASSERT sy-dynnr = c_dynnr.

    " CRET   - F8
    " OTHERS - simulate Enter press
    CASE iv_ucomm.
      WHEN 'CRET'.
        gv_confirm = abap_true.
      WHEN OTHERS. "TODO REFACTOR !!! A CLUTCH !
        " This will work unless any new specific logic appear
        " for other commands. The problem is that the password dialog
        " does not have Enter event (or I don't know how to activate it ;)
        " so Enter issues previous command from previous screen
        " But for now this works :) Fortunately Esc produces another flow
        gv_confirm = abap_true.
        LEAVE TO SCREEN 0.
    ENDCASE.

  ENDMETHOD.
  METHOD enrich_title_by_hostname.

    DATA lv_host TYPE string.

    FIND REGEX 'https?://([^/^:]*)' IN iv_repo_url SUBMATCHES lv_host.
    IF lv_host IS NOT INITIAL AND lv_host <> space.
      CLEAR s_title.
      CONCATENATE 'Login:' lv_host INTO s_title IN CHARACTER MODE SEPARATED BY space.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
* create class ZCL_ABAPGIT_AUTH_EXIT implementing ZIF_ABAPGIT_AUTH in following include,
* if using the development version of abapGit create a global class instead
* place the object in a different package than ZABAPGIT
INCLUDE zabapgit_authorizations_exit IF FOUND.

* create class ZCL_ABAPGIT_USER_EXIT implementing ZIF_ABAPGIT_EXIT in following include,
* if using the development version of abapGit create a global class instead
* place the object in a different package than ZABAPGIT
INCLUDE zabapgit_user_exit IF FOUND.

INCLUDE zabapgit_gui_pages_userexit IF FOUND.

****************************************************
* abapmerge - ZABAPGIT_FORMS
****************************************************
*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_FORMS
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  run
*&---------------------------------------------------------------------*
FORM run.

  DATA: lx_exception TYPE REF TO zcx_abapgit_exception,
        lv_ind       TYPE t000-ccnocliind.

  TRY.
      zcl_abapgit_migrations=>run( ).
      PERFORM open_gui.
    CATCH zcx_abapgit_exception INTO lx_exception.
      MESSAGE lx_exception TYPE 'E'.
  ENDTRY.

ENDFORM.                    "run

FORM open_gui RAISING zcx_abapgit_exception.

  IF sy-batch = abap_true.
    zcl_abapgit_background=>run( ).
  ELSE.

    zcl_abapgit_services_abapgit=>prepare_gui_startup( ).
    zcl_abapgit_ui_factory=>get_gui( )->go_home( ).
    CALL SELECTION-SCREEN 1001. " trigger screen

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  branch_popup
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TT_FIELDS      text
*      -->PV_CODE        text
*      -->CS_ERROR       text
*      -->CV_SHOW_POPUP  text
*      -->RAISING        text
*      -->zcx_abapgit_exception  text
*      -->##CALLED       text
*      -->##NEEDED       text
*----------------------------------------------------------------------*
FORM branch_popup TABLES   tt_fields TYPE zif_abapgit_definitions=>ty_sval_tt
                  USING    pv_code TYPE clike
                  CHANGING cs_error TYPE svale
                           cv_show_popup TYPE c
                  RAISING zcx_abapgit_exception ##called ##needed.
* called dynamically from function module POPUP_GET_VALUES_USER_BUTTONS

  DATA: lx_error  TYPE REF TO zcx_abapgit_exception,
        li_popups TYPE REF TO zif_abapgit_popups.

  TRY.
      li_popups = zcl_abapgit_ui_factory=>get_popups( ).
      li_popups->branch_popup_callback(
        EXPORTING
          iv_code       = pv_code
        CHANGING
          ct_fields     = tt_fields[]
          cs_error      = cs_error
          cv_show_popup = cv_show_popup ).

    CATCH zcx_abapgit_exception INTO lx_error.
      MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

ENDFORM.                    "branch_popup

FORM package_popup TABLES   tt_fields TYPE zif_abapgit_definitions=>ty_sval_tt
                   USING    pv_code TYPE clike
                   CHANGING cs_error TYPE svale
                            cv_show_popup TYPE c
                   RAISING  zcx_abapgit_exception ##called ##needed.
* called dynamically from function module POPUP_GET_VALUES_USER_BUTTONS

  DATA: lx_error  TYPE REF TO zcx_abapgit_exception,
        li_popups TYPE REF TO zif_abapgit_popups.

  TRY.
      li_popups = zcl_abapgit_ui_factory=>get_popups( ).
      li_popups->package_popup_callback(
        EXPORTING
          iv_code       = pv_code
        CHANGING
          ct_fields     = tt_fields[]
          cs_error      = cs_error
          cv_show_popup = cv_show_popup ).

    CATCH zcx_abapgit_exception INTO lx_error.
      MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

ENDFORM.                    "package_popup

FORM output.
  DATA: lt_ucomm TYPE TABLE OF sy-ucomm.

  PERFORM set_pf_status IN PROGRAM rsdbrunt IF FOUND.

  APPEND 'CRET' TO lt_ucomm.  "Button Execute
  APPEND 'SPOS' TO lt_ucomm.  "Button Save

  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = sy-pfkey
    TABLES
      p_exclude = lt_ucomm.

ENDFORM.

FORM exit RAISING zcx_abapgit_exception.
  CASE sy-ucomm.
    WHEN 'CBAC'.  "Back
      IF zcl_abapgit_ui_factory=>get_gui( )->back( ) = abap_true. " end of stack
        zcl_abapgit_ui_factory=>get_gui( )->free( ). " Graceful shutdown
      ELSE.
        LEAVE TO SCREEN 1001.
      ENDIF.
  ENDCASE.
ENDFORM.

FORM password_popup
      USING
        pv_repo_url TYPE string
      CHANGING
        cv_user     TYPE string
        cv_pass     TYPE string.

  lcl_password_dialog=>popup(
    EXPORTING
      iv_repo_url     = pv_repo_url
    CHANGING
      cv_user         = cv_user
      cv_pass         = cv_pass ).

ENDFORM.

FORM remove_toolbar USING pv_dynnr TYPE sy-dynnr.

  DATA: ls_header               TYPE rpy_dyhead,
        lt_containers           TYPE dycatt_tab,
        lt_fields_to_containers TYPE dyfatc_tab,
        lt_flow_logic           TYPE swydyflow.

  CALL FUNCTION 'RPY_DYNPRO_READ'
    EXPORTING
      progname             = sy-cprog
      dynnr                = pv_dynnr
    IMPORTING
      header               = ls_header
    TABLES
      containers           = lt_containers
      fields_to_containers = lt_fields_to_containers
      flow_logic           = lt_flow_logic
    EXCEPTIONS
      cancelled            = 1
      not_found            = 2
      permission_error     = 3
      OTHERS               = 4.
  IF sy-subrc IS NOT INITIAL.
    RETURN. " Ignore errors, just exit
  ENDIF.

  IF ls_header-no_toolbar = abap_true.
    RETURN. " No change required
  ENDIF.

  ls_header-no_toolbar = abap_true.

  CALL FUNCTION 'RPY_DYNPRO_INSERT'
    EXPORTING
      header                 = ls_header
      suppress_exist_checks  = abap_true
    TABLES
      containers             = lt_containers
      fields_to_containers   = lt_fields_to_containers
      flow_logic             = lt_flow_logic
    EXCEPTIONS
      cancelled              = 1
      already_exists         = 2
      program_not_exists     = 3
      not_executed           = 4
      missing_required_field = 5
      illegal_field_value    = 6
      field_not_allowed      = 7
      not_generated          = 8
      illegal_field_position = 9
      OTHERS                 = 10.
  IF sy-subrc <> 2 AND sy-subrc <> 0.
    RETURN. " Ignore errors, just exit
  ENDIF.

ENDFORM.
**********************************************************************
INITIALIZATION.
  PERFORM remove_toolbar USING '1001'. " Remove toolbar on html screen
  lcl_password_dialog=>on_screen_init( ).

START-OF-SELECTION.
  PERFORM run.

* Hide Execute button from screen
AT SELECTION-SCREEN OUTPUT.
  IF sy-dynnr = lcl_password_dialog=>c_dynnr.
    lcl_password_dialog=>on_screen_output( ).
  ELSE.
    PERFORM output.
  ENDIF.

* SAP back command re-direction
AT SELECTION-SCREEN ON EXIT-COMMAND.
  PERFORM exit.

AT SELECTION-SCREEN.
  IF sy-dynnr = lcl_password_dialog=>c_dynnr.
    lcl_password_dialog=>on_screen_event( sscrfields-ucomm ).
  ENDIF.
****************************************************
INTERFACE lif_abapmerge_marker.
ENDINTERFACE.
****************************************************
* abapmerge 0.14.1 - 2020-06-08T05:39:58.769Z
****************************************************
