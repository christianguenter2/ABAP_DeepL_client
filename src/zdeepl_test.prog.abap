*&---------------------------------------------------------------------*
*& Report zdeepl_test
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdeepl_test.

PARAMETERS:
  usage   TYPE abap_bool RADIOBUTTON GROUP r1 DEFAULT 'X',
  transl  TYPE abap_bool RADIOBUTTON GROUP r1,
  text    TYPE string LOWER CASE,
  target  TYPE char2,
  api_key TYPE string LOWER CASE OBLIGATORY.

CLASS deepl_controller DEFINITION.

  PUBLIC SECTION.
    METHODS:
      run
        RAISING
          zcx_deepl_exception.

ENDCLASS.

CLASS deepl_controller IMPLEMENTATION.

  METHOD run.

    DATA: lv_status  TYPE i,
          lt_headers TYPE tihttpnvp,
          lv_data    TYPE string.

    DATA(lo_deepl) = NEW zcl_deepl_api_proxy( api_key ).

    CASE abap_true.
      WHEN usage.

        lo_deepl->usage(
                    IMPORTING
                      ev_status      = lv_status
                      et_headers     = lt_headers
                      ev_data        = lv_data ).

      WHEN transl.

        lo_deepl->translate(
                    EXPORTING
                      iv_text        = text
                      iv_target_lang = target
                    IMPORTING
                      ev_status      = lv_status
                      et_headers     = lt_headers
                      ev_data        = lv_data ).

    ENDCASE.

    cl_demo_output=>write( lv_status ).
    cl_demo_output=>write( lt_headers ).
    cl_demo_output=>write( lv_data ).
    cl_demo_output=>display( ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  TRY.
      NEW deepl_controller( )->run( ).
    CATCH zcx_deepl_exception INTO DATA(lx_error).
      MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
