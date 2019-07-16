*&---------------------------------------------------------------------*
*& Report zdeepl_test
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdeepl_test.

CLASS deepl_controller DEFINITION.

  PUBLIC SECTION.
    METHODS:
      run
        RAISING
          zcx_deepl_exception.

ENDCLASS.

CLASS deepl_controller IMPLEMENTATION.

  METHOD run.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = 'https://api.deepl.com/v2/translate'
      IMPORTING
        client             = DATA(li_http_client)
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4 ).

    IF sy-subrc <> 0.
      zcx_deepl_exception=>raise_t100( ).
    ENDIF.

    TRY.
        DATA(li_rest) = CAST if_rest_client( NEW cl_rest_http_client( li_http_client ) ).

*        li_rest->create_request_entity( )->set_header_field(
*            iv_name  = 'auth_key'
*            iv_value = |512d5953-94ca-d3ef-d8c1-4ea0b08c065b| ).

        li_rest->set_request_header(
            iv_name  = 'auth_key'
            iv_value = '512d5953-94ca-d3ef-d8c1-4ea0b08c065b' ).

        li_rest->set_request_header(
            iv_name  = 'text'
            iv_value = 'this is a test' ).

        li_rest->set_request_header(
            iv_name  = 'target_lang'
            iv_value = 'DE' ).

        li_rest->get( ).

        DATA(lv_status) = li_rest->get_status( ).
        DATA(lt_headers) = li_rest->get_response_headers( ).
        DATA(lv_data) = li_rest->get_response_entity( )->get_string_data( ).


      CATCH cx_rest_client_exception INTO DATA(lx_error).
        zcx_deepl_exception=>raise( lx_error ).
    ENDTRY.

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
