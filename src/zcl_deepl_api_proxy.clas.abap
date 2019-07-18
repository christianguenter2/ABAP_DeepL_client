CLASS zcl_deepl_api_proxy DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING
          iv_api_key TYPE string,

      usage
        EXPORTING
          ev_status  TYPE i
          et_headers TYPE tihttpnvp
          ev_data    TYPE string
        RAISING
          zcx_deepl_exception,

      translate
        IMPORTING
          iv_text        TYPE string
          iv_target_lang TYPE char2
        EXPORTING
          ev_status      TYPE i
          et_headers     TYPE tihttpnvp
          ev_data        TYPE string
        RAISING
          zcx_deepl_exception.

  PRIVATE SECTION.
    DATA:
      mv_api_key TYPE string.

    METHODS:
      execute
        IMPORTING
          iv_verb    TYPE string
          iv_params  TYPE string OPTIONAL
        EXPORTING
          ev_status  TYPE i
          et_headers TYPE tihttpnvp
          ev_data    TYPE string
        RAISING
          zcx_deepl_exception,

      build_url
        IMPORTING
          iv_verb       TYPE string
          iv_params     TYPE string
        RETURNING
          VALUE(rv_url) TYPE string.

ENDCLASS.



CLASS zcl_deepl_api_proxy IMPLEMENTATION.

  METHOD build_url.

    CONSTANTS: co_base_url TYPE string VALUE `https://api.deepl.com/v2/`.

    rv_url = |{ co_base_url }|
          && |{ iv_verb }|
          && |?auth_key={ mv_api_key }|
          && COND #(
               WHEN iv_params IS NOT INITIAL
               THEN |&{ iv_params }| ).

  ENDMETHOD.


  METHOD constructor.

    mv_api_key = iv_api_key.

  ENDMETHOD.


  METHOD execute.

    CLEAR:
      ev_status,
      et_headers,
      ev_data.

    DATA(lv_url) = build_url(
                       iv_verb   = iv_verb
                       iv_params = iv_params ).

    cl_http_client=>create_by_url(
      EXPORTING
        url                = lv_url
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

        li_rest->get( ).

        ev_status = li_rest->get_status( ).
        et_headers = li_rest->get_response_headers( ).
        ev_data = li_rest->get_response_entity( )->get_string_data( ).

      CATCH cx_rest_client_exception INTO DATA(lx_error).
        zcx_deepl_exception=>raise( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD translate.

    execute(
      EXPORTING
        iv_verb    = `translate`
        iv_params  = |text={ iv_text }&target_lang={ iv_target_lang }|
      IMPORTING
        ev_status  = ev_status
        et_headers = et_headers
        ev_data    = ev_data ).

  ENDMETHOD.


  METHOD usage.

    execute(
      EXPORTING
        iv_verb    = `usage`
      IMPORTING
        ev_status  = ev_status
        et_headers = et_headers
        ev_data    = ev_data ).

  ENDMETHOD.

ENDCLASS.
