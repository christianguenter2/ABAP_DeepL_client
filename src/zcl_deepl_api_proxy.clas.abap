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
          iv_target_lang TYPE zif_deepl_definitions=>lang
        EXPORTING
          ev_status      TYPE i
          et_headers     TYPE tihttpnvp
          ev_data        TYPE string
        RAISING
          zcx_deepl_exception.

  PRIVATE SECTION.
    CONSTANTS: deepl_destination TYPE rfcdest VALUE 'DEEPL'.
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

      build_path
        IMPORTING
          iv_verb       TYPE string
          iv_params     TYPE string
        RETURNING
          VALUE(rv_url) TYPE string.

ENDCLASS.



CLASS zcl_deepl_api_proxy IMPLEMENTATION.

  METHOD build_path.

    rv_url = |{ iv_verb }|
          && |?auth_key={ mv_api_key }|
          && COND #(
               WHEN iv_params IS NOT INITIAL
               THEN |&{ iv_params }| ).

  ENDMETHOD.


  METHOD constructor.

    mv_api_key = iv_api_key.

  ENDMETHOD.


  METHOD execute.

    CONSTANTS: co_base_url          TYPE string VALUE `https://api-free.deepl.com/v2/`.

    CLEAR:
      ev_status,
      et_headers,
      ev_data.

    DATA(lv_path) = build_path(
                       iv_verb   = iv_verb
                       iv_params = iv_params ).

    cl_http_client=>create_by_destination(
      EXPORTING
        destination              = deepl_destination
      IMPORTING
        client                   = DATA(li_http_client)
      EXCEPTIONS
        argument_not_found       = 1
        destination_not_found    = 2
        destination_no_authority = 3
        plugin_not_active        = 4
        internal_error           = 5
        OTHERS                   = 6
    ).
    IF sy-subrc = 2.
      " Fallback to create_by_url
      cl_http_client=>create_by_url(
        EXPORTING
          url                = lv_path
        IMPORTING
          client             = li_http_client
        EXCEPTIONS
          argument_not_found = 1
          plugin_not_active  = 2
          internal_error     = 3
          OTHERS             = 4 ).
      IF sy-subrc <> 0.
        zcx_deepl_exception=>raise_t100( ).
      ENDIF.
    ELSEIF sy-subrc <> 0.
      zcx_deepl_exception=>raise_t100( ).
    ENDIF.

    TRY.
        li_http_client->request->set_header_field(
            name  = if_http_header_fields_sap=>request_uri
            value = lv_path ).

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
