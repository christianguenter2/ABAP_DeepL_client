CLASS zcx_deepl_exception DEFINITION
  PUBLIC INHERITING FROM cx_static_check
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      raise_t100
        RAISING
          zcx_deepl_exception,

      raise
        IMPORTING
          ix_previous TYPE REF TO cx_rest_client_exception
        RAISING
          zcx_deepl_exception.

    INTERFACES:
      if_t100_dyn_msg.

    METHODS:
      get_text REDEFINITION.

ENDCLASS.



CLASS zcx_deepl_exception IMPLEMENTATION.


  METHOD get_text.

    result = COND #(
               WHEN previous IS BOUND
               THEN previous->get_text( )
               ELSE super->get_text( ) ).

  ENDMETHOD.


  METHOD raise.

    IF ix_previous->if_t100_message~t100key-msgid IS NOT INITIAL.

      RAISE EXCEPTION TYPE zcx_deepl_exception
        MESSAGE
        ID ix_previous->if_t100_message~t100key-msgid
        NUMBER ix_previous->if_t100_message~t100key-msgno
        WITH
        ix_previous->if_t100_message~t100key-attr1
        ix_previous->if_t100_message~t100key-attr2
        ix_previous->if_t100_message~t100key-attr3
        ix_previous->if_t100_message~t100key-attr4.

    ELSE.
      RAISE EXCEPTION TYPE zcx_deepl_exception
        EXPORTING
          previous = ix_previous.
    ENDIF.

  ENDMETHOD.


  METHOD raise_t100.

    RAISE EXCEPTION TYPE zcx_deepl_exception
      USING MESSAGE.

  ENDMETHOD.

ENDCLASS.
