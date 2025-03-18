*&---------------------------------------------------------------------*
*& Report zsim_ord_poc
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsim_ord_poc MESSAGE-ID zotc
                            NO STANDARD PAGE HEADING.


INCLUDE zsim_ord_poc_top." Global Data Declaration
INCLUDE zsim_ord_poc_sel." Selection Screen
INCLUDE zsim_ord_poc_f01." Processing Logic

START-OF-SELECTION.

  DATA(lo_obj) = NEW lcl_order_creation( ).

* Fetch Customer Data
  lo_obj->fetch_data( ).

  IF gt_orders IS NOT INITIAL.

* Process Data based on creation method
    IF p_bapi = abap_true.
      lo_obj->process_data( ).
    ELSEIF p_idoc = abap_true.
      lo_obj->process_data_idoc( ).
    ENDIF.

* Display Output
    lo_obj->display_output( ).

  ENDIF.


*&---------------------------------------------------------------------*
*&  Include  zsim_ord_poc_top
*&---------------------------------------------------------------------*

    TYPES: BEGIN OF ty_orders,
             vbeln   TYPE vbak-vbeln,
             auart   TYPE vbak-auart,
             vkorg   TYPE vbak-vkorg,
             vtweg   TYPE vbak-vtweg,
             spart   TYPE vbak-spart,
             kunnr   TYPE vbak-kunnr,
             posnr   TYPE vbap-posnr,
             matnr   TYPE vbap-matnr,
             werks   TYPE vbap-werks,
             message TYPE bapiret2-message,
           END OF ty_orders.

    DATA: gv_count     TYPE i VALUE 0,
          gv_errcount  TYPE i VALUE 0,
          gv_totcount  TYPE i,
          gt_orders    TYPE TABLE OF ty_orders,
          gs_sucorders TYPE ty_orders,
          gs_errorders TYPE ty_orders,
          gt_sucorders TYPE TABLE OF ty_orders,
          gt_errorders TYPE TABLE OF ty_orders.


    CLASS lcl_order_creation DEFINITION.
      PUBLIC SECTION.
        METHODS:
          fetch_data,
          process_data,
          process_data_idoc,
          display_output.

    ENDCLASS.


*&---------------------------------------------------------------------*
*&  Include  zsim_ord_poc_sel
*&---------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS: s_kunnr FOR gv_kunnr,                           " Customer Number
                s_auart FOR gv_auart NO INTERVALS NO-EXTENSION, " Document Type
                s_vkorg FOR gv_vkorg NO INTERVALS NO-EXTENSION, " Sales Org
                s_vtweg FOR gv_vtweg NO INTERVALS NO-EXTENSION, " Dist Channel
                s_spart FOR gv_spart NO INTERVALS NO-EXTENSION, " Division
                s_matnr FOR gv_matnr,
                s_werks FOR gv_werks.

SELECTION-SCREEN: END OF BLOCK blk1.

SELECTION-SCREEN: BEGIN OF BLOCK blk2 WITH FRAME TITLE TEXT-002.

PARAMETERS: p_ordctr TYPE i DEFAULT 3 OBLIGATORY.   " Number of Orders to Simulate

SELECTION-SCREEN: END OF BLOCK blk2.

SELECTION-SCREEN: BEGIN OF BLOCK blk3 WITH FRAME TITLE TEXT-003.

PARAMETERS: p_bapi RADIOBUTTON GROUP grp1 DEFAULT 'X',
            p_idoc RADIOBUTTON GROUP grp1.

SELECTION-SCREEN: END OF BLOCK blk3.


*&---------------------------------------------------------------------*
*&  Include  zmotc_simulate_orders_f01
*&---------------------------------------------------------------------*

CLASS lcl_order_creation IMPLEMENTATION.

  METHOD fetch_data.

    " Existing fetch data logic

  ENDMETHOD.

  METHOD process_data.

    " Existing process_data logic for BAPI

  ENDMETHOD.

  METHOD process_data_idoc.

    " Simulate orders using BAPI logic
    process_data( ).

    " Process the successfully simulated orders into IDOCs
    LOOP AT gt_sucorders INTO DATA(ls_order).

      DATA: lt_edidd TYPE TABLE OF edidd,
            ls_edidd TYPE edidd,
            ls_edidc TYPE edidc.

      CLEAR ls_edidc.
      ls_edidc-mestyp = 'ORDERS'.
      ls_edidc-rcvprt = 'LS'.
      ls_edidc-rcvprn = 'OWN_SYSTEM'. " Receiver Partner Number

      " Fill control record
      APPEND ls_edidc TO lt_edidd.

      " Fill data record
      CLEAR ls_edidd.
      ls_edidd-segnam = 'E1EDK01'.
      ls_edidd-sdata = ls_order-vbeln. " Sample data, extend as needed
      APPEND ls_edidd TO lt_edidd.

      CALL FUNCTION 'IDOC_INBOUND_SINGLE'
        EXPORTING
          pi_idoc_control = ls_edidc
        TABLES
          pi_idoc_data    = lt_edidd
        EXCEPTIONS
          OTHERS          = 1.

      IF sy-subrc = 0.
        WRITE: / 'IDOC created for Order:', ls_order-vbeln.
      ELSE.
        WRITE: / 'Failed to create IDOC for Order:', ls_order-vbeln.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD display_output.

    " Existing display_output logic

  ENDMETHOD.

ENDCLASS.
