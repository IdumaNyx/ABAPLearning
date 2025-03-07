*&---------------------------------------------------------------------*
*& Report zsimulate_bapi_orders4
*&---------------------------------------------------------------------*
*& This program simulates sales order creation
*& It selects random customers, sales data and materials
*& And then calls BAPI_SALESORDER_SIMULATE to simulate order creation
*&---------------------------------------------------------------------*
REPORT ZSIMULATE_BAPI_ORDERS4.

INCLUDE ZSIMULATE_BAPI_ORDERS4_TOP.  " Global Declarations (Types, Data)
INCLUDE ZSIMULATE_BAPI_ORDERS4_SEL.  " Selection-Screen Elements
INCLUDE ZSIMULATE_BAPI_ORDERS4_F01.  " Subroutines (FORM Routines)
INCLUDE ZSIMULATE_BAPI_ORDERS4_CLS.  " Class Definition & Implementation

"-----------------------------------------------------------------------
" MAIN EXECUTION BLOCK
"-----------------------------------------------------------------------

START-OF-SELECTION.

DATA(go_order_simulator) = NEW zcl_order_simulator( ). " object of class zcl_order_simulator()

go_order_simulator->load_master_data( ). " Load master data from SAP tables
go_order_simulator->simulate_orders( ). " Generate and simulate sales orders based on selection criteria
go_order_simulator->display_results( ). " Display successful and Failed orders

-------------------------------------------------------------------------------------------------

*&---------------------------------------------------------------------*
*&  Include  zsimulate_bapi_orders4_top
*&---------------------------------------------------------------------*

TYPES:
  BEGIN OF ty_customer,
    kunnr TYPE kna1-kunnr,
    name1 TYPE kna1-name1,
  END OF ty_customer,

  BEGIN OF ty_sales,
    vkorg TYPE knvv-vkorg,
    vtweg TYPE knvv-vtweg,
    spart TYPE knvv-spart,
  END OF ty_sales,

  BEGIN OF ty_material,
    matnr TYPE mara-matnr,
    ersda TYPE mara-ersda,
    mtart TYPE mara-mtart,
  END OF ty_material,

  BEGIN OF ty_order,
    order_id   TYPE char10,
    customer   TYPE ty_customer,
    sales_data TYPE ty_sales,
    material   TYPE ty_material,
    quantity   TYPE i,
  END OF ty_order.

DATA:
  it_success_orders TYPE STANDARD TABLE OF ty_order WITH DEFAULT KEY,
  it_failed_orders  TYPE STANDARD TABLE OF ty_order WITH DEFAULT KEY,
  it_customers      TYPE STANDARD TABLE OF kna1,
  it_sales_data     TYPE STANDARD TABLE OF knvv,
  it_materials      TYPE STANDARD TABLE OF mara.

  
------------------------------------------------------------------------------------------------------------------------
*&---------------------------------------------------------------------*
*&  Include  zsimulate_bapi_orders4_sel
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Include ZSIMULATE_BAPI_ORDERS4_SEL
*& Selection-Screen Elements
*&---------------------------------------------------------------------*

"PARAMETERS:
  "p_o_num TYPE i DEFAULT 10 OBLIGATORY.   " Number of Orders to Simulate (Mandatory)

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
  PARAMETERS:
    p_o_num TYPE i DEFAULT 10 OBLIGATORY,   " Number of Orders to Simulate (Mandatory)
    p_o_type TYPE char4 DEFAULT 'TA',  " Order Type Filter (Optional)
    p_vkorg TYPE knvv-vkorg,             " Sales Organization (Optional)
    p_vtweg TYPE knvv-vtweg,             " Distribution Channel (Optional)
    p_spart TYPE knvv-spart.             " Division (Optional)
SELECTION-SCREEN END OF BLOCK b1.

------------------------------------------------------------------------------------------------------------------------


*&---------------------------------------------------------------------*
*&  Include  zsimulate_bapi_orders4_f01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Include ZSIMULATE_BAPI_ORDERS4_F01
*& Subroutines (FORM Routines) for generating random data
*&---------------------------------------------------------------------*

FORM select_random_data CHANGING eo_order TYPE ty_order.

" Local variables for random selections
  DATA: lv_random_index TYPE i,
        lv_max_customers TYPE i VALUE 0,
        lv_max_sales TYPE i VALUE 0,
        lv_max_materials TYPE i VALUE 0,
        lv_seed TYPE i.

  "lv_seed = sy-uzeit+4(2) * 100 + sy-uzeit+2(2) * 10 + sy-uzeit+0(2).
   lv_seed = sy-uzeit * sy-tabix * sy-index + sy-datum.
  lv_max_customers = LINES( it_customers ).
  lv_max_sales = LINES( it_sales_data ).
  lv_max_materials = LINES( it_materials ).

  IF lv_max_customers = 0 OR lv_max_sales = 0 OR lv_max_materials = 0.
    MESSAGE 'One or more master data tables are empty!' TYPE 'E'.
  ENDIF.

  DATA(lo_random_customer) = cl_abap_random_int=>create( seed = lv_seed min = 1 max = lv_max_customers ).
  lv_random_index = lo_random_customer->get_next( ).
  READ TABLE it_customers INDEX lv_random_index INTO eo_order-customer.
  IF sy-subrc <> 0.
    MESSAGE 'Error selecting random customer' TYPE 'E'.
  ENDIF.

  DATA(lo_random_sales) = cl_abap_random_int=>create( seed = lv_seed min = 1 max = lv_max_sales ).

  lv_random_index = lo_random_sales->get_next( ).
  READ TABLE it_sales_data INDEX lv_random_index INTO eo_order-sales_data.

  "--------------------------------------------------------------------------
*IF p_vkorg IS NOT INITIAL OR p_vtweg IS NOT INITIAL OR p_spart IS NOT INITIAL.
*    LOOP AT it_sales_data INTO eo_order-sales_data
*       WHERE ( vkorg = p_vkorg )
*         OR ( vtweg = p_vtweg )
*         OR ( spart = p_spart ).
*
*    lv_random_index = lo_random_sales->get_next( ).
*    EXIT. " Stop once we find a valid match
*  ENDLOOP.
*
*  ELSE.
*      lv_random_index = lo_random_sales->get_next( ).
*      READ TABLE it_sales_data INDEX lv_random_index INTO eo_order-sales_data.
*  ENDIF.
  "--------------------------------------------------------------------------
  IF sy-subrc <> 0.
    MESSAGE 'Error selecting random sales area' TYPE 'E'.
  ENDIF.

  DATA(lo_random_material) = cl_abap_random_int=>create( seed = lv_seed min = 1 max = lv_max_materials ).
  lv_random_index = lo_random_material->get_next( ).
  DATA: ls_material TYPE mara.
  READ TABLE it_materials INDEX lv_random_index INTO ls_material.
  IF sy-subrc = 0.
    eo_order-material-matnr = ls_material-matnr.
    eo_order-material-ersda = ls_material-ersda.
    eo_order-material-mtart = ls_material-mtart.
  ELSE.
    MESSAGE 'Error selecting random material' TYPE 'E'.
  ENDIF.

  DATA(lo_random_qty) = cl_abap_random_int=>create( seed = lv_seed min = 1 max = 100 ).
  eo_order-quantity = lo_random_qty->get_next( ).

ENDFORM.

------------------------------------------------------------------------------------------------------------------------------------

*&---------------------------------------------------------------------*
*&  Include  zsimulate_bapi_orders4_cls
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Include ZSIMULATE_BAPI_ORDERS4_CLS
*& Class Definition and Implementation
*&---------------------------------------------------------------------*

CLASS zcl_order_simulator DEFINITION FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: constructor,
             load_master_data,
             simulate_orders,
             display_results.

ENDCLASS.

CLASS zcl_order_simulator IMPLEMENTATION.

  METHOD constructor.

  ENDMETHOD.

  METHOD load_master_data.
    " Load customer general data from KNA1
    SELECT mandt, kunnr, name1 FROM kna1 INTO TABLE @it_customers.
    IF sy-subrc <> 0.
      WRITE: / 'Error loading customer data from KNA1'.
    ELSE.
      WRITE: / 'KNA1 data loaded: ', LINES( it_customers ), ' records.'.
    ENDIF.

    " Load customer sales area data from KNVV
    SELECT mandt, vkorg, vtweg, spart FROM knvv INTO TABLE @it_sales_data.
    IF sy-subrc <> 0.
      WRITE: / 'Error loading sales area data from KNVV'.
    ELSE.
      WRITE: / 'KNVV data loaded: ', LINES( it_sales_data ), ' records.'.
    ENDIF.

    " Load material data from MARA
    SELECT mandt, matnr, ersda, mtart FROM mara INTO TABLE @it_materials.
    IF sy-subrc <> 0.
      WRITE: / 'Error loading material data from MARA'.
    ELSE.
      WRITE: / 'MARA data loaded: ', LINES( it_materials ), ' records.'.
    ENDIF.

  ENDMETHOD.

  METHOD simulate_orders.
    DATA: lv_count TYPE i,
          ls_order TYPE ty_order,
          ls_return TYPE bapiret2,
          it_return TYPE TABLE OF bapiret2,
          ls_order_header TYPE bapisdhead,
          it_order_items TYPE TABLE OF bapiitemin,
          it_order_partners TYPE TABLE OF bapiparnr,
          ls_order_item TYPE bapiitemin,
          ls_order_partner TYPE bapiparnr.

    DO p_o_num TIMES.
      " Generate a new random order
      PERFORM select_random_data CHANGING ls_order.

     " Apply Selection-Screen Filters (Only if values are provided)
      IF ( p_vkorg IS NOT INITIAL AND ls_order-sales_data-vkorg <> p_vkorg ) OR
         ( p_vtweg IS NOT INITIAL AND ls_order-sales_data-vtweg <> p_vtweg ) OR
         ( p_spart IS NOT INITIAL AND ls_order-sales_data-spart <> p_spart ).
        CONTINUE. " Skip order if it does not match filter criteria
      ENDIF.

      " Populate order header fields
      CLEAR ls_order_header.
      ls_order_header-doc_type = p_o_type.  " Use selected Order Type (Optional)
      ls_order_header-sales_org = ls_order-sales_data-vkorg.
      ls_order_header-distr_chan = ls_order-sales_data-vtweg.
      ls_order_header-division = ls_order-sales_data-spart.

      " Populate Order Item (Material)"
      CLEAR: ls_order_item, it_order_items.
      ls_order_item-material = ls_order-material-matnr.
      ls_order_item-target_qty = ls_order-quantity.
      APPEND ls_order_item TO it_order_items.

      " Populate Order Partner (Customer)"
      CLEAR: ls_order_partner, it_order_partners.
      ls_order_partner-partn_role = 'AG'.
      ls_order_partner-partn_numb = ls_order-customer-kunnr.
      APPEND ls_order_partner TO it_order_partners.

      " Call the BAPI
      CALL FUNCTION 'BAPI_SALESORDER_SIMULATE'
        EXPORTING
          order_header_in = ls_order_header
        TABLES
          order_items_in = it_order_items
          order_partners = it_order_partners
          messagetable = it_return.

      " Check if the BAPI simulation was successful
      READ TABLE it_return INTO ls_return WITH KEY type = 'E'.
      IF sy-subrc = 0.
        " Order failed, store in failed orders table
        APPEND ls_order TO it_failed_orders.

        " Retry with a new order
        CLEAR ls_order.
        PERFORM select_random_data CHANGING ls_order.

        " Re-attempt BAPI
        CALL FUNCTION 'BAPI_SALESORDER_SIMULATE'
          EXPORTING
            order_header_in = ls_order_header
          TABLES
            order_items_in = it_order_items
            order_partners = it_order_partners
            messagetable = it_return.

        READ TABLE it_return INTO ls_return WITH KEY type = 'E'.
        IF sy-subrc = 0.
          APPEND ls_order TO it_failed_orders.
        ELSE.
          APPEND ls_order TO it_success_orders.
        ENDIF.

      ELSE.
        APPEND ls_order TO it_success_orders.
      ENDIF.

    ENDDO.

  ENDMETHOD.

  METHOD display_results.
    WRITE: / '--- Successful Orders ---'.
    LOOP AT it_success_orders INTO DATA(ls_succ_order).
      WRITE: / 'ORDER ID : ', ls_succ_order-order_id, 'NAME : ', ls_succ_order-customer-kunnr, 'QUANTITY : ', ls_succ_order-quantity.
    ENDLOOP.

    WRITE: / '--- Failed Orders ---'.
    LOOP AT it_failed_orders INTO DATA(ls_fail_order).
      WRITE: / 'NAME : ', ls_fail_order-customer-kunnr,
               'VKORG : ', ls_fail_order-sales_data-vkorg,
               'VTWEG : ', ls_fail_order-sales_data-vtweg,
               'VKORG : ', ls_fail_order-sales_data-vkorg,
               'ITEM : ', ls_fail_order-material-matnr,
               'QUANTITY : ', ls_fail_order-quantity.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.




