*&---------------------------------------------------------------------*
*& Include Z_POEC_PMI_TOP
*&---------------------------------------------------------------------*
*& Déclarations globales
*&---------------------------------------------------------------------*

" ===== VARIABLES POUR ÉCRAN DE SÉLECTION =====

DATA: gv_ebeln_ref TYPE ebeln,
      gv_matnr_ref TYPE matnr.

* FONCTIONNEMENT EN CLASSE

" ===== INSTANCE DE LA CLASSE =====

DATA: go_display TYPE REF TO zcl_purchase_order_display.

* FONCTIONNEMENT PROCEDURAL
*" ===== TABLES INTERNES GLOBALES =====
*
*DATA: gt_header TYPE TABLE OF zcl_event_handler_pmi=>ty_header,
*      gt_items  TYPE TABLE OF zcl_event_handler_pmi=>ty_item.
*
*" ===== VARIABLES GLOBALES =====
*
*DATA: gv_container_left  TYPE REF TO cl_gui_container,
*      gv_container_right TYPE REF TO cl_gui_container,
*      gv_splitter        TYPE REF TO cl_gui_splitter_container,
*      gv_alv_header      TYPE REF TO cl_gui_alv_grid,
*      gv_alv_items       TYPE REF TO cl_gui_alv_grid,
*      go_event_handler   TYPE REF TO zcl_event_handler_pmi.
