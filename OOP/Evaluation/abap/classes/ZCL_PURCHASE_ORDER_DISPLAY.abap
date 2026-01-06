*&---------------------------------------------------------------------*
*& Classe ZCL_PURCHASE_ORDER_DISPLAY
*&---------------------------------------------------------------------*
*& Classe de restitution des commandes d'achat avec ALV splitté
*&---------------------------------------------------------------------*

CLASS zcl_purchase_order_display DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    " Types publics
    TYPES: BEGIN OF ty_selection,
             ebeln_range TYPE RANGE OF ebeln,
             matnr_range TYPE RANGE OF matnr,
           END OF ty_selection.

    " Méthodes publiques
    METHODS: constructor
      IMPORTING
        is_selection TYPE ty_selection.

    METHODS: display
      RAISING
        cx_sy_create_object_error.

    METHODS: free.

  PRIVATE SECTION.
    " Attributs privés - Données
    DATA: mt_header TYPE TABLE OF zcl_event_handler_pmi=>ty_header,
          mt_items  TYPE TABLE OF zcl_event_handler_pmi=>ty_item.

    " Attributs privés - Objets ALV
    DATA: mo_container_left  TYPE REF TO cl_gui_container,
          mo_container_right TYPE REF TO cl_gui_container,
          mo_splitter        TYPE REF TO cl_gui_splitter_container,
          mo_alv_header      TYPE REF TO cl_gui_alv_grid,
          mo_alv_items       TYPE REF TO cl_gui_alv_grid,
          mo_event_handler   TYPE REF TO zcl_event_handler_pmi.

    " Attributs privés - Paramètres
    DATA: ms_selection TYPE ty_selection.

    " Méthodes privées
    METHODS: validate_selection
      RAISING
        cx_parameter_invalid_range.

    METHODS: get_data
      RAISING
        cx_sy_open_sql_db.

    METHODS: create_containers
      RAISING
        cx_sy_create_object_error.

    METHODS: create_alv_header
      RAISING
        cx_sy_create_object_error.

    METHODS: create_alv_items
      RAISING
        cx_sy_create_object_error.

    METHODS: create_event_handler
      RAISING
        cx_sy_create_object_error.

    METHODS: build_header_fieldcat
      RETURNING
        VALUE(rt_fieldcat) TYPE lvc_t_fcat.

ENDCLASS.

CLASS zcl_purchase_order_display IMPLEMENTATION.

  METHOD constructor.
    " J'initialise l'instance avec les critères de sélection
    me->ms_selection = is_selection.
  ENDMETHOD.

  METHOD display.
    " Je valide les critères de sélection
    me->validate_selection( ).

    " Je récupère les données
    me->get_data( ).

    " Je vérifie qu'on a des données à afficher
    IF me->mt_header IS INITIAL.
      MESSAGE 'Aucune donnée à afficher' TYPE 'S' DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.

    " Je crée l'interface graphique
    me->create_containers( ).
    me->create_alv_header( ).
    me->create_alv_items( ).
    me->create_event_handler( ).
  ENDMETHOD.

  METHOD validate_selection.
    " Variables locales
    DATA: lv_ebeln TYPE ebeln,
          lv_count TYPE i.

    " Je vérifie si un purchasing document est saisi
    READ TABLE me->ms_selection-ebeln_range INDEX 1 TRANSPORTING NO FIELDS.
    CHECK sy-subrc = 0.

    " Je récupère la première valeur low
    READ TABLE me->ms_selection-ebeln_range INDEX 1 INTO DATA(ls_ebeln).
    CHECK ls_ebeln-low IS NOT INITIAL.

    lv_ebeln = ls_ebeln-low.

    " Je vérifie l'existence dans la table
    SELECT COUNT(*) FROM zekko_pmi
      INTO lv_count
      WHERE ebeln = lv_ebeln.

    IF lv_count = 0.
      MESSAGE e001(00) WITH 'Purchasing document' lv_ebeln 'not found in table ZEKKO_PMI'.
    ENDIF.
  ENDMETHOD.

  METHOD get_data.
    " Variables locales
    DATA: ls_header TYPE zcl_event_handler_pmi=>ty_header,
          ls_item   TYPE zcl_event_handler_pmi=>ty_item.

    " Je sélectionne d'abord les items avec ou sans filtre matériel
    IF me->ms_selection-matnr_range IS INITIAL.
      " Pas de filtre matériel
      SELECT ebeln ebelp matnr werks menge netpr netwr meins
        FROM zekpo_pmi
        INTO ls_item
        WHERE ebeln IN @me->ms_selection-ebeln_range.
        APPEND ls_item TO me->mt_items.
      ENDSELECT.
    ELSE.
      " Avec filtre matériel
      SELECT ebeln ebelp matnr werks menge netpr netwr meins
        FROM zekpo_pmi
        INTO ls_item
        WHERE ebeln IN @me->ms_selection-ebeln_range
          AND matnr IN @me->ms_selection-matnr_range.
        APPEND ls_item TO me->mt_items.
      ENDSELECT.
    ENDIF.

    " Je récupère les headers des items trouvés
    IF me->mt_items IS NOT INITIAL.
      SELECT ebeln bstyp aedat ernam waers
        FROM zekko_pmi
        INTO ls_header
        FOR ALL ENTRIES IN @me->mt_items
        WHERE ebeln = @me->mt_items-ebeln.
        APPEND ls_header TO me->mt_header.
      ENDSELECT.

      " Je supprime les doublons de headers
      SORT me->mt_header BY ebeln.
      DELETE ADJACENT DUPLICATES FROM me->mt_header COMPARING ebeln.
    ENDIF.
  ENDMETHOD.

  METHOD create_containers.
    " Variables locales
    DATA: lo_container TYPE REF TO cl_gui_custom_container.

    " Je crée le container principal lié au custom control
    CREATE OBJECT lo_container
      EXPORTING
        container_name              = 'CUSTOM_CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF sy-subrc <> 0.
      MESSAGE 'Erreur création custom container' TYPE 'E'.
    ENDIF.

    " Je crée le splitter pour diviser l'écran en 2
    CREATE OBJECT me->mo_splitter
      EXPORTING
        parent            = lo_container
        rows              = 1
        columns           = 2
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.

    IF sy-subrc <> 0.
      MESSAGE 'Erreur création splitter' TYPE 'E'.
    ENDIF.

    " Je récupère les containers gauche et droit
    CALL METHOD me->mo_splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = me->mo_container_left.

    CALL METHOD me->mo_splitter->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = me->mo_container_right.

    " Je définis la largeur des colonnes (50/50)
    CALL METHOD me->mo_splitter->set_column_width
      EXPORTING
        id    = 1
        width = 50.
  ENDMETHOD.

  METHOD create_alv_header.
    " Variables locales
    DATA: lt_fieldcat TYPE lvc_t_fcat,
          ls_layout   TYPE lvc_s_layo.

    " Je crée l'objet ALV pour les headers
    CREATE OBJECT me->mo_alv_header
      EXPORTING
        i_parent          = me->mo_container_left
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    IF sy-subrc <> 0.
      MESSAGE 'Erreur création ALV header' TYPE 'E'.
    ENDIF.

    " Je construis le field catalog
    lt_fieldcat = me->build_header_fieldcat( ).

    " Je configure le layout
    ls_layout-zebra      = 'X'.
    ls_layout-cwidth_opt = 'X'.
    ls_layout-grid_title = 'HEADER'.
    ls_layout-smalltitle = 'X'.
    ls_layout-sel_mode   = 'A'.

    " J'affiche l'ALV
    CALL METHOD me->mo_alv_header->set_table_for_first_display
      EXPORTING
        is_layout       = ls_layout
      CHANGING
        it_outtab       = me->mt_header
        it_fieldcatalog = lt_fieldcat.
  ENDMETHOD.

  METHOD create_alv_items.
    " Je crée l'objet ALV pour les items (vide au départ)
    CREATE OBJECT me->mo_alv_items
      EXPORTING
        i_parent          = me->mo_container_right
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    IF sy-subrc <> 0.
      MESSAGE 'Erreur création ALV items' TYPE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD create_event_handler.
    " Je crée le gestionnaire d'événements avec injection de dépendances
    CREATE OBJECT me->mo_event_handler
      EXPORTING
        it_header    = me->mt_header
        it_items     = me->mt_items
        io_alv_items = me->mo_alv_items.

    " J'enregistre l'événement double-clic
    SET HANDLER me->mo_event_handler->on_double_click FOR me->mo_alv_header.
  ENDMETHOD.

  METHOD build_header_fieldcat.
    " Variables locales
    DATA: ls_fieldcat TYPE lvc_s_fcat.

    " Je configure chaque colonne
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'EBELN'.
    ls_fieldcat-scrtext_l = 'Purchasing Document'.
    ls_fieldcat-col_pos   = 1.
    ls_fieldcat-outputlen = 10.
    ls_fieldcat-key       = 'X'.
    APPEND ls_fieldcat TO rt_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'BSTYP'.
    ls_fieldcat-scrtext_l = 'Doc. Cat.'.
    ls_fieldcat-col_pos   = 2.
    ls_fieldcat-outputlen = 3.
    APPEND ls_fieldcat TO rt_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'AEDAT'.
    ls_fieldcat-scrtext_l = 'Changed on'.
    ls_fieldcat-col_pos   = 3.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO rt_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'ERNAM'.
    ls_fieldcat-scrtext_l = 'Created by'.
    ls_fieldcat-col_pos   = 4.
    ls_fieldcat-outputlen = 12.
    APPEND ls_fieldcat TO rt_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'WAERS'.
    ls_fieldcat-scrtext_l = 'Currency'.
    ls_fieldcat-col_pos   = 5.
    ls_fieldcat-outputlen = 5.
    APPEND ls_fieldcat TO rt_fieldcat.
  ENDMETHOD.

  METHOD free.
    " Je libère tous les objets dans l'ordre inverse de création
    IF me->mo_event_handler IS BOUND.
      FREE me->mo_event_handler.
    ENDIF.

    IF me->mo_alv_items IS BOUND.
      CALL METHOD me->mo_alv_items->free.
      FREE me->mo_alv_items.
    ENDIF.

    IF me->mo_alv_header IS BOUND.
      CALL METHOD me->mo_alv_header->free.
      FREE me->mo_alv_header.
    ENDIF.

    IF me->mo_splitter IS BOUND.
      CALL METHOD me->mo_splitter->free.
      FREE me->mo_splitter.
    ENDIF.

    " Les containers sont automatiquement libérés avec le splitter
    CLEAR: me->mo_container_left, me->mo_container_right.

    " Je vide les tables
    CLEAR: me->mt_header, me->mt_items.
  ENDMETHOD.

ENDCLASS.
