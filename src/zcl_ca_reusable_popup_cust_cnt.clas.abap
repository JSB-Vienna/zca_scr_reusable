"! <p class="shorttext synchronized" lang="en">Common object: Reusable popup with custom container</p>
"! ---------------------------------------------------------------------*
"!    Reuse this class as super class for your popup and redefine
"!    methods such as HANDLE_PBO, HANDLE_PAI or HANDLE_FCODE to
"!    bring in your functionalities. There is no need to create an
"!    own screen.
"! ---------------------------------------------------------------------*
CLASS zcl_ca_reusable_popup_cust_cnt DEFINITION PUBLIC
                                                INHERITING FROM zcl_ca_scr_fw_window_ctrl
                                                CREATE PUBLIC.

* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      zif_ca_scr_reusable.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @parameter iv_mode       | <p class="shorttext synchronized" lang="en">Screen mode (use C_MODE_*)</p>
      "! @parameter iv_toolbar    | <p class="shorttext synchronized" lang="en">1 = With appl. toolbar; 0 = Hide appl. toolbar</p>
      "! @parameter iv_starting_x | <p class="shorttext synchronized" lang="en">Starting in column</p>
      "! @parameter iv_starting_y | <p class="shorttext synchronized" lang="en">Starting in line</p>
      "! @parameter iv_ending_x   | <p class="shorttext synchronized" lang="en">Ending in column</p>
      "! @parameter iv_ending_y   | <p class="shorttext synchronized" lang="en">Ending in line</p>
      constructor
        IMPORTING
          iv_mode       TYPE syst_ucomm DEFAULT c_mode_display
          iv_toolbar    TYPE dml_boolean DEFAULT c_true
          iv_starting_x TYPE i DEFAULT 0
          iv_starting_y TYPE i DEFAULT 0
          iv_ending_x   TYPE i DEFAULT 0
          iv_ending_y   TYPE i DEFAULT 0.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   a l i a s e s
    ALIASES:
*     Screen elements
      c_prog_name_scrs     FOR  zif_ca_scr_reusable~c_prog_name_scrs,
      c_titlebar           FOR  zif_ca_scr_reusable~c_titlebar,
      c_pfstatus_popup     FOR  zif_ca_scr_reusable~c_pfstatus_popup,
      c_pfstatus_screen    FOR  zif_ca_scr_reusable~c_pfstatus_screen,
      c_scr_fname_ccont    FOR  zif_ca_scr_reusable~c_scr_fname_ccont,
*     Customer container
      mo_ccont_reuse       FOR  zif_ca_scr_reusable~mo_ccont_reuse.

*   i n s t a n c e   m e t h o d s
    METHODS:
      on_call_screen REDEFINITION,

      handle_pbo REDEFINITION,

      on_set_status REDEFINITION,

      on_process_fcode REDEFINITION,

      on_closed REDEFINITION.

ENDCLASS.



CLASS ZCL_CA_REUSABLE_POPUP_CUST_CNT IMPLEMENTATION.


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Handle Process Before Output - but set no GUI status!
    "-----------------------------------------------------------------*
    super->constructor( iv_mode       = iv_mode
                        iv_popup      = c_true
                        iv_starting_x = iv_starting_x
                        iv_starting_y = iv_starting_y
                        iv_ending_x   = iv_ending_x
                        iv_ending_y   = iv_ending_y ).

    me->mv_repid = c_prog_name_scrs.
    me->mv_dynnr = SWITCH #( iv_toolbar
                               WHEN c_true  THEN '0020'
                               WHEN c_false THEN '0021' ).
  ENDMETHOD.                    "constructor


  METHOD handle_pbo.
    "-----------------------------------------------------------------*
    "   Handle Process Before Output - but set no GUI status!
    "-----------------------------------------------------------------*
    IF me->mo_ccont_reuse IS BOUND.
      RETURN.
    ENDIF.

    me->mo_ccont_reuse = zcl_ca_cfw_util=>create_cust_cont(
                                         iv_cnt_name = c_scr_fname_ccont
                                         iv_repid    = me->mv_repid
                                         iv_dynnr    = me->mv_dynnr ).
  ENDMETHOD.                    "handle_pbo


  METHOD on_call_screen.
    "-----------------------------------------------------------------*
    "   Call simply the screen - nothing else
    "-----------------------------------------------------------------*
    CALL FUNCTION 'Z_CA_CALL_REUSABLE_SCREEN'
      EXPORTING
        iv_dynnr      = me->mv_dynnr
        iv_popup      = me->mv_popup
        iv_starting_x = me->mv_starting_x
        iv_starting_y = me->mv_starting_y
        iv_ending_x   = me->mv_ending_x
        iv_ending_y   = me->mv_ending_y.
  ENDMETHOD.                    "on_call_screen


  METHOD on_closed.
    "-----------------------------------------------------------------*
    "   Release fields and instances for garbage collection
    "-----------------------------------------------------------------*
    super->on_closed( ).

    me->mo_ccont_reuse->free( ).

    FREE: me->mo_ccont_reuse,
          me->mv_repid,
          me->mv_dynnr.
  ENDMETHOD.                    "on_closed


  METHOD on_process_fcode.
    "-----------------------------------------------------------------*
    "   Handle function code
    "-----------------------------------------------------------------*
    IF iv_fcode EQ c_fcode_cancel OR
       iv_fcode EQ c_fcode_enter.
      me->close( ).
      me->set_fcode_handled( ).
    ENDIF.
  ENDMETHOD.                    "on_process_fcode


  METHOD on_set_status.
    "-----------------------------------------------------------------*
    "   Activate functions depending on mode and set titlebar
    "-----------------------------------------------------------------*
    me->mo_status->set_pfstatus( iv_pfstatus       = c_pfstatus_popup
                                 iv_pfstatus_repid = c_prog_name_scrs ).

    me->mo_status->set_titlebar( iv_titlebar       = c_titlebar
                                 iv_titlebar_repid = c_prog_name_scrs ).
  ENDMETHOD.                    "on_set_status
ENDCLASS.
