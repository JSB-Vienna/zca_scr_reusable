"! <p class="shorttext synchronized" lang="en">Common object: Common elements of reusable screens</p>
INTERFACE zif_ca_scr_reusable PUBLIC.
* c o n s t a n t s
  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Program name to reusable screens, status and titlebar</p>
    c_prog_name_scrs  TYPE syrepid           VALUE 'SAPLZCA_SCR_REUSABLE'  ##no_text,
    "! <p class="shorttext synchronized" lang="en">Default program name</p>
    c_titlebar        TYPE gui_title         VALUE 'TITLE_REUSABLE' ##no_text,
    "! <p class="shorttext synchronized" lang="en">Default GUI status for popup</p>
    c_pfstatus_popup  TYPE syst_pfkey        VALUE 'POPUP_REUSABLE' ##no_text,
    "! <p class="shorttext synchronized" lang="en">Default GUI status for windows</p>
    c_pfstatus_screen TYPE syst_pfkey        VALUE 'SCREEN_REUSABLE' ##no_text,
    "! <p class="shorttext synchronized" lang="en">Name of custom container in reusable screens</p>
    c_scr_fname_ccont TYPE fieldname         VALUE 'CCONT_REUSE'  ##no_text.

* i n s t a n c e   a t t r i b u t e s
  DATA:
*   o b j e c t   r e f e r e n c e s
    "! <p class="shorttext synchronized" lang="en">Main Custom Container at screen</p>
    mo_ccont_reuse       TYPE REF TO cl_gui_custom_container.
ENDINTERFACE.
