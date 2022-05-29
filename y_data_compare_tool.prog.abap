*&---------------------------------------------------------------------*
*& Report y_data_compare_tool
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report y_data_compare_tool.


data g_alv_selections type ref to cl_salv_table.

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
* Handle double click from mismatch data reprot
*---------------------------------------------------------------------*
class lcl_handle_events definition.
  public section.
    methods:
      on_double_click for event double_click of cl_salv_events_table
        importing row column.

endclass.

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*---------------------------------------------------------------------*
class lcl_handle_events implementation.
  method on_double_click.
    perform on_double_click using row.
  endmethod.

endclass.

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
* Handle double click from Selections list display report
*---------------------------------------------------------------------*
class lcl_handle_events_getsel definition.
  public section.
    methods:
      on_double_click_getsel for event double_click of cl_salv_events_table
        importing row column.

endclass.

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*---------------------------------------------------------------------*
class lcl_handle_events_getsel implementation.
  method on_double_click_getsel.
    perform on_double_click_getsel using row.
  endmethod.

endclass.

data: g_events type ref to lcl_handle_events.
data: g_events_getsel type ref to lcl_handle_events_getsel.


type-pools rsds.

selection-screen begin of line.
selection-screen comment +0(15) text-002.
selection-screen position 23.
parameters p_pkg type db2cint4 default 50000.
selection-screen end of line.
selection-screen begin of line.
selection-screen comment +0(15) text-003.
selection-screen position 23.
parameters p_rfc type rfcdest default 'B01' obligatory.
selection-screen end of line.
selection-screen begin of line.
selection-screen comment +0(10) text-001.
selection-screen position 23.
parameters p_ecc type  dd02l-tabname obligatory.
selection-screen position 55.
*selection-screen comment +0(10) text-007.
parameters p_split(15) NO-DISPLAY.
selection-screen end of line.
selection-screen begin of line.
selection-screen comment +0(15) text-004.
selection-screen position 23.
parameters p_boh type  dd02l-tabname obligatory.
selection-screen end of line.
selection-screen begin of line.
*selection-screen comment +0(22) text-006.
selection-screen position 23.
parameters p_select  default abap_true no-display.
selection-screen end of line.

selection-screen skip 1.

selection-screen begin of line.
selection-screen comment +0(20) text-005.
selection-screen end of line.

selection-screen begin of line.
parameters p_ecc1(15).
selection-screen position 20.
parameters p_op1(5).
selection-screen position 30.
parameters p_bw1(15).
selection-screen end of line.

selection-screen begin of line.
parameters p_ecc2(15).
selection-screen position 20.
parameters p_op2(5).
selection-screen position 30.
parameters p_bw2(15).
selection-screen end of line.

selection-screen begin of line.
parameters p_ecc3(15).
selection-screen position 20.
parameters p_op3(5).
selection-screen position 30.
parameters p_bw3(15).
selection-screen end of line.

selection-screen begin of line.
parameters p_ecc4(15).
selection-screen position 20.
parameters p_op4(5).
selection-screen position 30.
parameters p_bw4(15).
selection-screen end of line.

selection-screen begin of line.
parameters p_ecc5(15).
selection-screen position 20.
parameters p_op5(5).
selection-screen position 30.
parameters p_bw5(15).
selection-screen end of line.

selection-screen begin of line.
parameters p_screen radiobutton group mode.
selection-screen position 3.
selection-screen comment +0(20) text-011.
selection-screen end of line.
selection-screen begin of line.
parameters p_file   radiobutton group mode.
selection-screen position 3.
selection-screen comment +0(30) text-012.
selection-screen end of line.

selection-screen skip 3.
selection-screen begin of line.
parameters p_igsel  radiobutton group sel.
selection-screen position 3.
selection-screen comment +0(17) text-010.
selection-screen end of line.
selection-screen begin of line.
parameters p_getsel radiobutton group sel.
selection-screen position 3.
selection-screen comment +0(17) text-009.
selection-screen position 20.
parameters p_getvar(30) type c.
selection-screen end of line.
selection-screen begin of line.
parameters p_savsel radiobutton group sel.
selection-screen position 3.
selection-screen comment +0(15) text-008.
selection-screen position 20.
parameters p_savvar(30) type c.
selection-screen end of line.

selection-screen skip 2.

selection-screen begin of line.
selection-screen pushbutton 2(20) but1 user-command msel.
selection-screen pushbutton 35(20) but2 user-command mdel.
selection-screen end of line.


types: begin of lty_mapping,
         ecc_field(15),
         operator(5),
         bw_field(15),
       end   of lty_mapping.

types: begin of lty_mismatch,
         row   type i,
         index type i,
       end   of lty_mismatch.



data gt_options       like table of /sapds/rfc_db_opt.
data gt_options_split like table of /sapds/rfc_db_opt.
data gt_options_split_range like table of /sapds/rfc_db_opt.
data gt_BW_range   type rsds_trange.
data gt_fields        like table of rfc_db_fld.
data gt_data          like table of /sapds/tab2048.
data gt_mapping       type table of lty_mapping.
data gt_mismatch      type table of lty_mismatch.
data g_ydcomp_sels    type ydcomp_sels.
data g_ydcomp_field   type ydcomp_field.
data g_ydcomp_texts   type ydcomp_texts.
data g_selvar         type ydcomp_field-id.
data: begin of g_variant,
        name(30),
      end   of g_variant.
data gt_variant like table of g_variant.



field-symbols <lt_zpox> type standard table.
field-symbols <l_zpox> type any.
field-symbols <lt_zpo1> type standard table.
field-symbols <lt_data1> type any table.
field-symbols <lt_data_collect> type any table.

initialization.
  move 'Choose Selections' to but1.
  move 'Delete Selections' to but2.

at selection-screen.
  if p_savsel is not initial and p_savvar is initial.
    message e000(0k) with 'Must Specify Saved Selections Name'.
  endif.
  if sy-ucomm = 'MSEL'.
    perform  choose_selections.
  endif.

  if sy-ucomm = 'MDEL'.
    perform  delete_selections.
  endif.

start-of-selection.

  get time stamp field data(start).

*  if p_getsel = abap_true and g_selvar is initial.
  if p_getsel = abap_true.
    if p_getvar is initial.
    perform get_selections changing g_selvar.
    else.
      move p_getvar to g_selvar.
    endif.
  endif.

  if p_screen = abap_true.
    if p_ecc1 is not initial.
      append initial line to gt_mapping assigning field-symbol(<l_mapping>).
      move p_ecc1 to <l_mapping>-ecc_field.
      move p_bw1  to <l_mapping>-bw_field.
      move p_op1  to <l_mapping>-operator.
    endif.
    if p_ecc2 is not initial.
      append initial line to gt_mapping assigning <l_mapping>.
      move p_ecc2 to <l_mapping>-ecc_field.
      move p_bw2  to <l_mapping>-bw_field.
      move p_op2  to <l_mapping>-operator.
    endif.
    if p_ecc3 is not initial.
      append initial line to gt_mapping assigning <l_mapping>.
      move p_ecc3 to <l_mapping>-ecc_field.
      move p_bw3  to <l_mapping>-bw_field.
      move p_op3  to <l_mapping>-operator.
    endif.
    if p_ecc4 is not initial.
      append initial line to gt_mapping assigning <l_mapping>.
      move p_ecc4 to <l_mapping>-ecc_field.
      move p_bw4  to <l_mapping>-bw_field.
      move p_op4  to <l_mapping>-operator.
    endif.
    if p_ecc5 is not initial.
      append initial line to gt_mapping assigning <l_mapping>.
      move p_ecc5 to <l_mapping>-ecc_field.
      move p_bw5  to <l_mapping>-bw_field.
      move p_op5  to <l_mapping>-operator.
    endif.
  else.
    perform upload_from_file.
  endif.

  data l_fields_coll like line of gt_fields.
  loop at gt_mapping assigning <l_mapping>.
    l_fields_coll-fieldname = <l_mapping>-ecc_field.
    collect l_fields_coll into gt_fields.
  endloop.

* build a dynamic table based on the required BW structure, used to hold
* after values for select statement.
  data l_bw_data          type ref to data.
  create data l_bw_data type table of (p_boh).
  assign l_bw_data->* to <lt_zpo1>.

* build a dynamic table based on the required BW structure, used to hold
* before values for select statement.
  data l_bw_datax         type ref to data.
  create data l_bw_datax type table of (p_boh).
  assign l_bw_datax->* to <lt_zpox>.


* show selection screen for ecc and BW data if requested.
  if p_igsel = abap_false or p_split is not initial.
    data lt_dummy_range   type rsds_trange.
    perform show_selections using p_rfc
                                  p_ecc
                                  gt_options
                                  lt_dummy_range
                                  'Select ECC restrictions'.
    data lt_dummy_options like table of /sapds/rfc_db_opt.
    perform show_selections using 'NONE'
                                  p_boh
                                  lt_dummy_options
                                  gt_BW_range
                                  'Select BW restrictions'.

  endif.

  if p_split is initial.
    move gt_options to gt_options_split_range.
    perform get_ecc_data.
  else.
    loop at gt_options_split assigning field-symbol(<l_options_split>).
      clear gt_options_split_range.
      append <l_options_split> to gt_options_split_range.
* first line needs and AND and wont get it from show_selections form, add
* and AND
      if  gt_options is not initial.
        append initial line to gt_options_split_range
                         assigning field-symbol(<l_options_split_range>).
        move 'AND' to <l_options_split_range>-text.
      endif.
      append lines of gt_options to gt_options_split_range.
      perform get_ecc_data.
    endloop.
  endif.

  if <lt_zpox> is initial.
    message e000(0k) with 'No ECC Data found'.
  endif.


* build dynamic SQL based on the selection screen values - note
* will be much more flexitble to load from spreadsheet

  select * from dd03l into table @data(lt_dd03l)
                      where tabname = @p_boh
                      and as4local = 'A'.

* build characteristic relevant sql constructs
  data l_fields type string.
  data l_groupby type string.
  data l_having type string.
  data l_and type string.
  data l_or type string.
  data l_comma type string.
  data l_where type string.

  loop at lt_dd03l assigning field-symbol(<l_dd03l>) where datatype <> 'CURR'
                                         and datatype <> 'QUAN'.
*  case <l_dd03l>-fieldname.
    loop at gt_mapping assigning <l_mapping> where bw_field = <l_dd03l>-fieldname.
      l_fields = |{ l_fields } { l_comma } { <l_mapping>-bw_field } |.
      l_groupby = |{ l_groupby } { l_comma } { <l_mapping>-bw_field } |.
      l_where = |{ l_where } { l_and } { <l_mapping>-bw_field }  { <l_mapping>-operator }  @<L_ZPOX>-{ <l_mapping>-bw_field }|.
      l_having = |{ l_having } { l_and } { <l_mapping>-bw_field }  { <l_mapping>-operator }  @<L_ZPOX>-{ <l_mapping>-bw_field }|.
      move 'AND' to l_and.
      move ',' to l_comma.
    endloop.

  endloop.


* add any selections to the sql where clause
  data lt_range_1 type table of rsdsselopt.
  data lt_range_2 type table of rsdsselopt.
  data lt_range_3 type table of rsdsselopt.
  data lt_range_4 type table of rsdsselopt.
  data lt_range_5 type table of rsdsselopt.
  data lt_range_6 type table of rsdsselopt.
  data lt_range_7 type table of rsdsselopt.
  data lt_range_8 type table of rsdsselopt.
  data lt_range_9 type table of rsdsselopt.
  data lt_range_10 type table of rsdsselopt.
  read table gt_BW_range assigning field-symbol(<l_BW_range>) index 1.
  data(l_range_count) =  1.
  if <l_BW_range> is assigned.
    loop at <l_BW_range>-frange_t assigning field-symbol(<l_frange_t>).
      if <l_frange_t>-selopt_t is not initial.
        case l_range_count.
          when 1.
            move <l_frange_t>-selopt_t to lt_range_1.
            assign component 'FIELDNAME' of structure <l_frange_t> to field-symbol(<l_fieldname>).
            l_where = | { l_where } { l_and } { <l_fieldname> } IN @LT_RANGE_1|.
            move 'AND' to l_and.
            l_range_count = l_range_count + 1.
          when 2.
            move <l_frange_t>-selopt_t to lt_range_2.
            assign component 'FIELDNAME' of structure <l_frange_t> to <l_fieldname>.
            l_where = | { l_where } { l_and } { <l_fieldname> } IN @LT_RANGE_2|.
            move 'AND' to l_and.
            l_range_count = l_range_count + 1.
          when 3.
            move <l_frange_t>-selopt_t to lt_range_3.
            assign component 'FIELDNAME' of structure <l_frange_t> to <l_fieldname>.
            l_where = | { l_where } { l_and } { <l_fieldname> } IN @LT_RANGE_3|.
            move 'AND' to l_and.
            l_range_count = l_range_count + 1.
          when 4.
            move <l_frange_t>-selopt_t to lt_range_4.
            assign component 'FIELDNAME' of structure <l_frange_t> to <l_fieldname>.
            l_where = | { l_where } { l_and } { <l_fieldname> } IN @LT_RANGE_4|.
            move 'AND' to l_and.
            l_range_count = l_range_count + 1.
          when 5.
            move <l_frange_t>-selopt_t to lt_range_5.
            assign component 'FIELDNAME' of structure <l_frange_t> to <l_fieldname>.
            l_where = | { l_where } { l_and } { <l_fieldname> } IN @LT_RANGE_5|.
            move 'AND' to l_and.
            l_range_count = l_range_count + 1.
          when 6.
            move <l_frange_t>-selopt_t to lt_range_6.
            assign component 'FIELDNAME' of structure <l_frange_t> to <l_fieldname>.
            l_where = | { l_where } { l_and } { <l_fieldname> } IN @LT_RANGE_6|.
            move 'AND' to l_and.
            l_range_count = l_range_count + 1.
          when 7.
            move <l_frange_t>-selopt_t to lt_range_7.
            assign component 'FIELDNAME' of structure <l_frange_t> to <l_fieldname>.
            l_where = | { l_where } { l_and } { <l_fieldname> } IN @LT_RANGE_7|.
            move 'AND' to l_and.
            l_range_count = l_range_count + 1.
          when 8.
            move <l_frange_t>-selopt_t to lt_range_8.
            assign component 'FIELDNAME' of structure <l_frange_t> to <l_fieldname>.
            l_where = | { l_where } { l_and } { <l_fieldname> } IN @LT_RANGE_8|.
            move 'AND' to l_and.
            l_range_count = l_range_count + 1.
          when 9.
            move <l_frange_t>-selopt_t to lt_range_9.
            assign component 'FIELDNAME' of structure <l_frange_t> to <l_fieldname>.
            l_where = | { l_where } { l_and } { <l_fieldname> } IN @LT_RANGE_9|.
            move 'AND' to l_and.
            l_range_count = l_range_count + 1.
          when 10.
            move <l_frange_t>-selopt_t to lt_range_10.
            assign component 'FIELDNAME' of structure <l_frange_t> to <l_fieldname>.
            l_where = | { l_where } { l_and } { <l_fieldname> } IN @LT_RANGE_10|.
            move 'AND' to l_and.
            l_range_count = l_range_count + 1.
        endcase.
      endif.
    endloop.
  endif.



* build key figure relevant sql constructs
  data(l_kf_count) = 0.
  loop at lt_dd03l assigning <l_dd03l> where datatype = 'CURR'
                                         or datatype = 'QUAN'.


    loop at gt_mapping assigning <l_mapping> where bw_field = <l_dd03l>-fieldname.
      if l_kf_count = 0.
        l_kf_count = l_kf_count + 1.
        l_having = |{ l_having } AND (|.
      endif.

      l_fields = |{ l_fields } { l_comma } SUM( { <l_mapping>-bw_field } ) AS { <l_mapping>-bw_field } |.
      l_having = |{ l_having } { l_or } SUM( { <l_mapping>-bw_field } )  { <l_mapping>-operator }  @<L_ZPOX>-{ <l_mapping>-bw_field }|.
      move 'OR' to l_or.
      move ',' to l_comma.
    endloop.

  endloop.

* close off the AND stateemnt around key figures, they are always OR'd
  if l_kf_count > 0.
    l_having = |{ l_having } )|.
  endif.


* select the data from BW matching line by line against aggreagated
* ecc data
  data(l_row) = 0.
  data(l_index) = 0.
  loop at <lt_zpox> assigning <l_zpox>.
    l_index = l_index + 1.
    select (l_fields)
     from (p_boh)
     appending corresponding fields of table @<lt_zpo1>
    where  (l_where)
    group by (l_groupby)
    having (l_having).
    if sy-subrc = 0.
      l_row = l_row + 1.
      append initial line to gt_mismatch assigning field-symbol(<l_mismatch>).
      <l_mismatch>-row = l_row.
      <l_mismatch>-index = l_index.
    endif.
  endloop.


* create the ALV report
  if <lt_zpo1> is not initial.
    try.
        cl_salv_table=>factory( importing r_salv_table = data(lr_alv_dat)
                        changing  t_table = <lt_zpo1> ).
      catch cx_salv_msg.
    endtry.

    data(lr_functions_dat) = lr_alv_dat->get_functions( ).
    lr_functions_dat->set_all( abap_true ).

* remove all columns from display apart from ones required.
    data(l_columns) = lr_alv_dat->get_columns( ).
    data(lt_BW_fields) = l_columns->get( ).
    loop at lt_BW_fields assigning field-symbol(<l_BW_fields>).
      read table  gt_mapping transporting no fields with key bw_field = <l_BW_fields>-columnname.
      if sy-subrc <> 0.

        data(l_column) = l_columns->get_column( <l_BW_fields>-columnname ).
        l_column->set_visible( abap_false ).
      endif.
    endloop.

    get time stamp field data(end).
*    write:/ start, end.

    data lo_events     type ref to cl_salv_events_table.
    lo_events = lr_alv_dat->get_event( ).
    create object g_events.
    set handler g_events->on_double_click for lo_events.

    data(l_display_settings) = lr_alv_dat->get_display_settings( ).
    l_display_settings->set_list_header( 'Qualifying Data' ).


    call method lr_alv_dat->display.

  else.
    message i000(0k) with 'No BW Data found'.
  endif.



form get_ecc_data.

  data lv_package_size  type int4.
  data lv_rowskips      type int4.
  data lv_finished      type int1.

  lv_package_size = p_pkg.
  lv_rowskips = 0.
  lv_finished = 0.



* get ecc data
  while lv_finished = 0.
    clear gt_data.


    call function '/SAPDS/RFC_READ_TABLE'
      destination p_rfc
      keeping logical unit of work
      exporting
*       query_table          = 'EKPO'
        query_table          = p_ecc
        rowcount             = lv_package_size
        rowskips             = lv_rowskips
      tables
        options              = gt_options_split_range
        fields               = gt_fields
        data                 = gt_data
      exceptions
        table_not_available  = 1
        table_without_data   = 2
        option_not_valid     = 3
        field_not_valid      = 4
        not_authorized       = 5
        data_buffer_exceeded = 6
        others               = 7.

    case sy-subrc.
      when 1.
        message e000(0k) with 'Invalid ECC table'.
      when 2.
        message e000(0k) with 'Empty ECC table'.
      when 3.
        message e000(0k) with 'Invalid ECC option'.
      when 4.
        message e000(0k) with 'Invalid ECC field'.
      when 5.
        message e000(0k) with 'Not Auth. ECC table'.
      when 6.
        message e000(0k) with 'Data Buffer Maxout ECC table'.
    endcase.


* need to get decimal places defined for fields - rfc_read_table doesn't get
    data gt_rfc_fields type table of rfc_fields.
    call function 'RFC_GET_STRUCTURE_DEFINITION'
      destination p_rfc
      exporting
        tabname          = p_ecc
*       UCLEN            =
*   IMPORTING
*       TABLENGTH        =
      tables
        fields           = gt_rfc_fields
      exceptions
        table_not_active = 1
        others           = 2.

    if sy-subrc <> 0.
* Implement suitable error handling here
    endif.

    sort gt_rfc_fields by fieldname.

    data lt_fieldcat type lvc_t_fcat.

    loop at gt_fields assigning field-symbol(<l_fields>).
      append initial line to lt_fieldcat assigning field-symbol(<l_fieldcat>).
      move sy-tabix to <l_fieldcat>-row_pos.
      move <l_fields>-fieldname to <l_fieldcat>-fieldname.
      move <l_fields>-type      to <l_fieldcat>-inttype.
      move <l_fields>-length    to <l_fieldcat>-intlen.
      read table gt_rfc_fields assigning field-symbol(<l_rfc_fields>)
                    with key fieldname = <l_fields>-fieldname  binary search.
      move <l_rfc_fields>-decimals to <l_fieldcat>-decimals.
    endloop.

* build a dynamic table to hold ecc data based on field def.s returned. There
* may be some fiddling required here for packed fields, not sure.

    if <lt_data1> is not assigned.
      call method cl_alv_table_create=>create_dynamic_table
        exporting
          it_fieldcatalog = lt_fieldcat
        importing
          ep_table        = data(lt_data1)
        exceptions
          others          = 8.
    endif.

    if sy-subrc = 0.
      assign lt_data1->* to <lt_data1>.
    endif.

* build a copy of dynamic table to be use for COLLECT logic
    if <lt_data_collect> is not assigned.
      call method cl_alv_table_create=>create_dynamic_table
        exporting
          it_fieldcatalog = lt_fieldcat
        importing
          ep_table        = data(lt_data_collect)
        exceptions
          others          = 8.
    endif.

    if sy-subrc = 0.
      assign lt_data_collect->* to <lt_data_collect>.
    endif.


* unpack the raw data
    loop at gt_data assigning field-symbol(<l_data>).
      insert initial line into table <lt_data1> assigning field-symbol(<l_data1>).
      loop at gt_fields assigning <l_fields>.
        assign component <l_fields>-fieldname of structure <l_data1> to field-symbol(<l_field>).
        <l_field> = <l_data>+<l_fields>-offset(<l_fields>-length).
      endloop.
    endloop.


* perform a COLLECT for cases when most granular data is not requested from ECC
    loop at <lt_data1> assigning <l_data1>.
      collect <l_data1> into <lt_data_collect>.
    endloop.


* right now logic is limited to 3 fields - hardcoded. Better to change to
* uploadable list. Pick the 3 fields out of the data to be used by the
* for all entries select
    loop at <lt_data_collect> assigning field-symbol(<l_data_collect>).
*    clear ls_zpo.

      insert initial line into table <lt_zpox> assigning <l_zpox>.
      loop at gt_mapping assigning <l_mapping>.
        assign component <l_mapping>-ecc_field of structure <l_data_collect> to <l_field>.
        assign component <l_mapping>-bw_field of structure <l_zpox> to field-symbol(<l_fieldBW>).
        <l_fieldBW> = <l_field>.
      endloop.

    endloop.

    lv_rowskips = lv_rowskips + lv_package_size.

    if lines( gt_data ) < lv_package_size.
      lv_finished = 1.
    endif.
    clear:  <lt_data1>, <lt_data_collect>, gt_data.

  endwhile.

endform.


form show_selections using p_dest type rfcdest
                           p_table type  dd02l-tabname
                           pt_options like gt_options
                           pt_BW_range like gt_BW_range
                           p_title.

  data lt_range     type rsds_trange.
  data lt_rsdstabs  type table of rsdstabs.
  data lt_fieldtab  type table of rsdsfields.
  data lt_field_texts type table of rsdstexts.
  data l_selid     type rsdynsel-selid.

  data lt_options type table of /sapds/rfc_db_opt.
  data lt_fields type table of rfc_db_fld.
  data l_where type string.
  data l_where_all type string.
  data l_and type string.



* get the ECC field definitions to build dynamic selections
  call function '/SAPDS/RFC_READ_TABLE'
    destination p_dest
    keeping logical unit of work
    exporting
      no_data              = abap_true
      query_table          = p_table
      rowcount             = 0
*     rowskips             = lv_rowskips
    tables
      options              = lt_options
      fields               = lt_fields
      data                 = gt_data
    exceptions
      table_not_available  = 1
      table_without_data   = 2
      option_not_valid     = 3
      field_not_valid      = 4
      not_authorized       = 5
      data_buffer_exceeded = 6
      others               = 7.

* dynamic selection only supports 75 fields, if more than that found,
* force user to select.
  if lines( lt_fields ) > 75.
    try.
        cl_salv_table=>factory( importing r_salv_table = data(lr_alv_dat)
                        changing  t_table = lt_fields ).
      catch cx_salv_msg.
    endtry.

* set alv header - max 75 fields allowed
    data(lr_display_settings) = lr_alv_dat->get_display_settings( ).
    lr_display_settings->set_list_header( 'Max. 75 Selections Allowed' ).

* enable multiple line selections on alv
    data(lr_selections) = lr_alv_dat->get_selections( ).
    lr_selections->set_selection_mode( value = if_salv_c_selection_mode=>multiple ).

* enable full tool bar on alv
    data(lr_functions_dat) = lr_alv_dat->get_functions( ).
    lr_functions_dat->set_all( abap_true ).

* set the alv heading
    data(l_display_settings) = lr_alv_dat->get_display_settings( ).
    l_display_settings->set_list_header( 'Too Many Fields For Dynamic Selection' ).


* display the alv
    call method lr_alv_dat->display.

    data(lt_sel_rows) = lr_selections->get_selected_rows( ).

  endif.

  if lt_sel_rows is not initial.
    loop at lt_sel_rows assigning field-symbol(<l_sel_rows>).
      read table lt_fields index <l_sel_rows> assigning field-symbol(<l_fields>).
      append initial line to lt_fieldtab assigning field-symbol(<l_fieldtab>).
      move <l_fields>-fieldname to <l_fieldtab>-fieldname.
      move <l_fields>-type      to <l_fieldtab>-type.
      <l_fieldtab>-where_leng = <l_fields>-length * 2. "DOUBLE BYTE

      append initial line to lt_field_texts assigning field-symbol(<l_field_texts>).
      move <l_fields>-fieldname to <l_field_texts>-fieldname.
*      <l_field_texts>-text = | { <l_fields>-fieldtext } - { <l_fields>-fieldname }|.
      <l_field_texts>-text = <l_fields>-fieldname.

    endloop.
  else.
    loop at lt_fields assigning <l_fields>.

      if sy-tabix > 75. exit. endif.

      append initial line to lt_fieldtab assigning <l_fieldtab>.
      move <l_fields>-fieldname to <l_fieldtab>-fieldname.
      move <l_fields>-type      to <l_fieldtab>-type.
      <l_fieldtab>-where_leng = <l_fields>-length * 2. "DOUBLE BYTE

      append initial line to lt_field_texts assigning <l_field_texts>.
      move <l_fields>-fieldname to <l_field_texts>-fieldname.
*      <l_field_texts>-text = | { <l_fields>-fieldtext } - { <l_fields>-fieldname }|.
      <l_field_texts>-text = <l_fields>-fieldname.
    endloop.
  endif.

* get number range split if requested on selection screne for ECC side.
  if p_split is not initial and p_dest <> 'NONE'
  and p_getsel = abap_false. " assume ranges will be saved
    perform get_number_ranges using lt_range.
* add the split range field if not already selected in the dialog to display.
    read table lt_fieldtab with key fieldname = p_split transporting no fields.
    if sy-subrc <> 0.
      read table lt_fields assigning <l_fields> with key fieldname = p_split.
      append initial line to lt_fieldtab assigning <l_fieldtab>.
      move <l_fields>-fieldname to <l_fieldtab>-fieldname.
      move <l_fields>-type      to <l_fieldtab>-type.
      <l_fieldtab>-where_leng = <l_fields>-length * 2. "DOUBLE BYTE
      append initial line to lt_field_texts assigning <l_field_texts>.
      move <l_fields>-fieldname to <l_field_texts>-fieldname.
*      <l_field_texts>-text = | { <l_fields>-fieldtext } - { <l_fields>-fieldname }|.
      <l_field_texts>-text = <l_fields>-fieldname.
    endif.
  endif.

  if p_savsel = abap_true.
    g_ydcomp_field-id =  p_savvar.
    g_ydcomp_field-id+30 =  p_ecc.
    g_ydcomp_field-id+60 =  p_boh.
    if p_title cs 'ECC'.
      g_ydcomp_field-id+90 = 'ECC'.
    else.
      g_ydcomp_field-id+90 = 'BOH'.
    endif.
    g_ydcomp_texts-id =  g_ydcomp_field-id.
* check if overwriting
    data l_question type string.
    data l_answer type boolean.
    select single * from ydcomp_sels into @data(l_check_exist)
                    where id = @g_ydcomp_field-id.
    if sy-subrc = 0.

      l_question = |You Are About To OVERWRITE Selelction { g_selvar }|.
      call function 'POPUP_TO_CONFIRM'
        exporting
          text_question = l_question
        importing
          answer        = l_answer.

      if l_answer = '1'.
        export lt_fieldtab = lt_fieldtab to database ydcomp_field(bb)  id g_ydcomp_field-id.
        export lt_field_texts = lt_field_texts to database ydcomp_texts(bb)  id g_ydcomp_texts-id.
      else.
        clear p_savsel.
        move abap_true to p_igsel.
      endif.
    endif.
  endif.

  if p_getsel = abap_true and g_selvar is not initial.
    g_ydcomp_sels-id =  g_selvar.
    g_ydcomp_sels-id+30 =  p_ecc.
    g_ydcomp_sels-id+60 =  p_boh.
    if p_title cs 'ECC'.
      g_ydcomp_sels-id+90 = 'ECC'.
    else.
      g_ydcomp_sels-id+90 = 'BOH'.
    endif.
    g_ydcomp_field-id =  g_ydcomp_sels-id.
    g_ydcomp_texts-id =  g_ydcomp_sels-id.
    import lt_range = lt_range from database ydcomp_sels(bb) id g_ydcomp_sels-id.
    import lt_fieldtab = lt_fieldtab from database ydcomp_field(bb) id g_ydcomp_field-id.
    import lt_field_texts = lt_field_texts from database ydcomp_texts(bb) id g_ydcomp_texts-id.
  endif.

* display dynamic selections
  call function 'FREE_SELECTIONS_INIT'
    exporting
      kind             = 'F'
      field_ranges_int = lt_range
    importing
      selection_id     = l_selid
    tables
      tables_tab       = lt_rsdstabs
      fields_tab       = lt_fieldtab
      field_texts      = lt_field_texts.


  call function 'FREE_SELECTIONS_DIALOG'
    exporting
      title        = p_title
      selection_id = l_selid
      as_window    = abap_true
      tree_visible = ' '
      just_display = abap_false
      start_col    = 10
    importing
      field_ranges = lt_range
    tables
      fields_tab   = lt_fieldtab
    exceptions
      no_action    = 1
      others       = 4.

  if sy-subrc = 1.
    message e000(0k) with 'Comparison Cancelled'.
  endif.

  if p_savsel = abap_true.
    g_ydcomp_sels-id =  p_savvar.
    g_ydcomp_sels-id+30 =  p_ecc.
    g_ydcomp_sels-id+60 =  p_boh.
    if p_title cs 'ECC'.
      g_ydcomp_sels-id+90 = 'ECC'.
    else.
      g_ydcomp_sels-id+90 = 'BOH'.
    endif.
    export lt_range = lt_range to database ydcomp_sels(bb) from g_ydcomp_sels id g_ydcomp_sels-id.
  endif.





  data lt_selopt type table of ddshselopt.
  data l_range_count type i.
  data lt_range_split type rsds_selopt_t.
  field-symbols <lt_selopt_t> type rsds_selopt_t.
  loop at lt_fieldtab assigning <l_fieldtab>.
    read table lt_range assigning field-symbol(<l_range>) index 1.
    if <l_range>-frange_t is not assigned.
* nothing selected, nothing to do here.
      exit.
    endif.
    loop at <l_range>-frange_t assigning <l_frange_t>
                               where fieldname = <l_fieldtab>-fieldname.
*                               AND   FIELDNAME <> P_SPLIT.
      check <l_frange_t>-selopt_t is not initial.
      l_range_count = l_range_count + 1.
      assign component 'SELOPT_T' of structure <l_frange_t> to <lt_selopt_t>.
      if <l_fieldtab>-fieldname <> p_split.
        perform extract_where_clause using <lt_selopt_t>
                                           <l_fieldtab>-fieldname
                                           l_where.
* AND the selections together
        l_where = |{ l_and } { l_where }|.
        shift l_where left deleting leading space.
*        append initial line to pt_options assigning field-symbol(<l_options>).
*        <l_options>-text = l_where.
        perform chop_where_clause using l_where
                                changing pt_options.
        move 'AND' to l_and.
      else.
        loop at <lt_selopt_t> assigning field-symbol(<l_selopt_t>).
          append <l_selopt_t> to lt_range_split.
          perform extract_where_clause using lt_range_split
                                             <l_fieldtab>-fieldname
                                             l_where.
*          append initial line to gt_options_split assigning <l_options>.
*          <l_options>-text = l_where.
          perform chop_where_clause using l_where
                                 changing gt_options_split.
          clear lt_range_split.
        endloop.
      endif.

    endloop.

  endloop.

** convert the where string to table format.
*call function 'SWA_STRING_TO_TABLE'
*  exporting
*    character_string                 = l_where_all
**   APPEND                           = ' '
*    LINE_SIZE                        = 72
**   CHECK_TABLE_TYPE                 = ' '
* IMPORTING
*   CHARACTER_TABLE                  =  pt_options
**   TOTAL_LENGTH                     =
**   LINE_SIZE_USED                   =
**   LINES_FILLED                     =
**   LAST_LINE_LENGTH                 =
** EXCEPTIONS
**   NO_FLAT_CHARLIKE_STRUCTURE       = 1
**   OTHERS                           = 2
*          .
*if sy-subrc <> 0.
** Implement suitable error handling here
*endif.


* max 10 selection fields supported
  if l_range_count > 10.
    message e000(0k) with 'Maximum 10 fields can be selected'.
  endif.


  move lt_range[] to pt_BW_range[].


endform.

form extract_where_clause using pt_range type rsds_selopt_t
                                p_fieldname type fieldname
                                p_where     type string.

  data lt_selopt type table of ddshselopt.

  loop at pt_range assigning field-symbol(<l_selopt_t>).
    append initial line to lt_selopt assigning field-symbol(<l_selopt>).
    <l_selopt>-shlpfield = p_fieldname.
    <l_selopt>-sign = <l_selopt_t>-sign.
    <l_selopt>-option = <l_selopt_t>-option.
    <l_selopt>-low = <l_selopt_t>-low.
    <l_selopt>-high = <l_selopt_t>-high.
  endloop.

  call function 'F4_CONV_SELOPT_TO_WHERECLAUSE'
    importing
      where_clause = p_where
    tables
      selopt_tab   = lt_selopt.

* RFC remote table read sometimes hates double spaces in the where clause,
* go figure?????
  replace '  ' with ' ' into p_where.
  do.
    replace '  ' with ' ' into p_where.
    if sy-subrc <> 0.
      exit.
    endif.
  enddo.

  do.
    replace 'EQ' with '=' into p_where.
    if sy-subrc <> 0.
      exit.
    endif.
  enddo.

endform.
form upload_from_file.

  data lt_filetable          type filetable.
  data l_subrc               type i.
  data lt_data               type table of lty_mapping.
  data l_filename            type string.


  call method cl_gui_frontend_services=>file_open_dialog
    exporting
      window_title            = 'SELECT IMPORT FILE'
*     file_filter             = lc_filter
      initial_directory       = 'C:\'
    changing
      file_table              = lt_filetable
      rc                      = l_subrc
    exceptions
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      others                  = 5.

* GET THE FILE NAME
* -----------------
  read table lt_filetable into l_filename index 1.

* IMPORT TO LOCAL TABLE
* ---------------------
  call method cl_gui_frontend_services=>gui_upload
    exporting
      filename                = l_filename
      has_field_separator     = abap_true
    changing
      data_tab                = lt_data
    exceptions
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      others                  = 19.

  if sy-subrc = 0.
    move lt_data to gt_mapping.
  else.
    message e000(0k) with 'No File Chosen'.
  endif.


endform.
form on_double_click using p_row.

  read table gt_mismatch assigning field-symbol(<l_mismatch>) with key row = p_row.
  read table <lt_zpo1> assigning field-symbol(<l_zpo1>) index p_row.
  read table <lt_zpox> assigning <l_zpox> index <l_mismatch>-index.

** build a dynamic table based on the required BW structure, used to hold
** after values for select statement.
*  field-symbols <lt_mismatch_data> type standard table.
*  data lt_mismatch_rows          type ref to data.
*  create data lt_mismatch_rows type table of (p_boh).
*  assign lt_mismatch_rows->* to <lt_mismatch_data>.

* add source system to mismatch lines

* describe BW table
  call method cl_abap_typedescr=>describe_by_name
    exporting
      p_name      = p_boh
    receiving
      p_descr_ref = data(l_type).

  data l_struct type ref to cl_abap_structdescr.
  l_struct ?= l_type.
  data(lt_components) = l_struct->get_components( ).

* describe additional column - RFCDEST
  data l_element type ref to cl_abap_elemdescr.
  call method cl_abap_elemdescr=>describe_by_name
    exporting
      p_name      = 'RFCDEST'
    receiving
      p_descr_ref = data(l_type_extra).

* add additional additional column to lt_components.
  l_element ?= l_type_extra.
  insert initial line into lt_components index 1 assigning field-symbol(<l_components>).
  move 'RFCDEST' to <l_components>-name.
  move l_element to <l_components>-type.

* create the dynamictable
  data l_mismatch_ref type ref to cl_abap_tabledescr.
  clear l_struct.
  l_struct = cl_abap_structdescr=>create( lt_components ).
  l_mismatch_ref = cl_abap_tabledescr=>create(
    p_line_type = l_struct
    p_table_kind = cl_abap_tabledescr=>tablekind_std ).

  data lt_mismatch_data type ref to data.
  field-symbols <lt_mismatch_data> type table.
  create data lt_mismatch_data type handle l_mismatch_ref.
  assign lt_mismatch_data->* to <lt_mismatch_data>.

* populate the dynamic table and add RFCDEST
  append initial line to <lt_mismatch_data> assigning field-symbol(<l_mismatch_data>).
  move-corresponding <l_zpo1> to <l_mismatch_data>.
  assign component 'RFCDEST' of structure <l_mismatch_data> to field-symbol(<l_rfcdest>).
  move 'BW' to <l_rfcdest>.
  append initial line to <lt_mismatch_data> assigning <l_mismatch_data>.
  move-corresponding <l_zpox> to <l_mismatch_data>.
  assign component 'RFCDEST' of structure <l_mismatch_data> to <l_rfcdest>.
  move p_rfc to <l_rfcdest>.

  if <lt_mismatch_data> is not initial.
    try.
        cl_salv_table=>factory( importing r_salv_table = data(lr_alv_dat)
                        changing  t_table = <lt_mismatch_data> ).
      catch cx_salv_msg.
    endtry.

    data(lr_functions_dat) = lr_alv_dat->get_functions( ).
    lr_functions_dat->set_all( abap_true ).

* remove all columns from display apart from ones required.
    data(l_columns) = lr_alv_dat->get_columns( ).
    data(lt_BW_fields) = l_columns->get( ).
    loop at lt_BW_fields assigning field-symbol(<l_BW_fields>).
      check <l_BW_fields>-columnname <> 'RFCDEST'.
      read table  gt_mapping transporting no fields with key bw_field = <l_BW_fields>-columnname.
      if sy-subrc <> 0.

        data(l_column) = l_columns->get_column( <l_BW_fields>-columnname ).
        l_column->set_visible( abap_false ).
      endif.
    endloop.

    data lo_events     type ref to cl_salv_events_table.
    lo_events = lr_alv_dat->get_event( ).
    create object g_events.
    set handler g_events->on_double_click for lo_events.

    data(l_display_settings) = lr_alv_dat->get_display_settings( ).

    l_display_settings->set_list_header( |'BW/{ p_rfc } Value Pair'| ).


    call method lr_alv_dat->display.

  endif.

endform.
form on_double_click_getsel using p_row.

  read table gt_variant assigning field-symbol(<l_variant>) index p_row.

  g_selvar = <l_variant>-name.

  call method g_alv_selections->close_screen.


endform.
form get_number_ranges using pt_range type rsds_trange.

  data l_more_data type boolean value abap_true.
  data lt_options type table of /sapds/rfc_db_opt.
  data lt_fields type table of rfc_db_fld.
  data lt_data type table of /sapds/tab2048.
*data gt_fields        like table of rfc_db_fld.
  field-symbols <lt_data1> type standard table.
  field-symbols <lt_ranges> type standard table.

  append initial line to lt_fields assigning field-symbol(<l_fields>).
  move p_split to <l_fields>-fieldname.


  append initial line to lt_options assigning field-symbol(<l_options>).
  <l_options>-text = |{ p_split } > ''|.


  while l_more_data = abap_true.

    clear  lt_data.
    if  <lt_data1> is assigned. clear <lt_data1>. endif.

    call function '/SAPDS/RFC_READ_TABLE'
      destination p_rfc
      exporting
        query_table = p_ecc
*       DELIMITER   = ' '
*       NO_DATA     = ' '
*       ROWSKIPS    = 0
        rowcount    = p_pkg
      tables
        options     = lt_options
        fields      = lt_fields
        data        = lt_data
* EXCEPTIONS
*       TABLE_NOT_AVAILABLE        = 1
*       TABLE_WITHOUT_DATA         = 2
*       OPTION_NOT_VALID           = 3
*       FIELD_NOT_VALID            = 4
*       NOT_AUTHORIZED             = 5
*       DATA_BUFFER_EXCEEDED       = 6
*       OTHERS      = 7
      .
    if sy-subrc <> 0.
* Implement suitable error handling here
    endif.

    data gt_rfc_fields type table of rfc_fields.
    call function 'RFC_GET_STRUCTURE_DEFINITION'
      destination p_rfc
      exporting
        tabname          = p_ecc
*       UCLEN            =
*   IMPORTING
*       TABLENGTH        =
      tables
        fields           = gt_rfc_fields
      exceptions
        table_not_active = 1
        others           = 2.

    sort gt_rfc_fields by fieldname.

    data lt_fieldcat type lvc_t_fcat.

    loop at lt_fields assigning <l_fields>.
      append initial line to lt_fieldcat assigning field-symbol(<l_fieldcat>).
      move sy-tabix to <l_fieldcat>-row_pos.
      move <l_fields>-fieldname to <l_fieldcat>-fieldname.
      move <l_fields>-type      to <l_fieldcat>-inttype.
      move <l_fields>-length    to <l_fieldcat>-intlen.
      read table gt_rfc_fields assigning field-symbol(<l_rfc_fields>)
                    with key fieldname = <l_fields>-fieldname  binary search.
      move <l_rfc_fields>-decimals to <l_fieldcat>-decimals.
    endloop.

* build a dynamic table to hold ecc data based on field def.s returned. There
* may be some fiddling required here for packed fields, not sure.

    if <lt_data1> is not assigned.
      call method cl_alv_table_create=>create_dynamic_table
        exporting
          it_fieldcatalog = lt_fieldcat
        importing
          ep_table        = data(lt_data1)
        exceptions
          others          = 8.
    endif.

    if sy-subrc = 0.
      assign lt_data1->* to <lt_data1>.
    endif.


    if <lt_ranges> is not assigned.
      read table lt_fieldcat into data(l_fieldcat) index 1.
      append initial line to lt_fieldcat assigning <l_fieldcat>.
      move l_fieldcat to <l_fieldcat>.
      <l_fieldcat>-fieldname = |{ <l_fieldcat>-fieldname }_TO|.
      call method cl_alv_table_create=>create_dynamic_table
        exporting
          it_fieldcatalog = lt_fieldcat
        importing
          ep_table        = data(lt_ranges)
        exceptions
          others          = 8.
      if sy-subrc = 0.
        assign lt_ranges->* to <lt_ranges>.
      endif.

    endif.

* unpack the raw data
    loop at lt_data assigning field-symbol(<l_data>).
      insert initial line into table <lt_data1> assigning field-symbol(<l_data1>).
      loop at lt_fields assigning <l_fields>.
        assign component <l_fields>-fieldname of structure <l_data1> to field-symbol(<l_field>).
        <l_field> = <l_data>+<l_fields>-offset(<l_fields>-length).
      endloop.
    endloop.

    read table <lt_data1> assigning <l_data1> index 1.
    assign component p_split of structure <l_data1> to <l_field>.
    append initial line to <lt_ranges> assigning field-symbol(<l_ranges>).
    assign component p_split of structure <l_ranges> to field-symbol(<l_field_from>).
    move <l_field> to <l_field_from>.
    data(l_field_to) = |{ p_split }_TO|.
    assign component l_field_to of structure <l_ranges> to field-symbol(<l_field_to>).
    read table <lt_data1> assigning <l_data1> index lines( <lt_data1> ).
    assign component p_split of structure <l_data1> to <l_field>.
    move <l_field> to <l_field_to>.
    read table lt_options assigning <l_options> index 1.
    <l_options>-text = |{ p_split } > '{ <l_field_to> }'|.


    if lines( <lt_data1> ) < p_pkg.
      l_more_data = abap_false.
    endif.

  endwhile.

  append initial line to pt_range assigning field-symbol(<l_range>).
  move 'RSDS_DUMMY' to <l_range>-tablename.
  append initial line to <l_range>-frange_t assigning field-symbol(<l_frange_t>).
  move p_split to <l_frange_t>-fieldname.
  loop at <lt_ranges> assigning <l_ranges>.
    append initial line to <l_frange_t>-selopt_t assigning field-symbol(<l_selopt_t>).
    move 'I' to <l_selopt_t>-sign.
    move 'BT' to <l_selopt_t>-option.
    assign component p_split of structure <l_ranges> to field-symbol(<low>).
    move <low> to <l_selopt_t>-low.
    assign component |{ p_split }_TO| of  structure <l_ranges> to field-symbol(<high>).
    move <high> to <l_selopt_t>-high.
  endloop.

endform.
form get_selections changing p_savvar like ydcomp_field-id.

  data p_ecc_boh(62).
  data p_ecc_pct(5).

  p_ecc_boh = '%'.
  p_ecc_boh+1 = p_ecc.
  p_ecc_boh+31 = p_boh.
  p_ecc_boh+61 = '%'.

  p_ecc_pct = |%ECC%|.

  select * from ydcomp_sels into table @data(lt_dcomp_sels)
           where id like @p_ecc_boh
           and   id like @p_ecc_pct.

  if sy-subrc <> 0.
    MESSAGE i000(0k) with 'No Selections Exist For These Tables'.
  endif.

  clear gt_variant.
  loop at lt_dcomp_sels assigning field-symbol(<l_dcomp_sels>).
    append initial line to gt_variant assigning field-symbol(<l_variant>).
    <l_variant>-name = <l_dcomp_sels>-id(30).
  endloop.

* create the ALV report
  if gt_variant is not initial.
    try.
        cl_salv_table=>factory( importing r_salv_table = g_alv_selections
                        changing  t_table = gt_variant ).
      catch cx_salv_msg.
    endtry.

    data(lr_functions_dat) = g_alv_selections->get_functions( ).
    lr_functions_dat->set_all( abap_true ).

    data lo_events     type ref to cl_salv_events_table.
    lo_events = g_alv_selections->get_event( ).
    create object g_events_getsel.
    set handler g_events_getsel->on_double_click_getsel for lo_events.

    data(l_display_settings) = g_alv_selections->get_display_settings( ).
    l_display_settings->set_list_header( 'Double Click To Choose' ).


    call method g_alv_selections->display.

  endif.

endform.
form get_selections_no_dialog changing p_savvar like ydcomp_field-id.

*  data p_ecc_boh(62).
*  data p_ecc_pct(5).
*
*  p_ecc_boh = '%'.
*  p_ecc_boh+1 = p_ecc.
*  p_ecc_boh+31 = p_boh.
*  p_ecc_boh+61 = '%'.
*
*  p_ecc_pct = |%ECC%|.
*
*  select * from ydcomp_sels into table @data(lt_dcomp_sels)
*           where id like @p_ecc_boh
*           and   id like @p_ecc_pct
*           and   name = @p_getvar.
*
*  if sy-subrc <> 0.
*    MESSAGE i000(0k) with 'No Selections Exist For These Tables'.
*  endif.
*
*  clear gt_variant.
*  loop at lt_dcomp_sels assigning field-symbol(<l_dcomp_sels>).
*    append initial line to gt_variant assigning field-symbol(<l_variant>).
*    <l_variant>-name = <l_dcomp_sels>-id(30).
*  endloop.
*
** create the ALV report
*  if gt_variant is not initial.
*    try.
*        cl_salv_table=>factory( importing r_salv_table = g_alv_selections
*                        changing  t_table = gt_variant ).
*      catch cx_salv_msg.
*    endtry.
*
*    data(lr_functions_dat) = g_alv_selections->get_functions( ).
*    lr_functions_dat->set_all( abap_true ).
*
*    data lo_events     type ref to cl_salv_events_table.
*    lo_events = g_alv_selections->get_event( ).
*    create object g_events_getsel.
*    set handler g_events_getsel->on_double_click_getsel for lo_events.
*
*    data(l_display_settings) = g_alv_selections->get_display_settings( ).
*    l_display_settings->set_list_header( 'Double Click To Choose' ).
*
*
*    call method g_alv_selections->display.
*
*  endif.

endform.
form chop_where_clause using p_where
                    changing pt_options like gt_options.

  data lt_result_tab type match_result_tab.
  data lt_result_tab_space type match_result_tab.
  data c_length type i value 72.
  data l_where type string.


  move p_where to l_where.

  do.
    find all occurrences of regex `'[^']*'` in l_where
      results lt_result_tab.

* set the length value to be the end offset instead
    loop at lt_result_tab assigning field-symbol(<l_result_main>).
      <l_result_main>-length = <l_result_main>-offset + <l_result_main>-length.
    endloop.



    find all occurrences of regex ` ` in l_where
      results lt_result_tab_space.

* set the line number for the row.
    data(l_index) =  0.
    loop at lt_result_tab_space assigning field-symbol(<l_result>).
      l_index = l_index + 1.
      <l_result>-line = l_index.
    endloop.

* find first space > c_length.
    loop at lt_result_tab_space assigning <l_result> where offset > c_length.
      exit.
    endloop.

* go back one entry - this represents potential split point
    if sy-subrc = 0.
      l_index = <l_result>-line - 1.
      read table lt_result_tab_space assigning <l_result> index l_index.

* now check if split point crosses a text literal - if no then split there,
* if yes then go back to one character before literal for split.
      loop at lt_result_tab assigning <l_result_main> where offset <= <l_result>-offset
                                                                    and length >=  <l_result>-offset.
        exit.
      endloop.
      if sy-subrc = 0.
        append initial line to pt_options assigning field-symbol(<l_options>).
        data(l_space_before) = <l_result_main>-offset.
        l_space_before = l_space_before - 1.
        move l_where+0(l_space_before) to <l_options>-text.
        shift l_where left by l_space_before places.
      else.
* chop string at lt_result_tab_space offset
        append initial line to pt_options assigning <l_options>.
        move l_where+0(<l_result>-offset) to <l_options>-text.
        shift l_where left by <l_result>-offset places.
      endif.
    else.
      append initial line to pt_options assigning <l_options>.
      move l_where to <l_options>-text.
      exit.
    endif.

  enddo.

endform.
form choose_selections.
  perform get_selections changing g_selvar.
  p_getvar = g_selvar(30).
  p_getsel = abap_true.
  clear: p_igsel, p_savsel.
endform.
form delete_selections.

  clear g_selvar.
  data l_question type string.
  data l_answer type boolean.

  perform get_selections changing g_selvar.
  if g_selvar is not initial.
    l_question = |Please Confirm DELETE Of Selections { g_selvar }|.
    call function 'POPUP_TO_CONFIRM'
      exporting
        text_question = l_question
      importing
        answer        = l_answer.

    if l_answer = '1'.
      data l_key like ydcomp_texts-id.
      l_key = g_selvar.
      l_key+30 = p_ecc.
      l_key+60 = p_boh.
      l_key+90 = '%'.
      delete from ydcomp_texts where id like l_key.
      delete from ydcomp_field where id like l_key.
      delete from ydcomp_sels where id like l_key.


    endif.
  endif.

  p_getvar = g_selvar(30).
  p_getsel = abap_true.
  clear: p_igsel, p_savsel.

endform.
