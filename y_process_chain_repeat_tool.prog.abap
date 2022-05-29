report y_process_chain_repeat_tool.
*&---------------------------------------------------------------------*
*& Report Y_PROCESS_CHAIN_REPEAT_TOOL
*&---------------------------------------------------------------------*
*& This program is designed to update selected process chains that
*& contain info pack variants to enable repetion settings to be
*& maintained.
*&---------------------------------------------------------------------*


data: rspv    type rspc_variant.
data: chain   type rspc_chain.
data: type    type rspc_type.

data: gt_rspcvariant type standard table of rspcvariant.
types:begin of ty_updates,
        chain_id          like rspcchain-chain_id,
        variante          like rspcchain-variante,
        auto_repeat_old   like rspcchain-auto_repeat,
        auto_repeat_new   like rspcchain-auto_repeat,
        repeat_trials_old like rspcchain-repeat_trials,
        repeat_trials_new like rspcchain-repeat_trials,
      end   of ty_updates.
data gt_updates type table of ty_updates.


field-symbols: <rspcvariant> type rspcvariant.

parameters p_wait type rspc_seconds.
parameters p_rept type rspc_times.

select-options:  so_chain    for chain obligatory.
select-options:  so_rspv     for rspv.
select-options:  so_type     for type default 'DTP_LOAD'.


selection-screen skip 2.

parameters: p_test type boolean default abap_true as checkbox.


at selection-screen on value-request for so_chain-low.
  perform f4_rspv using 'CHAIN_ID'.

at selection-screen on value-request for so_chain-high.
  perform f4_rspv using 'CHAIN_ID'.


at selection-screen on value-request for so_rspv-low.
  perform f4_rspv using 'VARIANTE'.

at selection-screen on value-request for so_rspv-high.
  perform f4_rspv using 'VARIANTE'.

at selection-screen on value-request for so_type-low.
  perform f4_rspv using 'RSPC_TYPE'.

at selection-screen on value-request for so_type-high.
  perform f4_rspv using 'RSPC_TYPE'.


* begin of BD1K911320
*DATA: GT_RSPCVARIANT TYPE STANDARD TABLE OF RSPCVARIANT.

*FIELD-SYMBOLS: <RSPCVARIANT> TYPE RSPCVARIANT.

start-of-selection.
* end of   BD1K911320

  select *
  into table @data(gt_rspcchain)
  from rspcchain
  where type in @so_type
  and chain_id in @so_chain
  and variante in  @so_rspv
  and objvers = 'M'.

  loop at gt_rspcchain assigning field-symbol(<rspcchain>).
    perform update_variant using <rspcchain>.
  endloop.




  perform output_report.




form f4_rspv using p_field.
  data return      type table of ddshretval with header line.

  case p_field.
    when 'VARIANTE'.
      select *
      into table @data(lt_rspvariant)
      from rspcchain
      WHERE objvers = 'A'.
    WHEN 'CHAIN_ID'.
      SELECT DISTINCT CHAIN_ID
      into table @lt_rspvariant
      from rspcchain
      WHERE objvers = 'A'.
    WHEN 'RSPC_TYPE'.
      SELECT DISTINCT TYPE
      into table @lt_rspvariant
      from rspcchain
      where objvers = 'A'.
  ENDCASE.


  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield    = p_field
      value_org   = 'S'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = p_field
    tables
      value_tab   = lt_rspvariant
      return_tab  = return.



endform.
form update_variant using i_chain type rspcchain.

  data l_chain                type ref to cl_rspc_chain.
  data lt_rspcvariant           type rspc_t_variant.
  field-symbols <l_rspcvariant> like line of lt_rspcvariant.

* instantiate the class
  create object l_chain
    exporting
      i_chain         = i_chain-chain_id
      i_no_transport  = 'X'
      i_with_dialog   = ' '
      i_objvers       = 'M'
    exceptions
      aborted_by_user = 1
      not_unique      = 2
      others          = 3.

  data(lt_chain) = l_chain->p_t_chain.

  loop at lt_chain assigning field-symbol(<l_chain>)
     where variante = i_chain-variante.
    append initial line to gt_updates assigning field-symbol(<l_updates>).
    move-corresponding <l_chain> to <l_updates>.
    move p_wait                  to <l_updates>-auto_repeat_new.
    move p_rept                  to <l_updates>-repeat_trials_new.
    move <l_chain>-auto_repeat   to <l_updates>-auto_repeat_old.
    move <l_chain>-repeat_trials to <l_updates>-repeat_trials_old.
    check p_test = abap_false.
    l_chain->set_autorepeat( exporting i_s_process = <l_chain>
                                       i_auto_repeat = p_wait
                                       i_repeat_trials = p_rept ).
    l_chain->activate( exporting i_noplan = abap_true ).

  endloop.



endform.
form output_report.

  try.
      cl_salv_table=>factory( importing r_salv_table = data(l_alv)
                              changing  t_table      = gt_updates ).
    catch cx_salv_msg.
  endtry.

* set the column hedings from the fieldcat.
  data(l_columns) = l_alv->get_columns( ).
  data l_col_text      type scrtext_m.
  data l_col_text_long type scrtext_l.
  try.
      data(l_column) = l_columns->get_column( 'AUTO_REPEAT_OLD' ).
    catch cx_salv_not_found.
  endtry.
  move 'Delay Seconds Old' to l_col_text.
  l_column->set_medium_text( l_col_text ).
  l_column->set_output_length( 20 ).

  try.
      l_column = l_columns->get_column( 'AUTO_REPEAT_NEW' ).
    catch cx_salv_not_found.
  endtry.
  move 'Delay Seconds New' to l_col_text.
  l_column->set_medium_text( l_col_text ).
  l_column->set_output_length( 20 ).

  try.
      l_column = l_columns->get_column( 'REPEAT_TRIALS_OLD' ).
    catch cx_salv_not_found.
  endtry.
  move 'Repeat Count Old' to l_col_text_long.
  l_column->set_long_text( l_col_text_long ).
  l_column->set_output_length( 20 ).

  try.
      l_column = l_columns->get_column( 'REPEAT_TRIALS_NEW' ).
    catch cx_salv_not_found.
  endtry.
  move 'Repeat Count New' to l_col_text_long.
  l_column->set_long_text( l_col_text_long ).
  l_column->set_output_length( 20 ).


  l_alv->display( ).


endform.
