*&---------------------------------------------------------------------*
*& Report Y_DATASERVICES_JOB_TOOL
*&---------------------------------------------------------------------*
*& This program is designed to be run after BOBJDSJOB variants are
*& imported into a target system updating the setting to point to
*& the new RFC connection and Server information
*& 1) Get list of Variants to change
*& 2) Update elements
*&---------------------------------------------------------------------*
report y_dataservices_job_tool.


data: rspv    type rspc_variant.

data: gt_rspcvariant type standard table of rspcvariant.
types:begin of ty_updates,
        variante like rspcvariant-variante,
        objvers  like rspcvariant-objvers,
        type     like rspcvariant-type,
        fnam     like rspcvariant-fnam,
        low_old  like rspcvariant-low,
        low_new  like rspcvariant-low,
      end   of ty_updates.
data gt_updates type table of ty_updates.


field-symbols: <rspcvariant> type rspcvariant.

select-options:  so_rspv     for rspv .

parameters: pr_syst type rsslogsys,
            pt_syst type rsslogsys,
            pr_repo type repo,
            pt_repo type repo,
            pr_serv type rsbobjds_server,
            pt_serv type rsbobjds_server.

selection-screen skip 2.

parameters: p_test type boolean default abap_true AS CHECKBOX.



at selection-screen on value-request for so_rspv-low.
  perform f4_rspv.

at selection-screen on value-request for so_rspv-high.
  perform f4_rspv.

at selection-screen on value-request for pt_syst.
  perform f4_dest.

at selection-screen on value-request for pt_repo.
  perform f4_repo.

at selection-screen on value-request for  pt_serv .
  perform f4_jobserv.



start-of-selection.

  if pt_syst is not initial.
    select *
    into table gt_rspcvariant
    from rspcvariant
    where type = 'BOBJDSJOB'
    and fnam = 'DEST'
    and variante in  so_rspv.

    loop at gt_rspcvariant assigning <rspcvariant>.
      if pr_syst is initial.
        <rspcvariant>-low =  pt_syst.
        perform update_variant using <rspcvariant>.
      elseif pr_syst = <rspcvariant>-low.
        <rspcvariant>-low =  pt_syst.
        perform update_variant using <rspcvariant>.
      endif.
    endloop.
    commit work.

  endif.

  if pt_repo is not initial.
    select *
    into table gt_rspcvariant
    from rspcvariant
    where type = 'BOBJDSJOB'
    and fnam = 'REPO'
    and variante in  so_rspv.

    loop at gt_rspcvariant assigning <rspcvariant>.
      if pr_repo is initial.
        <rspcvariant>-low =  pt_repo.
        perform update_variant using <rspcvariant>.
      elseif pr_repo = <rspcvariant>-low.
        <rspcvariant>-low =  pt_repo.
        perform update_variant using <rspcvariant>.
      endif.

    endloop.
    commit work.

  endif.

  if pt_serv is not initial.
    select *
    into table gt_rspcvariant
    from rspcvariant
    where type = 'BOBJDSJOB'
    and fnam = 'SERVER'
    and variante in  so_rspv.

    loop at gt_rspcvariant assigning <rspcvariant>.
      if pr_serv is initial.
        <rspcvariant>-low =  pt_serv.
        perform update_variant using <rspcvariant>.
      elseif pr_serv = <rspcvariant>-low.
        <rspcvariant>-low =  pt_serv.
        perform update_variant using <rspcvariant>.
      endif.

    endloop.
    commit work.

  endif.

  perform output_report.


form f4_dest .
  data: lt_return type table of ddshretval,
        ls_return like line of lt_return.

  call function 'F4IF_FIELD_VALUE_REQUEST'
    exporting
      tabname           = 'SM59_ALVNODE'
      fieldname         = 'RFCDEST'
    tables
      return_tab        = lt_return
    exceptions
      field_not_found   = 1
      no_help_for_field = 2
      inconsistent_help = 3
      no_values_found   = 4
      others            = 5.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  read table lt_return index 1 into ls_return.

  if sy-subrc = 0 and ls_return-fieldval ne space.
    pt_syst = ls_return-fieldval.
  else.
    exit.
  endif.
endform.

form f4_repo.
  data: lt_repo     type rsbobjds_t_repositories,
        lt_messages type rsbobjds_t_messages,

        return      type table of ddshretval with header line.

  if  pt_syst is initial.
    exit.
  endif.
  call function 'RSBOBJDS_REPOSITORY_GET_LIST'
    destination pt_syst
    tables
      e_t_repositories = lt_repo
      e_t_messages     = lt_messages.


  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield    = 'NAME'
      value_org   = 'S'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'RSBOBJDS_S_REPOSITORIES'
    tables
      value_tab   = lt_repo
      return_tab  = return.

  pt_repo = return-fieldval.

endform.

form f4_jobserv.
  data: lt_jobserv  type rsbobjds_t_jobserver,
        lt_messages type rsbobjds_t_messages,

        return      type table of ddshretval with header line.

  if  pt_syst is initial or  pt_repo is initial.
    exit.
  endif.

  call function 'RSBOBJDS_JOBSERVER_GET_LIST'
    destination pt_syst
    exporting
      i_repository  = pt_repo
    tables
      e_t_jobserver = lt_jobserv
      e_t_messages  = lt_messages.


  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield    = 'JOBSERVER'
      value_org   = 'S'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'RSBOBJDS_S_JOBSERVER'
    tables
      value_tab   = lt_jobserv
      return_tab  = return.

  pt_serv = return-fieldval.

endform.

form f4_rspv.
  data: lt_rspvattr type standard table of rspcvariantattr,
        return      type table of ddshretval with header line.

  select *
  into table lt_rspvattr
  from rspcvariantattr
  where type = 'BOBJDSJOB'
  and objvers = 'A'.


  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield    = 'VARIANTE'
      value_org   = 'S'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'RSPCVARIANTATTR'
    tables
      value_tab   = lt_rspvattr
      return_tab  = return.



endform.
form update_variant using i_rspcvariant type rspcvariant.

  data l_variant                type ref to cl_rspc_variant.
  data lt_rspcvariant           type rspc_t_variant.
  field-symbols <l_rspcvariant> like line of lt_rspcvariant.
  field-symbols <l_updates>     like line of gt_updates.

* instantiate the class
  l_variant =  cl_rspc_variant=>create( exporting    i_type      = i_rspcvariant-type
                                                  i_variant      = i_rspcvariant-variante
                                                  i_objvers      = i_rspcvariant-objvers
                                                  i_no_transport = abap_true ).

* get the variant details out of protected memory
  l_variant->get_info( importing e_t_rspcvariant = lt_rspcvariant ).


* change the required line
  read table lt_rspcvariant  assigning <l_rspcvariant>
                             with key type = i_rspcvariant-type
                                     variante = i_rspcvariant-variante
                                     objvers  = i_rspcvariant-objvers
                                     lnr      = i_rspcvariant-lnr.

  append initial line to gt_updates assigning <l_updates>.
  move i_rspcvariant-variante to <l_updates>-variante.
  move i_rspcvariant-objvers  to <l_updates>-objvers.
  move i_rspcvariant-type     to <l_updates>-type.
  move i_rspcvariant-fnam     to <l_updates>-fnam.
  move i_rspcvariant-low      to <l_updates>-low_new.
  move <l_rspcvariant>-low    to <l_updates>-low_old.

  check p_test is initial.

* perform the DB update if not in test mode.
  move-corresponding i_rspcvariant to <l_rspcvariant>.

* pass changed variant details back into save
  l_variant->save( exporting i_t_rspcvariant = lt_rspcvariant ).


endform.
form output_report.

  try.
      cl_salv_table=>factory( importing r_salv_table = data(l_alv)
                              changing  t_table      = gt_updates ).
    catch cx_salv_msg.
  endtry.

* set the column hedings from the fieldcat.
  data(l_columns) = l_alv->get_columns( ).
  data l_col_text type scrtext_L.
  data l_col_text_S type scrtext_S.
  data l_col_text_M type scrtext_M.
  try.
      data(l_column) = l_columns->get_column( 'LOW_OLD' ).
    catch cx_salv_not_found.
  endtry.
  move 'Old' to l_col_text.
  move 'Old' to l_col_text_S.
  move 'Old' to l_col_text_M.
  l_column->set_LONG_text( l_col_text ).
  l_column->set_SHORT_text( l_col_text_S ).
  l_column->set_MEDIUM_text( l_col_text_M ).
*  l_column->set_output_length( 20 ).

  try.
      l_column = l_columns->get_column( 'LOW_NEW' ).
    catch cx_salv_not_found.
  endtry.
  move 'New' to l_col_text.
  move 'New' to l_col_texT_S.
  move 'New' to l_col_text_M.
  l_column->set_LONG_text( l_col_text ).
  l_column->set_SHORT_text( l_col_text_S ).
  l_column->set_MEDIUM_text( l_col_text_M ).
*  l_column->set_output_length( 20 ).

  l_alv->display( ).


endform.
