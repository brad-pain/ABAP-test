FUNCTION Y_EXEC_ABAP_CALL_BW.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_PROGRAM) TYPE  PROGRAMM
*"     VALUE(I_VARIANT) TYPE  RALDB_VARI
*"----------------------------------------------------------------------

  DATA: l_print_parameters TYPE pri_params,
        l_archi_parameters TYPE arc_params.


  l_print_parameters-pdest = 'LOCL'.
  l_print_parameters-prcop = 1.
  l_print_parameters-plist = i_program.
  l_print_parameters-prnew = abap_true.
  l_print_parameters-pexpi = 8.
  l_print_parameters-linct = 65.
  l_print_parameters-linsz = 80.
  l_print_parameters-paart = 'X_65_80'.
  l_print_parameters-prsap = 'D'.
  l_print_parameters-prrec = sy-uname.
*  l_print_parameters-prdsn = 'LIST1S'.
  l_print_parameters-armod = '1'.
  l_print_parameters-priot = 5.
  l_print_parameters-prunx = 'D'.



  SUBMIT (i_program) USING SELECTION-SET i_variant

                     TO SAP-SPOOL WITHOUT SPOOL DYNPRO
                       SPOOL PARAMETERS l_print_parameters
                       ARCHIVE PARAMETERS l_archi_parameters
                     AND RETURN.




ENDFUNCTION.
