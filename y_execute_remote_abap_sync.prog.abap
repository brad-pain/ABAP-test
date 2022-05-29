*&---------------------------------------------------------------------*
*& Report Y_EXECUTE_REMOTE_ABAP_SYNC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Y_EXECUTE_REMOTE_ABAP_SYNC.


parameters p_prog type programm OBLIGATORY.
parameters p_vrnt type raldb_vari OBLIGATORY.
PARAMETERS p_dest type RFCDEST OBLIGATORY.


call function 'Y_EXEC_ABAP_CALL_BW'
  destination p_dest
  exporting
    i_program = p_prog
    i_variant = p_vrnt
  EXCEPTIONS
    OTHERS    = 4.


if sy-subrc <> 0.
  assert 1 = 2.
endif.
