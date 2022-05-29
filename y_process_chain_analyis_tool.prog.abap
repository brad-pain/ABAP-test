*&---------------------------------------------------------------------*
*& Report Y_PROCESS_CHAIN_ANALYIS_TOOL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Y_PROCESS_CHAIN_ANALYIS_TOOL.
**********************************************************************
* Selection Options
**********************************************************************
type-pools: slis.

data: rspc     TYPE RSPC_CHAIN,
      IOBJNM   TYPE RSDIOBJNM.

SELECT-OPTIONS:  so_rspc    FOR rspc,

                 so_iobj    FOR IOBJNM .

**********************************************************************
* End Selection Screen
*  Main Program Declarations
**********************************************************************
TYPES: BEGIN OF _output,
           dtp           TYPE rsbkdtpnm ,
           vartype        type RSTLOGO,
           DTPT          TYPE RSBKDTPTEXT,
           TF            Type  RSBKTFNM ,
           tftype        type RSTLOGO,
           TFTEXT        Type  RSBKTFTEXT,
           SOURCETYPE     type RSTLOGO,
           SOURCESUBTYPE  type RSO_TLOGO_SUBTYPE,
           SOURCENAME      type SOBJ_NAME,
           SOURCETEXT     Type  RSBKTFTEXT,
           TARGETTYPE     type RSTLOGO,
           TARGETSUBTYPE  type RSO_TLOGO_SUBTYPE,
           TARGETNAME      type SOBJ_NAME,
           TARGETTEXT     Type  RSBKTFTEXT,
           dep_type       TYPE c LENGTH 30,
           DTP_FILTER     TYPE c LENGTH 1,
           FORMULA        TYPE c LENGTH 1,
           END            TYPE c LENGTH 1,
           MASTER          TYPE c LENGTH 1,
           START          TYPE c LENGTH 1,
           ROUTINE        TYPE c LENGTH 1,
           EXPERT          TYPE c LENGTH 1,
           ODSO            TYPE c LENGTH 1,
           READS_NAV      TYPE c LENGTH 1,
           RULES          TYPE c LENGTH 1,
           hasiobj        TYPE c LENGTH 1,
           CHAIN0          TYPE RSPC_CHAIN,
          CHAIN1          TYPE RSPC_CHAIN,
          CHAIN2          TYPE RSPC_CHAIN,
          CHAIN3          TYPE RSPC_CHAIN,
          CHAIN4          TYPE RSPC_CHAIN,
          CHAIN5          TYPE RSPC_CHAIN,
          CHAIN6          TYPE RSPC_CHAIN,
          CHAIN7          TYPE RSPC_CHAIN,
          CHAIN8          TYPE RSPC_CHAIN,
          CHAIN           TYPE RSPC_CHAIN,
      end of _output,

      BEGIN OF _trfn,
        dtp     TYPE rsbkdtpnm ,
        TF      Type  RSBKTFNM ,
        TFTP    Type  RSBKTFTP,
        TFTEXT  Type  RSBKTFTEXT,
      end of _trfn,

      BEGIN OF _DTPRULE,
          dtp         TYPE rsbkdtpnm ,
          FIELD       Type  RSFIELDNM,
          SEL_ROUTINE Type  ROUTINE,
          CODEID      Type  RSCODEID,
          OBJVERS     Type  RSOBJVERS,
          LINE_NO     Type  RSLINENO,
          LINE        Type  EDPLINE,
      END OF _DTPRULE,

      BEGIN of _VARSELTAB,
         dtp          TYPE  rsbkdtpnm ,
         FIELD        Type  RSFIELDNM,
         SEL_ROUTINE  Type  ROUTINE,
         BEX_VARIABLE Type  RSZVNAM,
         BEX_PERIV    Type  PERIV,
         SEL_TYPE     Type  RSSELTYPE,
      end of _VARSELTAB,

      BEGIN of _SELTAB,
          dtp          TYPE rsbkdtpnm ,
          FIELD         Type  RSFIELDNM ,
          SIGN         Type RSSIGN ,
          OPTION       Type RSOPTION,
          LOW           Type  RSLOW,
          HIGH         Type RSHIGH,
          SEL_TYPE     Type RSSELTYPE,
      END OF _SELTAB,

      BEGIN OF _TRANS,
        TF      Type  RSTRANID ,
      END OF _TRANs,

      BEGIN OF _chain,
        CHAIN_ID  type RSPC_CHAIN,
      end of   _chain,

      BEGIN OF _DTP,
         DTP  type rsbkdtpnm,
      end of   _DTP,

       BEGIN OF _iobj,
         IOBJNM  type RSDIOBJNM,
      end of   _iobj,

      BEGIN OF _LOGDPID,
          LOGDPID TYPE RSLOGDPID,
      end of _LOGDPID,

      BEGIN OF _RSINFOCUBE,
          CUBE TYPE RSINFOCUBE,
      END OF _RSINFOCUBE,

      BEGIN OF _RSUPDID,
        UPDR  TYPE RSUPDID,
      END OF _RSUPDID,

      BEGIN OF _FLATCHAIN,
          CHAIN0  TYPE RSPC_CHAIN,
          CHAIN1  TYPE RSPC_CHAIN,
          CHAIN2  TYPE RSPC_CHAIN,
          CHAIN3  TYPE RSPC_CHAIN,
          CHAIN4  TYPE RSPC_CHAIN,
          CHAIN5  TYPE RSPC_CHAIN,
          CHAIN6  TYPE RSPC_CHAIN,
          CHAIN7  TYPE RSPC_CHAIN,
          CHAIN8  TYPE RSPC_CHAIN,
          CHAIN9  TYPE RSPC_CHAIN,
          CHAIN10  TYPE RSPC_CHAIN,
          CHAIN   TYPE RSPC_CHAIN,
          VTYPE  TYPE RSPC_TYPE,
          VARIANTE  TYPE RSPC_VARIANT,
      END OF  _FLATCHAIN,

      BEGIN OF _TEXT,
           dtp           TYPE rsbkdtpnm ,
           DTPT          TYPE RSBKDTPTEXT,
      END OF _TEXT.



data: g_objvers like rs_c_objvers-active .

**********************************************************************
* End copied delcartions
**********************************************************************
 data:  go_pr_vers_a type ref to CL_RSBK_DTP_V,

        GT_TRFN      TYPE HASHED TABLE OF _TRFN WITH UNIQUE KEY DTP TF,

        GT_RSPCCHAIN TYPE HASHED TABLE OF rspcchain WITH UNIQUE KEY
                                                          CHAIN_ID
                                                          OBJVERS
                                                          TYPE
                                                          VARIANTE
                                                          LNR,
        GS_RSPCCHAIN TYPE rspcchain,

        GT_CHAIN_LIST TYPE HASHED TABLE OF _CHAIN WITH UNIQUE KEY CHAIN_ID,

        GT_SELTAB     TYPE STANDARD TABLE OF _SELTAB,

        GT_VARSELTAB  TYPE STANDARD TABLE OF _VARSELTAB,

        GT_DTPRULE    TYPE STANDARD TABLE OF _DTPRULE,

        GT_DTP_FILTER TYPE HASHED TABLE OF _DTP WITH UNIQUE KEY DTP,

        GT_RSTRANRULE TYPE HASHED TABLE OF RSTRANRULE WITH UNIQUE KEY
                                                TRANID
                                                OBJVERS
                                                RULEID,
        GT_RSDATRNAV  TYPE HASHED TABLE OF RSDATRNAV WITH UNIQUE KEY ATRNAVNM,

        GT_RSTRANFIELD TYPE HASHED TABLE OF RSTRANFIELD WITH UNIQUE KEY
                                                      TRANID
                                                      OBJVERS
                                                      SEGID
                                                      RULEID
                                                      STEPID
                                                      PARAMTYPE
                                                      PARAMNM,

         GT_RSTRANS TYPE HASHED TABLE OF RSTRAN WITH UNIQUE KEY
                                                      TRANID
                                                      OBJVERS,

         GT_RSTRANSTEPODSO TYPE HASHED TABLE OF RSTRANSTEPODSO WITH UNIQUE KEY
                                                                 TRANID
                                                                 OBJVERS
                                                                 RULEID
                                                                 STEPID,
         GT_RSTRANSTEPMASTER TYPE HASHED TABLE OF RSTRANSTEPMASTER WITH UNIQUE KEY
                                                                TRANID
                                                                OBJVERS
                                                                RULEID
                                                                STEPID ,
         GT_TRANSOUTPUT     TYPE STANDARD TABLE OF  _Output ,

         GT_OUTPUT         TYPE STANDARD TABLE OF  _Output ,

         gt_RSPCVARIANT    type HASHED TABLE OF RSPCVARIANT WITH UNIQUE KEY
                                                  TYPE
                                                  VARIANTE
                                                  OBJVERS
                                                  LNR,
         gt_RSDIOBJ       TYPE HASHED TABLE OF _IOBJ WITH UNIQUE KEY
                                                     IOBJNM
                                                     ,
        gt_text          TYPE HASHED TABLE OF _text WITH UNIQUE KEY DTP,


        l_dtp TYPE rsbkdtpnm ,
        LS_RSBKDTP TYPE RSBKDTP,

        gt_FLATCHAIN    TYPE STANDARD TABLE OF _FLATCHAIN.

**********************************************************************
* Main Program
**********************************************************************

* Get list of process Chains
**********************************************************************
    g_objvers = 'A'.

    SELECT chain_id
    INTO TABLE GT_CHAIN_LIST
    FROM RSPCCHAINATTR
    WHERE CHAIN_ID IN SO_rspc
      AND OBJVERS = g_objvers.

* Get Iobj list
**********************************************************************
  IF LINES( SO_IOBJ ) > 0 .

  SELECT IOBJNM
  INTO TABLE gt_RSDIOBJ
  FROM RSDIOBJ
  WHERE IOBJNM IN SO_IOBJ
         AND OBJVERS = g_objvers.
  ENDIF.

* Get chain variants
**********************************************************************
  PERFORM GET_CHAIN_LIST.

  IF lines( GT_RSPCCHAIN ) > 0 .

  SELECT *
  INTO TABLE gt_RSPCVARIANT
  from  RSPCVARIANT
  FOR ALL ENTRIES IN GT_RSPCCHAIN
  WHERE
      TYPE = GT_RSPCCHAIN-TYPE
      and VARIANTE = GT_RSPCCHAIN-VARIANTE
      and objvers = 'A'.

  ENDIF.
  IF LINES( gt_RSDIOBJ ) > 0  .
    PERFORM GET_REF_IOBJ.
  ENDIF.


 LOOP AT GT_RSPCCHAIN INTO GS_RSPCCHAIN
        WHERE TYPE = 'DTP_LOAD'.

   SELECT SINGLE *
   INTO  LS_RSBKDTP
   FROM RSBKDTP
   WHERE DTP = gS_RSPCCHAIN-VARIANTE
      AND OBJVERS = G_OBJVERS.
   IF SY-SUBRC = 0.


  Free go_pr_vers_a.

  PERFORM get_DTP_DETAILS USING gS_RSPCCHAIN-VARIANTE .

    PERFORM GET_FILTER_FROM_DTP USING go_pr_vers_a.

    PERFORM GET_TRANFORMATIONS using go_pr_vers_a.

  ENDIF.

ENDLOOP.
*get tranformation detail
  SELECT *
  INTO TABLE GT_RSDATRNAV
  from  RSDATRNAV
  WHERE OBJVERS = 'A'.

  PERFORM GET_TRFN_DETAILS.

  PERFORM BUILD_OUTPUT.

  PERFORM ADD_ABAP.

  PERFORM ADD_IPAK.

  perform add_manrecs.

  PERFORM ADD_PROCESS_CHAINS.

  perform call_alv.


**********************************************************************
* End Main Program
**********************************************************************
*&---------------------------------------------------------------------*
*&      Form  GET_DTP_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DTP_DETAILS using p_dtp type rsbkdtpnm .
    types: begin of lty_s_callstack,
             stackid     type sytabix,
             r_dtp       type ref to cl_rsbk_dtp,
             r_dtp_200_b type ref to cl_rsbk_dtp_200_b,
             parent      type sydynnr,
           end of lty_s_callstack.
    data: l_callstack_pointer type sytabix.
    data: l_s_callstack  type lty_s_callstack,
          l_th_callstack type hashed table of lty_s_callstack
               with unique key stackid.
     DATA: l_actvt TYPE activ_auth,
                l_mode TYPE rsbkmode.
data l_r_dtp type ref to cl_rsbk_dtp.
*     ======= reference to model instance =========

      DATA: l_t_cmd TYPE TABLE OF rsbkcmd,
            l_s_cmd TYPE rsbkcmd.



          l_mode = rsbc_c_mode-display.
          l_s_callstack-r_dtp = cl_rsbk_dtp=>factory( p_dtp  ).
          INSERT l_s_callstack INTO TABLE l_th_callstack.
*     ---- set global parameter -------------------
          l_r_dtp = l_s_callstack-r_dtp.

            CALL METHOD cl_rsbk_dtp_200_b=>new
            EXPORTING
              i_r_dtp        = l_r_dtp
              i_mode         = l_mode
              i_display_only = RS_C_TRUE
*              i_r_navigator  = g_r_navigator
            RECEIVING
              r_r_dtp_200_b  = l_s_callstack-r_dtp_200_b.
   FREE go_pr_vers_a.
    CALL METHOD l_R_DTP->GET_OBJ_REF_OBJVERS
      EXPORTING
        I_OBJVERS          = 'A'
      RECEIVING
        R_R_VERS           = go_pr_vers_a.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_FILTER_FROM_DTP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_R_DTP  text
*----------------------------------------------------------------------*
FORM GET_FILTER_FROM_DTP  USING    P_R_DTP TYPE ref to CL_RSBK_DTP_V.

data:  lo_filter        type ref to CL_RSBC_FILTER,
       ls_seltab        type RSBK_S_SELECT,
       ls_VAR_SELECT    type MCH_VAR_SELECT,
       ls_DTPRULE       type MCH_S_SOURCECODE,
       ls_dtp_seltab    type _SELTAB,
       ls_dtp_DTPRULE   type _DTPRULE,
       ls_dtp_VARSELTAB type _VARSELTAB,
       ls_dtp           type _dtp,
       LS_TEXT_DEFAULT    Type  RSBKDTPTEXT,
       ls_teXT          TYPE _TEXT       .

CALL METHOD P_R_DTP->IF_RSBK_DTP_DISPLAY~GET_OBJ_REF_FILTER
  RECEIVING
    R_R_FILTER =  lo_filter.
    .

call METHOD P_R_DTP->GET_TEXT_DEFAULT
    RECEIVING
      R_TEXT_DEFAULT = LS_TEXT_DEFAULT.

   ls_dtp-DTP  = P_R_DTP->n_DTP.

  IF lines( lo_filter->N_T_SELTAB ) > 0.
     ls_dtp_seltab-DTP  = P_R_DTP->n_DTP.

    READ TABLE GT_DTP_FILTER TRANSPORTING NO FIELDS
      WITH TABLE KEY dtp = ls_dtp-DTP.

    IF sy-subrc ne 0.
      INSERT ls_dtp INTO TABLE GT_DTP_FILTER.
    ENDIF.

    LOOP AT lo_filter->N_T_SELTAB INTO ls_seltab .

      MOVE-CORRESPONDING LS_SELTAB to LS_DTP_SELTAB.

      APPEND LS_DTP_SELTAB to GT_SELTAB.

    ENDLOOP.

  endif.

  IF lines( lo_filter->N_T_VARSELTAB ) > 0.

    READ TABLE GT_DTP_FILTER TRANSPORTING NO FIELDS
      WITH TABLE KEY dtp = ls_dtp-DTP.

    IF sy-subrc ne 0.
      INSERT ls_dtp INTO TABLE GT_DTP_FILTER.
    ENDIF.

    ls_dtp_VARSELTAB-DTP  = P_R_DTP->n_DTP.

    LOOP AT lo_filter->N_T_VARSELTAB INTO ls_VAR_SELECT .

      MOVE-CORRESPONDING ls_VAR_SELECT to LS_DTP_VARSELTAB.

      APPEND LS_DTP_VARSELTAB to GT_VARSELTAB.

    ENDLOOP.

  endif.

  READ TABLE GT_TEXT TRANSPORTING NO FIELDS
  WITH KEY DTP = ls_dtp-DTP.

   IF sy-subrc ne 0.
      LS_TEXT-DTP =  ls_dtp-DTP.
      LS_TEXT-DTPT     = LS_TEXT_DEFAULT.
      INSERT LS_TEXT INTO TABLE GT_TEXT.
    ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_TRANFORMATIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GO_PR_VERS_A  text
*----------------------------------------------------------------------*
FORM GET_TRANFORMATIONS  USING    P_PR_VERS_A TYPE ref to CL_RSBK_DTP_V.

DATA: LT_TRANSFORM type RSBK_T_TRANSFORMATION,
      ls_transform  type RSBKDYNP201_TAB ,
      ls_trfn       TYPE _TRFN.

  call METHOD P_PR_VERS_A->IF_RSBK_DTP_DISPLAY~GET_T_TRANSFORMATION
    RECEIVING R_T_TRANSFORM = LT_TRANSFORM.

  ls_trfn-DTP  = P_PR_VERS_A->n_DTP.

  LOOP AT LT_TRANSFORM into Ls_TRANSFORM.

      MOVE-CORRESPONDING LS_TRANSFORM to ls_TRFN.

      READ TABLE GT_TRFN TRANSPORTING NO FIELDS
      WITH TABLE KEY DTP = ls_trfn-DTP
                     TF =  ls_trfn-TF.

      insert ls_trfn into table GT_TRFN.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_CHAIN_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_CHAIN_LIST  text
*----------------------------------------------------------------------*
FORM GET_CHAIN_LIST .

    DATA: lt_CHAIN          TYPE HASHED TABLE OF _CHAIN WITH UNIQUE KEY CHAIN_ID,
          lt_CHAIN_NEW      TYPE HASHED TABLE OF _CHAIN WITH UNIQUE KEY CHAIN_ID,
          LS_CHAIN          TYPE _CHAIN,

          LT_RSPCCHAIN      TYPE STANDARD TABLE OF RSPCCHAIN,
          LS_RSPCCHAIN      TYPE RSPCCHAIN,
          LS_FLATCHAIN      TYPE _FLATCHAIN,
          LS_FLATCHAIN2     TYPE _FLATCHAIN,
          LV_LOOPCNT        TYPE int4,
          LV_FNAME          TYPE c LENGTH 20.
FIELD-SYMBOLS: <CHAIN> TYPE RSPC_CHAIN.
  LOOP AT GT_CHAIN_LIST INTO LS_CHAIN.

      INSERT LS_CHAIN INTO TABLE LT_CHAIN_NEW.

  ENDLOOP.
  LV_LOOPCNT = 0.
  WHILE LINES( lt_CHAIN_NEW ) > 0  .

      SELECT *
      INTO CORRESPONDING FIELDS OF TABLE  LT_RSPCCHAIN
      FROM RSPCCHAIN
      FOR ALL ENTRIES IN LT_CHAIN_NEW
      WHERE OBJVERS = 'A'
        AND CHAIN_ID = LT_CHAIN_NEW-CHAIN_ID.

      CLEAR LT_CHAIN_NEW.

      LOOP AT LT_RSPCCHAIN INTO LS_RSPCCHAIN .

          IF LS_RSPCCHAIN-TYPE = 'CHAIN'.

              READ TABLE LT_CHAIN_NEW TRANSPORTING NO FIELDS
                WITH TABLE KEY CHAIN_ID = LS_RSPCCHAIN-VARIANTE.

              IF SY-SUBRC NE 0.

                  LS_CHAIN-CHAIN_ID = LS_RSPCCHAIN-VARIANTE.

                  INSERT LS_CHAIN INTO TABLE LT_CHAIN_NEW.

              ENDIF.

          ENDIF.

         READ TABLE GT_RSPCCHAIN TRANSPORTING NO FIELDS
                WITH TABLE KEY
                       CHAIN_ID   = LS_RSPCCHAIN-CHAIN_ID
                       OBJVERS    = LS_RSPCCHAIN-OBJVERS
                       TYPE       = LS_RSPCCHAIN-TYPE
                       VARIANTE   = LS_RSPCCHAIN-VARIANTE
                       LNR        = LS_RSPCCHAIN-LNR    .

         IF SY-SUBRC NE 0.
              CLEAR LS_FLATCHAIN.
               LS_FLATCHAIN-CHAIN     = LS_RSPCCHAIN-CHAIN_ID.
               LS_FLATCHAIN-VTYPE     = LS_RSPCCHAIN-TYPE.
               LS_FLATCHAIN-VARIANTE  = LS_RSPCCHAIN-VARIANTE .
               CASE LV_LOOPCNT .
               WHEN  0 .
                  LS_FLATCHAIN-CHAIN0 = LS_RSPCCHAIN-CHAIN_ID.
               WHEN OTHERS.
                  LV_FNAME = 'CHAIN' && LV_LOOPCNT.
                  ASSIGN COMPONENT  LV_FNAME OF STRUCTURE LS_FLATCHAIN TO <CHAIN>.
                  LOOP AT GT_FLATCHAIN INTO LS_FLATCHAIN2
                        WHERE VTYPE = 'CHAIN'
                          AND VARIANTE =  LS_RSPCCHAIN-CHAIN_ID.
                    LS_FLATCHAIN-CHAIN0  = LS_FLATCHAIN2-CHAIN0.
                    LS_FLATCHAIN-CHAIN1  = LS_FLATCHAIN2-CHAIN1.
                    LS_FLATCHAIN-CHAIN2  = LS_FLATCHAIN2-CHAIN2.
                    LS_FLATCHAIN-CHAIN3  = LS_FLATCHAIN2-CHAIN3.
                    LS_FLATCHAIN-CHAIN4  = LS_FLATCHAIN2-CHAIN4.
                    LS_FLATCHAIN-CHAIN5  = LS_FLATCHAIN2-CHAIN5.
                    LS_FLATCHAIN-CHAIN6  = LS_FLATCHAIN2-CHAIN6.
                    LS_FLATCHAIN-CHAIN7  = LS_FLATCHAIN2-CHAIN7.
                    LS_FLATCHAIN-CHAIN8  = LS_FLATCHAIN2-CHAIN8.
                    LS_FLATCHAIN-CHAIN9  = LS_FLATCHAIN2-CHAIN9.
                    LS_FLATCHAIN-CHAIN10 = LS_FLATCHAIN2-CHAIN10.
                    <CHAIN> = LS_RSPCCHAIN-CHAIN_ID.
                  ENDLOOP.

               ENDCASE.
               APPEND LS_FLATCHAIN TO GT_FLATCHAIN.
               INSERT LS_RSPCCHAIN INTO TABLE GT_RSPCCHAIN.

         ENDIF.

      ENDLOOP.
      LV_LOOPCNT = LV_LOOPCNT + 1.
  ENDWHILE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_TRFN_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_TRFN_DETAILS .
    DATA: ls_TRFM               TYPE _TRFN,
          LS_TRANS              TYPE _TRANS,
          LT_TRANS              TYPE STANDARD TABLE OF _TRANS,
          LS_TRAN               TYPE rstran,
          ls_output             TYPE _output,
          ls_RSTRANFIELD        TYPE RSTRANFIELD,
          LS_RSTRANSTEPMASTER   TYPE RSTRANSTEPMASTER,
          LS_RSTRANSTEPODSO     TYPE RSTRANSTEPODSO,
          LT_OUTPUT             TYPE STANDARD TABLE OF _OUTPUT,
          LS_RSDATRNAV          TYPE RSDATRNAV.

    LOOP AT GT_TRFN INTO LS_TRFM.

          LS_TRANS-TF = LS_TRFM-tf.
          APPEND ls_trans to LT_TRANS.

    ENDLOOP.

    SORT LT_TRANS.

    delete ADJACENT DUPLICATES FROM LT_TRANS.
    if lines( LT_TRANS ) = 0.

      Exit.

    endif.

    SELECT *
    INTO TABLE GT_RSTRANRULE
    from RSTRANRULE
    FOR ALL ENTRIES IN lt_trans
    WHERE OBJVERS = 'A'
      and TRANID = LT_TRANS-TF.

    SELECT *
    INTO TABLE GT_RSTRANFIELD
    from RSTRANFIELD
    FOR ALL ENTRIES IN lt_trans
    WHERE OBJVERS = 'A'
      and TRANID = LT_TRANS-TF.

    SELECT *
    INTO TABLE GT_RSTRANS
    from RSTRAN
    FOR ALL ENTRIES IN lt_trans
    WHERE OBJVERS = 'A'
      and TRANID = LT_TRANS-TF.

    SELECT *
    INTO TABLE GT_RSTRANSTEPODSO
    from RSTRANSTEPODSO
    FOR ALL ENTRIES IN lt_trans
    WHERE OBJVERS = 'A'
      and TRANID = LT_TRANS-TF.

    SELECT *
    INTO TABLE GT_RSTRANSTEPMASTER
    from RSTRANSTEPMASTER
    FOR ALL ENTRIES IN lt_trans
    WHERE OBJVERS = 'A'
      and TRANID = LT_TRANS-TF.

      LOOP AT gt_rstrans INTO ls_tran.

           CLEAR: LS_OUTPUT-FORMULA, LS_OUTPUT-END , LS_OUTPUT-START , LS_OUTPUT-EXPERT,LS_OUTPUT-MASTER , LS_OUTPUT-ODSO , LS_OUTPUT-RULES, LS_OUTPUT-READS_NAV, LS_OUTPUT-HASIOBJ, LS_OUTPUT-ROUTINE.

           ls_output-TF             = ls_tran-TRANID.
           ls_output-SOURCETYPE      = LS_TRAN-SOURCETYPE.
           ls_output-SOURCESUBTYPE  = LS_TRAN-SOURCESUBTYPE.
           ls_output-SOURCENAME      = LS_TRAN-SOURCENAME.
           ls_output-TARGETTYPE     = LS_TRAN-TARGETTYPE.
           ls_output-TARGETSUBTYPE  = LS_TRAN-TARGETSUBTYPE.
           ls_output-TARGETNAME     = LS_TRAN-TARGETNAME.
           ls_output-DEP_TYPE       = 'DIRECT'.

           IF LS_TRAN-ENDROUTINE IS NOT INITIAL.

             LS_OUTPUT-END = 'Y'.
             LS_OUTPUT-RULES = 'Y'.

           ENDIF.

           IF LS_TRAN-STARTROUTINE IS NOT INITIAL.

             LS_OUTPUT-START = 'Y'.
             LS_OUTPUT-RULES = 'Y'.

           ENDIF.

           IF LS_TRAN-EXPERT IS NOT INITIAL.

             LS_OUTPUT-EXPERT = 'Y'.
              LS_OUTPUT-RULES = 'Y'.

           ENDIF.

           READ TABLE GT_RSTRANRULE TRANSPORTING NO FIELDS
           WITH key RULETYPE = 'FORMULA'
                    OBJVERS  = 'A'
                    TRANID   = ls_tran-TRANID.

           IF SY-SUBRC = 0.

              LS_OUTPUT-FORMULA = 'Y'.
              LS_OUTPUT-RULES = 'Y'.

           ENDIF.

           READ TABLE GT_RSTRANRULE TRANSPORTING NO FIELDS
           WITH key RULETYPE = 'ROUTINE'
                    OBJVERS  = 'A'
                    TRANID   = ls_tran-TRANID.

           IF SY-SUBRC = 0.

              LS_OUTPUT-ROUTINE = 'Y'.
              LS_OUTPUT-RULES   =  'Y'.

           ENDIF.

           READ TABLE GT_RSTRANRULE TRANSPORTING NO FIELDS
           WITH key RULETYPE = 'MASTER'
                    OBJVERS  = 'A'
                    TRANID   = ls_tran-TRANID.

           IF SY-SUBRC = 0.

              LS_OUTPUT-MASTER = 'Y'.
              LS_OUTPUT-RULES = 'Y'.

           ENDIF.

           READ TABLE GT_RSTRANRULE TRANSPORTING NO FIELDS
           WITH key RULETYPE = 'ODSO'
                    OBJVERS  = 'A'
                    TRANID   = ls_tran-TRANID.

           IF SY-SUBRC = 0.

              LS_OUTPUT-ODSO = 'Y'.
              LS_OUTPUT-RULES = 'Y'.

           ENDIF.

           LOOP AT GT_RSTRANFIELD INTO LS_RSTRANFIELD
             WHERE TRANID = LS_TRAN-TRANID.
              READ TABLE GT_RSDATRNAV TRANSPORTING NO FIELDS
                WITH TABLE KEY ATRNAVNM = LS_RSTRANFIELD-FIELDNM.

              IF SY-SUBRC = 0.

                LS_OUTPUT-READS_NAV = 'Y'.
                LS_OUTPUT-RULES = 'Y'.

              ENDIF.

              READ TABLE GT_RSDIOBJ TRANSPORTING NO FIELDS
              WITH TABLE KEY IOBJNM = LS_RSTRANFIELD-FIELDNM.

              IF sy-SUBRC = 0.
                  LS_OUTPUT-HASIOBJ = 'Y'.
              ENDIF.

           ENDLOOP.

           APPEND ls_output to GT_TRANSOUTPUT .

      ENDLOOP.

      SORT GT_TRANSOUTPUT BY TF SOURCENAME TARGETNAME DEP_TYPE.

      DELETE ADJACENT DUPLICATES FROM  GT_TRANSOUTPUT COMPARING TF SOURCENAME TARGETNAME DEP_TYPE.

      LOOP AT GT_TRANSOUTPUT INTO LS_OUTPUT
        WHERE MASTER = 'Y'.

        LS_OUTPUT-DEP_TYPE = 'MD LOOKUP'.

        LOOP AT GT_RSTRANSTEPMASTER INTO LS_RSTRANSTEPMASTER
            WHERE TRANID = LS_OUTPUT-TF.

            LS_OUTPUT-SOURCETYPE = 'IOBJ'.
            LS_OUTPUT-SOURCENAME = LS_RSTRANSTEPMASTER-IOBJNM.

            APPEND LS_OUTPUT TO LT_OUTPUT.

        ENDLOOP.

      ENDLOOP.

      APPEND LINES OF LT_OUTPUT TO GT_TRANSOUTPUT.

      CLEAR LT_OUTPUT[].

      LOOP AT GT_TRANSOUTPUT INTO LS_OUTPUT
        WHERE ODSO = 'Y'.

        LS_OUTPUT-DEP_TYPE = 'DSO LOOKUP'.

        LOOP AT GT_RSTRANSTEPODSO INTO LS_RSTRANSTEPODSO
            WHERE TRANID = LS_OUTPUT-TF.
            LS_OUTPUT-SOURCETYPE = 'ODSO'.
            LS_OUTPUT-SOURCENAME = LS_RSTRANSTEPODSO-ODSOBJECT.

            APPEND LS_OUTPUT TO LT_OUTPUT.

        ENDLOOP.

      ENDLOOP.

      APPEND LINES OF LT_OUTPUT TO GT_TRANSOUTPUT.

      SORT GT_TRANSOUTPUT BY TF SOURCENAME TARGETNAME DEP_TYPE.

      DELETE ADJACENT DUPLICATES FROM  GT_TRANSOUTPUT COMPARING TF SOURCENAME TARGETNAME DEP_TYPE.

      CLEAR LT_OUTPUT[].

      LOOP AT   GT_TRANSOUTPUT INTO LS_OUTPUT
              WHERE READS_NAV = 'Y'.

        LOOP AT GT_RSTRANFIELD INTO LS_RSTRANFIELD
              WHERE TRANID = LS_OUTPUT-TF.

            READ TABLE GT_RSDATRNAV INTO LS_RSDATRNAV
              WITH TABLE KEY ATRNAVNM = LS_RSTRANFIELD-FIELDNM.

            IF SY-SUBRC = 0 .
              LS_OUTPUT-SOURCENAME = LS_RSDATRNAV-CHANM.
              LS_OUTPUT-SOURCETYPE = 'IOBJ'.
              LS_OUTPUT-DEP_TYPE = 'NAV LOAD'.
               APPEND LS_OUTPUT TO LT_OUTPUT.
            ENDIF.

        ENDLOOP.

      ENDLOOP.
      APPEND LINES OF LT_OUTPUT TO GT_TRANSOUTPUT.

      SORT GT_TRANSOUTPUT BY TF SOURCENAME TARGETNAME DEP_TYPE.

      DELETE ADJACENT DUPLICATES FROM  GT_TRANSOUTPUT COMPARING TF SOURCENAME TARGETNAME DEP_TYPE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BUILD_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_OUTPUT .
DATA: LS_OUTPUT TYPE _OUTPUT,
      LS_TRFN   TYPE _TRFN ,
      LV_FILTER TYPE C LENGTH 1
      .

  LOOP AT GT_TRFN INTO LS_TRFN.

    CLEAR LV_FILTER.

    READ TABLE GT_SELTAB TRANSPORTING NO FIELDS
      WITH KEY dtp = LS_TRFN-DTP.

    IF SY-SUBRC = 0.
      LV_FILTER = 'Y'.
    ENDIF.

   READ TABLE GT_VARSELTAB TRANSPORTING NO FIELDS
      WITH KEY dtp = LS_TRFN-DTP.

    IF SY-SUBRC = 0.
      LV_FILTER = 'Y'.
    ENDIF.

    LOOP AT GT_TRANSOUTPUT INTO LS_OUTPUT
        WHERE TF = LS_TRFN-TF.
        LS_OUTPUT-VARTYPE = 'DTPA'.
        LS_OUTPUT-TFTYPE  = 'TRFN'.
        LS_OUTPUT-DTP = LS_TRFN-DTP.
        LS_OUTPUT-DTP_FILTER = LV_FILTER.
        IF LV_FILTER = 'Y'.
          LS_OUTPUT-RULES = 'Y'.
        ENDIF.
        LS_OUTPUT-TFTEXT = LS_TRFN-TFTEXT.

        APPEND LS_OUTPUT TO GT_OUTPUT.

    ENDLOOP.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADD_ABAP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ADD_ABAP .
  DATA: LS_RSPCVARIANT  TYPE RSPCVARIANT,
        LS_RSPCVARIANT2 TYPE RSPCVARIANT,
        ls_output       TYPE _output.

  LOOP AT GT_RSPCVARIANT INTO LS_RSPCVARIANT
    WHERE ( TYPE = 'ABAP'
      OR TYPE  = 'ZABAP_RG'
      or type = 'ZABAP_BOH' )
      and FNAM = 'PROGRAM'.

      CLEAR ls_output.
      LS_OUTPUT-DTP = LS_RSPCVARIANT-LOW.
      LS_OUTPUT-DEP_TYPE = 'ABAP'.

      READ TABLE GT_RSPCVARIANT INTO LS_RSPCVARIANT2
      WITH KEY  TYPE = LS_RSPCVARIANT-TYPE
                      VARIANTE =  LS_RSPCVARIANT-VARIANTE
                      OBJVERS = 'A'
                      FNAM = 'VARIANT'                      .
      IF SY-SUBRC = 0.
          LS_OUTPUT-tF = LS_RSPCVARIANT2-LOW.
      ENDIF.
      LS_OUTPUT-vartype = 'ABAP'.
      APPEND LS_OUTPUT TO GT_OUTPUT.

  ENDLOOP.

      SORT GT_OUTPUT BY TF SOURCENAME TARGETNAME DEP_TYPE.

      DELETE ADJACENT DUPLICATES FROM  GT_OUTPUT COMPARING DTP TF SOURCENAME TARGETNAME DEP_TYPE.

ENDFORM.

FORM ADD_IPAK .
* Get a list of infopackages
* Exit if no packages are found
* Get filter information from Selection table for fields scheduler
* Get infopackage meta data from RSLDPIO
* Out Infopackage as DTP and OLTP as Target if 7.X
* Output infopackage as DTP and OLTP as Source and Update rules as TF for 3.x

DATA: LT_IPAK       TYPE STANDARD TABLE OF _LOGDPID,
      LS_IPAK       TYPE _LOGDPID,

      LT_RSUPDDAT TYPE STANDARD TABLE OF RSUPDDAT,
      Ls_RSUPDDAT TYPE  RSUPDDAT,

      LT_RSLDPSEL   TYPE HASHED TABLE OF RSLDPSEL WITH UNIQUE KEY
                                                        LOGDPID
                                                        OBJVERS
                                                        LNR,
     LS_RSLDPSEL    TYPE RSLDPSEL,

     LT_RSLDPIO     TYPE HASHED TABLE OF RSLDPIO WITH UNIQUE KEY
                                                        LOGDPID
                                                        OBJVERS,
     LS_RSLDPIO     TYPE RSLDPIO,

     LS_OUTPUT      TYPE _OUTPUT,


*     LT_RSTS        TYPE STANDARD TABLE OF RSTS,
*     LS_RSTS        TYPE RSTS,

     LT_RSUPDINFO   TYPE STANDARD TABLE OF RSUPDINFO,
     LS_RSUPDINFO   TYPE RSUPDINFO,

*     LT_RSTSRULES   TYPE STANDARD TABLE OF RSTSRULES,
*     LS_RSTSRULES   TYPE RSTSRULES,

     LT_UPDR        TYPE STANDARD TABLE OF _RSUPDID,
     LS_UPDR        TYPE _RSUPDID,

     LT_RSLDPIOT       TYPE STANDARD TABLE OF RSLDPIOT,

     LT_TADIR_RSDS      TYPE HASHED TABLE OF TADIR WITH UNIQUE KEY OBJ_NAME ,
     LV_RSDS           TYPE SOBJ_NAME,

     lt_RSPCVARIANT  TYPE STANDARD TABLE OF RSPCVARIANT,
     ls_RSPCVARIANT   TYPE RSPCVARIANT,

     LT_RSUPDKEY      TYPE STANDARD TABLE OF RSUPDKEY,
     LT_RSUPDROUT     TYPE STANDARD TABLE OF RSUPDROUT,
     LT_RSUPDINFO2    TYPE STANDARD TABLE OF RSUPDINFO.

*        data: l_t_isosmap type  rsaos_t_isosmap,
*              l_s_isosmap type  rsaos_s_isosmap,
*              l_t_ts      type  rsaos_t_ts,
*              l_s_ts      type  rsaos_s_ts.

* Get a list of infopackages

  LOOP AT GT_RSPCCHAIN INTO GS_RSPCCHAIN
          WHERE TYPE = 'LOADING' OR TYPE = 'PSAPROCESS'  OR TYPE = 'ODSPROCESS'
.
      IF GS_RSPCCHAIN-TYPE = 'PSAPROCESS' OR GS_RSPCCHAIN-TYPE =  'ODSPROCESS'.

          LOOP AT GT_RSPCVARIANT INTO LS_RSPCVARIANT
                WHERE FNAM = 'LOADING'
                  AND VARIANTE = GS_RSPCCHAIN-VARIANTE.
               LS_IPAK-LOGDPID = LS_RSPCVARIANT-LOW.
               APPEND LS_IPAK TO LT_IPAK.
          ENDLOOP.
      ELSE.
        LS_IPAK-LOGDPID = GS_RSPCCHAIN-VARIANTE.
        APPEND LS_IPAK TO LT_IPAK.
      ENDIF.
  ENDLOOP.

   SORT LT_IPAK.
   DELETE ADJACENT DUPLICATES FROM LT_IPAK.


* Exit if no packages are found

   IF lines( LT_IPAK ) = 0  .
     EXIT.
   ENDIF.

* Get filter information from Selection table for fields scheduler

   SELECT *
   INTO TABLE LT_RSLDPSEL
   FROM RSLDPSEL
   FOR ALL ENTRIES IN LT_IPAK
   WHERE OBJVERS = G_OBJVERS
        AND LOGDPID = LT_IPAK-LOGDPID.

* Get infopackage meta data from RSLDPIO

   SELECT *
   INTO TABLE  LT_RSLDPIO
   FROM RSLDPIO
   FOR ALL ENTRIES IN LT_IPAK
   WHERE OBJVERS = G_OBJVERS
        AND LOGDPID = LT_IPAK-LOGDPID.

   SELECT *
   INTO TABLE LT_TADIR_RSDS
    FROM   TADIR
    WHERE   PGMID = 'R3TR'
          AND OBJECT = 'RSDS'.


* Loop through list of infopackages
     LOOP AT LT_RSLDPIO INTO LS_RSLDPIO.
        CLEAR: LS_OUTPUT .
        LS_OUTPUT-VARTYPE = 'IPAK'.
        READ TABLE LT_RSLDPSEL INTO LS_RSLDPSEL
        WITH TABLE KEY LOGDPID = LS_RSLDPIO-LOGDPID
                       OBJVERS = LS_RSLDPIO-OBJVERS
                       LNR     = '        1'.


        CLEAR LS_OUTPUT-HASIOBJ.
        LS_OUTPUT-DTP   = LS_RSLDPIO-LOGDPID.
* GET TARGETS IF NOT IN RSLDPSEL THEN 'RSAOS_OLTPSOURCE_GET_BY_PROP'
        CLEAR LT_UPDR[].
        "dETERMINE IF 7 OR 3.X
        LV_RSDS   = LS_RSLDPIO-OLTPSOURCE.
        LV_RSDS+30 = LS_RSLDPIO-LOGSYS.
        READ TABLE LT_TADIR_RSDS TRANSPORTING NO FIELDS
            WITH KEY OBJ_NAME = LV_RSDS.
        IF SY-SUBRC = 0. " 7X
            LS_OUTPUT-DTP           =  LS_RSLDPIO-LOGDPID.
            LS_OUTPUT-targetNAME    = LS_RSLDPIO-OLTPSOURCE.
            LS_OUTPUT-targetNAME+30 = LS_RSLDPIO-LOGSYS.
            LS_OUTPUT-TARGETTYPE    = 'RSDS'.
            APPEND LS_OUTPUT TO GT_OUTPUT.
        ELSE.
          CASE LS_RSLDPIO-OLTPTYP.
            WHEN 'H'. "Hierarchies
              LS_OUTPUT-DTP           = LS_RSLDPIO-LOGDPID.
              LS_OUTPUT-SOURCETYPE    = 'RSDS'.
              LS_OUTPUT-SOURCENAME    = LS_RSLDPIO-OLTPSOURCE.
              LS_OUTPUT-SOURCENAME+30 = LS_RSLDPIO-LOGSYS.
              LS_OUTPUT-TARGETNAME    = LS_RSLDPIO-SOURCE.
              LS_OUTPUT-TARGETTYPE    = 'IOBJ'.
              LS_OUTPUT-TARGETSUBTYPE = 'HIER'.
              APPEND LS_OUTPUT TO GT_OUTPUT.
            WHEN OTHERS.
              LS_OUTPUT-DTP           = LS_RSLDPIO-LOGDPID.
              LS_OUTPUT-SOURCETYPE    = 'RSDS'.
              LS_OUTPUT-SOURCENAME    = LS_RSLDPIO-OLTPSOURCE.
              LS_OUTPUT-SOURCENAME+30 = LS_RSLDPIO-LOGSYS.
              LS_OUTPUT-TARGETNAME    = LS_RSLDPIO-SOURCE.
              LS_OUTPUT-TARGETTYPE    = 'ISTD'.

              APPEND LS_OUTPUT TO GT_OUTPUT.
              LS_OUTPUT-SOURCETYPE    = 'ISTD'.
              PERFORM GET_LIST_OF_UPDATE_RULES USING LS_RSLDPIO-LOGDPID CHANGING LT_UPDR.
*              RSUPDDAT
*              RSUPDINFO
*              RSUPDKEY
*              RSUPDROUT
              SELECT *
              INTO TABLE LT_RSUPDDAT
              FROM RSUPDDAT
              FOR ALL ENTRIES IN LT_UPDR
              WHERE UPDID = LT_UPDR-UPDR
                  and    OBJVERS = G_OBJVERS.
              LOOP AT  LT_UPDR INTO LS_UPDR.
                 CLEAR: LS_OUTPUT-ROUTINE, LS_OUTPUT-RULES, LS_OUTPUT-EXPERT , LS_OUTPUT-FORMULA, LS_OUTPUT-READS_NAV, LS_OUTPUT-MASTER, LS_OUTPUT-HASIOBJ .
                 LS_OUTPUT-tf = LS_UPDR-UPDR.
                 SELECT SINGLE  * INTO LS_RSUPDINFO
                 FROM  RSUPDINFO
                 WHERE OBJVERS = G_OBJVERS
                       AND UPDID = LS_UPDR-UPDR.
                 LS_OUTPUT-TFTYPE     = 'UPDR'.
                 LS_OUTPUT-TARGETNAME = LS_RSUPDINFO-INFOCUBE.
                 if LS_RSUPDINFO-INFOCUBE is NOT INITIAL.
                   PERFORM GET_INFOCUBE_TYPE USING LS_RSUPDINFO-INFOCUBE CHANGING LS_OUTPUT-TARGETtype.
                   IF LS_RSUPDINFO-INFOCUBE cA '$'.
                      LS_OUTPUT-TARGETNAME = REPLACE( VAL = LS_RSUPDINFO-INFOCUBE  REGEX = '\$T' with = '' ).
                      LS_OUTPUT-TARGETSUBTYPE = 'TEXT'.
                   ENDIF.
                 endif.
                 LS_OUTPUT-SOURCENAME = LS_RSUPDINFO-ISOURCE.
                 PERFORM GET_RULES_FOR_UPDR using LS_UPDR-UPDR CHANGING ls_output.
                 if LS_RSUPDINFO-EXPERTMODE = RS_C_TRUE.
                   LS_OUTPUT-EXPERT = 'Y'.
                   LS_OUTPUT-RULES = 'Y'.
                 ENDIF.
                 if LS_RSUPDINFO-STARTROUTINE is NOT INITIAL.
                   LS_OUTPUT-START = 'Y'.
                   LS_OUTPUT-RULES = 'Y'.
                 ENDIF.
                 LS_OUTPUT-DEP_TYPE = 'DIRECT'.

                 APPEND LS_OUTPUT TO GT_OUTPUT.

                 IF LS_OUTPUT-MASTER = 'Y'.

                       LOOP AT LT_RSUPDDAT INTO LS_RSUPDDAT
                          WHERE  UPDID = LS_UPDR-UPDR AND CMIOBJNM IS NOT INITIAL.
                           LS_OUTPUT-SOURCENAME = LS_RSUPDDAT-CMIOBJNM.
                           LS_OUTPUT-SOURCETYPE = 'IOBJ'.
                           LS_OUTPUT-DEP_TYPE = 'MD LOOKUP'.
                           APPEND LS_OUTPUT TO GT_OUTPUT.
                       ENDLOOP.

                 ENDIF.
               ENDLOOP.

               IF sy-subrc ne 0 .
                  LS_OUTPUT-DTP           =  LS_RSLDPIO-LOGDPID.

                 CASE  LS_RSLDPIO-OLTPTYP .
                   WHEN  'T'.
                     LS_OUTPUT-sourceNAME    = LV_RSDS.
                    LS_OUTPUT-SOURCETYPE    = 'ISIF'.
                    LS_OUTPUT-TARGETNAME    = LS_RSLDPIO-SOURCE.
                    LS_OUTPUT-TARGETTYPE    = 'IOBJ'.
                    LS_OUTPUT-TARGETSUBTYPE = 'TEXT'.
                  WHEN 'M'.
                    LS_OUTPUT-sourceNAME    = LV_RSDS.
                    LS_OUTPUT-TARGETNAME    = LS_RSLDPIO-SOURCE.
                    LS_OUTPUT-TARGETTYPE    = 'IOBJ'.
                    LS_OUTPUT-TARGETSUBTYPE = 'ATTR'.
                    LS_OUTPUT-SOURCETYPE    = 'ISIF'.
                   WHEN OTHERS.
                    LS_OUTPUT-TARGETNAME    = LV_RSDS.

                 ENDCASE.




                  APPEND LS_OUTPUT TO GT_OUTPUT.
               ENDIF.
          ENDCASE.

        ENDIF.
     ENDLOOP.


   SORT GT_OUTPUT BY TF SOURCENAME TARGETNAME DEP_TYPE.

   DELETE ADJACENT DUPLICATES FROM  GT_OUTPUT COMPARING DTP TF SOURCENAME TARGETNAME DEP_TYPE.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_REF_IOBJ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_REF_IOBJ .


DATA:     lt_IOBJ           TYPE HASHED TABLE OF _IOBJ WITH UNIQUE KEY IOBJNM,
          lt_IOBJ_NEW       TYPE HASHED TABLE OF _IOBJ WITH UNIQUE KEY IOBJNM,
          lt_IOBJ_std       TYPE STANDARD TABLE OF _IOBJ,
          LS_IOBJ           TYPE _IOBJ.

  LOOP AT  gt_RSDIOBJ  INTO LS_iobj.

      INSERT LS_IOBJ INTO TABLE LT_IOBJ_NEW.

  ENDLOOP.

  WHILE LINES( lt_IOBJ_NEW ) > 0  .

    SELECT CHANm
    INTO TABLE LT_IOBJ_STD
    FROM RSDCHA
    FOR ALL ENTRIES IN LT_IOBJ_NEW
    WHERE  CHABASNM = LT_IOBJ_NEW-IOBJNM
          AND OBJVERS = G_OBJVERS.

    SELECT CHABASNM
    INTO TABLE LT_IOBJ_STD
    FROM RSDCHA
    FOR ALL ENTRIES IN LT_IOBJ_NEW
    WHERE CHANm  = LT_IOBJ_NEW-IOBJNM
          AND OBJVERS = G_OBJVERS.

    SELECT DPANM
    APPENDING TABLE LT_IOBJ_STD
    FROM RSDDPA
    FOR ALL ENTRIES IN LT_IOBJ_NEW
    WHERE  CHABASNM = LT_IOBJ_NEW-IOBJNM
          AND OBJVERS = G_OBJVERS.


    SELECT TIMNM
    APPENDING TABLE LT_IOBJ_STD
    FROM RSDTIM
    FOR ALL ENTRIES IN LT_IOBJ_NEW
    WHERE  CHABASNM = LT_IOBJ_NEW-IOBJNM
          AND OBJVERS = G_OBJVERS.

    SELECT UNINM
    APPENDING TABLE LT_IOBJ_STD
    FROM RSDUNI
    FOR ALL ENTRIES IN LT_IOBJ_NEW
    WHERE  CHABASNM = LT_IOBJ_NEW-IOBJNM
          AND OBJVERS = G_OBJVERS.

    CLEAR lt_IOBJ_NEW.

    SORT  LT_IOBJ_NEW.

    DELETE ADJACENT DUPLICATES FROM LT_IOBJ_NEW.

    LOOP AT LT_IOBJ_STD INTO LS_IOBJ.
       READ TABLE GT_RSDIOBJ TRANSPORTING NO FIELDS
       WITH TABLE KEY IOBJNM = LS_IOBJ-IOBJNM.
       IF SY-SUBRC NE 0.
          INSERT LS_IOBJ INTO TABLE LT_IOBJ_NEW.
          INSERT LS_IOBJ INTO TABLE GT_RSDIOBJ.
       ENDIF.
    ENDLOOP.

  ENDWHILE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADD_MANRECS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ADD_MANRECS .
* get list of VARIANTS
* loop through output table
* read man recs  (YTRFN_DEP_
* APPEND records
  TYPES:  BEGIN OF _MAN_SEL ,
            TF TYPE RSBKTFNM,
          END OF _MAN_SEL.
  DATA:
      LT_TRFN_DEP TYPE HASHED TABLE OF YTRFN_DEP WITH UNIQUE KEY TF
                                                                  TARGETTYPE
                                                                  TARGETNAME,
      LS_TRFN_DEP TYPE  YTRFN_DEP,
      ls_output       TYPE _output,
      LT_MAN_SEL     TYPE STANDARD TABLE OF _MAN_SEL ,
      ls_man_sel     TYPE _MAN_SEL,
      lV_prev_tf      TYPE RSBKTFNM,
      lV_prev_dtp     TYPE RSBKTFNM,
      LT_OUTPUT       TYPE STANDARD TABLE OF _OUTPUT.

* BUILD SELECT LIST
* 1) pROCESS cHAIN variants
* 2) TRFN and Update Rules
**********************************************************************
 LOOP AT GT_RSPCCHAIN INTO GS_RSPCCHAIN.
      LS_MAN_SEL-TF = GS_RSPCCHAIN-VARIANTE.
      APPEND LS_MAN_SEL to lt_man_sel.
 ENDLOOP.

 LOOP AT gt_output INTO ls_output.
    LS_MAN_SEL-TF = ls_output-DTP.
    APPEND LS_MAN_SEL to lt_man_sel.
    LS_MAN_SEL-TF = ls_output-TF.
    APPEND LS_MAN_SEL to lt_man_sel.
 ENDLOOP.

  SORT Lt_MAN_SEL.
  DELETE LT_MAN_SEL where tf is INITIAL.
  delete ADJACENT DUPLICATES FROM LT_MAN_SEL.

  IF lines( LT_MAN_SEL ) > 0 .
     SELECT *
     INTO CORRESPONDING FIELDS OF TABLE LT_TRFN_DEP
     FROM YTRFN_DEP
     FOR ALL ENTRIES IN LT_MAN_SEL
     WHERE tf = LT_MAN_SEL-TF.

     IF sy-subrc = 0.
         LOOP AT LT_TRFN_DEP into LS_TRFN_DEP.

            CLEAR: lV_PREV_DTP, LV_PREV_TF.

            LOOP AT GT_OUTPUT INTO LS_OUTPUT
                WHERE DTP = LS_TRFN_DEP-TF.
                "CHECK IF SAME AS PREVOUS DTP AND TF IF SO DO NOTHING
                 IF LS_OUTPUT-DTP <> LV_PREV_DTP
                    OR LS_OUTPUT-TF <> LV_PREV_TF.
                "set ls_prev_dtp ls_prev_tf
                    LV_PREV_DTP = LS_OUTPUT-DTP.
                    LV_PREV_TF = LS_OUTPUT-TF.
                "COPY TRFN_DEP TO LS_OUTPUT
                     LS_OUTPUT-SOURCETYPE = LS_TRFN_DEP-TARGETTYPE.
                     LS_OUTPUT-SOURCENAME  = LS_TRFN_DEP-TARGETNAME.
                     LS_OUTPUT-DEP_TYPE     = LS_TRFN_DEP-DEP_TYPE.
                "APPEND LS_OUTPUT TO LT OUTPUT
                     APPEND LS_OUTPUT TO LT_OUTPUT.
                ENDIF.
            ENDLOOP.

            LOOP AT GT_OUTPUT INTO LS_OUTPUT
                WHERE TF = LS_TRFN_DEP-TF.
                 IF LS_OUTPUT-DTP <> LV_PREV_DTP
                    OR LS_OUTPUT-TF  <> LV_PREV_TF.
                "set ls_prev_dtp ls_prev_tf
                    LV_PREV_DTP = LS_OUTPUT-DTP.
                    LV_PREV_TF = LS_OUTPUT-TF.
                "COPY TRFN_DEP TO LS_OUTPUT
                     LS_OUTPUT-SOURCETYPE = LS_TRFN_DEP-TARGETTYPE.
                     LS_OUTPUT-SOURCENAME  = LS_TRFN_DEP-TARGETNAME.
                     LS_OUTPUT-DEP_TYPE   = LS_TRFN_DEP-DEP_TYPE.
                "APPEND LS_OUTPUT TO LT OUTPUT
                     APPEND LS_OUTPUT TO LT_OUTPUT.
                ENDIF.
            ENDLOOP.

         ENDLOOP.

         APPEND LINES OF LT_OUTPUT TO GT_OUTPUT.

         SORT GT_OUTPUT BY TF SOURCENAME TARGETNAME DEP_TYPE.

         DELETE ADJACENT DUPLICATES FROM  GT_OUTPUT COMPARING DTP TF SOURCENAME TARGETNAME DEP_TYPE.
     ENDIF.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CALL_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_ALV .
      data: ifc type slis_t_fieldcat_alv.
      data: xfc type slis_fieldcat_alv.
      data: repid type sy-repid.

      SORT GT_OUTPUT BY DTP TF SOURCENAME TARGETNAME DEP_TYPE.
      DELETE ADJACENT DUPLICATES FROM  GT_OUTPUT COMPARING DTP TF SOURCENAME TARGETNAME DEP_TYPE.
      repid = sy-repid.

      clear xfc. refresh ifc.

      clear xfc.
      xfc-reptext_ddic = 'Variant'.
      xfc-fieldname    = 'DTP'.
      XFC-OUTPUTLEN    = 30.
      xfc-tabname      = 'GT_OUTPUT'.
      append xfc to ifc.

      clear xfc.
      xfc-reptext_ddic = 'Transformation'.
      xfc-fieldname    = 'TF'.
      XFC-OUTPUTLEN    = 30.
      xfc-tabname      = 'GT_OUTPUT'.
      append xfc to ifc.

      clear xfc.
      xfc-reptext_ddic = 'Source Type'.
      xfc-fieldname    = 'SOURCETYPE'.
      XFC-OUTPUTLEN    = 4.
      xfc-tabname      = 'GT_OUTPUT'.
      xfc-NO_OUT       = 'X'.
      append xfc to ifc.

      clear xfc.
      xfc-reptext_ddic = 'Source Sub Type'.
      xfc-fieldname    = 'SOURCESUBTYPE'.
      XFC-OUTPUTLEN    = 4.
      xfc-tabname      = 'GT_OUTPUT'.
      xfc-NO_OUT       = 'X'.
      append xfc to ifc.

      clear xfc.
      xfc-reptext_ddic = 'Source'.
      xfc-fieldname    = 'SOURCENAME'.
      xfc-tabname      = 'GT_OUTPUT'.
      append xfc to ifc.

      clear xfc.
      xfc-reptext_ddic = 'Target Type'.
      xfc-fieldname    = 'TARGETTYPE'.
      xfc-tabname      = 'GT_OUTPUT'.
      XFC-OUTPUTLEN    = 4.
      xfc-NO_OUT       = 'X'.
      append xfc to ifc.

      clear xfc.
      xfc-reptext_ddic = 'Target Sub Type'.
      xfc-fieldname    = 'TARGETSUBTYPE'.
      xfc-tabname      = 'GT_OUTPUT'.
      XFC-OUTPUTLEN    = 4.
      xfc-NO_OUT       = 'X'.
      append xfc to ifc.

      clear xfc.
      xfc-reptext_ddic = 'Target'.
      xfc-fieldname    = 'TARGETNAME'.
      xfc-tabname      = 'GT_OUTPUT'.
      append xfc to ifc.

      clear xfc.
      xfc-reptext_ddic = 'Dependency Type'.
      xfc-fieldname    = 'DEP_TYPE'.
      xfc-tabname      = 'GT_OUTPUT'.
      append xfc to ifc.

      clear xfc.
      xfc-reptext_ddic = 'DTP Filter'.
      xfc-fieldname    = 'DTP_FILTER'.
      xfc-tabname      = 'GT_OUTPUT'.
      xfc-NO_OUT       = 'X'.
      append xfc to ifc.


      clear xfc.
      xfc-reptext_ddic = 'Start Routine'.
      xfc-fieldname    = 'START'.
      xfc-tabname      = 'GT_OUTPUT'.
      xfc-NO_OUT       = 'X'.
      append xfc to ifc.

      clear xfc.
      xfc-reptext_ddic = 'Field Routine'.
      xfc-fieldname    = 'ROUTINE'.
      xfc-tabname      = 'GT_OUTPUT'.
       xfc-NO_OUT       = 'X'.
      append xfc to ifc.

      clear xfc.
      xfc-reptext_ddic = 'Formula'.
      xfc-fieldname    = 'FORMULA'.
      xfc-tabname      = 'GT_OUTPUT'.
       xfc-NO_OUT       = 'X'.
      append xfc to ifc.

      clear xfc.
      xfc-reptext_ddic = 'Master Data Lookup'.
      xfc-fieldname    = 'MASTER'.
      xfc-tabname      = 'GT_OUTPUT'.
       xfc-NO_OUT       = 'X'.
      append xfc to ifc.

      clear xfc.
      xfc-reptext_ddic = 'DSO Lookup'.
      xfc-fieldname    = 'ODSO'.
      xfc-tabname      = 'GT_OUTPUT'.
       xfc-NO_OUT       = 'X'.
      append xfc to ifc.

      clear xfc.
      xfc-reptext_ddic = 'Reads Naviagational Attribute'.
      xfc-fieldname    = 'READS_NAV'.
      xfc-tabname      = 'GT_OUTPUT'.
       xfc-NO_OUT       = 'X'.
      append xfc to ifc.

      clear xfc.
      xfc-reptext_ddic = 'End Routine'.
      xfc-fieldname    = 'END'.
      xfc-tabname      = 'GT_OUTPUT'.
       xfc-NO_OUT       = 'X'.
      append xfc to ifc.

      clear xfc.
      xfc-reptext_ddic = 'Expert Routing'.
      xfc-fieldname    = 'EXPERT'.
      xfc-tabname      = 'GT_OUTPUT'.
       xfc-NO_OUT       = 'X'.
      append xfc to ifc.

      clear xfc.
      xfc-reptext_ddic = 'Review'.
      xfc-fieldname    = 'RULES'.
      xfc-tabname      = 'GT_OUTPUT'.
      append xfc to ifc.


      clear xfc.
      xfc-reptext_ddic = 'Root Chain'.
      xfc-fieldname    = 'CHAIN0'.
      xfc-DATATYPE     = 'RSPC_CHAIN'.
      XFC-OUTPUTLEN    = 25.
      xfc-tabname      = 'GT_OUTPUT'.
      append xfc to ifc.

      clear xfc.
      xfc-reptext_ddic = 'Chain Layer 1'.
      xfc-fieldname    = 'CHAIN1'.
      xfc-NO_OUT       = 'X'.
      xfc-DATATYPE     = 'RSPC_CHAIN'.
      XFC-OUTPUTLEN    = 25.
      xfc-tabname      = 'GT_OUTPUT'.
      append xfc to ifc.

      clear xfc.
      xfc-reptext_ddic = 'Chain Layer 2'.
      xfc-fieldname    = 'CHAIN2'.
      xfc-NO_OUT       = 'X'.
      xfc-DATATYPE     = 'RSPC_CHAIN'.
      XFC-OUTPUTLEN    = 25.
      xfc-tabname      = 'GT_OUTPUT'.
      append xfc to ifc.

      clear xfc.
      xfc-reptext_ddic = 'Chain Layer 3'.
      xfc-fieldname    = 'CHAIN3'.
      xfc-NO_OUT       = 'X'.
      xfc-DATATYPE     = 'RSPC_CHAIN'.
      XFC-OUTPUTLEN    = 25.
      xfc-tabname      = 'GT_OUTPUT'.
      append xfc to ifc.

      clear xfc.
      xfc-reptext_ddic = 'Chain Layer 4'.
      xfc-fieldname    = 'CHAIN4'.
      xfc-NO_OUT       = 'X'.
      xfc-DATATYPE     = 'RSPC_CHAIN'.
      XFC-OUTPUTLEN    = 25.
      xfc-tabname      = 'GT_OUTPUT'.
      append xfc to ifc.

      clear xfc.
      xfc-reptext_ddic = 'Chain Layer 5'.
      xfc-fieldname    = 'CHAIN5'.
      xfc-NO_OUT       = 'X'.
      xfc-DATATYPE     = 'RSPC_CHAIN'.
      XFC-OUTPUTLEN    = 25.
      xfc-tabname      = 'GT_OUTPUT'.
      append xfc to ifc.

      clear xfc.
      xfc-reptext_ddic = 'Chain Layer 6'.
      xfc-fieldname    = 'CHAIN6'.
      xfc-NO_OUT       = 'X'.
      xfc-DATATYPE     = 'RSPC_CHAIN'.
      XFC-OUTPUTLEN    = 25.
      xfc-tabname      = 'GT_OUTPUT'.
      append xfc to ifc.

      clear xfc.
      xfc-reptext_ddic = 'Chain Layer 7'.
      xfc-fieldname    = 'CHAIN7'.
      xfc-NO_OUT       = 'X'.
      xfc-DATATYPE     = 'RSPC_CHAIN'.
      XFC-OUTPUTLEN    = 25.
      xfc-tabname      = 'GT_OUTPUT'.
      append xfc to ifc.


      clear xfc.
      xfc-reptext_ddic = 'Process Chain'.
      xfc-fieldname    = 'CHAIN'.
      xfc-DATATYPE     = 'RSPC_CHAIN'.
      XFC-OUTPUTLEN    = 25.
      xfc-tabname      = 'GT_OUTPUT'.
      append xfc to ifc.

      clear xfc.
      xfc-reptext_ddic = 'Source Name'.
      xfc-fieldname    = 'SOURCETEXT'.
      xfc-DATATYPE     = 'RSBKDTPTEXT'.
      XFC-OUTPUTLEN    = 25.
      xfc-tabname      = 'GT_OUTPUT'.
      append xfc to ifc.

      clear xfc.
      xfc-reptext_ddic = 'Target Name'.
      xfc-fieldname    = 'TARGETTEXT'.
      xfc-DATATYPE     = 'RSBKDTPTEXT'.
      xfc-tabname      = 'GT_OUTPUT'.
      append xfc to ifc.

      clear xfc.
      xfc-reptext_ddic = 'Variant Name'.
      xfc-fieldname    = 'DTPT '.
      xfc-DATATYPE     = 'RSBKDTPTEXT'.
      xfc-tabname      = 'GT_OUTPUT'.
      append xfc to ifc.

      clear xfc.
      xfc-reptext_ddic = 'Transformation Text'.
      xfc-fieldname    = 'TFTEXT'.
      xfc-DATATYPE     = 'RSBKTFTEXT'.
      xfc-tabname      = 'GT_OUTPUT'.
      append xfc to ifc.

      IF SO_IOBJ is NOT INITIAL.
          clear xfc.
          xfc-reptext_ddic = 'Has Requested Infoobject'.
          xfc-fieldname    = 'hasiobj'.
          xfc-tabname      = 'GT_OUTPUT'.
*          xfc-NO_OUT       = 'X'.
          append xfc to ifc.
      ENDIF.
*     Call ABAP List Viewer (ALV)
      call function 'REUSE_ALV_GRID_DISPLAY'
           exporting
                i_callback_program      = repid
                i_callback_user_command = 'HANDLE_USER_COMMAND'
                it_fieldcat             = ifc
           tables
                t_outtab                = gt_output.
ENDFORM.

form handle_user_command using r_ucomm     like sy-ucomm
                               rs_selfield type slis_selfield.
 DATA: LS_OUTPUT type _output,
       lv_logsys TYPE RSSLOGSYS,
       LV_OLTP   TYPE RSDS_DS_ALL,
       LV_OBJ_NAME TYPE SOBJ_NAME,
       LV_DTA       type RSDTATYPE,
       lv_chain   TYPE rspc_chain,
       LV_display TYPE rs_bool,
       l_t_chaint TYPE rspc_t_chaint,
       lV_mode     TYPE rspc_frontendmode,
       LV_DTPA        TYPE RSBKDTPNM.

  LV_display = 'X'.

  READ TABLE gt_output INDEX  RS_SELFIELD-TABINDEX INTO ls_output.
  if rs_selfield-FIELDNAME CP 'CHAIN*' .
    rs_selfield-FIELDNAME = 'CHAIN'.
    lv_chain = rs_selfield-value.
  ENDIF.
  case r_ucomm.
    when '&IC1'.

    case rs_selfield-FIELDNAME.
      when  'DTP'.
        CASE LS_OUTPUT-VARTYPE.
           WHEN 'DTPA'.

            TRY.
                 LV_DTPA   = rs_selfield-value.
                 CALL FUNCTION 'RSBK_DTP_MAINTAIN'
                   EXPORTING
                     i_dtp          = LV_DTPA
                     i_mode         = rsbc_c_mode-display
                     i_display_only = LV_display.

                 DATA l_r_message TYPE REF TO cx_rs_error.
                 CATCH cx_rs_error INTO l_r_message.
                 MESSAGE l_r_message TYPE 'E'.

              ENDTRY.

          WHEN 'ABAP'.

             EDITOR-CALL FOR REPORT rs_selfield-value display-mode.

          WHEN  'IPAK'.

              LV_OBJ_NAME = rs_selfield-value.
              CALL  FUNCTION 'RSO_ISIP_MAINTAIN'
              EXPORTING
                  I_OBJNM = LV_OBJ_NAME.
           WHEN OTHERS.
         ENDCASE.

      when 'TF'.

         CASE LS_OUTPUT-TFTYPE.
           WHEN 'TRFN'.
            set parameter id 'TRANID' field rs_selfield-value.
            call transaction 'RSTRANGUI' and skip first screen.
           WHEN 'UPDR'.

            LV_OBJ_NAME =  rs_selfield-value.
            CASE ls_output-TARGETTYPE.

              WHEN 'ODSO'. "O  ODS Object
                LV_DTA = 'O'.
              WHEN 'CUBE'. "I  InfoCube
                LV_DTA = 'I'.
              WHEN 'IOBJ'. "C  InfoObject
                LV_DTA = 'C'.
              WHEN OTHERS.
            ENDCASE.
            PERFORM RSO_UPDR_MAINTAIN USING LV_OBJ_NAME LV_DTA.


          WHEN OTHERS.
         ENDCASE.

      WHEN  'SOURCENAME'.

         CASE LS_OUTPUT-SOURCETYPE.

          WHEN 'RSDS'.

            LV_OLTP = LS_OUTPUT-SOURCENAME(30).
            lv_logsys = LS_OUTPUT-SOURCENAME+30(10).

            PERFORM rsds_display USING   lv_logsys LV_OLTP.
          when 'ODSO'.
            PERFORM MANAGE_SOURCE USING LS_OUTPUT.
          WHEN 'CUBE'.
            PERFORM MANAGE_SOURCE USING LS_OUTPUT.
          WHEN 'IOBJ'.
            PERFORM MANAGE_SOURCE USING LS_OUTPUT.
           WHEN OTHERS.
         ENDCASE.

      WHEN  'TARGETNAME'.

         CASE LS_OUTPUT-TARGETTYPE.

          WHEN 'RSDS'.

            LV_OLTP = LS_OUTPUT-targetNAME(30).
            lv_logsys = LS_OUTPUT-targetNAME+30(10).

            PERFORM rsds_display USING   lv_logsys LV_OLTP.
          when 'ODSO'.
            PERFORM MANAGE_TARGET USING LS_OUTPUT.
          WHEN 'CUBE'.
            PERFORM MANAGE_TARGET USING LS_OUTPUT.
          WHEN 'IOBJ'.
            PERFORM MANAGE_TARGET USING LS_OUTPUT.
           WHEN OTHERS.
         ENDCASE.
      when 'CHAIN'.
          lV_mode = 'PLAN'.
          CALL FUNCTION 'RSPC_CHAIN_MAINTAIN'
              EXPORTING
                i_chain         = lv_chain
                i_mode          = lV_mode
                i_display_only  = LV_display
              IMPORTING
                e_t_chaint      = l_t_chaint
              EXCEPTIONS
                internal_error  = 1
                aborted_by_user = 2
                OTHERS          = 3.
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                      DISPLAY LIKE 'E'.
            ENDIF.
      endcase.

  endcase.

endform.

FORM rsds_display USING P_LOGSYS TYPE RSSLOGSYS p_ds TYPE RSDS_DS_ALL .
  DATA: l_logsys TYPE rsds-logsys.


* ==== Maintain
  CALL METHOD cl_rsds_rsds=>maintain_ds
    EXPORTING
      i_datasource = p_ds
      i_logsys     = P_LOGSYS
*      i_modify     = i_modify
      i_objvers    = 'A'
    EXCEPTIONS
      failed       = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_LIST_OF_UPDATE_RULES
*&---------------------------------------------------------------------*
*      For a infopackage return a list of update rules in use
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_LIST_OF_UPDATE_RULES
      USING P_LOGID TYPE RSLOGDPID
      CHANGING CT_UPDR TYPE STANDARD TABLE  .

*DATA: LS_RSLDPIO     TYPE RSLDPIO,
*      l_t_isosmap     type  rsaos_t_isosmap,
*      l_s_isosmap     type  rsaos_s_isosmap,
*      l_t_ts          type  rsaos_t_ts,
*      l_s_ts          type  rsaos_s_ts,
*      LT_RSUPDINFO    TYPE STANDARD TABLE OF RSUPDINFO,
*      LS_RSUPDINFO    TYPE RSUPDINFO,
*      LS_UPDR         TYPE _RSUPDID,
*      LT_RSLDPSEL      TYPE STANDARD TABLE OF RSLDPSEL,
*      LS_RSLDPSEL      TYPE RSLDPSEL.
* CLEAR: CT_UPDR.
*
*  SELECT SINGLE * INTO LS_RSLDPIO FROM RSLDPIO WHERE LOGDPID = P_LOGID AND OBJVERS = G_OBJVERS.
*
* call function 'RSAOS_OLTPSOURCE_GET_BY_PROP'
*                exporting
*                  i_isource      = LS_rsldpio-source
*                  i_logsys       = LS_rsldpio-logsys
*                  i_objvers      = G_OBJVERS
*                importing
*                  e_t_isosmap    = l_t_isosmap
*                  e_t_ts         = l_t_ts
*                exceptions
*                  not_unique     = 1
*                  not_exist      = 2
*                  internal_error = 3
*                  others         = 4.
** Get list of Update rules
*  if  l_t_isosmap is not INITIAL.
*    SELECT *
*    into TABLE  LT_RSUPDINFO
*    from  RSUPDINFO
*    FOR ALL ENTRIES IN L_T_ISOSMAP
*    WHERE ISOURCE =  L_T_ISOSMAP-ISOURCE
*                and OBJVERS = G_OBJVERS.
*
*    SELECT *
*    INTO TABLE LT_RSLDPSEL
*    FROM RSLDPSEL
*    WHERE OBJVERS = G_OBJVERS
*        AND LOGDPID = P_LOGID.
*
*    READ TABLE  LT_RSLDPSEL INDEX 1 into LS_RSLDPSEL  .
*
*    if LS_RSLDPSEL-SELUPDIC is INITIAL.
*
*        LOOP AT LT_RSUPDINFO INTO LS_RSUPDINFO.
*            LS_UPDR-UPDR = LS_RSUPDINFO-UPDID.
*            APPEND LS_UPDR TO CT_UPDR.
*        ENDLOOP.
*    ELSE.
*       LOOP AT LT_RSUPDINFO INTO LS_RSUPDINFO.
*            READ TABLE LT_RSLDPSEL TRANSPORTING NO FIELDS
*                with KEY SELUPDIC = LS_RSUPDINFO-INFOCUBE.
*
*            IF sy-subrc = 0.
*                LS_UPDR-UPDR = LS_RSUPDINFO-UPDID.
*                APPEND LS_UPDR TO CT_UPDR.
*            ENDIF.
*
*        ENDLOOP.
*
*    endif.
* ENDif.
*  FREE: LT_RSUPDINFO , LT_RSLDPSEL, l_t_isosmap.
ENDFORM.

form RSO_UPDR_MAINTAIN using I_OBJNM TYPE  SOBJ_NAME P_DTA TYPE RSDTATYPE .
  DATA: l_s_updinfo    TYPE rsau_s_updinfo,
        l_updid        TYPE rsau_s_updinfo-updid,
        l_mode         TYPE rs_modus,
        l_dtatype      TYPE rsdtatype,
        l_objvers_dep  TYPE rsobjvers,
        l_tlogo        TYPE rstlogo,
        l_data_target  TYPE REF TO cl_rsd_dta,
        l_s_dta        TYPE rsd_s_dta,
        I_OBJVERS       TYPE  RSOBJVERS ,
        I_DISPLAY  TYPE  RS_BOOL  .

    I_OBJVERS  = RS_C_OBJVERS-ACTIVE.
    I_DISPLAY = RS_C_TRUE.

  l_updid = i_objnm.

  SELECT SINGLE * FROM rsupdinfo INTO l_s_updinfo
       WHERE updid = l_updid AND objvers = i_objvers.

  l_mode = 3.

  CALL FUNCTION 'RSAU_UPDR_CALL_TRANSACTION'
       EXPORTING
            i_dta                     = l_s_updinfo-infocube
            i_isource                 = l_s_updinfo-isource
            i_mode                    = l_mode
            i_objvers                 = l_s_updinfo-objvers
            i_dtatype                 = P_DTA
*      IMPORTING
*           E_UR_MAINTAINED           =
*           E_OTHER_IC_MAINTAINED     =
       EXCEPTIONS
            no_update_rule_exist      = 1
            no_sap_version            = 2
            no_active_version         = 3
            update_rule_already_exist = 4
            OTHERS                    = 5
            .
  IF sy-subrc <> 0.
*   should not be possible because this has been tested before
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING
object_not_found
.
  ENDIF.
endform.
*&---------------------------------------------------------------------*
*&      Form  GET_INFOCUBE_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_RSUPDINFO_INFOCUBE  text
*      <--P_LS_OUTPUT_TARGETTYPE  text
*----------------------------------------------------------------------*
FORM GET_INFOCUBE_TYPE  USING    P_INFOCUBE
                        CHANGING P_ARGETTYPE.
DATA : o_r_dta TYPE REF TO cl_rsd_dta.
DATA : n_infoprov TYPE rsinfoprov.
DATA : t_dta_pro TYPE rsd_t_dta_pro,
        lv_str   TYPE int4,
        LV_TEXT TYPE C LENGTH 20.


CALL METHOD cl_rsd_dta=>factory
   EXPORTING
     i_infoprov =  P_INFOCUBE
   RECEIVING
     r_r_dta    = o_r_dta
   EXCEPTIONS
     OTHERS     = 1.

  if o_r_dta is NOT INITIAL.
      Call METHOD o_r_dta->GET_TLOGO
      RECEIVING R_TLOGO = P_ARGETTYPE.
  ELSE.
      P_ARGETTYPE = 'IOBJ'. " cubes AND dso HAVE TO BE INFO PROVIDERS
  ENDIF.
FREE o_r_dta.
IF sy-subrc <> 0.



ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_RULES_FOR_UPDR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_UPDR_UPDR  text
*      <--P_LS_OUTPUT  text
*----------------------------------------------------------------------*
FORM GET_RULES_FOR_UPDR  USING    P_UPDR
                         CHANGING P_OUTPUT TYPE _OUTPUT
                        .
  data: LT_RSUPDDAT TYPE STANDARD TABLE OF RSUPDDAT,
        Ls_RSUPDDAT TYPE  RSUPDDAT,
           LV_DTP_FILTER     TYPE c LENGTH 1,
           LV_FORMULA        TYPE c LENGTH 1,
           LV_END            TYPE c LENGTH 1,
           LV_MASTER          TYPE c LENGTH 1,
           LV_LV_START          TYPE c LENGTH 1,
           LV_ROUTINE        TYPE c LENGTH 1,
           LV_EXPERT          TYPE c LENGTH 1,
           LV_ODSO            TYPE c LENGTH 1,
           LV_READS_NAV      TYPE c LENGTH 1,
           LV_RULES          TYPE c LENGTH 1.

  SELECT *
   INTO table LT_RSUPDDAT
   from RSUPDDAT
   WHERE UPDID = P_UPDR
      and    OBJVERS = G_OBJVERS.

    LOOP AT LT_RSUPDDAT INTO Ls_RSUPDDAT.
        IF LS_RSUPDDAT-ROUTINE NE '0000'.
          P_OUTPUT-ROUTINE = 'Y'.
          P_OUTPUT-RULES = 'Y'.
        ENDIF.

        IF LS_RSUPDDAT-FORMULA_ID is not initial.
          P_OUTPUT-FORMULA = 'Y'.
          P_OUTPUT-RULES = 'Y'.
        ENDIF.
        IF LS_RSUPDDAT-CMIOBJNM IS NOT INITIAL.
          P_OUTPUT-MASTER = 'Y'.
          P_OUTPUT-RULES = 'Y'.
        ENDIF.

        IF GT_RSDIOBJ IS NOT INITIAL.
           READ TABLE GT_RSDIOBJ TRANSPORTING NO FIELDS
              WITH TABLE KEY IOBJNM = LS_RSUPDDAT-ICIOBJNM.

              IF sy-SUBRC = 0.
                  P_OUTPUT-HASIOBJ = 'Y'.
              ENDIF.

            READ TABLE GT_RSDIOBJ TRANSPORTING NO FIELDS
              WITH TABLE KEY IOBJNM = LS_RSUPDDAT-CSIOBJNM.

              IF sy-SUBRC = 0.
                  P_OUTPUT-HASIOBJ = 'Y'.
              ENDIF.
        ENDIF.
    ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADD_PROCESS_CHAINS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ADD_PROCESS_CHAINS .
  DATA: LS_OUTPUT TYPE _OUTPUT,
       LS_FLATCHAIN TYPE _FLATCHAIN,
       LT_OUTPUT     TYPE STANDARD TABLE OF _OUTPUT,
       LS_RSPCvariant TYPE RSPCVARIANT,
       LS_RSPCvariant2 TYPE RSPCVARIANT.


 LOOP AT GT_FLATCHAIN INTO LS_FLATCHAIN.
      LOOP AT GT_OUTPUT INTO LS_OUTPUT
          WHERE  DTP = LS_FLATCHAIN-VARIANTE.
        MOVE-CORRESPONDING LS_FLATCHAIN TO LS_OUTPUT.
        APPEND LS_OUTPUT TO LT_OUTPUT.
      ENDLOOP.

      IF sy-subrC ne 0.
          CASE LS_FLATCHAIN-VTYPE .
            WHEN 'CHAIN'. "Do Nothing
            WHEN 'ODSACTIVAT'. "Do Nothing
            WHEN 'START'.  "Do Nothing
            WHEN 'HIERSAVE'.  "Do Nothing
            WHEN 'PSADELETE'.  "Do Nothing
            WHEN 'AND'. "Do Nothing
            when 'DROPCUBE'. "Do Nothing
            when 'TRIGGER'. "Do Nothing
            WHEN 'ATTRIBCHAN'. "Do Nothing
            WHEN 'INTERRUPT'.  "Do Nothing

            WHEN 'CHGLOGDEL'. "Do Nothing
            WHEN 'REQUDEL'.    "Do Nothing
            WHEN 'COMPRESS'.    "Do Nothing
            WHEN 'INDEX'.    "Do Nothing
            WHEN 'DROPINDEX'.    "Do Nothing
            WHEN 'DBSTAT'.    "Do Nothing
            WHEN 'DATACHANGE'.   "Do Nothing
            WHEN 'ROLLUP'.  "Do Nothing
            WHEN 'PLSWITCHL'.  "Do Nothing
            WHEN 'PLSWITCHP'.  "Do Nothing
            WHEN 'EXOR'.  "Do Nothing
            WHEN 'PSAPROCESS'.
                  LOOP AT GT_RSPCVARIANT INTO LS_RSPCVARIANT
                     WHERE FNAM = 'LOADING'
                       AND VARIANTE = LS_FLATCHAIN-VARIANTE.

                    LOOP AT GT_OUTPUT INTO LS_OUTPUT
                         WHERE  DTP = LS_RSPCVARIANT-LOW.
                          MOVE-CORRESPONDING LS_FLATCHAIN TO LS_OUTPUT.
                          APPEND LS_OUTPUT TO LT_OUTPUT.
                  ENDLOOP.
                ENDLOOP.
            WHEN 'ODSPROCESS'.
              LOOP AT GT_RSPCVARIANT INTO LS_RSPCVARIANT
                     WHERE FNAM = 'LOADING'
                       AND VARIANTE = LS_FLATCHAIN-VARIANTE.

                    LOOP AT GT_OUTPUT INTO LS_OUTPUT
                         WHERE  DTP = LS_RSPCVARIANT-LOW.
                          MOVE-CORRESPONDING LS_FLATCHAIN TO LS_OUTPUT.
                          APPEND LS_OUTPUT TO LT_OUTPUT.
                  ENDLOOP.
                ENDLOOP.
            WHEN 'REPA_ER'. "Do Nothing
            WHEN 'ABAP'. " ADJUST THIS LOOKUP
                READ TABLE GT_RSPCVARIANT INTO LS_RSPCVARIANT
                    WITH KEY FNAM = 'PROGRAM'
                             VARIANTE = LS_FLATCHAIN-VARIANTE.
                 READ TABLE GT_RSPCVARIANT INTO LS_RSPCVARIANT2
                    WITH KEY FNAM = 'VARIANT'
                             VARIANTE = LS_FLATCHAIN-VARIANTE.

                    LOOP AT GT_OUTPUT INTO LS_OUTPUT
                         WHERE  DTP = LS_RSPCVARIANT-LOW
                              AND TF = LS_RSPCVARIANT2-LOW.
                          MOVE-CORRESPONDING LS_FLATCHAIN TO LS_OUTPUT.
                          APPEND LS_OUTPUT TO LT_OUTPUT.
                  ENDLOOP.

            WHEN 'ZABAP_RG'.
                 READ TABLE GT_RSPCVARIANT INTO LS_RSPCVARIANT
                    WITH KEY FNAM = 'PROGRAM'
                             VARIANTE = LS_FLATCHAIN-VARIANTE.
                 READ TABLE GT_RSPCVARIANT INTO LS_RSPCVARIANT2
                    WITH KEY FNAM = 'VARIANT'
                             VARIANTE = LS_FLATCHAIN-VARIANTE.

                    LOOP AT GT_OUTPUT INTO LS_OUTPUT
                         WHERE  DTP = LS_RSPCVARIANT-LOW
                              AND TF = LS_RSPCVARIANT2-LOW.
                          MOVE-CORRESPONDING LS_FLATCHAIN TO LS_OUTPUT.
                          APPEND LS_OUTPUT TO LT_OUTPUT.
                  ENDLOOP.
            WHEN 'ZABAP_BOH'.
                 READ TABLE GT_RSPCVARIANT INTO LS_RSPCVARIANT
                    WITH KEY FNAM = 'PROGRAM'
                             VARIANTE = LS_FLATCHAIN-VARIANTE.
                 READ TABLE GT_RSPCVARIANT INTO LS_RSPCVARIANT2
                    WITH KEY FNAM = 'VARIANT'
                             VARIANTE = LS_FLATCHAIN-VARIANTE.

                    LOOP AT GT_OUTPUT INTO LS_OUTPUT
                         WHERE  DTP = LS_RSPCVARIANT-LOW
                              AND TF = LS_RSPCVARIANT2-LOW.
                          MOVE-CORRESPONDING LS_FLATCHAIN TO LS_OUTPUT.
                          APPEND LS_OUTPUT TO LT_OUTPUT.
                  ENDLOOP.
            WHEN OTHERS.
              CLEAR LS_OUTPUT.
              MOVE-CORRESPONDING LS_FLATCHAIN TO LS_OUTPUT.
              LS_OUTPUT-dtp = LS_FLATCHAIN-VARIANTE.
              LS_OUTPUT-DEP_TYPE = LS_FLATCHAIN-VTYPE.
              APPEND ls_output to lt_output.

          ENDCASE.

      ENDIF.
  ENDLOOP.

  CLEAR GT_OUTPUT.

  APPEND LINES OF LT_OUTPUT TO GT_OUTPUT.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MANAGE_TARGET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MANAGE_TARGET using P_OUTPUT TYPE _OUTPUT .
  DATA: LT_icube TYPE RSSM_T_ICUBE,
        LS_ICUBE TYPE rssm_s_icube,
        ls_output type _output.

    LOOP AT gt_output into ls_output
          WHERE TARGETNAME = P_OUTPUT-TARGETNAME.
        IF ls_output-SOURCETYPE = 'IOBJ' AND ls_OUTPUT-SOURCESUBTYPE = 'HIER'.
          ELSEIF ls_OUTPUT-SOURCETYPE = 'IOBJ' AND ls_OUTPUT-SOURCESUBTYPE = 'TEXT'.
            LS_ICUBE-ICUBE = ls_OUTPUT-SOURCENAME && '$T'.
              APPEND LS_ICUBE TO LT_ICUBE.
          ELSE.
              LS_ICUBE-ICUBE = ls_OUTPUT-SOURCENAME.
              APPEND LS_ICUBE TO LT_ICUBE.

        ENDIF.
    ENDLOOP.

    IF P_OUTPUT-TARGETTYPE = 'IOBJ' AND P_OUTPUT-TARGETSUBTYPE = 'HIER'.
    ELSEIF P_OUTPUT-TARGETTYPE = 'IOBJ' AND P_OUTPUT-TARGETSUBTYPE = 'TEXT'.
      LS_ICUBE-ICUBE = P_OUTPUT-TARGETNAME && '$T'.
        APPEND LS_ICUBE TO LT_ICUBE.
        CALL FUNCTION 'RSSM_CALL_ICUBE_PFLEGE'
              TABLES
                E_T_ICUBE = LT_icubE .
    ELSE.
        LS_ICUBE-ICUBE = P_OUTPUT-TARGETNAME.
        APPEND LS_ICUBE TO LT_ICUBE.
        CALL FUNCTION 'RSSM_CALL_ICUBE_PFLEGE'
              TABLES
                E_T_ICUBE = LT_icubE .
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MANAGE_SOURCE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MANAGE_SOURCE USING P_OUTPUT TYPE _OUTPUT .
*RSSM_CALL_ICUBE_PFLEGE
DATA: LT_icube TYPE RSSM_T_ICUBE,
      LS_ICUBE TYPE rssm_s_icube.
  IF P_OUTPUT-SOURCETYPE = 'IOBJ' AND P_OUTPUT-SOURCESUBTYPE = 'HIER'.
  ELSEIF P_OUTPUT-SOURCETYPE = 'IOBJ' AND P_OUTPUT-SOURCESUBTYPE = 'TEXT'.
    LS_ICUBE-ICUBE = P_OUTPUT-SOURCENAME && '$T'.
      APPEND LS_ICUBE TO LT_ICUBE.
      CALL FUNCTION 'RSSM_CALL_ICUBE_PFLEGE'
            TABLES
              E_T_ICUBE = LT_icubE .
  ELSE.
      LS_ICUBE-ICUBE = P_OUTPUT-SOURCENAME.
      APPEND LS_ICUBE TO LT_ICUBE.
      CALL FUNCTION 'RSSM_CALL_ICUBE_PFLEGE'
            TABLES
              E_T_ICUBE = LT_icubE .
  ENDIF.



ENDFORM.
