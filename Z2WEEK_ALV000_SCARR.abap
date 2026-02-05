*&---------------------------------------------------------------------*
*& Report Z2WEEK_ALV000
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z2WEEK_ALV000_SCARR.

TABLES:     scarr.

TYPE-POOLS: slis.                                 "ALV Declarations

*Data Declaration: 데이터 담기 (엑셀에 첫 행 구성)
*----------------
TYPES: BEGIN OF t_scarr,
  mandt     TYPE scarr-mandt,
  carrid    TYPE scarr-carrid,
  carrname  TYPE scarr-carrname,
  currcode  TYPE scarr-currcode,
  url       TYPE scarr-url,
 END OF t_scarr.

*it_scarr로 표를 만든거임. internal-table
*wa_scarr는 표는 아니지만, 5개 필드 들어갈 수 있는 공간 만든거임. work-area
DATA: it_scarr TYPE STANDARD TABLE OF t_scarr INITIAL SIZE 0,
      wa_scarr TYPE t_scarr.

*ALV data declarations
DATA: fieldcatalog TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      gd_tab_group TYPE slis_t_sp_group_alv,
      gd_layout    TYPE slis_layout_alv,
      gd_repid     LIKE sy-repid.


DATA : t TYPE slis_t_sp_group_alv . "gd_tab_group와 같은 타입이지만, 다른 이름으로 선언한 것 뿐.
************************************************************************
*Start-of-selection.
START-OF-SELECTION.

  PERFORM data_retrieval.
  PERFORM build_fieldcatalog.
  PERFORM build_layout.
  PERFORM display_alv_report.


*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       Build Fieldcatalog for ALV Report
*----------------------------------------------------------------------*
*제목을 붙이는 작업
FORM build_fieldcatalog.

  fieldcatalog-fieldname   = 'MANDT'.
  fieldcatalog-seltext_m   = 'Client'.
  fieldcatalog-col_pos     = 0.
  fieldcatalog-outputlen   = 10.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'CARRID'.
  fieldcatalog-seltext_m   = 'Airline Code'.
  fieldcatalog-col_pos     = 1.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'CARRNAME'.
  fieldcatalog-seltext_m   = 'Airline name'.
  fieldcatalog-col_pos     = 2.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'CURRCODE'.
  fieldcatalog-seltext_m   = 'Local currency of airline'.
  fieldcatalog-col_pos     = 3.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'URL'.
  fieldcatalog-seltext_m   = 'Airline URL'.
  fieldcatalog-col_pos     = 4.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.
ENDFORM.                    " BUILD_FIELDCATALOG


*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       Build layout for ALV grid report
*----------------------------------------------------------------------*
FORM build_layout.

  gd_layout-no_input          = 'X'.
  gd_layout-colwidth_optimize = 'X'.
  gd_layout-zebra = 'X'.

ENDFORM.                    " BUILD_LAYOUT


*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_REPORT
*&---------------------------------------------------------------------*
*       Display report using ALV grid
*----------------------------------------------------------------------*
FORM display_alv_report.
  gd_repid = sy-repid.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = gd_repid
      is_layout          = gd_layout "위에 붙인 3가지 옵션(build_layout)
      it_fieldcat        = fieldcatalog[] "필드별 제목 넣었던 거
      i_save             = 'X'
    TABLES
      t_outtab           = it_scarr "내가 담은 테이블 출력
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " DISPLAY_ALV_REPORT


*&---------------------------------------------------------------------*
*&      Form  DATA_RETRIEVAL
*&---------------------------------------------------------------------*
FORM data_retrieval.
  DATA: ld_color(1) TYPE c.

  SELECT mandt carrid carrname currcode url
*   UP TO 10 ROWS
    FROM scarr    "항공사 테이블
    INTO TABLE it_scarr.

ENDFORM.                    " DATA_RETRIEVAL