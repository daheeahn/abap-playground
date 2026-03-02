*&---------------------------------------------------------------------*
*& Report Z7WEEK_SEL_EX07
*&---------------------------------------------------------------------*
*& SPFLI 테이블의 항공사 코드(CARRID)에 대해
*& 셀렉트 옵션 사용법을 확인하기 위한 기본 예제 프로그램
*&---------------------------------------------------------------------*
REPORT Z7WEEK_SEL_EX07.

TABLES spfli.

SELECTION-SCREEN BEGIN OF BLOCK part1 WITH FRAME TITLE text-001.
SELECT-OPTIONS s_carrid FOR spfli-carrid.
SELECTION-SCREEN END OF BLOCK part1.

START-OF-SELECTION.
  WRITE : '셀렉트옵션'.