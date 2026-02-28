*&---------------------------------------------------------------------*
*& Report Z6WEEK_SEL_EX06_SBOOK
*&---------------------------------------------------------------------*
*& SBOOK 테이블 기준으로, PARAMETERS와 LISTBOX 방식으로 Selection Screen을 만들고, 도움말 체크
*&---------------------------------------------------------------------*
REPORT Z6WEEK_SEL_EX06_SBOOK.

*SELECTION-SCREEN BEGIN OF BLOCK part5 WITH FRAME TITLE text-005.
*PARAMETERS p_carrid TYPE spfli-carrid
*                    AS LISTBOX VISIBLE LENGTH 20
*                    DEFAULT 'LH'.
*SELECTION-SCREEN END OF BLOCK part5.

SELECTION-SCREEN BEGIN OF BLOCK part1 WITH FRAME TITLE text-001.
PARAMETERS :  P01 TYPE sbook-mandt,
              P02 TYPE sbook-carrid,
              P03 TYPE sbook-connid,
              P04 TYPE sbook-fldate,
              P05 TYPE sbook-bookid,
              P06 TYPE sbook-customid,
              P07 TYPE sbook-custtype,
              P08 TYPE sbook-smoker,
              P09 TYPE sbook-luggweight,
              P10 TYPE sbook-wunit,
              P11 TYPE sbook-invoice,
              P12 TYPE sbook-class,
              P13 TYPE sbook-forcuram,
              P14 TYPE sbook-forcurkey,
              P15 TYPE sbook-loccuram,
              P16 TYPE sbook-loccurkey,
              P17 TYPE sbook-order_date,
              P18 TYPE sbook-counter,
              P19 TYPE sbook-agencynum,
              P20 TYPE sbook-cancelled,
              P21 TYPE sbook-reserved,
              P22 TYPE sbook-passname,
              P23 TYPE sbook-passform,
              P24 TYPE sbook-passbirth.
SELECTION-SCREEN END OF BLOCK part1.

SELECTION-SCREEN BEGIN OF BLOCK part2 WITH FRAME TITLE text-002.
PARAMETERS :  SB25 TYPE sbook-mandt AS LISTBOX VISIBLE LENGTH 20,
              SB26 TYPE sbook-carrid AS LISTBOX VISIBLE LENGTH 20,
              SB27 TYPE sbook-connid AS LISTBOX VISIBLE LENGTH 20,
              SB28 TYPE sbook-fldate AS LISTBOX VISIBLE LENGTH 20,
              SB29 TYPE sbook-bookid AS LISTBOX VISIBLE LENGTH 20,
              SB30 TYPE sbook-customid AS LISTBOX VISIBLE LENGTH 20,
              SB31 TYPE sbook-custtype AS LISTBOX VISIBLE LENGTH 20,
              SB32 TYPE sbook-smoker AS LISTBOX VISIBLE LENGTH 20,
              SB33 TYPE sbook-luggweight AS LISTBOX VISIBLE LENGTH 20,
              SB34 TYPE sbook-wunit AS LISTBOX VISIBLE LENGTH 20,
              SB35 TYPE sbook-invoice AS LISTBOX VISIBLE LENGTH 20,
              SB36 TYPE sbook-class AS LISTBOX VISIBLE LENGTH 20,
              SB37 TYPE sbook-forcuram AS LISTBOX VISIBLE LENGTH 20,
              SB38 TYPE sbook-forcurkey AS LISTBOX VISIBLE LENGTH 20,
              SB39 TYPE sbook-loccuram AS LISTBOX VISIBLE LENGTH 20,
              SB40 TYPE sbook-loccurkey AS LISTBOX VISIBLE LENGTH 20,
              SB41 TYPE sbook-order_date AS LISTBOX VISIBLE LENGTH 20,
              SB42 TYPE sbook-counter AS LISTBOX VISIBLE LENGTH 20,
              SB43 TYPE sbook-agencynum AS LISTBOX VISIBLE LENGTH 20,
              SB44 TYPE sbook-cancelled AS LISTBOX VISIBLE LENGTH 20,
              SB45 TYPE sbook-reserved AS LISTBOX VISIBLE LENGTH 20,
              SB46 TYPE sbook-passname AS LISTBOX VISIBLE LENGTH 20,
              SB47 TYPE sbook-passform AS LISTBOX VISIBLE LENGTH 20,
              SB48 TYPE sbook-passbirth AS LISTBOX VISIBLE LENGTH 20.
SELECTION-SCREEN END OF BLOCK part2.