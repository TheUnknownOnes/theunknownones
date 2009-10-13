UNIT uTestUtils;

INTERFACE

IMPLEMENTATION

USES
  Dialogs,
  uUtils, SysUtils;

PROCEDURE TestUnit;
VAR
  s1: string;

  PROCEDURE Check(CONST sIs, sPart, sRest: string);
  BEGIN
    Assert(sIs = sPart, '"' + sIs + '" <> "' + sPart + '"');
    Assert(s1 = sRest, '"' + s1 + '" <> "' + sRest + '"');
  END;

BEGIN
  s1 := '12.345..678';
  Check(LeftToken(s1, '.', True), '12', '345..678');
  Check(LeftToken(s1, '.', True), '345', '.678');
  Check(LeftToken(s1, '.', True), '', '678');

  s1 := '12.345..678';
  Check(LeftToken(s1, '..', True), '12.345', '678');
  Check(LeftToken(s1, '..', True), '678', '');
  Check(LeftToken(s1, '..', True), '', '');

  s1 := '12.345..678';
  Check(LeftToken(s1, 'q', True), '12.345..678', '');

  s1 := '12.345..678';
  Check(RightToken(s1, '.', True), '678', '12.345.');
  Check(RightToken(s1, '.', True), '', '12.345');
  Check(RightToken(s1, '.', True), '345', '12');

  s1 := '12.345..678';
  Check(RightToken(s1, '..', True), '678', '12.345');
  Check(RightToken(s1, '..', True), '12.345', '');
  Check(RightToken(s1, '..', True), '', '');

  s1 := '12.345..678';
  Check(RightToken(s1, 'q', True), '12.345..678', '');
END;

INITIALIZATION
  TRY
    TestUnit;
  EXCEPT
    on e: Exception DO
      ShowMessage(e.Message);
  END;

END.

