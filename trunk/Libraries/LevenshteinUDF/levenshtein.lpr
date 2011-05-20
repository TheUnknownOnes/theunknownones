library levenshtein;

{
DECLARE EXTERNAL FUNCTION levenshtein_1000
varchar(1000) null,
varchar(1000) null
RETURNS double precision
ENTRY_POINT 'levenshtein' MODULE_NAME 'levenshtein';
}

{$mode objfpc}{$H+}

uses
  Classes,
  uFBParamDescription,
  uIB2007,
  Math;

{$R *.res}

function _Levenshtein(const String1,String2: string): Double; // similarity engine
var
{$IFDEF VER100} // Delphi 3 can only do 4kbx4kb array - does not support dynamic arrays
   Matrix:Array[0..4095] of Array[0..4095] of Integer;
{$ELSE}
   Matrix:Array of Array of Integer;
{$ENDIF}
   maxl,i,j,n,m,distanz,cost:Integer;
   s_i,t_j:char;
begin
   Result:=0.0;
   If (String1='') or (String2='') then Exit;
{$IFDEF ASSEMBLY}
   asm // 1090108: By Ozz (FASTER THAN Length()!)
     MOV EAX, String1;       // Store Str Address
     MOV EAX, [EAX-$04]; // Move to "Size" Int32
     MOV N, EAX;    // Put into Result
     MOV EAX, String2;       // Store Str Address
     MOV EAX, [EAX-$04]; // Move to "Size" Int32
     MOV M, EAX;    // Put into Result
   End;
{$ELSE}
   N:=Length(String1);
   M:=Length(String2);
{$ENDIF}
{$IFNDEF VER100}
  setlength(Matrix,n+1,m+1);
{$ENDIF}
  if n=0 then distanz:=m
  else begin
    if m=0 then distanz:=n
    else begin
      for i:=0 to n do Matrix[i,0]:=i;
      for j:=0 to m do Matrix[0,j]:=j;
      for i:=1 to n do begin
        s_i:=String1[i];
        for j:=1 to m do begin
          t_j:=String2[j];
          if s_i=t_j then cost:=0
          else cost:=1;
          Matrix[i,j]:=Min(Min(Matrix[i-1,j]+1,Matrix[i,j-1]+1),Matrix[i-1,j-1]+cost);
        end;
      end;
      distanz:=Matrix[n,m];
    end;
  end;
  if m>n then maxl:=m
  else maxl:=n;
  result:=1-(distanz/maxl);
  if result<0 then result:=0
  else Result:=Result*100;
end;

function VarcharParamToString(AParam : PParamVary) : String;
begin
  SetLength(Result, AParam^.vary_length);
  Move(AParam^.vary_string, Result[1], AParam^.vary_length);
end;

function levenshtein(AString1, AString2 : PParamVary) : PDouble; export; cdecl;
begin
  if (Assigned(AString1)) and
     (Assigned(AString2)) then
  begin
    Result := ib_util_malloc(SizeOf(Double));
    Result^ := _Levenshtein(VarcharParamToString(AString1), VarcharParamToString(AString2));
  end
  else
  begin
    Result := nil;
  end;
end;

exports
  levenshtein;

begin
  IsMultiThread := true;
end.

