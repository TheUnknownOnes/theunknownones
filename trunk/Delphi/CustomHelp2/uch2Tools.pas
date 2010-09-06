unit uch2Tools;

interface

uses
  SysUtils, StrUtils;

function ch2StrEncodeURL(AStr : AnsiString) : String;

function ch2StripTags(line: string; c1, c2: Char): string;

implementation

function ch2StrEncodeURL(AStr : AnsiString) : String;
var
  c : AnsiChar;
begin
  Result := '';

  for c in AStr do
    Result := Result + '%' + IntToHex(Ord(c), 2);
end;

function ch2StripTags( line: string; c1, c2: Char): string;
//published by mkinzler
//http://www.delphipraxis.net/727100-post14.html
var
   p, p1, p2, pr, pt: PChar;
   res: string;
   Buf: string;
   i: Integer;
begin
   p:= PChar(line);
   SetLength( Buf, Length( line));
   while( p <> nil) and ( p <> '') do
   begin
      p1 := StrScan( p, c1);
      if p1 <> nil then
      begin
        p2 := StrScan( p1, c2);
        pt := StrScan( p1+1, c1);
        if pt <> Nil then
            if pt < p2 then //Weitere Tagöffnung vor Tagschliessung
            begin
              pr := PChar( Buf);
              StrLCopy( pr, p, p1-p);
              Res := Res + pr;
              p := p1;
              p1 := pt;
            end;
        if p2 <> Nil then
        begin
          pr := PChar( Buf);
          StrLCopy( pr, p, p1-p);
          Res := Res + pr;
          p := p2+1;
        end
        else
        begin
          Res := Res + p;
          p:= nil;
        end
      end
      else
      begin
        Res := Res + p;
        p:= nil;
      end;
   end;
   result := res;
end;

end.
