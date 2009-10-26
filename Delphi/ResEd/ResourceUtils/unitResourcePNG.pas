//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit unitResourcePNG;

interface

{$i defines.inc}

uses Windows, Classes, SysUtils, graphics
     {$ifdef HavePNGImage}, PNGImage {$else}, resEd_PngImage {$endif}
     ,unitResourceElement,
     unitResourceGraphics;

type
//------------------------------------------------------------------------
// PNG resource Element class

  TPngResourceElement = class (TGraphicsResourceElement)
  protected
    function GetHeight: Integer; override;
    function GetPixelFormat: TPixelFormat; override;
    function GetWidth: Integer; override;
    class function SupportsData (Size : Integer; data : Pointer) : Boolean; override;
  public
    class function GetBaseType : AnsiString; override;
    procedure GetImage (picture : TPicture); override;
  end;


implementation

{ TPngResourceElement }

class function TPngResourceElement.GetBaseType: AnsiString;
begin
  Result := 'PNG';
end;

function TPngResourceElement.GetHeight: Integer;
begin
  Result := PWORD (PAnsiChar (data) + 6 + SizeOf (Word))^;
end;

procedure TPngResourceElement.GetImage(picture: TPicture);
var
  myPNG : TPNGObject;
begin
  myPNG:=TPNGObject.Create;
  data.Seek (0, soFromBeginning);
  myPNG.LoadFromStream(data);
  picture.Graphic:=myPNG;
end;

function TPngResourceElement.GetPixelFormat: TPixelFormat;
begin
  Result := pf8Bit;
end;

function TPngResourceElement.GetWidth: Integer;
begin
  result := PWORD (PAnsiChar (data) + 6)^;
end;

class function TPngResourceElement.SupportsData(Size: Integer;
  data: Pointer): Boolean;
var
  p : PAnsiChar;
begin
  p := PAnsiChar (data);
  Inc (p);

  Result := (StrLIComp (p, 'PNG', 3) = 0);
end;

initialization
  RegisterResourceElement (TPngResourceElement);
finalization
  UnregisterResourceElement (TPngResourceElement);
end.


