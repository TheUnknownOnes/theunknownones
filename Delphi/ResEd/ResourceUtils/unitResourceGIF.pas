unit unitResourceGIF;

interface

{$i ..\Source\defines.inc}

uses Windows, Classes, SysUtils, graphics,
     {$ifdef HaveGIFImage} gifimg, {$else} resed_gifimage, {$endif}
     unitResourceElement,
     unitResourceGraphics;

type
//------------------------------------------------------------------------
// GIF resource Element class

  TGifResourceElement = class (TGraphicsResourceElement)
  protected
    function GetHeight: Integer; override;
    function GetPixelFormat: TPixelFormat; override;
    function GetWidth: Integer; override;
    procedure InitNew; override;
    class function SupportsData (Size : Integer; data : Pointer) : Boolean; override;
  public
    class function GetBaseType : AnsiString; override;
    procedure GetImage (picture : TPicture); override;
  end;


implementation

{ TGifResourceElement }

class function TGifResourceElement.GetBaseType: AnsiString;
begin
  Result := 'GIF';
end;

function TGifResourceElement.GetHeight: Integer;
begin
  Result := PWORD (PAnsiChar (data) + 6 + SizeOf (Word))^;
end;

procedure TGifResourceElement.GetImage(picture: TPicture);
begin
  picture.graphic := TGifImage.Create;
  data.Seek (0, soFromBeginning);
  TGifImage (picture.graphic).LoadFromStream (data)
end;

function TGifResourceElement.GetPixelFormat: TPixelFormat;
begin
  Result := pf8Bit;
end;

function TGifResourceElement.GetWidth: Integer;
begin
  result := PWORD (PAnsiChar (data) + 6)^;
end;

procedure TGifResourceElement.InitNew;
var
  img : TGIFImage;
  bmp : TBitmap;
begin
  bmp := nil;
  img := TGIFImage.Create;
  try
    bmp := TBitmap.Create;
    bmp.Width := 64;
    bmp.Height := 64;
    img.Assign(bmp);
    img.SaveToStream (data);
  finally
    img.Free;
    bmp.Free
  end
end;

class function TGifResourceElement.SupportsData(Size: Integer;
  data: Pointer): Boolean;
var
  p : PAnsiChar;
begin
  p := PAnsiChar (data);

  Result := (StrLIComp (p, 'GIF87', 5) = 0) or (StrLIComp (p, 'GIF89', 5) = 0);
end;

initialization
  RegisterResourceElement (TGIFResourceElement);
finalization
  UnregisterResourceElement (TGIFResourceElement);
end.
