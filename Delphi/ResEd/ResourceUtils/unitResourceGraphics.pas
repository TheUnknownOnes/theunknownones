(*======================================================================*
 | unitResourceGraphics                                                 |
 |                                                                      |
 | Encapsulates graphics in resources (icon, cursor, bitmap)            |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      05/01/2001  CPWW  Original                                  |
 *======================================================================*)

unit unitResourceGraphics;

interface

uses Windows, Classes, SysUtils, unitResourceElement, graphics, resed_unitExIcon
     {$ifndef VER200}, resEd_PngImage, resed_gifimage
     {$else}, PngImage
     {$endif};

type

//------------------------------------------------------------------------
// Base class

  TGraphicsResourceElement = class (TResourceElement)
  protected
    function GetHeight: Integer; virtual; abstract;
    function GetPixelFormat: TPixelFormat; virtual; abstract;
    function GetWidth: Integer; virtual; abstract;
  public
    procedure GetImage (picture : TPicture); virtual; abstract;
    procedure SetImage (image : TPicture); virtual;

    property Width : Integer read GetWidth;
    property Height : Integer read GetHeight;
    property PixelFormat : TPixelFormat read GetPixelFormat;
  end;

  TGraphicsResourceElementClass = class of TGraphicsResourceElement;

//------------------------------------------------------------------------
// Bitmap resource Element class

  TBitmapResourceElement = class (TGraphicsResourceElement)
  protected
    function GetHeight: Integer; override;
    function GetPixelFormat: TPixelFormat; override;
    function GetWidth: Integer; override;
    procedure InitNew; override;
    procedure InternalGetImage (s : TStream; picture : TPicture);
    procedure InternalSetImage (s : TStream; image : TPicture);

  public
    class function GetBaseType : AnsiString; override;
    procedure GetImage (picture : TPicture); override;
    procedure SetImage (image : TPicture); override;
    procedure LoadImage (const FileName : AnsiString);
  end;

//------------------------------------------------------------------------
// DIB resource Element class
//
// Same as RT_BITMAP resources, but they have a TBitmapFileHeader at the start
// of the resource, before the TBitmapInfoHeader.  See
// \program files\Microsoft Office\office\1033\outlibr.dll

  TDIBResourceElement = class (TBitmapResourceElement)
  protected
    class function SupportsData (Size : Integer; data : Pointer) : Boolean; override;
    procedure InitNew; override;
  public
    class function GetBaseType : AnsiString; override;
    procedure GetImage (picture : TPicture); override;
    procedure SetImage (image : TPicture); override;
  end;

  TIconCursorResourceElement = class;

//------------------------------------------------------------------------
// Icon / Cursor group resource Element class

  TIconCursorGroupResourceElement = class (TResourceElement)
  private
    fDeleting : Boolean;
    function GetResourceCount: Integer;
    function GetResourceElement(idx: Integer): TIconCursorResourceElement;
  protected
    procedure InitNew; override;
  public
    procedure GetImage (picture : TPicture);
    property ResourceCount : Integer read GetResourceCount;
    property ResourceElement [idx : Integer] : TIconCursorResourceElement read GetResourceElement;
    function Contains (Element : TIconCursorResourceElement) : Boolean;
    procedure RemoveFromGroup (Element : TIconCursorResourceElement);
    procedure AddToGroup (Element : TIconCursorResourceElement);
    procedure LoadImage (const FileName : AnsiString);
    procedure BeforeDelete; override;
  end;

//------------------------------------------------------------------------
// Icon group resource Element class

  TIconGroupResourceElement = class (TIconCursorGroupResourceElement)
  public
    class function GetBaseType : AnsiString; override;
  end;

//------------------------------------------------------------------------
// Cursor group resource Element class

  TCursorGroupResourceElement = class (TIconCursorGroupResourceElement)
  public
    class function GetBaseType : AnsiString; override;
  end;

//------------------------------------------------------------------------
// Icon / Cursor resource Element class

  TIconCursorResourceElement = class (TGraphicsResourceElement)
  private
    BitDepth : Integer;
  protected
    function GetHeight: Integer; override;
    function GetPixelFormat: TPixelFormat; override;
    function GetWidth: Integer; override;
  protected
    procedure InitNew; override;
  public
    procedure BeforeDelete; override;
    procedure GetImage (picture : TPicture); override;
    procedure SetImage (image : TPicture); override;
    property Width : Integer read GetWidth;
    property Height : Integer read GetHeight;
    property PixelFormat : TPixelFormat read GetPixelFormat;
  end;

//------------------------------------------------------------------------
// Icon resource Element class

  TIconResourceElement = class (TIconCursorResourceElement)
  public
    class function GetBaseType : AnsiString; override;
  end;

//------------------------------------------------------------------------
// Cursor resource Element class

  TCursorResourceElement = class (TIconCursorResourceElement)
  protected
  public
    class function GetBaseType : AnsiString; override;
  end;

const
  DefaultIconCursorWidth : Integer = 32;
  DefaultIconCursorHeight : Integer = 32;
  DefaultIconCursorPixelFormat : TPixelFormat = pf4Bit;
  DefaultCursorHotspot : DWord = $00100010;

  DefaultBitmapWidth : Integer = 128;
  DefaultBitmapHeight : Integer = 96;
  DefaultBitmapPixelFormat : TPixelFormat = pf24Bit;

implementation

type

TResourceDirectory = packed record
  Element : packed record case boolean of
    False : (cursorWidth, cursorHeight : word);
    True : (iconWidth, iconHeight, iconColorCount, iconReserved : BYTE)
  end;
  wPlanes, wBitCount : word;
  lBytesInRes : DWORD;
  wNameOrdinal : word
end;
PResourceDirectory = ^TResourceDirectory;

resourcestring
  rstCursors = 'Cursors';
  rstIcons = 'Icons';

{ TBitmapResourceElement }

(*----------------------------------------------------------------------*
 | TBitmapResourceElement.GetBaseType                                   |
 *----------------------------------------------------------------------*)
class function TBitmapResourceElement.GetBaseType: AnsiString;
begin
  result := IntToStr (Integer (RT_BITMAP));
end;

(*----------------------------------------------------------------------*
 | TBitmapResourceElement.GetHeight                                     |
 *----------------------------------------------------------------------*)
function TBitmapResourceElement.GetHeight: Integer;
begin
  result := PBitmapInfoHeader (data.Memory)^.biHeight
end;

(*----------------------------------------------------------------------*
 | TBitmapResourceElement.GetImage                                      |
 *----------------------------------------------------------------------*)
procedure TBitmapResourceElement.GetImage(picture: TPicture);
var
  s : TMemoryStream;
  hdr : TBitmapFileHeader;
begin
  s := TMemoryStream.Create;
  try
    hdr.bfType :=$4D42;         // TBitmap.LoadFromStream requires a bitmapfileheader
    hdr.bfSize := data.size;    // before the data...
    hdr.bfReserved1 := 0;
    hdr.bfReserved2 := 0;
    hdr.bfOffBits := sizeof (hdr);

    s.Write (hdr, sizeof (hdr));
    data.Seek (0, soFromBeginning);
    s.CopyFrom (data, data.size);

    InternalGetImage (s, picture)
  finally
    s.Free
  end
end;

(*----------------------------------------------------------------------*
 | TBitmapResourceElement.GetPixelFormat                                |
 *----------------------------------------------------------------------*)
function TBitmapResourceElement.GetPixelFormat: TPixelFormat;
begin
  result := GetBitmapInfoPixelFormat (PBitmapInfoHeader (data.Memory)^);
end;

(*----------------------------------------------------------------------*
 | TBitmapResourceElement.GetWidth                                      |
 *----------------------------------------------------------------------*)
function TBitmapResourceElement.GetWidth: Integer;
begin
  result := PBitmapInfoHeader (data.Memory)^.biWidth
end;

(*----------------------------------------------------------------------*
 | TBitmapResourceElement.SetImage                                      |
 *----------------------------------------------------------------------*)
procedure TBitmapResourceElement.InitNew;
var
  bi : TBitmapInfoHeader;
  imageSize : DWORD;
  bits : PAnsiChar;
begin
  bi.biSize := SizeOf (bi);
  bi.biWidth := DefaultBitmapWidth;
  bi.biHeight := DefaultBitmapHeight;
  bi.biPlanes := 1;
  bi.biBitCount := GetPixelFormatBitCount (DefaultBitmapPixelFormat);
  bi.biCompression := BI_RGB;

  imageSize := BytesPerScanLine (DefaultBitmapWidth, bi.biBitCount, 32) * DefaultBitmapHeight;
  bi.biSizeImage := imageSize;

  bi.biXPelsPerMeter := 0;
  bi.biYPelsPerMeter := 0;

  bi.biClrUsed := 0;
  bi.biClrImportant := 0;

  data.Write (bi, SizeOf (bi));

  bits := AllocMem (ImageSize);
  try
    data.Write (bits^, ImageSize);
  finally
    ReallocMem (bits, 0)
  end
end;

procedure TBitmapResourceElement.InternalGetImage(s : TStream; picture: TPicture);
var
  pHdr : PBitmapInfoHeader;
  pal : HPalette;
  colors : DWORD;
  hangOnToPalette : Boolean;
  newBmp : TBitmap;
begin
  s.Seek (0, soFromBeginning);
  picture.Bitmap.IgnorePalette := False;
  picture.Bitmap.LoadFromStream (s);

  pHdr := PBitmapInfoHeader (data.Memory);

                              // TBitmap makes all RLE encoded bitmaps into pfDevice
                              // ... that's not good enough for us!  At least
                              // select the correct pixel format, preserve their carefully set
                              // up palette, etc.
                              //
                              // But revisit this - we probably shouldn't call LoadFromStream
                              // at all if this is the case...
                              //
                              // You can get a couple of RLE bitmaps out of winhlp32.exe

  if PHdr^.biCompression in [BI_RLE4, BI_RLE8] then
  begin
    hangOnToPalette := False;
    if pHdr^.biBitCount in [1, 4, 8] then
    begin
      pal := picture.Bitmap.Palette;
      if pal <> 0 then
      begin
        colors := 0;
        GetObject (pal, SizeOf (colors), @Colors);

        if colors = 1 shl pHdr^.biBitCount then
        begin
          hangOnToPalette := True;

          newBmp := TBitmap.Create;
          try
            case pHdr^.biBitCount of
              1 : newBmp.PixelFormat := pf1Bit;
              4 : newBmp.PixelFormat := pf4Bit;
              8 : newBmp.PixelFormat := pf8Bit;
            end;

            newBmp.Width := Picture.Bitmap.Width;
            newBmp.Height := Picture.Bitmap.Height;
            newBmp.Palette := CopyPalette (pal);
            newBmp.Canvas.Draw (0, 0, picture.Bitmap);
            picture.Bitmap.Assign (newBmp);
          finally
            newBmp.Free
          end
        end
      end
    end;

    if not hangOnToPalette then
      case pHdr^.biBitCount of
        1 : picture.Bitmap.PixelFormat := pf1Bit;
        4 : picture.Bitmap.PixelFormat := pf4Bit;
        8 : picture.Bitmap.PixelFormat := pf8Bit;
        else
          picture.Bitmap.PixelFormat := pf24Bit
      end
  end
end;

(*----------------------------------------------------------------------*
 | TBitmapResourceElement.InternalSetImage                              |
 |                                                                      |
 | Save image 'image' to stream 's' as a bitmap                         |
 |                                                                      |
 | Parameters:                                                          |
 |                                                                      |
 |   s : TStream           The stream to save to                        |
 |   image : TPicture      The image to save                            |
 *----------------------------------------------------------------------*)
procedure TBitmapResourceElement.InternalSetImage(s: TStream; image: TPicture);
var
  bmp : TBitmap;
begin
  s.Size := 0;
  bmp := TBitmap.Create;
  try
    bmp.Assign (image.graphic);
    bmp.SaveToStream (s);
  finally
    bmp.Free;
  end
end;

(*----------------------------------------------------------------------*
 | TBitmapResourceElement.SetImage                                      |
 *----------------------------------------------------------------------*)
procedure TBitmapResourceElement.LoadImage(const FileName: AnsiString);
var
  s : TMemoryStream;
begin
  s := TMemoryStream.Create;
  try
    s.LoadFromFile(FileName);
    data.Clear;
    data.Write ((PAnsiChar (s.Memory) + sizeof (TBitmapFileHeader))^, s.Size - sizeof (TBitmapFileHeader));
  finally
    s.Free;
  end
end;

procedure TBitmapResourceElement.SetImage(image : TPicture);
var
  s : TMemoryStream;
begin
  s := TMemoryStream.Create;
  try
    InternalSetImage (s, image);
    data.Clear;
    data.Write ((PAnsiChar (s.Memory) + sizeof (TBitmapFileHeader))^, s.Size - sizeof (TBitmapFileHeader));
  finally
    s.Free;
  end
end;

{ TIconGroupResourceElement }

(*----------------------------------------------------------------------*
 | TIconGroupResourceElement.GetBaseType                                |
 *----------------------------------------------------------------------*)
class function TIconGroupResourceElement.GetBaseType: AnsiString;
begin
  result := IntToStr (Integer (RT_GROUP_ICON));
end;

{ TCursorGroupResourceElement }

(*----------------------------------------------------------------------*
 | TCursorGroupResourceElement.GetBaseType                              |
 *----------------------------------------------------------------------*)
class function TCursorGroupResourceElement.GetBaseType: AnsiString;
begin
  result := IntToStr (Integer (RT_GROUP_CURSOR));
end;

{ TIconResourceElement }

(*----------------------------------------------------------------------*
 | TIconResourceElement.GetBaseType                                     |
 *----------------------------------------------------------------------*)
class function TIconResourceElement.GetBaseType: AnsiString;
begin
  result := IntToStr (Integer (RT_ICON));
end;

{ TCursorResourceElement }

(*----------------------------------------------------------------------*
 | TCursorResourceElement.GetBaseType                                   |
 *----------------------------------------------------------------------*)
class function TCursorResourceElement.GetBaseType: AnsiString;
begin
  result := IntToStr (Integer (RT_CURSOR));
end;

{ TGraphicsResourceElement }


{ TIconCursorResourceElement }

(*----------------------------------------------------------------------*
 | TIconCursorResourceElement.GetHeight                                 |
 *----------------------------------------------------------------------*)
function TIconCursorResourceElement.GetHeight: Integer;
var
  infoHeader : PBitmapInfoHeader;
begin
  if self is TCursorResourceElement then        // Not very 'OOP'.  Sorry
    infoHeader := PBitmapInfoHeader (PAnsiChar (data.Memory) + sizeof (DWORD))
  else
    infoHeader := PBitmapInfoHeader (PAnsiChar (data.Memory));

  result := infoHeader.biHeight div 2
end;

{Characters for the header}
const
  PngHeader: Array[0..7] of AnsiChar = (#137, #80, #78, #71, #13, #10, #26, #10);

(*----------------------------------------------------------------------*
 | TIconCursorResourceElement.GetImage                                  |
 *----------------------------------------------------------------------*)
procedure TIconCursorResourceElement.GetImage(picture: TPicture);
var
  iconCursor : TExIconCursor;
  strm : TMemoryStream;
  hdr : TIconHeader;
  dirEntry : TIconDirEntry;
  infoHeader : PBitmapInfoHeader;
  isPNG : Boolean;
  APNG  : TPNGObject;
  Header : Array[0..7] of AnsiChar;
begin
  if data.Size = 0 then Exit;

  strm := Nil;
  if self is TCursorResourceElement then
  begin
    hdr.wType := 2;
    infoHeader := PBitmapInfoHeader (PAnsiChar (data.Memory) + sizeof (DWORD));
    iconCursor := TExCursor.Create
  end
  else
  begin
    hdr.wType := 1;

    Data.Seek(0, soBeginning);
    Data.Read(Header,8);
    isPNG:=Header=pngHeader;

    if not isPNG then
      infoHeader := PBitmapInfoHeader (PAnsiChar (data.Memory));
    iconCursor := TExIcon.Create
  end;

  try
    strm := TMemoryStream.Create;
    hdr.wReserved := 0;
    hdr.wCount := 1;

    strm.Write (hdr, sizeof (hdr));

    if not isPNG then
    begin
      dirEntry.bWidth := infoHeader^.biWidth;
      dirEntry.bHeight := infoHeader^.biHeight div 2;
      dirEntry.bColorCount := GetBitmapInfoNumColors (infoHeader^);
      dirEntry.bReserved := 0;

      dirEntry.wPlanes := infoHeader^.biPlanes;
      dirEntry.wBitCount := infoHeader^.biBitCount;
    end
    else
    begin
      APNG:=TPNGObject.Create;
      APNG.LoadFromStream(data);

      dirEntry.bWidth := 0;
      dirEntry.bHeight := 0;
      dirEntry.bColorCount := 0;
      dirEntry.bReserved := 0;

      dirEntry.wPlanes := 1;
      dirEntry.wBitCount := APNG.Header.BitDepth;
      APNG.Free;
      Data.Seek(0, soBeginning);
    end;

    dirEntry.dwBytesInRes := data.Size;
    dirEntry.dwImageOffset := sizeof (hdr) + sizeof (dirEntry);

    strm.Write (dirEntry, sizeof (dirEntry));
    strm.CopyFrom (data, 0);

    strm.Seek (0, soFromBeginning);

    iconcursor.LoadFromStream (strm);
    picture.Graphic := iconcursor
  finally
    strm.Free;
    iconcursor.Free
  end
end;

(*----------------------------------------------------------------------*
 | TIconCursorResourceElement.SetImage                                  |
 *----------------------------------------------------------------------*)
procedure TIconCursorResourceElement.SetImage(image: TPicture);
var
  icon : TExIconCursor;
begin
  icon := TExIconCursor (image.graphic);
  data.Clear;
  data.CopyFrom (icon.Images [icon.CurrentImage].MemoryImage, 0);
end;


(*----------------------------------------------------------------------*
 | TIconCursorResourceElement.GetPixelFormat                            |
 *----------------------------------------------------------------------*)
function TIconCursorResourceElement.GetPixelFormat: TPixelFormat;
var
  infoHeader : PBitmapInfoHeader;
begin

    if self is TCursorResourceElement then
      infoHeader := PBitmapInfoHeader (PAnsiChar (data.Memory) + sizeof (DWORD))
    else
      infoHeader := PBitmapInfoHeader (PAnsiChar (data.Memory));

    result := GetBitmapInfoPixelFormat (infoHeader^);

end;

(*----------------------------------------------------------------------*
 | TIconCursorResourceElement.GetWidth                                  |
 *----------------------------------------------------------------------*)
function TIconCursorResourceElement.GetWidth: Integer;
var
  infoHeader : PBitmapInfoHeader;
begin
  if self is TCursorResourceElement then
    infoHeader := PBitmapInfoHeader (PAnsiChar (data.Memory) + sizeof (DWORD))
  else
    infoHeader := PBitmapInfoHeader (PAnsiChar (data.Memory));

  result := infoHeader.biWidth
end;

{ TIconCursorGroupResourceElement }

(*----------------------------------------------------------------------*
 | TIconCursorGroupResourceElement.BeforeDelete
 |                                                                      |
 *----------------------------------------------------------------------*)
procedure TIconCursorGroupResourceElement.AddToGroup(
  Element: TIconCursorResourceElement);
var
  attributes : PResourceDirectory;
  infoHeader : PBitmapInfoHeader;
  cc : Integer;
begin
  data.Size := Data.Size + sizeof (TResourceDirectory);
  attributes := PResourceDirectory (PAnsiChar (Data.Memory) + sizeof (TIconHeader));

  Inc (Attributes, PIconHeader (data.Memory)^.wCount);

  attributes^.wNameOrdinal :=  StrToInt (Element.ResourceName);
  attributes^.lBytesInRes := Element.Data.Size;

  if Element is TIconResourceElement then
  begin
    infoHeader := PBitmapInfoHeader (PAnsiChar (Element.data.Memory));
    attributes^.Element.iconWidth := infoHeader^.biWidth;
    attributes^.Element.iconHeight := infoHeader^.biHeight div 2;
    cc := GetBitmapInfoNumColors (infoHeader^);
    if cc < 256 then
      attributes^.Element.iconColorCount := cc
    else
      attributes^.Element.iconColorCount := 0;
    attributes^.Element.iconReserved := 0
  end
  else
  begin
    infoHeader := PBitmapInfoHeader (PAnsiChar (Element.data.Memory) + sizeof (DWORD));
    attributes^.Element.cursorWidth := infoHeader^.biWidth;
    attributes^.Element.cursorHeight := infoHeader^.biHeight div 2
  end;

  attributes^.wPlanes := infoHeader^.biPlanes;
  attributes^.wBitCount := infoHeader^.biBitCount;

  Inc (PIconHeader (data.Memory)^.wCount);
end;

procedure TIconCursorGroupResourceElement.BeforeDelete;
begin
  fDeleting := True;
  try
    while ResourceCount > 0 do
      Parent.DeleteResource (Parent.IndexOfResource (ResourceElement [0]));
  finally
    fDeleting := False
  end
end;

(*----------------------------------------------------------------------*
 | TIconCursorGroupResourceElement.Contains                             |
 *----------------------------------------------------------------------*)
function TIconCursorGroupResourceElement.Contains(
  Element: TIconCursorResourceElement): Boolean;
var
  i, id : Integer;
  attributes : PResourceDirectory;
begin
  Result := False;
  if ResourceNameToInt (Element.ResourceType) = ResourceNameToInt (ResourceType) - DIFFERENCE then
  begin
    attributes := PResourceDirectory (PAnsiChar (Data.Memory) + sizeof (TIconHeader));
    id := ResourceNameToInt (Element.ResourceName);

    for i := 0 to PIconHeader (Data.Memory)^.wCount - 1 do
      if attributes^.wNameOrdinal = id then
      begin
        Result := True;
        break
      end
      else
        Inc (attributes)
  end
end;

(*----------------------------------------------------------------------*
 | TIconCursorGroupResourceElement.GetImage                             |
 *----------------------------------------------------------------------*)
procedure TIconCursorGroupResourceElement.GetImage(picture: TPicture);
var
  i, hdrOffset, imgOffset : Integer;
  iconCursor : TExIconCursor;
  strm : TMemoryStream;
  hdr : TIconHeader;
  dirEntry : TIconDirEntry;
  pdirEntry : PIconDirEntry;
  infoHeader : PBitmapInfoHeader;
  Header : Array[0..7] of AnsiChar;
  isPNG : Boolean;
  APNG : TPNGObject;
begin
  if data.Size = 0 then Exit;

  strm := Nil;
  if self is TCursorGroupResourceElement then
  begin
    hdr.wType := 2;
    hdrOffset := SizeOf (DWORD);
    iconCursor := TExCursor.Create
  end
  else
  begin
    hdr.wType := 1;
    hdrOffset := 0;
    iconCursor := TExIcon.Create;
  end;

  try
    strm := TMemoryStream.Create;
    hdr.wReserved := 0;
    hdr.wCount := ResourceCount;

    strm.Write (hdr, sizeof (hdr));

    for i := 0 to ResourceCount - 1 do
    begin
      ResourceElement[i].Data.Seek(0, soBeginning);
      ResourceElement[i].Data.Read(Header,8);
      isPNG:=Header=pngHeader;
      ResourceElement[i].Data.Seek(0, soBeginning);

      if IsPNG then
      begin
        APNG:=TPNGObject.Create;
        APNG.LoadFromStream(ResourceElement[i].Data);
        APNG.Header.BitDepth:=TIconCursorResourceElement(ResourceElement[i]).BitDepth;

        dirEntry.bWidth := 0;
        dirEntry.bHeight := 0;
        dirEntry.wPlanes := 1;
        dirEntry.bColorCount := 0;
        dirEntry.bReserved := 0;
        dirEntry.wBitCount := APNG.Header.BitDepth;
        dirEntry.dwBytesInRes := resourceElement [i].data.Size;
        dirEntry.dwImageOffset := 0;
        APNG.Free;
        ResourceElement[i].Data.Seek(0, soBeginning);
      end
      else
      begin
        infoHeader := PBitmapInfoHeader (PAnsiChar (ResourceElement [i].Data.Memory) + hdrOffset);
        dirEntry.bWidth := infoHeader^.biWidth;
        dirEntry.bHeight := infoHeader^.biHeight div 2;
        dirEntry.wPlanes := infoHeader^.biPlanes;
        dirEntry.bColorCount := GetBitmapInfoNumColors (infoHeader^);
        dirEntry.bReserved := 0;
        dirEntry.wBitCount := infoHeader^.biBitCount;
        dirEntry.dwBytesInRes := resourceElement [i].data.Size;
        dirEntry.dwImageOffset := 0;
      end;

      strm.Write (dirEntry, sizeof (dirEntry));
    end;

    for i := 0 to ResourceCount - 1 do
    begin
      imgOffset := strm.Position;
      pDirEntry := PIconDirEntry (PAnsiChar (strm.Memory) + SizeOf (TIconHeader) + i * SizeOf (TIconDirEntry));
      pDirEntry^.dwImageOffset := imgOffset;

      strm.CopyFrom (ResourceElement [i].Data, 0);
    end;

    if ResourceCount > 0 then
    begin
      strm.Seek (0, soFromBeginning);
      iconcursor.LoadFromStream (strm);
      picture.Graphic := iconcursor;
    end
    else
      picture.Graphic := Nil
  finally
    strm.Free;
    iconcursor.Free
  end
end;

(*----------------------------------------------------------------------*
 | TIconCursorGroupResourceElement.GetResourceCount                     |
 *----------------------------------------------------------------------*)
function TIconCursorGroupResourceElement.GetResourceCount: Integer;
begin
  result := PIconHeader (Data.Memory)^.wCount
end;

(*----------------------------------------------------------------------*
 | TIconCursorGroupResourceElement.GetResourceElement                   |
 *----------------------------------------------------------------------*)
function GetPixelFormatFromBitDepth(ABD: Integer): TPixelFormat;
begin
  case ABD of
      1  : result := pf1bit;
      4  : result := pf4bit;
      8  : result := pf8bit;
      15 : result := pf16bit; // 16 bpp RGB.  1 unused, 5 R, 5 G, 5 B
      16 : result := pf16bit; // 16 bpp BITFIELDS
      24 : result := pf24bit;
      32 : result := pf32bit;  // Either RGB (8 unused, 8 R, 8 G, 8 B) or 32 bit BITFIELDS
    end;
end;

function TIconCursorGroupResourceElement.GetResourceElement(
  idx: Integer): TIconCursorResourceElement;
var
  i : Integer;
  res : TResourceElement;
  attributes : PResourceDirectory;
  iconCursorResourceType : AnsiString;
begin
  result := Nil;
  attributes := PResourceDirectory (PAnsiChar (Data.Memory) + sizeof (TIconHeader));
  Inc (attributes, idx);

  // DIFFERENCE (from Windows.pas) is 11.  It's the difference between a 'group
  // resource' and the resource itself.  They called it 'DIFFERENCE' to be annoying.

  iconCursorResourceType := IntToStr (ResourceNameToInt (ResourceType) - DIFFERENCE);
  for i := 0 to Parent.ResourceCount - 1 do
  begin
    res := Parent.ResourceElement [i];
    if (res is TIconCursorResourceElement) and (iconCursorResourceType = res.ResourceType) and (attributes.wNameOrdinal = ResourceNameToInt (res.ResourceName)) then
    begin
      result := TIconCursorResourceElement (res);
      result.BitDepth:=attributes.wBitCount;
      break;
    end
  end
end;

(*----------------------------------------------------------------------*
 | TIconCursorGroupResourceElement.InitNew                              |
 *----------------------------------------------------------------------*)
procedure TIconCursorGroupResourceElement.InitNew;
var
  imageResource : TIconCursorResourceElement;
  iconHeader : TIconHeader;
  dir : TResourceDirectory;
  nm : AnsiString;

begin
  iconHeader.wCount := 1;
  iconHeader.wReserved := 0;

  if Self is TCursorGroupResourceElement then
  begin
    iconHeader.wType := 2;
    nm := Parent.GetUniqueResourceName (TCursorResourceElement.GetBaseType);
    imageResource := TCursorResourceElement.CreateNew (Parent, ResourceLanguage, nm)
  end
  else
  begin
    iconHeader.wType := 1;
    nm := Parent.GetUniqueResourceName (TIconResourceElement.GetBaseType);
    imageResource := TIconResourceElement.CreateNew (Parent, ResourceLanguage, nm)
  end;

  data.Write (iconHeader, SizeOf (iconHeader));

  if Self is TIconGroupResourceElement then
  begin
    dir.Element.iconWidth := DefaultIconCursorWidth;
    dir.Element.iconHeight := DefaultIconCursorHeight;
    dir.Element.iconColorCount := GetPixelFormatNumColors (DefaultIconCursorPixelFormat);
    dir.Element.iconReserved := 0
  end
  else
  begin
    dir.Element.cursorWidth := DefaultIconCursorWidth;
    dir.Element.cursorHeight := DefaultIconCursorHeight
  end;

  dir.wPlanes := 1;
  dir.wBitCount := GetPixelFormatBitCount (DefaultIconCursorPixelFormat);
  dir.lBytesInRes := imageResource.Data.Size;
  dir.wNameOrdinal := ResourceNametoInt (imageResource.ResourceName);

  data.Write (dir, SizeOf (dir));
end;

(*----------------------------------------------------------------------*
 | TIconCursorResourceElement.BeforeDelete                              |
 |                                                                      |
 | If we're deleting an icon/curor resource, remove its reference from  |
 | the icon/cursor group resource.                                      |
 *----------------------------------------------------------------------*)
procedure TIconCursorResourceElement.BeforeDelete;
var
  i : Integer;
  Element : TResourceElement;
  resGroup : TIconCursorGroupResourceElement;
begin
  for i := 0 to Parent.ResourceCount - 1 do
  begin
    Element := Parent.ResourceElement [i];
    if (Element.ResourceType = IntToStr (ResourceNameToInt (ResourceType) + DIFFERENCE)) then
    begin
      resGroup := Element as TIconCursorGroupResourceElement;
      if resGroup.Contains (Self) then
      begin
        resGroup.RemoveFromGroup (Self);
        break
      end
    end
  end
end;

procedure TIconCursorGroupResourceElement.LoadImage(
  const FileName: AnsiString);
var
  img : TExIconCursor;
  hdr : TIconHeader;
  i : Integer;
  dirEntry : TResourceDirectory;
  res : TIconCursorResourceElement;
  resTp : AnsiString;
begin
  BeforeDelete;         // Make source there are no existing image resources

  if Self is TIconGroupResourceElement then
  begin
    hdr.wType := 1;
    img := TExIcon.Create;
    resTp := TIconResourceElement.GetBaseType;
  end
  else
  begin
    hdr.wType := 2;
    img := TExCursor.Create;
    resTp := TCursorResourceElement.GetBaseType;
  end;

  img.LoadFromFile (FileName);

  hdr.wReserved := 0;
  hdr.wCount := img.ImageCount;

  data.Clear;

  data.Write (hdr, SizeOf (hdr));

  for i := 0 to img.ImageCount - 1 do
  begin
    if hdr.wType = 1 then
    begin
      dirEntry.Element.iconWidth := img.Images [i].FWidth;
      dirEntry.Element.iconHeight := img.Images [i].FHeight;
      dirEntry.Element.iconColorCount := GetPixelFormatNumColors (img.Images [i].FPixelFormat);
      dirEntry.Element.iconReserved := 0
    end
    else
    begin
      dirEntry.Element.cursorWidth := img.Images [i].FWidth;
      dirEntry.Element.cursorHeight := img.Images [i].FHeight;
    end;

    dirEntry.wPlanes := 1;
    dirEntry.wBitCount := GetPixelFormatBitCount (img.Images [i].FPixelFormat);
    
    dirEntry.lBytesInRes := img.Images [i].FMemoryImage.Size;

    if hdr.wType = 1 then
      res := TIconResourceElement.Create (Parent, ResourceLanguage, Parent.GetUniqueResourceName (resTp), resTp, img.Images [i].FMemoryImage.Size, img.Images [i].FMemoryImage.Memory)
    else
      res := TCursorResourceElement.Create (Parent, ResourceLanguage, Parent.GetUniqueResourceName (resTp), resTp, img.Images [i].FMemoryImage.Size, img.Images [i].FMemoryImage.Memory);
    Parent.AddResource (res);
    dirEntry.wNameOrdinal := ResourceNameToInt (res.ResourceName);

    data.Write (dirEntry, SizeOf (dirEntry));
  end
end;

(*----------------------------------------------------------------------*
 | TIconCursorGroupResourceElement.RemoveFromGroup                      |
 *----------------------------------------------------------------------*)
procedure TIconCursorGroupResourceElement.RemoveFromGroup(
  Element: TIconCursorResourceElement);
var
  i, id, count : Integer;
  attributes, ap : PResourceDirectory;
begin
  if ResourceNametoInt (Element.ResourceType) = ResourceNameToInt (ResourceType) - DIFFERENCE then
  begin
    attributes := PResourceDirectory (PAnsiChar (Data.Memory) + sizeof (TIconHeader));
    id := ResourceNametoInt (Element.ResourceName);

    Count := PIconHeader (Data.Memory)^.wCount;

    for i := 0 to Count - 1 do
      if attributes^.wNameOrdinal = id then
      begin
        if i < Count - 1 then
        begin
          ap := Attributes;
          Inc (ap);
          Move (ap^, Attributes^, SizeOf (TResourceDirectory) * (Count - i - 1));
        end;

        Data.Size := data.Size - SizeOf (TResourceDirectory);
        PIconHeader (Data.Memory)^.wCount := Count - 1;
        if (Count = 1) and not fDeleting then
          Parent.DeleteResource (Parent.IndexOfResource (Self));
        break
      end
      else
        Inc (attributes)
  end
end;

(*----------------------------------------------------------------------*
 | TIconCursorResourceElement.InitNew                                   |
 *----------------------------------------------------------------------*)
procedure TIconCursorResourceElement.InitNew;
var
  hdr : TBitmapInfoHeader;
  cImageSize : DWORD;
  pal : HPALETTE;
  entries : PPALETTEENTRY;
  w : DWORD;
  p : PAnsiChar;

begin
  if Self is TCursorResourceElement then
    Data.Write (DefaultCursorHotspot, SizeOf (DefaultCursorHotspot));

  hdr.biSize := SizeOf (hdr);
  hdr.biWidth := DefaultIconCursorWidth;
  hdr.biHeight := DefaultIconCursorHeight * 2;
  hdr.biPlanes := 1;
  hdr.biBitCount := GetPixelFormatBitCount (DefaultIconCursorPixelFormat);

  if DefaultIconCursorPixelFormat = pf16Bit then
    hdr.biCompression := BI_BITFIELDS
  else
    hdr.biCompression := BI_RGB;

  hdr.biSizeImage := 0; // See note in unitExIcon

  hdr.biXPelsPerMeter := 0;
  hdr.biYPelsPerMeter := 0;

  hdr.biClrUsed := GetPixelFormatNumColors (DefaultIconCursorPixelFormat);
  hdr.biClrImportant := hdr.biClrUsed;

  Data.Write (hdr, SizeOf (hdr));

  pal := 0;
  case DefaultIconCursorPixelFormat of
    pf1Bit : pal := SystemPalette2;
    pf4Bit : pal := SystemPalette16;
    pf8Bit : pal := SystemPalette256
  end;

  entries := Nil;
  try
    if pal > 0 then
    begin
      GetMem (entries, hdr.biClrUsed * sizeof (PALETTEENTRY));
      GetPaletteEntries (pal, 0, hdr.biClrUsed, entries^);

      data.Write (entries^, hdr.biClrUsed * SizeOf (PALETTEENTRY))
    end
    else
      if hdr.biCompression = BI_BITFIELDS then
      begin { 5,6,5 bitfield }
        w := $0f800;  // 1111 1000 0000 0000  5 bit R mask
        data.Write (w, SizeOf (w));
        w := $07e0;   // 0000 0111 1110 0000  6 bit G mask
        data.Write (w, SizeOf (w));
        w := $001f;   // 0000 0000 0001 1111  5 bit B mask
        data.Write (w, SizeOf (w))
      end

  finally
    ReallocMem (entries, 0)
  end;

  // Write dummy image
  cImageSize := BytesPerScanLine (hdr.biWidth, hdr.biBitCount, 32) * DefaultIconCursorHeight;
  p := AllocMem (cImageSize);
  try
    data.Write (p^, cImageSize);
  finally
    ReallocMem (p, 0)
  end;

  // Write dummy mask
  cImageSize := DefaultIconCursorHeight * DefaultIconCursorWidth div 8;

  GetMem (p, cImageSize);
  FillChar (p^, cImageSize, $ff);

  try
    data.Write (p^, cImageSize);
  finally
    ReallocMem (p, 0)
  end;
end;

{ TDIBResourceElement }

class function TDIBResourceElement.GetBaseType: AnsiString;
begin
  Result := 'DIB';
end;

procedure TDIBResourceElement.GetImage(picture: TPicture);
begin
  InternalGetImage (data, Picture);
end;

procedure TDIBResourceElement.InitNew;
var
  hdr : TBitmapFileHeader;
begin
  hdr.bfType := $4d42;
  hdr.bfSize := SizeOf (TBitmapFileHeader) + SizeOf (TBitmapInfoHeader);
  hdr.bfReserved1 := 0;
  hdr.bfReserved2 := 0;
  hdr.bfOffBits := hdr.bfSize;
  data.Write (hdr, SizeOf (hdr));

  inherited;
end;

procedure TDIBResourceElement.SetImage(image: TPicture);
begin
  InternalSetImage (data, image);
end;

class function TDIBResourceElement.SupportsData(Size: Integer;
  data: Pointer): Boolean;
var
  p : PBitmapFileHeader;
  hdrSize : DWORD;
begin
  Result := False;
  p := PBitmapFileHeader (data);
  if (p^.bfType = $4d42) and (p^.bfReserved1 = 0) and (p^.bfReserved2 = 0) then
  begin
    hdrSize := PDWORD (PAnsiChar (data) + SizeOf (TBitmapFileHeader))^;

    case hdrSize of
      SizeOf (TBitmapInfoHeader) : Result := True;
      SizeOf (TBitmapV4Header) : Result := True;
      SizeOf (TBitmapV5Header) : Result := True
    end
  end
end;

{ TGraphicsResourceElement }

procedure TGraphicsResourceElement.SetImage(image: TPicture);
begin
  data.Clear;
  image.Graphic.SaveToStream (data);
end;

initialization
  //TPicture.RegisterFileFormat ('ICO', rstIcons, TExIcon);
  //TPicture.RegisterFileFormat ('CUR', rstCursors, TExCursor);
  //TPicture.UnregisterGraphicClass (TIcon);


  RegisterResourceElement (TBitmapResourceElement);
  RegisterResourceElement (TDIBResourceElement);
  RegisterResourceElement (TIconGroupResourceElement);
  RegisterResourceElement (TCursorGroupResourceElement);
  RegisterResourceElement (TIconResourceElement);
  RegisterResourceElement (TCursorResourceElement);
finalization
  //TPicture.UnregisterGraphicClass (TExIcon);
  //TPicture.UnregisterGraphicClass (TExCursor);
  //TPicture.RegisterFileFormat ('ICO', 'Icon', TIcon);
  UnregisterResourceElement (TCursorResourceElement);
  UnregisterResourceElement (TIconResourceElement);
  UnregisterResourceElement (TCursorGroupResourceElement);
  UnregisterResourceElement (TIconGroupResourceElement);
  UnregisterResourceElement (TDIBResourceElement);
  UnregisterResourceElement (TBitmapResourceElement);
end.
