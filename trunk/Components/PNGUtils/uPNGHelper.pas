//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uPNGHelper;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, pngImage, gdipapi, gdipobj, ActiveX, ImgList, ShellAPI,
  gdiputil;

type
  TPNGHelper = class helper for TPNGObject
  public
    procedure LoadFromResourceName(AInstance: HINST; AResourceGroup : string; AResourceName: String); overload;
  end;

  function PNGtoIcon(const APNG : TGPBitmap;
                      ACursor : Boolean = false;
                      AHotSpotX : Integer = 0;
                      AHotSpotY : Integer = 0) : HICON; overload;

  function PNGtoIcon(const APNG : TPngObject;
                      ACursor : Boolean = false;
                      AHotSpotX : Integer = 0;
                      AHotSpotY : Integer = 0) : HICON; overload;


  function IconFromPNGFile (const AFileName : String) : HICON;
  function IconFromPNGStream(AStream: IStream) : HICON;

  function IconFromTPNGObject(APNG: TPngObject) : HICON;
  
  function AddPNGToImageListTransparent(APNG: TPngObject; AImageList : TCustomImagelist) : Integer;

  procedure ModifyImageList16(const AImageList : TCustomImageList);

  procedure GetAsPNG(Instance : THandle;
                    Identifier : String;
                    const PNG : TPngObject);

  procedure GetAsBitmap(Instance : THandle;
                        Identifier : String;
                        Backcolor : Graphics.TColor;
                        const Bitmap : Graphics.TBitmap);

  function AddPNGToImageList(Instance : THandle;
                             Identifier : String;
                             Backcolor : Graphics.TColor;
                             ImageList : TCustomImageList):Integer; overload;

  function AddPNGToImageList(PNG : TPngObject;
                             Backcolor : Graphics.TColor;
                             ImageList : TCustomImageList):Integer; overload;

  function AlphaBlendColors(Color1, Color2 : Graphics.TColor;
                            Alpha: Byte):Graphics.TColor;

  procedure PNGtoBitmap(PNG : TPngObject;
                        BMP : Graphics.TBitmap;
                        BackColor : Graphics.TColor); overload;

  procedure PNGtoBitmap(PNG : TPngObject;
                        BMP : Graphics.TBitmap;
                        BackColor : Graphics.TColor;
                        Width, Height: Integer); overload;

  procedure ReplacePNGInImageList(Index : Integer;
                                  Instance : THandle;
                                  Identifier : String;
                                  Backcolor : Graphics.TColor;
                                  ImageList : TCustomImageList);

  procedure ReplacePNGInImageListTransparent(Index: integer;
                                             APNG: TPngObject;
                                             ImageList : TCustomImageList);

  procedure DrawTransparentBackground(ACanvas : TCanvas; APatternSize : Word = 8);

type
  TMixPNGPosition = (mppTopLeft, mppTopRight, mppBottomLeft, mppBottomRight,
                     mppCenter);

  procedure MixPNGs(const ABackgroundImage: TPNGObject;
                    const AForegroundImage: TPNGObject;
                    const AMixedImage: TPngObject;
                    AMixPosition : TMixPNGPosition); overload;

  procedure MixPNGs(const ABackgroundImage: TPNGObject;
                    const AForegroundImage: TPNGObject;
                    const AMixedImage: TPngObject;
                    AX, AY: Integer); overload;

var
  gdiplusToken: ULONG;
  StartupInput: TGDIPlusStartupInput;

implementation

uses
  CommCtrl;

procedure DrawTransparentBackground(ACanvas : TCanvas; APatternSize : Word = 8);
var
  BMP : TBitmap;
  x,y : Integer;
begin
  BMP:=TBitmap.Create;
  try
    BMP.Width:=APatternSize * 2;
    BMP.Height:=APatternSize * 2;
    BMP.PixelFormat:=pf8bit;

    BMP.Canvas.Brush.Style:=bsSolid;
    BMP.Canvas.Pen.Style:=psClear;

    BMP.Canvas.Brush.Color:=$00EEEEEE;

    BMP.Canvas.FillRect(Rect(0, 0, BMP.Width div 2, BMP.Height div 2));
    BMP.Canvas.FillRect(Rect(BMP.Width div 2, BMP.Height div 2, BMP.Width - 1, BMP.Height - 1));

    BMP.Canvas.Brush.Color:=clWhite;

    BMP.Canvas.FillRect(Rect(BMP.Width div 2, 0, BMP.Width - 1, BMP.Height div 2));
    BMP.Canvas.FillRect(Rect(0, BMP.Height div 2, BMP.Width div 2, BMP.Height - 1));

    x:=ACanvas.ClipRect.Left;
    y:=ACanvas.ClipRect.Top;

    while x < ACanvas.ClipRect.Right do
    begin

      while y < ACanvas.ClipRect.Bottom do
      begin
        BitBlt(ACanvas.Handle, x, y, bmp.Width, bmp.Height, bmp.Canvas.Handle, 0, 0, SRCCOPY);
        Inc(y, BMP.Height);
      end;
      
      y:=ACanvas.ClipRect.Top;
      Inc(x, BMP.Width);
    end;
  finally
    BMP.Free;
  end;
end;


procedure Startup;
begin
  // Initialize StartupInput structure
  StartupInput.DebugEventCallback := nil;
  StartupInput.SuppressBackgroundThread := False;
  StartupInput.SuppressExternalCodecs   := False;
  StartupInput.GdiplusVersion := 1;
  // Initialize GDI+
  GdiplusStartup(gdiplusToken, @StartupInput, nil);
end;

procedure Shutdown;
begin
  // Close GDI +
  GdiplusShutdown(gdiplusToken);
end;

function IconFromTPNGObject(APNG: TPngObject) : HICON;
var
  Stream: TMemoryStream;
  StreamAdapt: TStreamAdapter;
begin
  Stream:=TMemoryStream.Create;

  try
    APNG.SaveToStream(Stream);
    Stream.Seek(0, soBeginning);
    StreamAdapt:=TStreamAdapter.Create(Stream);

    result:=IconFromPNGStream(StreamAdapt);

  finally
    Stream.Free;
  end;
end;

function IconFromPNGStream(AStream: IStream) : HICON;
var
  png : TGPBitmap;
begin
  Startup;

  png:=TGPBitmap.Create(AStream);
  Result:=PngToIcon(png);
  png.Free;

  Shutdown;
end;

function AddPNGToImageListTransparent(APNG: TPngObject; AImageList : TCustomimagelist) : Integer;
var
  ico : HICON;
begin
  ico:=PNGtoIcon(APNG);
  Result:=ImageList_AddIcon(AImageList.Handle, ico);
  DestroyIcon(ico);
end;

function IconFromPNGFile (const AFileName : String) : HICON;
var
  png : TGPBitmap;
begin
  Startup;
  Result:=0;

  if FileExists(AFileName) then
  begin
    png:=TGPBitmap.Create(AFileName);
    Result:=PngToIcon(png);
    png.Free;
  end;

  Shutdown;
end;

function PNGtoIcon(const APNG : TGPBitmap;
                      ACursor : Boolean = false;
                      AHotSpotX : Integer = 0;
                      AHotSpotY : Integer = 0) : HICON;
var
  Width, Height : Cardinal;
  hNewBitmap,
  hMonoBitmap : HBITMAP;
  IconInfo : _ICONINFO;
begin
  Width:=apng.GetWidth;
  Height:=APNG.GetHeight;

  hMonoBitmap:=CreateBitmap(Width,Height,1,1,nil);

  APNG.GetHBITMAP(0,hNewBitmap);

  IconInfo.fIcon:=not ACursor;
  if ACursor then
  begin
    IconInfo.xHotspot:=AHotSpotX;
    IconInfo.yHotspot:=AHotSpotY;
  end;
  IconInfo.hbmMask:=hMonoBitmap;

  IconInfo.hbmColor:=hNewBitmap;
  Result:=CreateIconIndirect(IconInfo);

  DeleteObject(hNewBitmap);
  DeleteObject(hMonoBitmap);
end;

procedure ModifyImageList16(const AImageList : TCustomImageList);
var
  SHFileInfo: TSHFileInfo;
  SysIcons: THandle;
begin
  try
    SysIcons := SHGetFileInfo(PChar(copy(ParamStr(0), 1, 3)), 0, SHFileInfo,
      SizeOf(SHFileInfo), SHGFI_SYSICONINDEX or SHGFI_SMALLICON or
      SHGFI_ICON);
  finally
    DestroyIcon(SHFileInfo.hIcon);
  end;
  AImageList.Handle := SysIcons;
  AImageList.Clear;
end;

function PNGtoIcon(const APNG : TPngObject;
                      ACursor : Boolean = false;
                      AHotSpotX : Integer = 0;
                      AHotSpotY : Integer = 0) : HICON;
var
  Width, Height : Integer;
  BitmapHeader  : PBitmapV5Header;
  hNewBitmap,
  hMonoBitmap   : HBITMAP;
  Bits          : Pointer;
  x,
  y             : Integer;
  DC            : HDC;
  IconInfo      : _ICONINFO;
  Pixel         : ^Integer;
  ScanLine      : PRGBTriple;
  AlphaScanline : pByteArray;
begin
  if not Assigned(APNG) then
  begin
    Result:=0;
    exit;
  end;


  //Die Höhe und die Breite brauchen wir später noch ein paar Mal
  Width := APNG.Width;
  Height := APNG.Height;

  //So ein Icon hat einen "Bitmap Version 5 Header"
  //(Wichtig: Als Pointer, damit er später vom CreateDIBSection "genommen" wird)
  New(BitmapHeader);
  FillMemory(BitmapHeader,SizeOf(BitmapV5Header),1);

  //Die Größe der Struktur setzen, damit Windows weiß, "wie weit es gehen kann" :)
  BitmapHeader.bV5Size := sizeof(BITMAPV5HEADER);

  BitmapHeader.bV5Width := Width;
  //Wichtig: negative Höhe angeben, sonst ist das Ergebniss an der X-Achse gespiegelt
  // ... bzw. der Ursprung des Bildes an der falschen Stelle (technisch richtiger)
  BitmapHeader.bV5Height := -Height;

  //Wir haben eine Ebene
  BitmapHeader.bV5Planes := 1;
  // ... und 32bit pro Bixel
  BitmapHeader.bV5BitCount := 32;

  //Das Bild wird nicht komprimiert
  BitmapHeader.bV5Compression := BI_BITFIELDS;
  //Und nun müssen wir noch sagen, wo sich die Farben und der Alpha-Wert innerhalb
  // ... der 32bit eines Pixels befinden
  BitmapHeader.bV5RedMask := $00FF0000;
  BitmapHeader.bV5GreenMask := $0000FF00;
  BitmapHeader.bV5BlueMask := $000000FF;
  BitmapHeader.bV5AlphaMask := $FF000000;


  DC := GetDC(0);
  //Ein neues Bitmap anlegen
  hNewBitmap := CreateDIBSection( DC,
                                PBitmapInfo(BitmapHeader)^, //Hier ein wenig tricksen
                                                            // ... damit der V5Header "reinpasst"
                                DIB_RGB_COLORS, //wir haben RGB-Farben
                                Bits, //eine Pointer auf das erste Pixel
                                0, //bedeutet, das wir das Bitmap im RAM haben wollen
                                0); // ... und deswegen brauchen wir auch kein Offset
  Dispose(BitmapHeader); //der Header hat seine Schuldigkeit getan
  ReleaseDC(0,dc); // ... und auch unser DC

  //man nehme ein Bitmap, welches wir später als Maske "verkaufen"
  hMonoBitmap:=CreateBitmap(Width,Height,1,1,nil);

  //Und los gehts beim ersten Pixel
  Pixel := Bits;
  for y := 0 to Height-1 do
  begin
    //aus dem PNG die Farbwerte einer Zeile holen
    ScanLine := APNG.Scanline[y];
    // ... und dazu die Alpha-Werte
    AlphaScanline := APNG.AlphaScanline[y];
    for x := 0 to Width - 1 do
    begin
      //ein Pixel-Wert setzt sich aus ...
      if Assigned(AlphaScanline) then
        Pixel^ := AlphaScanLine[x]  // ... dem Alpha-Wert (falls es einen gibt), ...
      else
        Pixel^:=255;
      Pixel^ := Pixel^ shl 8;
      Inc(Pixel^, Scanline^.rgbtRed);  // ... einem Rot-Anteil, ...
      Pixel^ := Pixel^ shl 8;
      Inc(Pixel^, Scanline^.rgbtGreen); // ... einem Grün-Anteil, ...
      Pixel^ := Pixel^ shl 8;
      Inc(Pixel^, Scanline^.rgbtBlue); // ... und einem Blau-Anteil zusammen

      //weiter gehts mit dem nächsten Pixel innerhalb unseres RAM-Bitmaps
      Inc(Pixel);

      //und auch ein neues Pixel von unserem PNG währe nicht schlecht
      Inc(ScanLine);
    end;
  end;

  //Mit der IconInfo-Struktur können wir einige Eigenschaften des Icons setzen

  // ... z.B. ob es ein Cursor ...
  IconInfo.fIcon := not ACursor;
  if ACursor then
  begin
    // ... mit einem Hotspot ist
    IconInfo.xHotspot := AHotSpotX;
    IconInfo.yHotspot := AHotSpotY;
  end;
  //Aber auf jeden Fall brauchen wir ein Bitmap das als Maske dient
  IconInfo.hbmMask := hMonoBitmap;
  // ... und natürlich ein Bitmap mit dem eigentlichen Bild
  IconInfo.hbmColor := hNewBitmap;

  //Et voila ... ein schönes neues Icon
  Result := CreateIconIndirect(IconInfo);

  //natürlich räumen wir nachher auf
  DeleteObject(hNewBitmap);
  DeleteObject(hMonoBitmap);

  //Wichtig: Das Icon, das zurückgegeben wird muss auch mit DestroyIcon
  // ... freigegeben werden, wenn wir es nicht mehr brauchen.
end;

procedure PNGtoBitmap(PNG : TPngObject; BMP : Graphics.TBitmap; BackColor : Graphics.TColor);
begin
  BMP.PixelFormat:=pf32bit;
  BMP.Width:=PNG.Width;
  BMP.Height:=PNG.Height;
  BMP.Canvas.Brush.Color:=Backcolor;
  BMP.Canvas.FillRect(Rect(0,0,BMP.Width,BMP.Height));
  PNG.Draw(BMP.Canvas,Rect(0,0,BMP.Width,BMP.Height));
end;

procedure PNGtoBitmap(PNG : TPngObject; BMP : Graphics.TBitmap; BackColor : Graphics.TColor; Width, Height: Integer);
begin
  BMP.PixelFormat:=pf32bit;
  BMP.Width:=Width;
  BMP.Height:=Height;
  BMP.Canvas.Brush.Color:=Backcolor;
  BMP.Canvas.FillRect(Rect(0,0,BMP.Width,BMP.Height));
  if Assigned(PNG) then
    PNG.Draw(BMP.Canvas,Rect(0,0,BMP.Width,BMP.Height));
end;

procedure GetAsPNG(Instance:THandle;Identifier:String;const PNG:TPngObject);
begin
  PNG.LoadFromResourceName(Instance, 'PNG', Identifier);
end;

procedure GetAsBitmap(Instance:THandle;Identifier:String;Backcolor:Graphics.TColor;const Bitmap:Graphics.TBitmap);
var
  PNG:TPngObject;
begin
  PNG:=TPngObject.Create;
  GetAsPNG(Instance,Identifier,PNG);

  PNGtoBitmap(PNG,Bitmap,Backcolor);

  PNG.Free;
end;

function AddPNGToImageList(Instance:THandle;Identifier:String;Backcolor:Graphics.TColor;ImageList:TCustomImageList):Integer;
var
  BMP:Graphics.TBitmap;
begin
  BMP:=Graphics.TBitmap.Create;
  GetAsBitmap(Instance,Identifier,Backcolor,BMP);
  Result:=ImageList.AddMasked(BMP,BMP.Canvas.Pixels[BMP.Width-1,0]);
  BMP.Free;
end;

procedure ReplacePNGInImageList(Index:Integer;Instance:THandle;Identifier:String;Backcolor:Graphics.TColor;ImageList:TCustomImageList);
var
  BMP:Graphics.TBitmap;
begin
  BMP:=Graphics.TBitmap.Create;
  GetAsBitmap(Instance,Identifier,Backcolor,BMP);
  ImageList.ReplaceMasked(Index,BMP,BMP.Canvas.Pixels[BMP.Width-1,0]);
  BMP.Free;
end;

procedure ReplacePNGInImageListTransparent(Index: integer;
                                             APNG: TPngObject;
                                             ImageList : TCustomImageList);
var
  ico : HICON;
begin
  ico:=PNGtoIcon(APNG);
  ImageList_ReplaceIcon(ImageList.Handle, Index, ico);
  DestroyIcon(ico);
end;

function AddPNGToImageList(PNG: TPngObject; Backcolor:Graphics.TColor;ImageList:TCustomImageList):Integer;
var
  BMP:Graphics.TBitmap;
begin
  BMP:=Graphics.TBitmap.Create;
  PNGtoBitmap(PNG, BMP, Backcolor, ImageList.Width, ImageList.Height);
  Result:=ImageList.AddMasked(BMP,BMP.Canvas.Pixels[BMP.Width-1,0]);
  BMP.Free;
end;

function AlphaBlendColors(Color1, Color2 : Graphics.TColor; Alpha: Byte):Graphics.TColor;
var
  graphicsp: TGPGraphics;
  bmp      : Graphics.TBitmap;
  brush1   : TGPBrush;
  brush2   : TGPBrush;
begin
  bmp:=Graphics.TBitmap.Create;
  bmp.PixelFormat:=pf32bit;
  bmp.Width:=1;
  bmp.Height:=1;

  bmp.Canvas.Brush.Color:=Color1;
  bmp.Canvas.FillRect(rect(0,0,1,1));
  Color1:=bmp.Canvas.Pixels[0,0];

  bmp.Canvas.Brush.Color:=Color2;
  bmp.Canvas.FillRect(rect(0,0,1,1));
  Color2:=bmp.Canvas.Pixels[0,0];

  graphicsp:=TGPGraphics.Create(bmp.Canvas.Handle);
  brush1:=TGPSolidBrush.Create(MakeColor(GetRValue(Color1),
                                         GetGValue(Color1),
                                         GetBValue(Color1)));

  brush2:=TGPSolidBrush.Create(MakeColor(Alpha,
                                         GetRValue(Color2),
                                         GetGValue(Color2),
                                         GetBValue(Color2)));

  graphicsp.FillRectangle(brush1,MakeRect(0,0,1,1));
  graphicsp.FillRectangle(brush2,MakeRect(0,0,1,1));

  brush1.Free;
  brush2.Free;
  graphicsp.Free;

  Result:=bmp.Canvas.Pixels[0,0];

  bmp.Free;
end;

procedure MixPNGs(const ABackgroundImage: TPNGObject;
                  const AForegroundImage: TPNGObject;
                  const AMixedImage: TPngObject;
                  AMixPosition : TMixPNGPosition); overload;
begin
  case AMixPosition of
    mppTopLeft: MixPNGs(ABackgroundImage, AForegroundImage, AMixedImage, 0, 0);
    mppTopRight: MixPNGs(ABackgroundImage, AForegroundImage, AMixedImage, ABackgroundImage.Width-AForegroundImage.Width, 0);
    mppBottomLeft: MixPNGs(ABackgroundImage, AForegroundImage, AMixedImage, 0, ABackgroundImage.Height-AForegroundImage.Height);
    mppBottomRight: MixPNGs(ABackgroundImage, AForegroundImage, AMixedImage, ABackgroundImage.Width-AForegroundImage.Width, ABackgroundImage.Height-AForegroundImage.Height);
    mppCenter: MixPNGs(ABackgroundImage, AForegroundImage, AMixedImage,
                        (ABackgroundImage.Width div 2) - (AForegroundImage.Width div 2),
                        (ABackgroundImage.Height div 2) - (AForegroundImage.Height div 2));
  end;
end;

procedure MixPNGs(const ABackgroundImage: TPNGObject;
                  const AForegroundImage: TPNGObject;
                  const AMixedImage: TPngObject;
                  AX, AY: Integer); overload;
var
  graphicsBack: TGPGraphics;
  imgfg, imgbg : TGPImage;
  strmfg, strmbg : TMemoryStream;
  strmafg, strmabg: TStreamAdapter;
  encClsid: TGUID;
begin
  strmbg:=TMemoryStream.Create;
  ABackgroundImage.SaveToStream(strmbg);
  strmbg.Seek(0, soBeginning);
  strmabg:=TStreamAdapter.Create(strmbg, soOwned);
  imgbg:=TGPImage.Create(strmabg);

  strmfg:=TMemoryStream.Create;
  AForegroundImage.SaveToStream(strmfg);
  strmfg.Seek(0, soBeginning);
  strmafg:=TStreamAdapter.Create(strmfg, soOwned);
  imgfg:=TGPImage.Create(strmafg);

  GetEncoderClsid('image/png', encClsid);

  graphicsBack:=TGPGraphics.Create(imgbg);
  try
    graphicsBack.DrawImage(imgfg, AX, AY);

    strmabg.SetSize(0);
    imgbg.Save(strmabg, encClsid);
    strmbg.Seek(0, soBeginning);
    AMixedImage.LoadFromStream(strmbg);
  finally
    graphicsBack.Free;
    imgfg.Free;
    imgbg.Free;
  end;
end;

{ TPNGHelper }

procedure TPNGHelper.LoadFromResourceName(AInstance: HINST; AResourceGroup,
  AResourceName: String);
var
  strm : TResourceStream;
begin
  strm:=TResourceStream.Create(AInstance, AResourceName, PChar(AResourceGroup));
  try
    strm.Seek(0, soBeginning);
    self.LoadFromStream(strm);
  finally
    strm.Free;
  end;
end;

end.
