unit uMain;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, ComObj, ActiveX, QRPreview_TLB, StdVcl, ShObjIdlQuot,
  ShlObj, QrPrntr, Graphics;

const
  gc_FileExt = '.qrp';

type
  TQRPreview = class(TAutoObject, IQRPreview, IQueryInfo, IExtractImage, IPersistFile)
  private
    FColorDepth : DWord;
    FPageSize       : TSize;
    FFile       : string;
    FThumbBMP   : TBitmap;
    FInstID     : Integer;
    FFirstPage  : TMetafile;
    FSizeStr    : String;
    FTitle      : String;
    FPageCount  : Integer;
    procedure Log(LogLine: String);


    function CreateThumbnail(Size: Windows.TSize; ColorDepth: DWord): HBITMAP;
  protected
    {IQueryInfo}
    function GetInfoTip(dwFlags: DWORD; var ppwszTip: PWideChar): HResult; stdcall;
    function GetInfoFlags(out pdwFlags: DWORD): HResult; stdcall;

    {IPersistFile}
    function IsDirty: HResult; stdcall;
    function Load(pszFileName: POleStr; dwMode: Longint): HResult; stdcall;
    function Save(pszFileName: POleStr; fRemember: BOOL): HResult; stdcall;
    function SaveCompleted(pszFileName: POleStr): HResult; stdcall;
    function GetCurFile(out pszFileName: POleStr): HResult; stdcall;
    function GetClassID(out classID: TCLSID): HResult; stdcall;

    {function to read QRP}
    function GetQRPInfo : String;

    {IExtractImage}
    function GetLocation(Buffer: POleStr;
                         BufferSize: DWORD;
                         var Priority: DWORD;
                         var Size: Windows.TSize;
                         ColorDepth: DWORD;
                         var Flags: DWORD): HResult; stdcall;
    function Extract(var BitmapHandle: HBITMAP): HResult; stdcall;
  public
    procedure Initialize; override;
    destructor Destroy; override;
  end;

implementation

uses ComServ, Classes, Types, SysUtils, StrUtils, uResample;

{$define NODEBUG}

procedure TQRPreview.Log(LogLine : String);
var
  sl   : TStrings;
  fNam : String;
begin
  {$ifdef DEBUG}     
  sl:=TStringList.Create;
  fNam:='C:\Temp\Debug'+IntToStr(FInstID)+'.txt';

  if (FileExists(fNam)) then
    sl.LoadFromFile(fNam);

  sl.Add(DateTimeToStr(Now)+': '+LogLine);
  sl.SaveToFile(fNam);
  sl.Free;
  {$endif}
end;

{ TQRPreview }

function TQRPreview.GetInfoTip(dwFlags: DWORD;
  var ppwszTip: PWideChar): HResult;
var
  szTip: string;
begin
  Result := S_OK;

  szTip    := GetQRPInfo;

  ppwszTip:=CoTaskMemAlloc(SizeOf(WideChar) * (Length(szTip) + 1));

  if (ppwszTip <> nil) then
    ppwszTip := StringToWideChar(szTip, ppwszTip, SizeOf(WideChar) *
      Length(szTip) + 1);
  szTip:='';
end;

function TQRPreview.GetLocation(Buffer: POleStr; BufferSize: DWORD;
  var Priority: DWORD; var Size: Windows.TSize; ColorDepth: DWORD;
  var Flags: DWORD): HResult;
begin
  FColorDepth:=ColorDepth;
  FPageSize:=Size;

  Log('Breite='+IntToStr(Size.cx));
  Log('Höhe='+IntToStr(Size.cy));
  Log('Flags=0x'+IntToHex(Flags,4));
  Log('Farbe='+IntToStr(ColorDepth));
    
  Flags:=0;
    
  result:=S_OK;
end;

function TQRPreview.GetInfoFlags(out pdwFlags: DWORD): HResult;
begin
  pdwFlags := 0;
  Result   := E_NOTIMPL;
end;

procedure TQRPreview.Initialize;
begin
  FInstID:=GetTickCount;
  inherited;
end;

function TQRPreview.IsDirty: HResult;
begin
  Result := E_NOTIMPL;
end;

function TQRPreview.Load(pszFileName: POleStr; dwMode: Integer): HResult;
begin
  Result := S_OK;

  FFile  := pszFileName;

end;

function TQRPreview.Save(pszFileName: POleStr; fRemember: BOOL): HResult;
begin
  Result := E_NOTIMPL;
end;

function TQRPreview.SaveCompleted(pszFileName: POleStr): HResult;
begin
  Result := E_NOTIMPL;
end;

function TQRPreview.GetCurFile(out pszFileName: POleStr): HResult;
begin
  Result := E_NOTIMPL;
end;

function TQRPreview.CreateThumbnail(Size: Windows.TSize; ColorDepth: DWord): HBITMAP;
var
  memDC    : HDC;
  hOld,
  hBMP     : HBitmap;

  MF : TMetafile;
  Report : TQRPrinter;

  NewHeight,
  NewWidth : INteger;
  NewTop,
  NewLeft  : Integer;
  memBMP,
  memBMPsmall : TBitmap;
begin
  memBMP:=TBitmap.Create;
  FThumbBMP:=TBitmap.Create;
  FThumbBMP.Height:=Size.cy;
  FThumbBMP.Width:=Size.cx;

  case ColorDepth of
    1  : FThumbBMP.PixelFormat:=pf1bit;
    4  : FThumbBMP.PixelFormat:=pf4bit;
    8  : FThumbBMP.PixelFormat:=pf8bit;
    16 : FThumbBMP.PixelFormat:=pf16bit;
    24 : FThumbBMP.PixelFormat:=pf24bit;
    32 : FThumbBMP.PixelFormat:=pf32bit;
  end;

  memBMP.PixelFormat:=FThumbBMP.PixelFormat;

  try
    Report:=TQRPrinter.Create(nil);
    Report.Load(FFile);
    MF:=Report.GetPage(1);
    Report.Free;
  except end;


  if (MF.Width>MF.Height) then
  begin
    NewWidth:=Size.cx-4;
    NewHeight:=Round(MF.Height*(Size.cx / MF.Width))-4;
  end
  else
  begin
    NewHeight:=size.cy-4;
    NewWidth:=Round(MF.Width*(Size.cy / MF.Height))-4;
  end;

  NewLeft:=(Size.cx div 2)-(NewWidth div 2)+1;
  NewTop:=(Size.cy div 2)-(NewHeight div 2)+1;

  Log('Size.x='+IntToStr(Size.cx));
  Log('Size.y='+IntToStr(Size.cy));
  
  Log('Pic.Left='+IntToStr(NewLeft));
  Log('Pic.Top='+IntToStr(NewTop));
  Log('Pic.Width='+IntToStr(NewWidth));
  Log('Pic.Height='+IntToStr(NewHeight));

  memBMP.Width:=MF.Width;
  memBMP.Height:=MF.Height;

  try
    memBMP.Canvas.Draw(0,0,MF);
  except end;

  memBMPSmall:=TBitmap.Create;
  memBMPSmall.PixelFormat:=FThumbBMP.PixelFormat;
  memBMPSmall.Height:=NewHeight;
  memBMPSmall.Width:=NewWidth;

  Stretch(memBMP,memBMPsmall, HermiteFilter, 1);

  FThumbBMP.Canvas.Brush.Style:=bsSolid;
  FThumbBMP.Canvas.Brush.Color:=$00FEFEFE;
  FThumbBMP.Canvas.FillRect(FThumbBMP.Canvas.ClipRect);
 // FThumbBMP.Canvas.Brush.Color:=clWhite;
 // FThumbBMP.Canvas.FillRect(Rect(NewLeft,NewTop,NewWidth+NewLeft,NewTop+NewHeight));

  BitBlt(FThumbBMP.Canvas.Handle, NewLeft, NewTop, NewWidth, NewHeight,
         memBMPSmall.Canvas.Handle, 0, 0, SRCCOPY) ;

  //FThumbBMP.Canvas.StretchDraw(Rect(NewLeft,NewTop,NewWidth+NewLeft,NewHeight+NewTop),memBMP);

  FThumbBMP.Canvas.Pen.Color:=clBlack;
  FThumbBMP.Canvas.Pen.Width:=1;
  FThumbBMP.Canvas.Brush.Style:=bsClear;
  FThumbBMP.Canvas.Rectangle(NewLeft,NewTop,NewWidth+NewLeft,NewTop+NewHeight);

  memDC:=CreateCompatibleDC(FThumbBMP.Canvas.Handle);
  hbmp:=CreateCompatibleBitmap(FThumbBMP.Canvas.Handle, Size.cx, Size.cy);
  hOld:=SelectObject(memDC, hbmp);
  BitBlt(memDC, 0, 0, Size.cx, Size.cy, FThumbBMP.Canvas.Handle, 0, 0, SRCCOPY);

  FreeAndNil(FThumbBMP);

  SelectObject(memDC, hOld);
  DeleteDC(memDC);

  memBMP.Free;
  memBMPSmall.Free;
  MF.Free;

  Result:=hbmp;
end;

destructor TQRPreview.Destroy;
begin
  Log('Begin "TQRPreview.Destroy"');
  FFirstPage.Free;
  inherited;
  Log('End "TQRPreview.Destroy"');
end;

function TQRPreview.Extract(var BitmapHandle: HBITMAP): HResult;
begin
  BitmapHandle:=CreateThumbnail(FPageSize, FColorDepth);

  Result:=S_OK;
end;

function TQRPreview.GetClassID(out classID: TCLSID): HResult;
begin
  Result := E_NOTIMPL;
end;

function TQRPreview.GetQRPInfo: string;
var
  Report : TQRPrinter;
begin
  if FFile = '' then Exit;

  try
    Report:=TQRPrinter.Create(nil);
    Report.Load(FFile);
    if Assigned(FFirstPage) then
      FreeAndNil(FFirstPage);
    FFirstPage:=Report.GetPage(1);
    FTitle:=Report.Title;
    FPageCount:=Report.PageCount;
    FSizeStr:=IntToStr(Report.PaperWidthValue)+' x '+IntToStr(Report.PaperLengthValue)+' mm';
    Report.Free;
  except end;

  Result:='QuickReport '+IfThen(FTitle<>'','"'+FTitle+'"')+#13#10;
  Result:=Result+'Seiten: '+IntToStr(FPageCount)+#13#10;
  Result:=Result+'Format: '+FSizeStr;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TQRPreview, Class_QRPreview_,
    ciMultiInstance, tmApartment);
  
end.
