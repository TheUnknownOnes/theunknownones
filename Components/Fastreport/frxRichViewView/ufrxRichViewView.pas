unit ufrxRichViewView;

interface

uses
  Windows, Classes, frxClass, frxDsgnIntf, fs_iinterpreter, Graphics, frxPrinter,
  Dialogs, RichView, RVReport, Forms, Math, ZLib;

type
  TfrxCustomRichViewView = class(TfrxStretcheable)
  private
    FMetaFile : TMetaFile;
    FPage : Integer;
    FReportHelper: TRVReportHelper;
    FFileName: String;
    FContent: String;
    procedure DoDrawMetafile;
//    function UsePrinterCanvas: Boolean;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetDescription: String; override;

    function CalcHeight: Extended; override;
    function DrawPart: Extended; override;
    procedure InitPart; override;
                   
    procedure Draw(Canvas: TCanvas; ScaleX, ScaleY, OffsetX, OffsetY: Extended); override;

    procedure GetData; override;

    procedure LoadFromFile(AFileName: String);
    procedure LoadContentFromStream(AStream: TStream);
    property ReportHelper : TRVReportHelper read FReportHelper;

    property FileName : String read FFileName write FFileName;
    property Content : String read FContent write FContent;
  end;

  TfrxRichViewView = class(TfrxCustomRichViewView)
  published
    property Frame;
    
    property DataField;
    property DataSet;
    property DataSetName;  

    property FileName;
    property Content;
  end;


implementation

uses
  RVStyle, RVScroll, SysUtils, Variants;

{ TfrxCustomRichViewView }

{$R CustomRichView.res}

function TfrxCustomRichViewView.CalcHeight: Extended;
var
  lCanvas : TCanvas;
begin
  lCanvas:=TCanvas.Create;
  lCanvas.Handle:=GetDC(0);

  ReportHelper.Init(lCanvas, Round(Width));
  ReportHelper.FormatNextPage(Round(Report.Engine.FreeSpace-Top));

  FMetaFile.Clear;
  FMetaFile.Width:=0;
  FMetaFile.Height:=0;

  if ReportHelper.Finished then
  begin
    Result:=ReportHelper.GetLastPageHeight;
    InitPart;
    ReportHelper.FormatNextPage(Round(Result));
    DoDrawMetafile;
  end
  else
  begin
    Result:=Report.Engine.FreeSpace-Top;

    while not ReportHelper.Finished do
    begin
      ReportHelper.FormatNextPage(Round(Report.Engine.FreeSpace));
      if not ReportHelper.Finished then
        Result:=Result+Report.Engine.FreeSpace
      else
        Result:=Result+ReportHelper.GetLastPageHeight;
    end;
  end;

  ReleaseDC(0, lCanvas.Handle);
  lCanvas.Free;
end;


constructor TfrxCustomRichViewView.Create(AOwner: TComponent);
begin
  inherited;
  Width:=100;
  Height:=100;

  FMetaFile:=TMetafile.Create;
  FMetaFile.Width:=0;

  FReportHelper:=TRVReportHelper.Create(nil);
  FReportHelper.RichView.RVFOptions := [rvfoSavePicturesBody,rvfoSaveControlsBody,rvfoSaveBinary,
                   rvfoSaveBack,rvfoLoadBack,rvfoSaveTextStyles,rvfoSaveParaStyles,
                   rvfoSaveLayout,rvfoLoadLayout];
  FReportHelper.RichView.RTFReadProperties.UnicodeMode:=rvruMixed;
  FReportHelper.RichView.Options:= FReportHelper.RichView.Options + [rvoTagsArePChars];

  FReportHelper.RichView.Style:=TRVStyle.Create(FReportHelper.RichView);
  FReportHelper.RichView.TopMargin:=0;
  FReportHelper.RichView.LeftMargin:=0;
  FReportHelper.RichView.BottomMargin:=0;
  FReportHelper.RichView.RightMargin:=0;
end;

procedure TfrxCustomRichViewView.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, not FMetaFile.Empty);
end;

destructor TfrxCustomRichViewView.Destroy;
begin
  FReportHelper.Free;
  FMetaFile.Free;
  inherited;
end;

procedure TfrxCustomRichViewView.Draw(Canvas: TCanvas; ScaleX, ScaleY, OffsetX,
  OffsetY: Extended);
begin
  BeginDraw(Canvas, ScaleX, ScaleY, OffsetX, OffsetY);
  DrawBackground;

  if IsDesigning then
  begin
    Canvas.brush.Style := bsBDiagonal;
    Canvas.brush.color := clBlue;
    SetBkColor(Canvas.Handle, ColorToRGB(clWhite));
    Canvas.FillRect(Rect(FX, FY, FX1, FY1));
  end
  else
  begin
    Canvas.StretchDraw(Rect(FX, FY, FX1, FY1), FMetaFile);
  end;

  DrawFrame;
end;

function TfrxCustomRichViewView.DrawPart: Extended;
begin
  if ReportHelper.Finished then
  begin
    Result:=0;
    FMetaFile.Clear;
  end
  else
  begin
    ReportHelper.FormatNextPage(Round(Height));
    DoDrawMetafile;

    if FReportHelper.Finished then
    begin
      Result:=Height-FReportHelper.GetLastPageHeight;
      FSaveHeight:=Abs(FSaveHeight);
    end
    else
      Result:=0;
  end;
end;

procedure TfrxCustomRichViewView.GetData;
var
  ss : TStringStream;  
begin
  inherited;
  if IsDataField then
  begin
    if DataSet.IsBlobField(DataField) then
    begin
      ss := TStringStream.Create('');
      DataSet.AssignBlobTo(DataField, ss)
    end
    else
      ss := TStringStream.Create(VarToStr(DataSet.Value[DataField]));
    try
      FReportHelper.RichView.LoadRVFFromStream(ss);
    finally
      ss.Free;
    end;
  end
  else
  if FileName<>EmptyStr then
  begin
    FReportHelper.RichView.LoadRVF(FileName);
  end
  else
  if Content<>EmptyStr then
  begin
    ss:=TStringStream.Create(Content);
    try
      FReportHelper.RichView.LoadRVFFromStream(ss);
    finally
      ss.Free;
    end;
  end;
end;

class function TfrxCustomRichViewView.GetDescription: String;
begin
  Result:='RichView View';
end;

procedure TfrxCustomRichViewView.InitPart;
var
  lCanvas : TCanvas;
begin
  inherited;

  lCanvas:=TCanvas.Create;
  lCanvas.Handle:=GetDC(0);

  ReportHelper.Init(lCanvas, Round(Width));

  ReleaseDC(0, lCanvas.Handle);
  lCanvas.Free;

  FPage:=1;
end;

procedure TfrxCustomRichViewView.LoadFromFile(AFileName: String);
begin
  ReportHelper.RichView.Clear;
  ReportHelper.RichView.LoadRVF(AFileName);
end;

procedure TfrxCustomRichViewView.LoadContentFromStream(AStream: TStream);
begin
  ReportHelper.RichView.Clear;
  ReportHelper.RichView.LoadRVFFromStream(AStream);
end;

procedure TfrxCustomRichViewView.DoDrawMetafile;
var
  EMFCanvas: TMetafileCanvas;
  lCanvas : TCanvas;
begin
  lCanvas:=nil;

  lCanvas:=TCanvas.Create;
  lCanvas.Handle:=GetDC(0);

  FMetaFile.Clear;
  FMetaFile.Enhanced := True;
  FMetaFile.Width := Round(Width);
  if FReportHelper.Finished then
    FMetaFile.Height := Round(FReportHelper.GetLastPageHeight)
  else
    FMetaFile.Height := Round(Height);

  EMFCanvas := TMetafileCanvas.Create(TMetaFile(FMetaFile), lCanvas.Handle);
  try
    FMetaFile.Transparent := True;
    ReportHelper.DrawPage(FPage, EMFCanvas, False, Round(FMetaFile.Height));
    inc(FPage);
  finally
    EMFCanvas.Free;
  end;

  ReleaseDC(0, lCanvas.Handle);
  lCanvas.Free;
end;

procedure TfrxCustomRichViewView.ReadData(Stream: TStream);
var
  dcs : TDecompressionStream;
  ms : TMemoryStream;
begin
  try
    if Stream.Size>0 then
    begin
      dcs:=TDecompressionStream.Create(Stream);
      ms:=TMemoryStream.Create;
      ms.LoadFromStream(dcs);
      dcs.Free;
      FMetaFile.LoadFromStream(ms);
      ms.Free;
    end
  except
  end;
end;

procedure TfrxCustomRichViewView.WriteData(Stream: TStream);
var
  ms : TCompressionStream;
begin
  if (FMetaFile.Width>0) and (FMetaFile.Height>0) then
  begin
    ms:=TCompressionStream.Create(Stream,zcMax);
    FMetaFile.SaveToStream(ms);
    ms.Free;
  end;
end;

type
  TfrxCustomRichViewViewRTTI = class(TfsRTTIModule)
  public
    constructor Create(AScript: TfsScript); override;
  end;

{ TfrxCustomRichViewViewRTTI }

constructor TfrxCustomRichViewViewRTTI.Create(AScript: TfsScript);
begin
  inherited Create(AScript);
  with AScript do
  begin
    with AddClass(TfrxCustomRichViewView, 'TfrxStretcheable') do
    begin

    end;
  end;
end;

type
  TfrxRichViewViewRTTI = class(TfsRTTIModule)
  public
    constructor Create(AScript: TfsScript); override;
    function CallEvent(Instance: TObject; ClassType: TClass;
    const MethodName: String; Caller: TfsMethodHelper): Variant;
  end;

{ TfrxCustomRichViewViewRTTI }

function TfrxRichViewViewRTTI.CallEvent(Instance: TObject; ClassType: TClass;
  const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if SameText('LoadFromStream', MethodName) then
  begin
    TfrxRichViewView(Instance).LoadContentFromStream(TStream(Integer(Caller.Params[0])));
  end;
end;

constructor TfrxRichViewViewRTTI.Create(AScript: TfsScript);
begin
  inherited Create(AScript);
  with AScript do
  begin
    with AddClass(TfrxRichViewView, 'TfrxCustomRichViewView') do
    begin
      AddMethod('procedure LoadFromStream(AStream: TStream);', CallEvent);
    end;
  end;
end;

var
  bmpButton: TBitmap;


initialization
  bmpButton:=TBitmap.Create;
  bmpButton.LoadFromResourceName(HInstance, 'TFRXCUSTOMRICHVIEWVIEW');

  frxObjects.RegisterObject(TfrxRichViewView, bmpButton);

  fsRTTIModules.Add(TfrxCustomRichViewViewRTTI);
  fsRTTIModules.Add(TfrxRichViewViewRTTI);

finalization
  bmpButton.Free;

  if frxObjects<>nil then
    frxObjects.Unregister(TfrxRichViewView);

  if fsRTTIModules <> nil then
  begin
    fsRTTIModules.Remove(TfrxRichViewViewRTTI);
    fsRTTIModules.Remove(TfrxCustomRichViewViewRTTI);
  end;
end.
