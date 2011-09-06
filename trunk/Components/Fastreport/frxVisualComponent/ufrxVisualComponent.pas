//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************

unit ufrxVisualComponent;

interface

{$R 'ufrxVisualComponent.res'}

uses
  Classes,
  Forms,
  SysUtils,
  frxClass,
  frxDsgnIntf,
  fs_iinterpreter,
  Windows,
  Messages,
  Graphics,
  frxRes;

const
  DEF_LINE_COLOR_MM = $00E9E9E9;
  DEF_LINE_COLOR_CM = $00C5C5C5;
  DEF_PIXELS_PER_CM = 80;
  DEF_SHOW_LINES_MM = true;
  FRX_VISUALCOMPONENT_GROUP = 'VisualComponents';

type
  TfrxVisualComponentFrame = class(TFrame)
  private
    FPixelsPerCM: Integer;
    FLineColorMM: TColor;
    FLineColorCM: TColor;
    FShowLinesMM: Boolean;
    procedure SetPixelsPerCM(const Value: Integer);
    procedure SetLineColorCM(const Value: TColor);
    procedure SetLineColorMM(const Value: TColor);
    procedure SetShowLinesMM(const Value: Boolean);

    procedure DesignerRepaint;

    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property PixelsPerCM : Integer read FPixelsPerCM write SetPixelsPerCM default DEF_PIXELS_PER_CM;
    property LineColorCM : TColor read FLineColorCM write SetLineColorCM default DEF_LINE_COLOR_CM;
    property LineColorMM : TColor read FLineColorMM write SetLineColorMM default DEF_LINE_COLOR_MM;
    property ShowLinesMM : Boolean read FShowLinesMM write SetShowLinesMM default DEF_SHOW_LINES_MM;
  end;

  TfrxVisualComponent = class(TfrxView)
  protected
    _FFrame : TfrxVisualComponentFrame;

    procedure SetWidth(Value: Extended); override;
    procedure SetHeight(Value: Extended); override;
  published
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure Draw(Canvas: TCanvas; ScaleX, ScaleY, OffsetX, OffsetY: Extended); override;
  published
    property Frame;
  end;

  TfrxVisualComponentRTTI = class(TfsRTTIModule)
  public
    constructor Create(AScript: TfsScript); override;
  end;

  function TryLoadFrxObjectIcon(AInstance: HINST; AName : String) : Integer; overload;
  function TryLoadFrxObjectIcon(AName : String) : Integer; overload;

implementation

var
  ScreenLogPixels : Integer;

procedure InitScreenLogPixels;
var
  DC: HDC;
begin
  DC := GetDC(0);
  try
    ScreenLogPixels := GetDeviceCaps(DC, LOGPIXELSX);
  finally
    ReleaseDC(0,DC);
  end;
end;

function TryLoadFrxObjectIcon(AName : String) : Integer;
begin
  Result:=TryLoadFrxObjectIcon(HInstance, AName);
end;

function TryLoadFrxObjectIcon(AInstance: HINST; AName : String) : Integer;
var
  bmp : TBitmap;
begin
  Result := -1;
  bmp := TBitmap.Create;
  try
    try
      bmp.LoadFromResourceName(AInstance, AName);
    except
      bmp.LoadFromResourceName(HInstance, 'TFRXVISUALCOMPONENT');
    end;
    if not bmp.Empty then
      Result := frxResources.ObjectImages.AddMasked(bmp, bmp.Canvas.Pixels[0,0]);
  finally
    bmp.Free;
  end;
end;

{ TfrxVisualComponent }

constructor TfrxVisualComponent.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TfrxVisualComponent.Destroy;
begin
  inherited;
end;

procedure TfrxVisualComponent.Draw(Canvas: TCanvas; ScaleX, ScaleY, OffsetX,
  OffsetY: Extended);
var
  emf : TMetafile;
  emfc : TMetafileCanvas;
begin
  BeginDraw(Canvas, ScaleX, ScaleY, OffsetX, OffsetY);

  emf := TMetafile.Create;
  try
    emf.Width := _FFrame.Width;
    emf.Height := _FFrame.Height;
    emfc := TMetafileCanvas.Create(emf, Canvas.Handle);
    try
      _FFrame.PaintTo(emfc, 0, 0);
    finally
      emfc.Free;
    end;
    Canvas.StretchDraw(Rect(FX, FY, FX1, FY1), emf);
  finally
    emf.Free;
  end;

  DrawFrame;
end;

procedure TfrxVisualComponent.SetHeight(Value: Extended);
begin
  inherited;
  _FFrame.Height := Round(((_FFrame.PixelsPerCM * 2.54) * Value) / ScreenLogPixels);
end;

procedure TfrxVisualComponent.SetWidth(Value: Extended);
begin
  inherited;
  _FFrame.Width := Round(((_FFrame.PixelsPerCM * 2.54) * Value) / ScreenLogPixels);
end;

{ TfrxVisualComponentRTTI }

constructor TfrxVisualComponentRTTI.Create(AScript: TfsScript);
begin
  inherited;
  with AScript do
  begin
    AddClass(TfrxVisualComponent, 'TfrxView');
  end;
end;

{ TfrxVisualComponentFrame }

constructor TfrxVisualComponentFrame.Create(AOwner: TComponent);
begin
  FPixelsPerCM := DEF_PIXELS_PER_CM;
  FShowLinesMM := DEF_SHOW_LINES_MM;
  FLineColorMM := DEF_LINE_COLOR_MM;
  FLineColorCM := DEF_LINE_COLOR_CM;

  inherited;
end;

procedure TfrxVisualComponentFrame.DesignerRepaint;
begin
  if csDesigning in ComponentState then
    Repaint;
end;

procedure TfrxVisualComponentFrame.Resize;
begin
  inherited;
  if csDesigning in ComponentState then Repaint;
end;

procedure TfrxVisualComponentFrame.SetLineColorCM(const Value: TColor);
begin
  FLineColorCM := Value;
  DesignerRepaint;
end;

procedure TfrxVisualComponentFrame.SetLineColorMM(const Value: TColor);
begin
  FLineColorMM := Value;
  DesignerRepaint;
end;

procedure TfrxVisualComponentFrame.SetPixelsPerCM(const Value: Integer);
begin
  FPixelsPerCM := Value;
  DesignerRepaint;
end;

procedure TfrxVisualComponentFrame.SetShowLinesMM(const Value: Boolean);
begin
  FShowLinesMM := Value;
  DesignerRepaint;
end;

procedure TfrxVisualComponentFrame.WMEraseBkgnd(var Message: TWmEraseBkgnd);
var
  Canvas : TCanvas;
  x, y : Integer;
begin
  if (csDesigning in ComponentState) then
  begin
    Message.Result := 1;
    Canvas := TCanvas.Create;
    Canvas.Handle := Message.DC;

    Canvas.Brush.Style:=bsSolid;
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ClientRect);

    Canvas.Pen.Width:=1;

    if FShowLinesMM then
    begin
      Canvas.Pen.Color := FLineColorMM;

      for y := 0 to Round((Height) / (FPixelsPerCM / 10)) do
      begin
        Canvas.MoveTo(0, y * (FPixelsPerCM div 10));
        Canvas.LineTo(Width, y * (FPixelsPerCM div 10));
      end;

      for x := 0 to Round((Width) / (FPixelsPerCM / 10)) do
      begin
        Canvas.MoveTo(x*(FPixelsPerCM div 10), 0);
        Canvas.LineTo(x*(FPixelsPerCM div 10), Height);
      end;
    end;

    Canvas.Font.Color := FLineColorCM;
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Color := FLineColorCM;

    for y := 1 to Height div FPixelsPerCM do
    begin
      Canvas.MoveTo(0, (y * FPixelsPerCM) + 2);
      Canvas.TextOut(Canvas.PenPos.X, Canvas.PenPos.Y, Format('%d', [y]));
      Canvas.MoveTo(0, (y * FPixelsPerCM));
      Canvas.LineTo(Width, y * FPixelsPerCM);
    end;

    for x := 1 to Width div FPixelsPerCM do
    begin
      Canvas.MoveTo(x * FPixelsPerCM + 2, 0);
      Canvas.TextOut(Canvas.PenPos.X, Canvas.PenPos.Y, Format('%d', [x]));
      Canvas.MoveTo(x * FPixelsPerCM, 0);
      Canvas.LineTo(x * FPixelsPerCM, Height);
    end;

    Canvas.Free;
  end
  else
   inherited;
end;

var
  bmp : TBitmap;

initialization
  InitScreenLogPixels;
  bmp := TBitmap.Create;
  bmp.LoadFromResourceName(HInstance, 'TFRXVISUALCOMPONENT');
  frxObjects.RegisterCategory(FRX_VISUALCOMPONENT_GROUP, nil, 'VisualComponents',
                              TryLoadFrxObjectIcon(''));
  bmp.Free;
  fsRTTIModules.Add(TfrxVisualComponentRTTI);

finalization
  fsRTTIModules.Remove(TfrxVisualComponentRTTI);

end.
