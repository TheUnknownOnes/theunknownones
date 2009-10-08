//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uEffectPNGImage;

interface

uses
  Classes,
  Controls,
  Windows,
  Graphics,
  PNGImage,
  Messages,
  uEffectPNG;

type
  TEffectPNGImage = class(TGraphicControl)
  private
    FDrawing : Boolean;
    FPicture: TPNGObject;
    FForceDBP: Boolean;
    FEffects: TPNGEffects;
    FCenter: Boolean;

    procedure SetForceDBP(const Value: Boolean);
    procedure SetPicture(const Value: TPNGObject);
    procedure SetEffects(const Value: TPNGEffects);
    procedure SetCenter(const Value: Boolean);

  protected
    procedure Paint(); override;

    procedure OnEffectChange(Sender : TObject);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DrawTo(ADc : HDC); overload;
    procedure DrawTo(ACanvas : TCanvas); overload;
  published
    property ForceDoubleBufferedParent  : Boolean     read FForceDBP    write SetForceDBP   default true;
    property Picture                    : TPNGObject  read FPicture     write SetPicture;

    property Effects                    : TPNGEffects read FEffects     write SetEffects;
    property Center                     : Boolean     read FCenter      write SetCenter default true;
    property Align;
    property Anchors;
    property AutoSize;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    {$ifdef VER190}
    property OnMouseActivate;
    property OnMouseEnter;
    property OnMouseLeave;
    {$endif}
    property OnMouseDown;


    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;


implementation


constructor TEffectPNGImage.Create(AOwner: TComponent);
begin
  FPicture:=TPNGObject.Create;
  FForceDBP:=true;
  FEffects:=TPNGEffects.Create();
  FEffects.OnChange:=OnEffectChange;
  FCenter:=true;

  inherited;
end;

destructor TEffectPNGImage.Destroy;
begin
  FPicture.Free;
  FEffects.Free;
  
  inherited;
end;

procedure TEffectPNGImage.DrawTo(ACanvas: TCanvas);
begin
  DrawTo(ACanvas.Handle);
end;

procedure TEffectPNGImage.OnEffectChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TEffectPNGImage.DrawTo(ADc: HDC);
begin
  Perform(WM_PAINT,ADC,0);
end;

procedure TEffectPNGImage.Paint;
var
  Result : TPNGObject;
  TopLeft : TPoint;
begin
  if FDrawing then exit;

  if Assigned(Parent) and FForceDBP and (not Parent.DoubleBuffered) then
    Self.Parent.DoubleBuffered:=true;

  FDrawing:=true;

  if csDesigning in ComponentState then
  begin
    with inherited Canvas do
    begin
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;
  end;

  Result:=TPNGObject.Create;
  Result.Assign(FPicture);

  if not Result.Empty then
  begin
    FEffects.ApplyEffects(Result);

    if not FCenter then
    begin
      TopLeft.X:=0;
      TopLeft.Y:=0;
    end
    else
    begin
      TopLeft.X:=(Width-Result.Width) div 2;
      TopLeft.Y:=(Height-Result.Height) div 2;
    end;

    Canvas.Draw(TopLeft.X,TopLeft.Y,Result);
  end;

  Result.Free;

  FDrawing:=false;
end;

procedure TEffectPNGImage.SetCenter(const Value: Boolean);
begin
  FCenter := Value;
  Invalidate;
end;

procedure TEffectPNGImage.SetEffects(const Value: TPNGEffects);
begin
  FEffects.Assign(Value);
end;

procedure TEffectPNGImage.SetForceDBP(const Value: Boolean);
begin
  FForceDBP := Value;
end;

procedure TEffectPNGImage.SetPicture(const Value: TPNGObject);
begin
  FPicture.Assign(Value);
  Invalidate;
end;

end.
