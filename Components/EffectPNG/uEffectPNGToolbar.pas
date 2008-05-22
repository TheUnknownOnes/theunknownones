//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uEffectPNGToolbar;

interface

uses
  Classes,
  ComCtrls,
  PNGImage,
  uEffectPNG,
  Controls;

type
  TEffectPNGToolButton = class(TToolButton)
  private
    FImage: TPNGObject;
    procedure SetImage(const Value: TPNGObject);
  protected
    procedure Paint(); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  published
    property Image : TPNGObject read FImage write SetImage;

  end;

  TEffectPNGToolBar = class(TToolBar)
  private
    FImageEffectDefault: TPNGEffects;
    FImageEffectDisabled: TPNGEffects;
    procedure SetImageEffectDefault(const Value: TPNGEffects);
    procedure SetImageEffectDisabled(const Value: TPNGEffects);
    procedure OnEffectChange(Sender : TObject);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  published
    property ImageEffectDefault : TPNGEffects read FImageEffectDefault write SetImageEffectDefault;
    property ImageEffectDisabled : TPNGEffects read FImageEffectDisabled write SetImageEffectDisabled;
  end;

implementation

constructor TEffectPNGToolButton.Create(AOwner: TComponent);
begin
  FImage:=TPNGObject.Create;
  
  inherited;
end;

destructor TEffectPNGToolButton.Destroy;
begin
  FImage.Free;
  
  inherited;
end;

procedure TEffectPNGToolButton.Paint;
var
  PNGToDraw : TPNGObject;
begin
  inherited;

  if Parent is TEffectPNGToolBar then
  begin
    if not FImage.Empty then
    begin
      PNGToDraw:=TPNGObject.Create;
      PNGToDraw.Assign(FImage);

      if Self.Enabled then
        TEffectPNGToolBar(Parent).ImageEffectDefault.ApplyEffects(PNGToDraw)
      else
        TEffectPNGToolBar(Parent).ImageEffectDisabled.ApplyEffects(PNGToDraw);

      Self.Canvas.Draw((Self.Width div 2)-(PNGToDraw.Width div 2),
                       (Self.Height div 2)-(PNGToDraw.Height div 2),
                       PNGToDraw);

      PNGToDraw.Free;
    end;
  end;
end;

procedure TEffectPNGToolButton.SetImage(const Value: TPNGObject);
begin
  FImage.Assign(Value);
  Invalidate;
end;

constructor TEffectPNGToolBar.Create(AOwner: TComponent);
begin
  FImageEffectDefault:=TPNGEffects.Create;
  FImageEffectDefault.OnChange:=OnEffectChange;

  FImageEffectDisabled:=TPNGEffects.Create;
  FImageEffectDisabled.OnChange:=OnEffectChange;
  
  inherited;
end;

destructor TEffectPNGToolBar.Destroy;
begin
  FImageEffectDefault.Free;

  FImageEffectDisabled.Free;
  
  inherited;
end;

procedure TEffectPNGToolBar.OnEffectChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TEffectPNGToolBar.SetImageEffectDefault(const Value: TPNGEffects);
begin
  FImageEffectDefault.Assign(Value);
  Invalidate;
end;

procedure TEffectPNGToolBar.SetImageEffectDisabled(const Value: TPNGEffects);
begin
  FImageEffectDisabled.Assign(Value);
  Invalidate;
end;

end.
