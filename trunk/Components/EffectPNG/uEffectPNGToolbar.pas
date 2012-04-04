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
    FUseParentEffect: Boolean;
    FEffect: TPNGEffects;
    procedure SetImage(const Value: TPNGObject);
    procedure SetEffect(const Value: TPNGEffects);
    procedure SetUseParentEffect(const Value: Boolean);
    procedure OnEffectChange(Sender : TObject);
  protected
    procedure Paint(); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  published
    property Image : TPNGObject read FImage write SetImage;
    property Effect : TPNGEffects read FEffect write SetEffect;
    property UseParentEffect : Boolean read FUseParentEffect write SetUseParentEffect default True;
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
  FEffect:=TPNGEffects.Create;
  FEffect.OnChange:=OnEffectChange;
  FUseParentEffect:=True;
  inherited;
end;

destructor TEffectPNGToolButton.Destroy;
begin
  FImage.Free;
  FEffect.Free;
  inherited;
end;

procedure TEffectPNGToolButton.OnEffectChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TEffectPNGToolButton.Paint;
var
  PNGToDraw : TPNGObject;
  myEffect : TPngEffects;
  tb : TToolBar;
begin
  inherited;

  if (UseParentEffect and (Parent is TEffectPNGToolBar)) then
  begin
    if Self.Enabled then
      myEffect:=TEffectPNGToolBar(Parent).ImageEffectDefault
    else
      myEffect:=TEffectPNGToolBar(Parent).ImageEffectDisabled;
  end
  else
    myEffect:=Self.Effect;

  if not FImage.Empty then
  begin
    PNGToDraw:=TPNGObject.Create;
    PNGToDraw.Assign(FImage);

    myEffect.ApplyEffects(PNGToDraw);

    if self.Parent is TToolBar then
    begin
      tb := TToolBar(Self.Parent);
      Self.Canvas.Draw((tb.ButtonWidth div 2)-(PNGToDraw.Width div 2),
                       (tb.ButtonHeight div 2)-(PNGToDraw.Height div 2),
                       PNGToDraw);
    end
    else
    begin
      Self.Canvas.Draw((Self.Width div 2)-(PNGToDraw.Width div 2),
                       (Self.Height div 2)-(PNGToDraw.Height div 2),
                       PNGToDraw);
    end;

    PNGToDraw.Free;
  end;
end;

procedure TEffectPNGToolButton.SetEffect(const Value: TPNGEffects);
begin
  FEffect.Assign(Value);
  Invalidate;
end;

procedure TEffectPNGToolButton.SetImage(const Value: TPNGObject);
begin
  FImage.Assign(Value);
  Invalidate;
end;

procedure TEffectPNGToolButton.SetUseParentEffect(const Value: Boolean);
begin
  FUseParentEffect := Value;
  invalidate;
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
