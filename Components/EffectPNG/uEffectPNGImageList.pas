//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uEffectPNGImageList;

interface

uses
  Classes,
  Windows,
  Graphics,
  SysUtils,
  uEffectPNG,
  uPNGImageList,
  uBaseImageList,
  PNGImage;

type
  TCustomEffectPNGImageList = class(TCustomPNGImageList)
  private
    FEffects : array[Low(TilDrawState)..High(TilDrawState)] of TPNGEffects;
    procedure SetEffect(const Index: TilDrawState; const Value: TPNGEffects);
    function GetEffect(const Index: TilDrawState): TPNGEffects;

    procedure OnEffectChange(Sender : TObject); virtual;
  protected
    procedure DoDraw(AIndex : Integer; ACanvas : TCanvas; APos : TPoint; AState : TilDrawStates); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;

    property EffectEnabled : TPNGEffects index ildEnabled read GetEffect write SetEffect;
    property EffectDisabled : TPNGEffects index ildDisabled read GetEffect write SetEffect;
    property EffectHighlight : TPNGEffects index ildHighlight read GetEffect write SetEffect;
    property EffectFocused : TPNGEffects index ildFocused read GetEffect write SetEffect;
    property EffectSelected : TPNGEffects index ildSelected read GetEffect write SetEffect;
  end;

  TEffectPNGImageList = class(TCustomEffectPNGImageList)
  public
    property Items;
    property Count;
  published
    property Images;

    property EffectEnabled;
    property EffectDisabled;
    property EffectHighlight;
    property EffectFocused;
    property EffectSelected;


    property OnChange;
  end;

implementation

{ TCustomEffectPNGImageList }


constructor TCustomEffectPNGImageList.Create(AOwner: TComponent);
var
  idx : TilDrawState;
begin
  inherited;

  for idx := Low(TilDrawState) to High(TilDrawState) do
  begin
    FEffects[idx]:=TPNGEffects.Create;
    FEffects[idx].OnChange:=OnEffectChange;
  end;
end;

destructor TCustomEffectPNGImageList.Destroy;
var
  idx : TilDrawState;
begin
  for idx := Low(TilDrawState) to High(TilDrawState) do
    FEffects[idx].Free;

  inherited;
end;

procedure TCustomEffectPNGImageList.DoDraw(AIndex: Integer; ACanvas: TCanvas;
  APos: TPoint; AState: TilDrawStates);
var
  PNG : TPNGObject;
begin
  PNG:=TPNGObject.Create;
  try
    PNG.Assign(Items[AIndex]);

    if ildEnabled in AState then
      FEffects[ildEnabled].ApplyEffects(PNG)
    else
    if ildDisabled in AState then
      FEffects[ildDisabled].ApplyEffects(PNG);

    if ildHighlight in AState then
      FEffects[ildHighlight].ApplyEffects(PNG);

    if ildFocused in AState then
      FEffects[ildFocused].ApplyEffects(PNG);

    if ildSelected in AState then
      FEffects[ildSelected].ApplyEffects(PNG);

    ACanvas.Draw(APos.X, APos.Y, PNG);
  finally
    PNG.Free;
  end;

end;


function TCustomEffectPNGImageList.GetEffect(const Index: TilDrawState): TPNGEffects;
begin
  Result:=FEffects[Index];
end;

procedure TCustomEffectPNGImageList.OnEffectChange(Sender: TObject);
begin
  DoChange(-1);
end;

procedure TCustomEffectPNGImageList.SetEffect(const Index: TilDrawState;
  const Value: TPNGEffects);
begin
  FEffects[Index].Assign(Value);
end;

end.
