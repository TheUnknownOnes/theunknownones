//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uEffectPNGEdit;

interface

{$I jedi.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, {$ifndef DELPHI12_UP}TntStdCtrls,{$endif} uEffectPng, ShlObj, ComObj, pngimage,
  uEnumStringList;

type
  TEffectPNGEdit = class({$ifdef DELPHI12_UP}TEdit{$else}TTntEdit{$endif})
  private
    FPaintCtl: TWinControl;
    FPNGEffectsEnabled : TPNGEffects;
    FPNGEffectsDisabled : TPNGEffects;
    FPNG : TPNGObject;
    FEffectPNG: TPNGObject;
    
    FItems : TEnumStringList;
    FAutoComplete: IAutoComplete2;

    procedure SetPNG(const Value: TPNGObject);

    procedure SetPaintCtlVisibility;
    procedure SetPNGEffectsDisabled(const Value: TPNGEffects);
    procedure SetPNGEffectsEnabled(const Value: TPNGEffects);
    function GetItems: TStrings;
    procedure SetItems(const Value: TStrings);
  protected
    function GetEffectPNG: TPNGObject;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property PNG: TPNGObject read FPNG write SetPNG;
    property PNGEffectsEnabled : TPNGEffects read FPNGEffectsEnabled write SetPNGEffectsEnabled;
    property PNGEffectsDisabled : TPNGEffects read FPNGEffectsDisabled write SetPNGEffectsDisabled;

    property SearchItems: TStrings read GetItems write SetItems;
  end;

implementation

type
  TEffectWinCtl = class(TWinControl)
  private
    FEffectEdit: TEffectPNGEdit;
  protected
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure PaintWindow(DC: HDC); override;
    procedure DoEnter; override;
    procedure WndProc(var Message: TMessage); override;
  public

    constructor Create(AEffectEdit: TEffectPNGEdit); reintroduce;
  end;


{ TEffectPNGEdit }

constructor TEffectPNGEdit.Create(AOwner: TComponent);
begin
  inherited;
  FItems:=TEnumStringList.Create;
  FEffectPNG:=TPNGObject.Create;
  FPNG:=TPNGObject.Create;
  FPNGEffectsEnabled:=TPNGEffects.Create;
  FPNGEffectsDisabled:=TPNGEffects.Create;

  FPaintCtl:=TEffectWinCtl.Create(self);
end;

destructor TEffectPNGEdit.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FEffectPNG);
  FreeAndNil(FPNG);
  FreeAndNil(FPNGEffectsDisabled);
  FreeAndNil(FPNGEffectsEnabled);
  inherited;
end;

procedure TEffectPNGEdit.DoEnter;
begin
  inherited;
  SetPaintCtlVisibility;
end;

procedure TEffectPNGEdit.DoExit;
begin
  inherited;
  SetPaintCtlVisibility;
end;

function TEffectPNGEdit.GetEffectPNG: TPNGObject;
begin
  FEffectPNG.Assign(FPNG);
  if not FPNG.Empty then
  begin
    if self.Enabled then
      FPNGEffectsEnabled.ApplyEffects(FEffectPNG)
    else
      FPNGEffectsDisabled.ApplyEffects(FEffectPNG);
    result:=FEffectPNG;
  end
  else
    Result:=FPNG;
end;

function TEffectPNGEdit.GetItems: TStrings;
begin
  Result:=TStrings(FItems);
end;

procedure TEffectPNGEdit.SetItems(const Value: TStrings);
begin
  FItems.Text:=Value.Text;
end;

procedure TEffectPNGEdit.SetPaintCtlVisibility;
begin
  FPaintCtl.Visible:=(not Self.Focused) and (Self.GetTextLen=0);
end;

procedure TEffectPNGEdit.SetParent(AParent: TWinControl);
begin
  inherited;
  if not (csDesigning in Self.ComponentState) and
     not (csDestroying in Self.ComponentState) and
     not Assigned(FAutoComplete) then
  begin
    FAutoComplete  := CreateComObject(CLSID_AutoComplete) as IAutoComplete2;
    OleCheck(FAutoComplete.SetOptions(ACO_AUTOSUGGEST or ACO_UPDOWNKEYDROPSLIST));
    OleCheck(FAutoComplete.Init(self.Handle, FItems.DefaultInterface, nil, nil));
  end;
end;

procedure TEffectPNGEdit.SetPNG(const Value: TPNGObject);
begin
  FPNG.Assign(Value);
end;

procedure TEffectPNGEdit.SetPNGEffectsDisabled(const Value: TPNGEffects);
begin
  FPNGEffectsDisabled.Assign(Value);
end;

procedure TEffectPNGEdit.SetPNGEffectsEnabled(const Value: TPNGEffects);
begin
  FPNGEffectsEnabled.Assign(Value);
end;

procedure TEffectPNGEdit.WMPaint(var Message: TWMPaint);
begin
  SetPaintCtlVisibility;
  inherited;
end;

{ TEffectWinCtl }

constructor TEffectWinCtl.Create(AEffectEdit: TEffectPNGEdit);
begin
  inherited Create(AEffectEdit);
  FEffectEdit:=AEffectEdit;
  Parent:=AEffectEdit;
  Align:=alClient;
end;

procedure TEffectWinCtl.DoEnter;
begin
  if FEffectEdit.Enabled then
    FEffectEdit.SetFocus;
end;

procedure TEffectWinCtl.PaintWindow(DC: HDC);
var
  canv : TCanvas;
begin
  inherited;

  canv:=TCanvas.Create;
  canv.Handle:=DC;
  canv.Draw(0,0,FEffectEdit.GetEffectPNG);
  canv.Free;
end;


procedure TEffectWinCtl.WMPaint(var Message: TWMPaint);
var
  lpPaint: TPaintStruct;
  DC : HDC;
begin
  DC:=BeginPaint(self.Handle, lpPaint);
  PaintWindow(DC);
  EndPaint(Self.Handle, lpPaint);
end;

procedure TEffectWinCtl.WndProc(var Message: TMessage);
begin
  if Message.Msg=CM_MOUSEACTIVATE then
    DoEnter;

  inherited;
end;

end.
