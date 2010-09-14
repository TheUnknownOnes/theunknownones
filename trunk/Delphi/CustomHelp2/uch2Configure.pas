unit uch2Configure;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uch2Main, ComCtrls, ExtCtrls, Math;

type
  Tch2FormConfigure = class(TForm)
    GroupBox1: TGroupBox;
    lv_Provider: TListView;
    GroupBox2: TGroupBox;
    lv_GUI: TListView;
    Panel1: TPanel;
    btn_Cancel: TButton;
    btn_OK: TButton;
    procedure lv_GUIChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btn_OKClick(Sender: TObject);
    procedure lv_ProviderDblClick(Sender: TObject);
    procedure lv_ProviderCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
  private
    { Private-Deklarationen }
  public
    class function Execute() : Boolean;
  end;

implementation

type
  TGUIData = record
    GUI : Ich2GUI;
  end;
  PGUIData = ^TGUIData;

  TProviderData = record
    Provider : Ich2Provider;
  end;
  PProviderData = ^TProviderData;

{$R *.dfm}

{ Tch2FormConfigure }

procedure Tch2FormConfigure.btn_OKClick(Sender: TObject);
var
  gd : PGUIData;
  i : TListItem;
begin
  for i in lv_GUI.Items do
  begin
    if i.Checked then
    begin
      gd := i.Data;
      ch2Main.CurrentGUI := gd^.GUI;
      break;
    end;
  end;
end;

class function Tch2FormConfigure.Execute: Boolean;
var
  form : Tch2FormConfigure;
begin
  form := Tch2FormConfigure.Create(nil);
  try
    Result := IsPositiveResult(form.ShowModal);
  finally
    form.Free;
  end;
end;

procedure Tch2FormConfigure.FormCreate(Sender: TObject);
var
  gd : PGUIData;
  pd : PProviderData;
  i : IInterface;
  g : Ich2GUI absolute i;
  p : Ich2Provider absolute i;
begin
  for i in ch2Main.GUIs do
  begin
    New(gd);
    gd.GUI := g;
    with lv_GUI.Items.Add do
    begin
      Data := gd;
      Caption := g.GetName;
      SubItems.Add(g.GetDescription);
      Checked := ch2Main.CurrentGUI = g;
    end;
  end;


  for i in ch2Main.Providers do
  begin
    New(pd);
    pd.Provider := p;
    with lv_Provider.Items.Add do
    begin
      Data := pd;
      Caption := p.GetName;
      SubItems.Add(p.GetDescription);
    end;
  end;
end;

procedure Tch2FormConfigure.FormDestroy(Sender: TObject);
var
  i : TListItem;
begin
  for i in lv_GUI.Items do
    Dispose(PGUIData(i.Data));

  for i in lv_Provider.Items do
    Dispose(PProviderData(i.Data));
end;

procedure Tch2FormConfigure.lv_GUIChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  i : TListItem;
  cnt : Integer;
begin
  if Change=ctState then
  begin
    if Item.Checked then
    begin
      for i in lv_GUI.Items do
      begin
        if i <> Item then
          i.Checked := false;
      end;
    end
    else
    begin
      cnt := 0;
      for i in lv_GUI.Items do
      begin
        if i.Checked then
          inc(cnt)
      end;

      if cnt = 0 then
        Item.Checked := true;
    end;
  end;
end;

procedure Tch2FormConfigure.lv_ProviderCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
  pd1,
  pd2 : PProviderData;
begin
  pd1 := Item1.Data;
  pd2 := Item2.Data;

  //thats not a bug ... its descending ordering
  Compare := CompareValue(pd2^.Provider.GetPriority, pd1^.Provider.GetPriority);
end;

procedure Tch2FormConfigure.lv_ProviderDblClick(Sender: TObject);
var
  pd : PProviderData;
begin
  if Assigned(lv_Provider.Selected) then
  begin
    pd := lv_Provider.Selected.Data;
    pd^.Provider.Configure;
    lv_Provider.CustomSort(nil, 0);
  end;
end;

end.
