unit uch2ProviderStaticWebsearch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uch2Main, Registry, StdCtrls, ExtCtrls, Spin;

type
  Tch2StatWebItem = class

  end;

  Tch2ProviderStaticWebsearch = class(TInterfacedObject, Ich2Provider)
  private
    FPriority : Integer;
  public
    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;

    {$REGION 'Ich2Provider'}
    function GetGUID : TGUID;
    function GetDescription : String;
    function GetName : String;

    procedure ProvideHelp(AKeyword : String; AGUI : Ich2GUI);
    procedure Configure;

    function GetPriority : Integer;
    {$ENDREGION}

  end;

  Tch2FormConfigStaticWebsearch = class(TForm)
    Panel1: TPanel;
    btn_Cancel: TButton;
    btn_OK: TButton;
    GroupBox1: TGroupBox;
    ed_Prio: TSpinEdit;
    Label1: TLabel;
    ColorBox1: TColorBox;
    ColorBox2: TColorBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

const
  Setttings_Value_Priority = 'Priority';

{ Tch2ProviderStaticWebsearch }

procedure Tch2ProviderStaticWebsearch.AfterConstruction;
var
  Reg : TRegistry;
begin
  inherited;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID], true) then
    begin
      if Reg.ValueExists(Setttings_Value_Priority) then
        FPriority := reg.ReadInteger(Setttings_Value_Priority)
      else
        FPriority := 0;

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure Tch2ProviderStaticWebsearch.BeforeDestruction;
var
  Reg : TRegistry;
begin
  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID], true) then
    begin
      Reg.WriteInteger(Setttings_Value_Priority, FPriority);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;

  inherited;
end;

procedure Tch2ProviderStaticWebsearch.Configure;
begin

end;

function Tch2ProviderStaticWebsearch.GetDescription: String;
begin
  Result := 'Open a webpage by the composition of your url and the keyword';
end;

function Tch2ProviderStaticWebsearch.GetGUID: TGUID;
const
  g : TGUID = '{DB121BA8-E497-4050-BD59-32CB7204B6CF}';
begin
  Result := g;
end;

function Tch2ProviderStaticWebsearch.GetName: String;
begin
  Result := 'Static Websearch';
end;

function Tch2ProviderStaticWebsearch.GetPriority: Integer;
begin
  Result := FPriority;
end;

procedure Tch2ProviderStaticWebsearch.ProvideHelp(AKeyword: String;
  AGUI: Ich2GUI);
var
  parent : pointer;
  idx : Integer;
begin

end;

initialization
  ch2Main.RegisterProvider(Tch2ProviderStaticWebsearch.Create as Ich2Provider);

end.
