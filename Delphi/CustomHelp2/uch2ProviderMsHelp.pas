unit uch2ProviderMsHelp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Spin, ComCtrls, ToolWin, StdCtrls, ExtCtrls, uch2MSHelpServices,
  uch2Main;

type
  Tch2ProviderMSHelp = class(TInterfacedObject, Ich2Provider)
  private
    function GetNamespaceEnabled(ANamespace: String): Boolean;
    procedure SetNamespaceEnabled(ANamespace: String; AEnabled: Boolean);
  public
    class function GetNamespaces: IHxRegNamespaceList; static;

    {$REGION 'Ich2Provider'}
    function GetGUID : TGUID;
    function GetDescription : String;
    function GetName : String;

    procedure ProvideHelp(AKeyword : String; AGUI : Ich2GUI);

    procedure Configure;

    function GetPriority : Integer;
    {$ENDREGION}
  end;

  Tch2FormProviderMsHelp = class(TForm)
    GroupBox2: TGroupBox;
    lvNamespaces: TListView;
    Panel2: TPanel;
    GroupBox1: TGroupBox;
    ed_Prio: TSpinEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FProvider:Tch2ProviderMSHelp;
    procedure Init;
  public
    class procedure Execute(AProvider: Tch2ProviderMSHelp);
  end;

implementation

uses
  ActiveX, Registry;

const
  REG_KEY_NAMESPACES = 'Namespaces';
  REG_VALUE_ENABLED = 'Enabled';

{$R *.dfm}

{ Tch2ProviderMSHelp }

function Tch2ProviderMSHelp.GetNamespaceEnabled(ANamespace: String): Boolean;
var
  Reg : TRegistry;
begin
  inherited;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID]+'\'+REG_KEY_NAMESPACES+'\'+ANamespace, true) then
    begin
      if not Reg.ValueExists(REG_VALUE_ENABLED) then
      begin
        Reg.WriteBool(REG_VALUE_ENABLED, True);
      end;

      Result:=reg.ReadBool(REG_VALUE_ENABLED);

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

class function Tch2ProviderMSHelp.GetNamespaces: IHxRegNamespaceList;
begin
  Result := CoHxRegistryWalker.Create.RegisteredNamespaceList[''];
end;

procedure Tch2ProviderMSHelp.Configure;
begin
  Tch2FormProviderMsHelp.Execute(self);
end;

function Tch2ProviderMSHelp.GetDescription: String;
begin
  Result:='nothing to see here';
end;

function Tch2ProviderMSHelp.GetGUID: TGUID;
begin
  Result:=StringToGUID('{83582CD6-7C5C-4325-9BA0-4B0D97FB057F}');
end;

function Tch2ProviderMSHelp.GetName: String;
begin
  Result:='MsHelp 2.x'
end;

function Tch2ProviderMSHelp.GetPriority: Integer;
begin
  Result:=0;
end;

procedure Tch2ProviderMSHelp.ProvideHelp(AKeyword: String; AGUI: Ich2GUI);
begin

end;

procedure Tch2ProviderMSHelp.SetNamespaceEnabled(ANamespace: String;
  AEnabled: Boolean);
var
  Reg : TRegistry;
begin
  inherited;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID]+'\'+REG_KEY_NAMESPACES+'\'+ANamespace, true) then
    begin
      Reg.WriteBool(REG_VALUE_ENABLED, AEnabled);

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

{ Tch2FormProviderMsHelp }

class procedure Tch2FormProviderMsHelp.Execute(AProvider: Tch2ProviderMSHelp);
var
  Form : Tch2FormProviderMsHelp;
begin
  Form:=Tch2FormProviderMsHelp.Create(nil);
  try
    Form.FProvider:=AProvider;
    Form.Init;
    Form.ShowModal;
  finally
    Form.Free;
  end;
end;

procedure Tch2FormProviderMsHelp.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  idx: Integer;
begin
  for idx := 0 to lvNamespaces.Items.Count - 1 do
  begin
    FProvider.SetNamespaceEnabled(lvNamespaces.Items[idx].Caption,
                                  lvNamespaces.Items[idx].Checked);
  end;
end;

procedure Tch2FormProviderMsHelp.Init;
var
  EnumNamespaces : IEnumVariant;
  Namespaces: IHxRegNamespaceList;
  Namespace : OleVariant;
  NamespaceIntf : IHxRegNamespace;
  fetched : cardinal;

  li : TListItem;
begin
  Namespaces:=FProvider.GetNamespaces;
  EnumNamespaces:=Namespaces._NewEnum as IEnumVARIANT;
  lvNamespaces.Items.BeginUpdate;
  while EnumNamespaces.Next(1, Namespace, fetched)=S_OK do
  begin
    supports(Namespace, IHxRegNamespace, NamespaceIntf);
    li:=lvNamespaces.Items.Add;
    li.Caption:=NamespaceIntf.Name;
    li.SubItems.Add(NamespaceIntf.GetProperty(HxRegNamespaceDescription));
    li.Checked:=FProvider.GetNamespaceEnabled(li.Caption);
  end;
  lvNamespaces.Items.EndUpdate;
end;


initialization
  ch2Main.RegisterProvider(Tch2ProviderMSHelp.Create as Ich2Provider);


end.
