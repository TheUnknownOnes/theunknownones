//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit UnitResEdManifestEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, unitResourceXPManifests, unitResourceElement, StdCtrls,
  unitMSXML2_TLB, StrUtils;

type
  TFormResEdManifestEditor = class(TForm)
    edAppName: TEdit;
    Label1: TLabel;
    edAppDesc: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    btnOk: TButton;
    cbUIA: TCheckBox;
    cbREL: TComboBox;
  private
    FResourceElement: TResourceElement;
    { Private-Deklarationen }
  public
    property RD : TResourceElement read FResourceElement write FResourceElement;
    procedure EditManifestResource;
  end;

implementation

uses XMLIntf, ComObj;

{$R *.dfm}

{ TFormResEdManifestEditor }

procedure TFormResEdManifestEditor.EditManifestResource;
var
  xml: IXMLDOMDocument2;
  Node : IXMLDOMNode;
  ss : TStringStream;
  manifest : AnsiString;
begin
  xml:=CreateOleObject('MSXML2.DOMDocument.4.0') as IXMLDOMDocument2;
  xml.setProperty('NewParser', true);
  xml.setProperty('SelectionLanguage', 'XPath');
 
  xml.async:=False;

  ss:=TStringStream.Create('');
  RD.Data.Seek(0, soBeginning);
  RD.Data.SaveToStream(ss);
  xml.loadXML(ss.DataString);
  ss.Free;

  xml.setProperty('SelectionNamespaces', 'xmlns:a="urn:schemas-microsoft-com:asm.v1" '+
                                         'xmlns:b="urn:schemas-microsoft-com:asm.v3"');

  Node:=xml.selectSingleNode('/a:assembly/a:description');
  if Assigned(Node) then
    Self.edAppDesc.Text:=Node.text;

  Node:=xml.selectSingleNode('/a:assembly/a:assemblyIdentity/@name');
  if Assigned(Node) then
    Self.edAppName.Text:=Node.text;

  Node:=xml.selectSingleNode('//b:trustInfo/b:security/b:requestedPrivileges/b:requestedExecutionLevel/@level');
  if Assigned(Node) then
    Self.cbREL.ItemIndex:=Self.cbREL.Items.IndexOf(Node.text);

  Node:=xml.selectSingleNode('//b:trustInfo/b:security/b:requestedPrivileges/b:requestedExecutionLevel/@uiAccess');
  if Assigned(Node) then
    Self.cbUIA.Checked:=AnsiSameText(Node.text,'True');

  if self.ShowModal = mrOk then
  begin
    Node:=xml.selectSingleNode('/a:assembly/a:description');
    if Assigned(Node) then
      Node.text:=Self.edAppDesc.Text;

    Node:=xml.selectSingleNode('/a:assembly/a:assemblyIdentity/@name');
    if Assigned(Node) then
      Node.text:=Self.edAppName.Text;

    Node:=xml.selectSingleNode('//b:trustInfo/b:security/b:requestedPrivileges/b:requestedExecutionLevel/@level');
    if Assigned(Node) then
      Node.text:=Self.cbREL.Text;

    Node:=xml.selectSingleNode('//b:trustInfo/b:security/b:requestedPrivileges/b:requestedExecutionLevel/@uiAccess');
    if Assigned(Node) then
      Node.text:=IfThen(Self.cbUIA.Checked,'True','False');

    manifest:=xml.xml;
    RD.Data.Clear;
    RD.Data.Write(PAnsiChar (manifest)^, Length (manifest))
  end;
end;

end.
