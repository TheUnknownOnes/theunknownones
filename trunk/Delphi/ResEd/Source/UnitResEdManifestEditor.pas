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

{$I ..\..\Common\Jedi\Jedi.inc}

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
    cbWin7Compatibility: TCheckBox;
    cbDPIAware: TCheckBox;
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

{$IFNDEF DELPHI2009_UP}
function UTF8ToString(const AString: UTF8String): String;
begin
  Result:=UTF8Decode(AString);
end;

function UTF8EncodeToShortString(const AString: UTF8String): ShortString;
begin
  Result:=UTF8Decode(AString);
end;
{$ENDIF}

{ TFormResEdManifestEditor }

procedure TFormResEdManifestEditor.EditManifestResource;
var
  xml: IXMLDOMDocument2;
  Node : IXMLDOMNode;
  aNode : IXMLDOMNode;
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
                                         'xmlns:b="urn:schemas-microsoft-com:asm.v3" '+
                                         'xmlns:c="urn:schemas-microsoft-com:compatibility.v1" '+
                                         'xmlns:d="http://schemas.microsoft.com/SMI/2005/WindowsSettings" ');

  Node:=xml.selectSingleNode('/a:assembly/a:description');
  if Assigned(Node) then
    Self.edAppDesc.Text:=UTF8ToString(Node.text);

  Node:=xml.selectSingleNode('/a:assembly/a:assemblyIdentity/@name');
  if Assigned(Node) then
    Self.edAppName.Text:=UTF8ToString(Node.text);

  Node:=xml.selectSingleNode('/a:assembly/b:trustInfo/b:security/b:requestedPrivileges/b:requestedExecutionLevel/@level');
  if Assigned(Node) then
    Self.cbREL.ItemIndex:=Self.cbREL.Items.IndexOf(Node.text);

  Node:=xml.selectSingleNode('/a:assembly/b:trustInfo/b:security/b:requestedPrivileges/b:requestedExecutionLevel/@uiAccess');
  if Assigned(Node) then
    Self.cbUIA.Checked:=AnsiSameText(Node.text,'True');

  Node:=xml.selectSingleNode('/a:assembly/c:compatibility/c:application/c:supportedOS[@Id="{35138b9a-5d96-4fbd-8e2d-a2440225f93a}"]');
  Self.cbWin7Compatibility.Checked:=Assigned(Node);

  Node:=xml.selectSingleNode('/a:assembly/b:application/b:windowsSettings/d:dpiAware');
  if Assigned(Node) then
    Self.cbDPIAware.Checked:=AnsiSameText(Node.text, 'True');

  if self.ShowModal = mrOk then
  begin
    Node:=xml.selectSingleNode('/a:assembly/a:description');
    if Assigned(Node) then
      Node.text:=UTF8EncodeToShortString(Self.edAppDesc.Text);

    Node:=xml.selectSingleNode('/a:assembly/a:assemblyIdentity/@name');
    if Assigned(Node) then
      Node.text:=UTF8EncodeToShortString(Self.edAppName.Text);

    Node:=xml.selectSingleNode('/a:assembly/b:trustInfo/b:security/b:requestedPrivileges/b:requestedExecutionLevel/@level');
    if Assigned(Node) then
      Node.text:=Self.cbREL.Text;

    Node:=xml.selectSingleNode('/a:assembly/b:trustInfo/b:security/b:requestedPrivileges/b:requestedExecutionLevel/@uiAccess');
    if Assigned(Node) then
      Node.text:=IfThen(Self.cbUIA.Checked,'True','False');

    {$REGION 'Win7Comp'}
    Node:=xml.selectSingleNode('/a:assembly/c:compatibility/c:application/c:supportedOS[@Id="{35138b9a-5d96-4fbd-8e2d-a2440225f93a}"]');
    if cbWin7Compatibility.Checked and (not Assigned(Node)) then
    begin
      Node:=xml.selectSingleNode('/a:assembly/c:compatibility');
      if not Assigned(Node) then
      begin
        Node:=xml.selectSingleNode('/a:assembly');

        Node:=Node.appendChild(xml.createNode('element','compatibility','urn:schemas-microsoft-com:compatibility.v1'));
      end;

      if Node.selectSingleNode('c:application')=nil then
      begin
        Node:=Node.appendChild(xml.createNode('element','application','urn:schemas-microsoft-com:compatibility.v1'));
      end
      else
      begin
        Node:=Node.selectSingleNode('c:application');
      end;

      Node:=Node.appendChild(xml.createNode('element','supportedOS','urn:schemas-microsoft-com:compatibility.v1'));
      aNode:=xml.createAttribute('Id');
      aNode.nodeValue:='{35138b9a-5d96-4fbd-8e2d-a2440225f93a}';
      Node.attributes.setNamedItem(aNode);
    end
    else
    if (not cbWin7Compatibility.Checked) and Assigned(Node) then
      Node.parentNode.removeChild(Node);
    {$ENDREGION}

    {$REGION 'dpiAware'}
    Node:=xml.selectSingleNode('/a:assembly/b:application/b:windowsSettings/d:dpiAware');
    if not Assigned(Node) then
    begin
      Node:=xml.selectSingleNode('/a:assembly/b:application');
      if not Assigned(Node) then
      begin
        Node:=xml.selectSingleNode('/a:assembly');

        Node:=Node.appendChild(xml.createNode('element','application','urn:schemas-microsoft-com:asm.v3'));
      end;

      if Node.selectSingleNode('b:windowsSettings')=nil then
      begin
        Node:=Node.appendChild(xml.createNode('element','windowsSettings','urn:schemas-microsoft-com:asm.v3'));
      end
      else
      begin
        Node:=Node.selectSingleNode('b:windowsSettings');
      end;

      Node:=Node.appendChild(xml.createNode('element','dpiAware','http://schemas.microsoft.com/SMI/2005/WindowsSettings'));
    end;
    Node.text:=IfThen(Self.cbDPIAware.Checked,'true','false');
    {$ENDREGION}

    manifest:=xml.xml;
    RD.Data.Clear;
    RD.Data.Write(PAnsiChar (manifest)^, Length (manifest))
  end;
end;

end.
