//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uPNGImageList_Designers;

interface

uses
  Windows,
  Classes,
  DesignIntf,
  DesignEditors,
  VCLEditors,
  PropertyCategories,
  Graphics,
  uPNGCommon,
  SysUtils,
  TypInfo,
  uPNGImageList_FormPNGObjectList;


procedure Register;

implementation

type
  TPNGListProperty = class(TPropertyEditor)
  private
    function GetPNGList(out AList : TPNGObjectList) : Boolean;
  public
    procedure SetValue(const AValue : String); override;
    function GetValue : String; override;
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TPNGObjectList), nil, EmptyStr, TPNGListProperty);
end;

{ TPNGListProperty }

procedure TPNGListProperty.Edit;
var
  PNGList : TPNGObjectList;
begin
  if GetPNGList(PNGList) then
    if TForm_PNGObjectList.Execute(PNGList) then
      Modified;
end;

function TPNGListProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paDialog, paReadOnly];
end;

function TPNGListProperty.GetPNGList(out AList: TPNGObjectList): Boolean;
begin
  Result:=(GetPropType.Kind = tkClass) and
          (TObject(GetOrdValue) is TPNGObjectList);

  if Result then
    AList:=TPNGObjectList(GetOrdValue);
end;

function TPNGListProperty.GetValue: String;
var
  PNGList : TPNGObjectList;
begin
  if GetPNGList(PNGList) then
    Result:=Format('%d Images', [PNGList.Count])
  else
    Result:='Not assigned';
end;

procedure TPNGListProperty.SetValue(const AValue: String);
begin

end;

end.
