unit uDesignOptionParser;

interface

uses
  Classes,
  SysUtils,
  uOptionParser,
  DesignIntf,
  DesignEditors;

procedure Register;

implementation

type
  TOptionParserDefaultEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const Prop: IProperty; var Continue: Boolean); override;

  end;

procedure Register;
begin
  RegisterComponents('TUO', [TOptionParser]);
  RegisterComponentEditor(TOptionParser, TOptionParserDefaultEditor);
end;

{ TOptionParserDefaultEditor }

procedure TOptionParserDefaultEditor.EditProperty(const Prop: IProperty;
  var Continue: Boolean);
begin
  if SameText(Prop.GetName, 'OptionDefs') then
  begin
    Prop.Edit;
    Continue := false;
  end;

end;

end.
