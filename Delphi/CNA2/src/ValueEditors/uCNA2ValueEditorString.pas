unit uCNA2ValueEditorString;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uCNA2ValueEditors, StdCtrls,
  TypInfo;

type
  Tcna2ValueEditorString = class(Tcna2ValueEditor)
    ed_Value: TEdit;
  private
    { Private-Deklarationen }

  protected
    function GetValue : Variant; override;
  public
    function GetDesiredHeight : Integer; override;

    procedure Init(ATypeInfo : PTypeInfo;
                   AValue : Variant;
                   AExpressionEvaluation : Boolean); override;
    
    class function CanHandle(ATypeInfo : PTypeInfo) : Boolean; override;
  end;

implementation

{$R *.dfm}

{ Tcna2ValueString }

class function Tcna2ValueEditorString.CanHandle(ATypeInfo: PTypeInfo): Boolean;
begin
  Result := ATypeInfo.Kind in [tkString,
                               tkLString,
                               tkWString];
end;

function Tcna2ValueEditorString.GetDesiredHeight: Integer;
begin
  Result := ed_Value.Height;
end;

function Tcna2ValueEditorString.GetValue: Variant;
begin
  Result := ed_Value.Text;
end;

procedure Tcna2ValueEditorString.Init(ATypeInfo: PTypeInfo;
                                      AValue: Variant;
                                      AExpressionEvaluation : Boolean);
begin
  inherited;

  ed_Value.Text := AValue;
end;

end.
