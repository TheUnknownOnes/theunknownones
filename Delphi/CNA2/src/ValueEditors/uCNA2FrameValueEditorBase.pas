unit uCNA2FrameValueEditorBase;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TypInfo;

type
  Tframe_ValueEditorBase = class(TFrame)
  protected
    function GetValue: Variant; virtual; abstract;
    procedure SetValue(const Value: Variant); virtual; abstract;
  public
    class function Handles(ATypeKind : TTypeKind) : Boolean; virtual;

    property Value : Variant read GetValue write SetValue;
  end;
  TValueEditorFrameClass = class of Tframe_ValueEditorBase;

implementation

{$R *.dfm}

{ Tframe_ValueEditorBase }

class function Tframe_ValueEditorBase.Handles(ATypeKind: TTypeKind): Boolean;
begin
  Result := false;
end;

end.
